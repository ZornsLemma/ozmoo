from __future__ import print_function
import argparse
import atexit
import base64
import contextlib
import copy
import datetime
import hashlib
import os
import re
import subprocess
import sys

MAX_DFS_DISC_TITLE_LEN = 12
MAX_ADFS_DISC_TITLE_LEN = 19

# SFTODO: We should not refuse to build for Electron if the game won't fit on a sideways RAM only machine but will work on one with shadow RAM - obviously the loader should give a sensible message if the resulting game is run on an Electron without shadow RAM. I suspect Border Zone is an example of where this is a concern (albeit not quite the one just outlined) - it requires PAGE<=&1500 on the Electron because the build assumes we have to allocate 8K for mode 6, which means it *will* run fine on Electrons with PAGE=&E00, but it would refuse to run on an Electron with shadow RAM and PAGE=&1900 even though the game would (I assume) fit just fine (the BBC shadow executable needs PAGE<=&3100, and it would be higher if we weren't artificially capping it). I don't know if it's easy to fix this or not off the top of my head.


# https://stackoverflow.com/questions/299446/how-do-i-change-directory-back-to-my-original-working-directory-with-python
@contextlib.contextmanager
def cwd(path):
    oldpwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(oldpwd)


def make_temp(s, auto_delete=True):
    temp_file = os.path.join(temp_dir, s)
    if auto_delete:
        temp_files_to_delete.add(temp_file)
    return temp_file


def delete_temp_files():
    for filename in temp_files_to_delete:
        # We may have generated a temporary filename but never created the file
        # (e.g. due to a build failure).
        if os.path.isfile(filename):
            info("Deleting temp file %s" % filename, verbose_threshold=2)
            os.remove(filename)


def die(s):
    show_deferred_output()
    print("Error: %s" % s, file=sys.stderr)
    sys.exit(1)


def info(s, verbose_threshold=None):
    if verbose_threshold is None:
        verbose_threshold = 1
    if cmd_args.verbose_level >= verbose_threshold:
        if defer_output:
            deferred_output.append((False, s))
        else:
            print(s)


def warn(s):
    if defer_output:
        deferred_output.append((True, s))
    else:
        print("Warning: %s" % s, file=sys.stderr)


def show_deferred_output():
    global defer_output
    defer_output = False
    global deferred_output
    for is_warning, s in deferred_output:
        if is_warning:
            warn(s)
        else:
            info(s)
    deferred_output = []


def ourhex(i):
    assert i >= 0
    return hex(i)[2:].rstrip("L")


def page_le(i):
    return "PAGE<=&" + ourhex(i).upper()


def basic_hex_int(i):
    if i < 0:
        return "-&" + ourhex(-i).upper()
    else:
        return "&" + ourhex(i).upper()


def basic_int(i):
    as_decimal = str(i)
    as_hex = basic_hex_int(i)
    return as_decimal if len(as_decimal) < len(as_hex) else as_hex


def basic_string(value):
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, int):
        return basic_int(value)
    return value


def our_parse_int(s):
    if s.startswith("$") or s.startswith("&"):
        return int(s[1:], 16)
    if s.startswith("0x"):
        return int(s[2:], 16)
    return int(s)


def read_be_word(data, i):
    return data[i]*256 + data[i+1]


def write_le(data, i, v, n):
    while n > 0:
        data[i] = v & 0xff
        i += 1
        v >>= 8
        n -= 1


def init_cap(s):
    return s[0].upper() + s[1:]


def divide_round_up(x, y):
    if x % y == 0:
        return x // y
    else:
        return (x // y) + 1


def round_up(x, y):
    return divide_round_up(x, y) * y


def bytes_to_pages(x):
    return divide_round_up(x, bytes_per_page)


def format_kb(bytes):
    return ("%.2f" % (bytes / 1024.0)).rstrip("0").rstrip(".") + "K"


def pad_to(data, size):
    assert len(data) <= size
    return data + bytearray(size - len(data))


def pad_to_multiple_of(data, block_size):
    return pad_to(data, block_size * divide_round_up(len(data), block_size))


def disc_size(contents):
    return sum(bytes_to_pages(len(f.binary())) for f in contents)


def same_double_page_alignment(lhs, rhs):
    return lhs % 512 == rhs % 512


def double_sided_dfs():
    return cmd_args.double_sided and not cmd_args.adfs


def die_missing_executable(name):
    die("Can't execute '" + name + "'; is it on your PATH?")


def test_executable(name, quiet=False):
    try:
        child = subprocess.Popen([name], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        child.wait()
        return True
    except:
        if quiet:
            return False
        die_missing_executable(name)


def run_and_check(args, output_filter=None, warning_filter=None):
    if output_filter is None:
        output_filter = lambda x: True
    if warning_filter is None:
        warning_filter = lambda x: False
    if cmd_args.verbose_level >= 2:
        print(" ".join(args))
    child = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    child.wait()
    child_output = [line for line in child.stdout.readlines() if output_filter(line)]
    child_warnings = [line for line in child_output if warning_filter(line)]
    # The child's stdout and stderr will both be output to our stdout, but that's not
    # a big deal.
    something_wrong = child.returncode != 0 or len(child_warnings) > 0
    if (something_wrong or cmd_args.verbose_level >= 2) and len(child_output) > 0:
        output_to = sys.stderr if something_wrong else sys.stdout
        if cmd_args.verbose_level < 2:
            print(" ".join(args), file=output_to)
        print("".join(x.decode(encoding="ascii") for x in child_output), file=output_to)
    if child.returncode != 0:
        die("%s failed" % args[0])


# Check for the existence of a tool on PATH and its version. Return None if the
# tool can't be executed, otherwise return a tuple representing its version,
# with (0, 0) meaning we couldn't determine the version. This won't handle
# arbitrary tools but works well enough for beebasm and basictool, which is all
# we need.
def get_tool_version(name, version_finder=None):
    try:
        child = subprocess.Popen([name, "--help"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        child.wait()
    except:
        return None
    version = (0, 0)
    string_version = "unknown"
    for line in child.stdout.readlines():
        # Under rare circumstances (Fredrik experienced this running this script
        # in the context of Ozmoo Online, and it also occurs - at least with
        # Python 2 under Linux with bash - if you tee the output of
        # make-acorn.py to a file) sys.stdout.encoding may be None. In that case
        # we'll hope "utf-8" will work, as None will definitely fail.
        line = line.decode(sys.stdout.encoding if sys.stdout.encoding is not None else "utf-8")
        if version_finder is None:
            c = line.split()
            if len(c) >= 2 and c[0] == name:
                version_components = c[1].split(".")
                if len(version_components) >= 2:
                    version = tuple(int("0" + re.findall("^\d+", x)[0]) for x in version_components[:2])
                    string_version = line.strip()
                    break
        else:
            possible_version, possible_string_version = version_finder(line)
            if possible_version is not None:
                version = possible_version
                string_version = possible_string_version
                break
    global tool_versions
    if name not in string_version:
        string_version = name + " " + string_version
    tool_versions[name] = string_version
    return version


# Check to see if we have at least a minimum version of a tool. Return True if
# we do, if we don't then return False (if quiet) or die (if not quiet).
def check_tool_version(name, min_version, quiet=False, version_finder=None):
    our_version = get_tool_version(name, version_finder=version_finder)
    if our_version is None:
        if quiet:
            return False
        die_missing_executable(name)
    if our_version < min_version:
        if quiet:
            return False
        die("You need at least version %d.%02d of %s." % tuple(list(min_version) + [name]))
    return True


# Parse a line of acme --help output for the version
def acme_version_finder(line):
    i = line.find("release")
    if i == -1:
        return None, None
    line = line[i:]
    c = line.split()
    if len(c) >= 2:
        version_components = c[1].split(".")
        if len(version_components) >= 2:
            version = tuple(int("0" + re.findall("^\d+", x)[0]) for x in version_components[:2])
            string_version = c[1]
            return version, string_version
    return None, None


# Generate a relatively clear error message if we can't find one of our tools,
# rather than failing on a complex build command.
def prechecks():
    check_tool_version("acme", min_acme_version, version_finder=acme_version_finder)
    if cmd_args.force_beebasm:
        check_tool_version("beebasm", min_beebasm_version)
    elif cmd_args.force_basictool:
        check_tool_version("basictool", min_basictool_version)
    else:
        # We prefer basictool if it's available and the user isn't expressing a
        # preference, because the Advanced BASIC Editor's "pack" is much better
        # than the ad-hoc crunching implemented here for beebasm.
        if check_tool_version("basictool", min_basictool_version, quiet=True):
            cmd_args.force_basictool = True
        else:
            if check_tool_version("beebasm", min_beebasm_version, quiet=True):
                cmd_args.force_beebasm = True
            else:
                die("You need basictool %d.%02d (or newer) or beebasm %d.%02d (or newer) on your PATH to build this." %
                    (min_basictool_version + min_beebasm_version))


def apply_patch(patch, game_name):
    patch = patch.split(" ")
    def pop():
        return int(patch.pop(0),16)
    patch_address = pop()
    patch_check = pop()
    if read_be_word(game_data, patch_address) == patch_check:
        while len(patch) > 0:
            game_data[patch_address] = pop()
            patch_address += 1
    else:
        warn("Story file matches serial number and version for %s, but contents differ; failed to patch" % game_name)


def check_if_special_game():
    # SFTODO: The fact we use one option to control special game handling means
    # it isn't possible to build Trinity with the 80 column patch but without
    # having --x-for-examine forced on. Similar cases could potentially occur in
    # the future if this does other things. I don't think this is a huge concern
    # (Trinity is so big you need a co-pro to run it and the extra memory for the
    # --x-for-examine code is likely negligible) but it's slightly inelegant.
    if cmd_args.no_special_game_check:
        return

    # Don't force --x-for-examine for the benchmark game by default. The user
    # can explicitly specify it if they want to see the performance impact, but
    # for consistency with older benchmark times it seems fairer to keep it off
    # by default.
    if cmd_args.benchmark:
        return

    release = read_be_word(game_data, header_release)
    serial = game_data[header_serial:header_serial+6].decode("ascii")
    game_key = "r%d-s%s" % (release, serial)

    trinity_releases = {
	    "r11-s860509": "fddd 2058 01",
	    "r12-s860926": "fddd 2048 01",
	    "r15-s870628": "fd8d 2048 01"
    }
    # This (Ozmoo upstream) patch shortens a message to work better on 40 column
    # screens. The diff is approximately:
    #         PRINT           "  "
    # -L0012: PRINT           "[Press "
    # +L0012: PRINT           " [Cursor "
    #         JZ              L01 [TRUE] L0013
    #         PRINT           "UP"
    #         JUMP            L0014
    #  L0013: PRINT           "DOWN"
    # -L0014: PRINT           " arrow"
    # -L0015: PRINT           " to scroll]"
    # +L0014: PRINT_CHAR      ']'
    beyond_zork_releases = {
        "r47-s870915": "f347 14c2 00 a6 0b 64 23 57 62 97 80 84 a0 02 ca b2 13 44 d4 a5 8c 00 09 b2 11 24 50 9c 92 65 e5 7f 5d b1 b1 b1 b1 b1 b1 b1 b1 b1 b1 b1",
        "r49-s870917": "f2c0 14c2 00 a6 0b 64 23 57 62 97 80 84 a0 02 ca b2 13 44 d4 a5 8c 00 09 b2 11 24 50 9c 92 65 e5 7f 5d b1 b1 b1 b1 b1 b1 b1 b1 b1 b1 b1",
        "r51-s870923": "f2a8 14c2 00 a6 0b 64 23 57 62 97 80 84 a0 02 ca b2 13 44 d4 a5 8c 00 09 b2 11 24 50 9c 92 65 e5 7f 5d b1 b1 b1 b1 b1 b1 b1 b1 b1 b1 b1",
        "r57-s871221": "f384 14c2 00 a6 0b 64 23 57 62 97 80 84 a0 02 ca b2 13 44 d4 a5 8c 00 09 b2 11 24 50 9c 92 65 e5 7f 5d b1 b1 b1 b1 b1 b1 b1 b1 b1 b1 b1",
        "r60-s880610": "f2dc 14c2 00 a6 0b 64 23 57 62 97 80 84 a0 02 ca b2 13 44 d4 a5 8c 00 09 b2 11 24 50 9c 92 65 e5 7f 5d b1 b1 b1 b1 b1 b1 b1 b1 b1 b1 b1"
    }
    is_trinity = (z_machine_version == 4) and game_key in trinity_releases
    is_beyond_zork = (z_machine_version == 5) and game_key in beyond_zork_releases
    if is_trinity:
        info("Game recognised as 'Trinity'")
        # We don't patch if the game is only going to be run in 80 column modes.
        if cmd_args.max_mode > 3:
            apply_patch(trinity_releases[game_key], "Trinity")
    if is_beyond_zork:
        info("Game recognised as 'Beyond Zork'")
        if cmd_args.interpreter_num is None:
            cmd_args.interpreter_num = 2
        cmd_args.function_keys = True
        # Ozmoo command history doesn't work in Beyond Zork; this probably applies on the Commodore machines
        # too but I haven't been able to test it. I think this is because the cursor keys are specified as
        # terminating characters so they are passed to the game instead of being left for the Ozmoo read line
        # code to deal with. (frotz command history does work in Beyond Zork, FWIW, although I can't help
        # thinking Ozmoo's behaviour is more technically correct, not that I'm any expert here.) There's
        # therefore no point bloating the code with history support.
        cmd_args.no_history = True
        # We don't override the user's choice of --no-cursor-editing; this is just a matter of user preference,
        # both work in Beyond Zork the same as they would anywhere else.
        # We don't patch if the game is only going to be run in 80 column modes.
        if cmd_args.max_mode > 3:
            apply_patch(beyond_zork_releases[game_key], "Beyond Zork")

    # Automatic "X for examine" logic copied from upstream make.rb.
    x_for_examine_releases = {
        "r11-s860509": "Trinity",
        "r12-s860926": "Trinity",
        "r15-s870628": "Trinity",
        "r77-s850814": "AMFV",
        "r79-s851122": "AMFV",
        "r52-s871125": "Zork 1 SG"
    }
    if not cmd_args.x_for_examine:
        if z_machine_version < 4:
            try:
                serial_as_date = datetime.datetime.strptime(serial, "%y%m%d")
            except ValueError:
                info("Z-code version < 4 and serial is not a date, enabling --x-for-examine")
                cmd_args.x_for_examine = True
            else:
                if serial_as_date.year >= 1980 and serial_as_date.year < 1990:
                    info("Z-code version < 4 and serial is in 1980-1989, enabling --x-for-examine")
                    cmd_args.x_for_examine = True
        elif z_machine_version in (4, 5) and game_key in x_for_examine_releases:
            info("Z-code version 4 or 5 and serial and release match game without native X support, enabling --x-for-examine")
            cmd_args.x_for_examine = True


# common_labels contains the value of every label which had the same value in
# every build it existed in. The idea here is that we can use it to allow the
# loader access to labels without needing to duplicate values or trying to parse
# source files.
def update_common_labels(labels):
    for label, value in labels.items():
        common_value = common_labels.get(label, None)
        if common_value is None:
            common_labels[label] = value
        else:
            if value != common_value:
                del common_labels[label]


# SFTODO: Should this really derive from Exception?
class LoaderScreen(Exception):
    def __init__(self):
        loader_screen = LoaderScreen._get_title_page()
        # SFTODO: Since the loader screen might have been supplied by the user, we should probably not use assert to check for errors here.
        original_lines = [loader_screen[i:i+40] for i in range(0, len(loader_screen), 40)]
        assert len(original_lines) == 25
        sections = [[], [], []]
        section = 0
        self.space_line = None
        substitutions = {
            b"TITLE": cmd_args.title,
            b"SUBTITLE": str(cmd_args.subtitle),
            b"OZMOO": best_effort_version,
            b"SPACE": "${SPACE}", # preserve ${SPACE} when substituting
        }
        for i, line in enumerate(original_lines):
            line = LoaderScreen._to_nearly_ascii(line)
            is_unwanted_subtitle = b"${SUBTITLE}" in line and cmd_args.subtitle is None
            line = substitute(line, substitutions, lambda x: x.encode("ascii"))[:40]
            line = line.rstrip()
            if b"LOADER OUTPUT STARTS HERE" in line:
                section = 1
            elif b"LOADER OUTPUT ENDS HERE" in line:
                section = 2
            if is_unwanted_subtitle:
                # By shuffling the subtitle into the middle section whether it
                # appears in the header or footer, we can get rid of it without
                # breaking the line numbering.
                sections[1].append(line)
            else:
                sections[section].append(line)
        # Move the "LOADER OUTPUT ENDS HERE" line from the start of section 2 to
        # the end of section 1.
        sections[1].append(sections[2].pop(0))
        assert len(sections[1]) >= 13
        self.header = sections[0]
        self.footer = sections[2]

        self.footer_space_line = None
        for i, line in enumerate(self.footer):
            if b"${SPACE}" in line and self.footer_space_line is None:
                self.footer_space_line = i
                self.footer[i] = line.replace(b"${SPACE}", b"").rstrip()
        assert self.footer_space_line is not None

        self.normal_fg = 135
        self.header_fg = 131
        self.highlight_fg = 131
        self.highlight_bg = 129
        for line in sections[1]:
            def colour_code(c):
                if c == 32:
                    return 135
                if c >= 129 and c <= 135:
                    return c
                warn("Invalid colour code %d found in colour definitions on loader screen" % c)
                return 135
            if b"Normal foreground" in line:
                self.normal_fg = colour_code(line[0])
            elif b"Header foreground" in line:
                self.header_fg = colour_code(line[0])
            elif b"Highlight foreground" in line:
                self.highlight_fg = colour_code(line[0])
            elif b"Highlight background" in line:
                self.highlight_bg = colour_code(line[0])

    @staticmethod
    def _get_title_page():
        if cmd_args.custom_title_page is None:
            cmd_args.custom_title_page = "https://edit.tf/#0:GpPdSTUmRfqBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECAak91JNSZF-oECBAgQIECBAgQIECBAgQIECBAgQIECBAgQICaxYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsBpPdOrCqSakyL9QIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIEEyfBiRaSCfVqUKtRBTqQaVSmgkRaUVAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQTt_Lbh2IM2_llz8t_XdkQIECBAgQIECBAgQIECBAgQIECAHIy4cmXkgzb-WXPy39d2RAgQIECBAgQIECBAgQIECBAgQIAcjTn0bNOfR0QZt_LLn5b-u7IgQIECBAgQIECBAgQIECBAgCSNOfRs059HRBiw49eflv67siBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIEEyfBiRaSCfVqUKtRBFnRKaCRFpRUCBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAk906EGHF-oECBAgQIECBAgQIECBAgQIECBAgQIECBAgQICaxYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsB0N_fLyy5EGLygSe59qbPn_UCBAgQIECBAgQIECBAgQIECA"
        if cmd_args.custom_title_page.startswith("http"):
            return LoaderScreen._decode_edittf_url(cmd_args.custom_title_page)
        with open(cmd_args.custom_title_page, "rb") as f:
            data = f.read()
        if data.startswith(b"http"):
            with open(cmd_args.custom_title_page, "r") as f:
                return LoaderScreen._decode_edittf_url(f.readline())
        return bytearray(data[0:40*25])

    # SFTODO: Do I need to do the three character switches the OS performs automatically? We will be outputting the mode 7 header/footer using PRINT not direct memory access.
    @staticmethod
    def _decode_edittf_url(url):
        i = url.index("#")
        s = url[i+1:]
        i = s.index(":")
        s = s[i+1:]
        s += "===="
        packed_data = bytearray(base64.urlsafe_b64decode(s))
        unpacked_data = bytearray()
        buffer = 0
        buffer_bits = 0
        while len(packed_data) > 0 or buffer_bits > 0:
            if buffer_bits < 7:
                if len(packed_data) > 0:
                    packed_byte = packed_data.pop(0)
                else:
                    packed_byte = 0
                buffer = (buffer << 8) | packed_byte
                buffer_bits += 8
            byte = buffer >> (buffer_bits - 7)
            if byte < 32:
                byte += 128
            unpacked_data.append(byte)
            buffer &= ~(0b1111111 << (buffer_bits - 7))
            buffer_bits -= 7
        # SFTODO: At the moment if the edit.tf page contains double-height text the
        # user must make sure to duplicate it on both lines. We could potentially adjust
        # this automatically.
        return unpacked_data

    @staticmethod
    def _to_nearly_ascii(line):
        # Teletext data is 7-bit, but the way the Acorn OS handles mode 7 means
        # we must use top-bit-set codes for codes 0-31. (We could use
        # top-bit-set codes for everything, but we generate more compact BASIC
        # code if we prefer 7-bit characters; Python 3 makes it painful to embed
        # 8-bit characters directly in our BASIC code and it is slighty iffy
        # anyway.)
        line = bytearray([x & 0x7f for x in line])
        return bytearray([x | 0x80 if x < 32 else x for x in line])

    @staticmethod
    def _data_to_basic(data):
        basic = []
        for i, line in enumerate(data):
            if len(line) == 0:
                basic.append("PRINT")
            else:
                s = ""
                in_quote = False
                for b in line:
                    if b >= 128+32:
                        b -= 128
                    if b == ord('"') or b >= 128:
                        if in_quote:
                            s += '";'
                            in_quote = False
                        s += "CHR$%d" % b
                    else:
                        if not in_quote:
                            s += ';"'
                            in_quote = True
                        s += chr(b)
                if in_quote:
                    s += '"'
                if s[0] == ";":
                    s = s[1:]
                if len(line) == 40 or i == len(data)-1:
                    s += ";"
                basic.append("PRINT" + s)
        return "\n".join(basic)

    def add_loader_symbols(self, loader_symbols):
        # Symbols for the BBC loader screen
        loader_symbols["NORMAL_FG"] = basic_int(self.normal_fg)
        loader_symbols["HEADER_FG"] = basic_int(self.header_fg)
        loader_symbols["HIGHLIGHT_FG"] = basic_int(self.highlight_fg)
        loader_symbols["HIGHLIGHT_BG"] = basic_int(self.highlight_bg)
        loader_symbols["HEADER"] = LoaderScreen._data_to_basic(self.header)
        loader_symbols["FOOTER"] = LoaderScreen._data_to_basic(self.footer)
        loader_symbols["FOOTER_Y"] = basic_int(25 - len(self.footer))
        loader_symbols["MIDDLE_START_Y"] = basic_int(len(self.header))
        loader_symbols["SPACE_Y"] = basic_int((25 - len(self.footer)) + self.footer_space_line)
        # Symbols for the Electron loader screen
        loader_symbols["TITLE"] = cmd_args.title[:40]
        if cmd_args.subtitle is not None:
            loader_symbols["SUBTITLE"] = cmd_args.subtitle[:40]
        loader_symbols["OZMOO"] = ("Powered by " + best_effort_version)[:40]
        # Other symbols
        if cmd_args.no_sd_card_reset:
            loader_symbols["NO_SD_CARD_RESET"] = basic_int(1)
        if not cmd_args.no_tube_cache:
            loader_symbols["CACHE_START_ADDR"] = basic_hex_int(cache_start_addr)


class GameWontFit(Exception):
    pass


class DiscFull(Exception):
    pass


class DfsImage(object):
    bytes_per_sector = 256
    sectors_per_track = 10
    bytes_per_track = sectors_per_track * bytes_per_sector
    tracks = 80
    bytes_per_surface = tracks * bytes_per_track

    def __init__(self, contents, title, boot_option = 3): # 3 = *EXEC
        self.data = bytearray(2 * DfsImage.bytes_per_sector)
        sectors = DfsImage.tracks * DfsImage.sectors_per_track
        self.data[0x107] = sectors & 0xff
        self.data[0x106] = ((sectors >> 8) & 0x3) | (boot_option << 4)
        title = title[:MAX_DFS_DISC_TITLE_LEN]
        title = pad_to(bytearray(title, "ascii"), MAX_DFS_DISC_TITLE_LEN)
        self.data[0x000:0x008] = title[:8]
        self.data[0x100:0x104] = title[8:]
        for f in contents:
            self.add_file(f)

    def _num_files(self):
        return self.data[0x105] // 8

    def first_free_sector(self):
        return len(self.data) // DfsImage.bytes_per_sector

    def add_file(self, f):
        b = f.binary()
        if len(self.data) + DfsImage.bytes_per_sector * divide_round_up(len(b), DfsImage.bytes_per_sector) > DfsImage.bytes_per_surface:
            raise DiscFull()
        self._add_to_catalogue("$", f.leafname, f.load_addr, f.exec_addr, len(b), self.first_free_sector())
        self.data += pad_to_multiple_of(b, DfsImage.bytes_per_sector)

    def _add_to_catalogue(self, directory, name, load_addr, exec_addr, length, start_sector):
        assert self._num_files() < 31
        assert len(directory) == 1
        assert len(name) <= 7
        self.data[0x105] += 1*8
        self.data[0x010:0x100] = self.data[0x008:0x0f8]
        self.data[0x110:0x200] = self.data[0x108:0x1f8]
        name = (name + " "*7)[:7] + directory
        self.data[0x008:0x010] = bytearray(name, "ascii")
        self.data[0x00f] |= 128 # lock the file
        write_le(self.data, 0x108, load_addr, 2)
        write_le(self.data, 0x10a, exec_addr, 2)
        write_le(self.data, 0x10c, length, 2)
        self.data[0x10e] = (
                (((exec_addr >> 16) & 0x3) << 6) |
                (((length >> 16) & 0x3) << 4) |
                (((load_addr >> 16) & 0x3) << 2) |
                ((start_sector >> 8) & 0x3))
        self.data[0x10f] = start_sector & 0xff

    def add_pad_file(self, predicate):
        pad_start_sector = self.first_free_sector()
        pad_length_sectors = 0
        while not predicate(pad_start_sector + pad_length_sectors):
            pad_length_sectors += 1
        if pad_length_sectors > 0:
            self.add_file(File("PAD", 0, 0, bytearray(DfsImage.bytes_per_sector * pad_length_sectors)))

    @staticmethod
    def write_ssd(image, filename):
        data = image.data
        if cmd_args.pad:
            data = pad_to(data, DfsImage.bytes_per_surface)
        with open(filename, "wb") as f:
            f.write(data)

    @staticmethod
    def write_dsd(image0, image2, filename):
        data0 = image0.data
        data2 = image2.data
        with open(filename, "wb") as f:
            for track in range(DfsImage.tracks):
                i = track * DfsImage.bytes_per_track
                if not cmd_args.pad and i >= len(data0) and i >= len(data2):
                    break
                f.write(pad_to(data0[i:i+DfsImage.bytes_per_track], DfsImage.bytes_per_track))
                f.write(pad_to(data2[i:i+DfsImage.bytes_per_track], DfsImage.bytes_per_track))


class AdfsImage(object):
    bytes_per_sector = 256
    sectors_per_track = 16
    bytes_per_track = sectors_per_track * bytes_per_sector
    tracks = 80
    bytes_per_surface = tracks * bytes_per_track

    UNLOCKED = 0
    LOCKED = 1
    SUBDIRECTORY = 2

    def __init__(self, contents, title, boot_option = 3): # 3 = *EXEC
        self.catalogue = []
        self.data = bytearray(AdfsImage.bytes_per_sector * 2)
        self.total_sectors = AdfsImage.tracks * AdfsImage.sectors_per_track
        if cmd_args.double_sided:
            self.total_sectors *= 2
        write_le(self.data, 0xfc, self.total_sectors, 2)
        self.data[0x1fd] = boot_option
        self.data += AdfsImage._make_directory("$", title)
        self.md5 = hashlib.md5()
        for f in contents:
            self.add_file(f)

    def add_file(self, f):
        b = f.binary()
        start_sector = self._add_object_data(b)
        self.catalogue.append([f.leafname, f.load_addr, f.exec_addr, len(b), start_sector, AdfsImage.LOCKED])
        self.md5.update(b)

    def add_directory(self, name):
        start_sector = self._add_object_data(self._make_directory(name))
        self.catalogue.append([name, 0, 0, 0x500, start_sector, AdfsImage.SUBDIRECTORY | AdfsImage.LOCKED])

    def _add_object_data(self, object_data):
        start_sector = len(self.data) // AdfsImage.bytes_per_sector
        self.data += pad_to_multiple_of(object_data, AdfsImage.bytes_per_sector)
        if len(self.data) > self.total_sectors * AdfsImage.bytes_per_sector:
            raise DiscFull()
        return start_sector

    @staticmethod
    def _make_directory(name, title=None):
        if title is None:
            title = name
        data = bytearray(5 * AdfsImage.bytes_per_sector)
        data[0x001:0x005] = b"Hugo"
        data[0x005] = 0 # number of catalogue entries
        if name == "$":
            data[0x4cc] = ord("$")
        else:
            data[0x4cc:0x4d6] = (name.encode("ascii") + b"\r" + b"\0"*10)[:10]
        data[0x4d9:0x4ec] = (title.encode("ascii") + b"\r" + b"\0"*MAX_ADFS_DISC_TITLE_LEN)[:MAX_ADFS_DISC_TITLE_LEN]
        data[0x4d6] = 2 # parent is always $ in this code
        data[0x4fb:0x4ff] = b"Hugo"
        assert data[0] == data[0x4fa]
        return data

    def _finalise(self):
        first_free_sector = len(self.data) // 256
        write_le(self.data, 0, first_free_sector, 3)
        free_space_len = self.total_sectors - first_free_sector
        write_le(self.data, 0x100, free_space_len, 3)
        self.data[0x1fe] = 3 * 1 # number of free space map entries
        self.data[0x205] = len(self.catalogue)
        self.catalogue = sorted(self.catalogue, key=lambda x: x[0])
        for i, entry in enumerate(self.catalogue):
            offset = 0x205 + i * 0x1a
            self.data[offset:offset+10] = (entry[0].encode("ascii") + bytearray(10))[:10]
            attributes = entry[5]
            self.data[offset] |= 128 # read
            if not (attributes & self.SUBDIRECTORY):
                self.data[offset+1] |= 128 # write
            self.data[offset+2] |= 128 # locked
            if attributes & self.SUBDIRECTORY:
                self.data[offset+3] |= 128
            write_le(self.data, offset+ 0xa, entry[1], 4) # load addr
            write_le(self.data, offset+ 0xe, entry[2], 4) # exec addr
            write_le(self.data, offset+0x12, entry[3], 4) # length
            write_le(self.data, offset+0x16, entry[4], 3) # start_sector
        self.md5.update(self.data[0:0x700])
        # Use a "random" disc ID which won't vary gratuitously from run to run.
        self.data[0x1fb:0x1fd] = self.md5.digest()[0:2]
        self.data[0xff] = AdfsImage._checksum(self.data[0:0xff])
        self.data[0x1ff] = AdfsImage._checksum(self.data[0x100:0x1ff])

    def _get_write_data(self):
        self._finalise()
        if cmd_args.pad:
            return pad_to(self.data, self.total_sectors * AdfsImage.bytes_per_sector)
        return self.data

    @staticmethod
    def _checksum(data):
        c = 0
        s = 0
        for b in data[::-1]:
            s += b + c
            c = (s & 0x100) >> 8
            s = s & 0xff
        return s

    def write_adf(self, filename):
        with open(filename, "wb") as f:
            f.write(self._get_write_data())

    def write_adl(self, filename):
        data = self._get_write_data()
        max_track = min(divide_round_up(len(data), AdfsImage.bytes_per_track), AdfsImage.tracks)
        with open(filename, "wb") as f:
            for track in range(max_track):
                for surface in range(2):
                    i = (surface*AdfsImage.tracks + track) * AdfsImage.bytes_per_track
                    f.write(pad_to(data[i:i+AdfsImage.bytes_per_track], AdfsImage.bytes_per_track))


class File(object):
    def __init__(self, leafname, load_addr, exec_addr, contents):
        self.leafname = leafname
        self.surface = 0
        self.load_addr = load_addr
        self.exec_addr = exec_addr
        self.contents = contents

    # This is called binary() so we have the same interface as Executable.
    def binary(self):
        return self.contents


# SFTODO: In a few places I am doing set(args) - this is fine if all the elements stand alone like "-DFOO=1", but if there are multi-element entries ("--setpc", "$0900") I will need to do something different. I am not sure if this will be an issue or not. I should maybe switch to making the args a set in the first place.
class Executable(object):
    # Things could get confusing if an output_name is accidentally re-used, so
    # we track them in here and assert that each output_name is new. This
    # wouldn't work if we didn't have caching in make_ozmoo_executable(), since
    # an identical rebuild should generate the same output name.
    all_output_names = set()

    def __init__(self, asm_filename, leafname, version_maker, start_addr, args):
        self.asm_filename = asm_filename
        self.leafname = leafname
        self.surface = 0 # may be overridden later if we're building double-sided DFS
        self.version_maker = version_maker
        self.start_addr = start_addr
        self.load_addr = start_addr
        if leafname not in ("TURBO", "OZMOO2P"):
            self.load_addr |= host
        self.exec_addr = self.load_addr
        self.args = args
        self._relocations = None
        self._binary = None
        output_name = os.path.splitext(os.path.basename(asm_filename))[0].replace("-", "_")
        if version_maker is not None:
            output_name += "_" + version_maker(start_addr, args)
        else:
            output_name += "_" + ourhex(start_addr)
        assert output_name not in Executable.all_output_names
        Executable.all_output_names.add(output_name)
        self._labels_filename = make_temp("acme_labels_" + output_name)
        self._report_filename = make_temp("acme_report_" + output_name)
        self._asm_output_filename = make_temp(output_name)

        with cwd(os.path.join(ozmoo_base_dir, "asm")):
            def up(path):
                return os.path.join("..", path)
            cpu = "65c02" if "-DCMOS=1" in args else "6502"
            def acme_output_filter(line):
                game_wont_fit_str = b"GameWontFit"
                i = line.find(game_wont_fit_str)
                if i != -1:
                    raise GameWontFit(line[i+len(game_wont_fit_str)+2:].strip())
                return True
            run_and_check(["acme", "--cpu", cpu, "--format", "plain", "--setpc", "$" + ourhex(start_addr)] + self.args + ["-l", up(self._labels_filename), "-r", up(self._report_filename), "--outfile", up(self._asm_output_filename), asm_filename], acme_output_filter, lambda x: x.startswith(b"Warning"))

        self.labels = self._parse_labels()
        with open(self._asm_output_filename, "rb") as f:
            self._asm_output = bytearray(f.read())
        if "ACORN_RELOCATABLE" in self.labels:
            self.truncate_at("reloc_count")
        update_common_labels(self.labels)

    def _parse_labels(self):
        labels = {}
        with open(self._labels_filename, "r") as f:
            for line in f.readlines():
                line = line[:-1]
                components = line.split("=")
                value = components[1].strip()
                i = value.find(";")
                if i != -1:
                    value = value[:i]
                labels[components[0].strip()] = int(value.strip().replace("$", "0x"), 0)
        return labels

    def rebuild_at(self, start_addr):
        return Executable(self.asm_filename, self.leafname, self.version_maker, start_addr, self.args)

    def add_loader_symbols(self, symbols):
        if cmd_args.adfs or cmd_args.nfs_install_only:
            symbols[self.leafname + "_BINARY"] = self.leafname
        else:
            symbols[self.leafname + "_BINARY"] = ":%d.$.%s" % (self.surface, self.leafname)

    def truncate_at(self, label):
        self._asm_output = self._asm_output[:self.labels[label]-self.labels["program_start"]]

    def _make_relocations(self):
        assert "ACORN_RELOCATABLE" in self.labels
        # Ideally other_start_addr will be different from self.start_addr,
        # otherwise we can't generate any relocations. We use 0xe00 because a)
        # it's the lowest possible useful address for non-tube builds (tube
        # builds aren't and don't need to be relocatable) b) it matches OSHWM on
        # a Master, which means the labels/report for this assembly can be used
        # directly when debugging. a) means that it doesn't matter if
        # other_start_addr == self.start_addr, because we will simply generate a
        # zero-length list of relocations and we're no worse off as a result
        # because we can't run any lower anyway.
        other_start_addr = 0xe00
        if "ACORN_RELOCATE_WITH_DOUBLE_PAGE_ALIGNMENT" in self.labels:
            other_start_addr += (self.start_addr - other_start_addr) % 0x200
        assert other_start_addr <= self.start_addr
        other = self.rebuild_at(other_start_addr)
        assert other is not None
        return Executable._diff(other._asm_output, self._asm_output)

    @staticmethod
    def _diff(alternate, master):
        assert len(alternate) == len(master)
        expected_delta = None
        relocations = []
        for i in range(len(master)):
            if master[i] != alternate[i]:
                this_delta = alternate[i] - master[i]
                if expected_delta is None:
                    expected_delta = this_delta
                else:
                    assert this_delta == expected_delta
                relocations.append(i)
        if len(relocations) == 0:
            return bytearray([0, 0])
        assert relocations[0] != 0 # we can't encode this
        delta_relocations = []
        last_relocation = 0
        for relocation in relocations:
            delta_relocation = relocation - last_relocation
            last_relocation = relocation
            assert delta_relocation > 0
            # We need to encode the delta_relocation as an 8-bit byte. We use 0 to mean
            # 'move 255 bytes along but don't perform a relocation'.
            while delta_relocation >= 256:
                delta_relocations.append(0)
                delta_relocation -= 255
            assert delta_relocation > 0
            delta_relocations.append(delta_relocation)
        count = len(delta_relocations)
        return bytearray([count & 0xff, count >> 8] + delta_relocations)

    def binary(self):
        if self._binary is not None:
            return self._binary
        if "ACORN_RELOCATABLE" in self.labels:
            if self._relocations is None:
                self._relocations = self._make_relocations()
            binary = self._asm_output + self._relocations
        else:
            binary = self._asm_output
        # A second processor binary *could* extend past 0x8000 but in practice
        # it won't come even close.
        assert self.start_addr + len(binary) <= 0x8000
        self._binary = binary
        return binary


class OzmooExecutable(Executable):
    def __init__(self, leafname, start_addr, args):
        def version_maker(start_addr, args):
            if "-DACORN_ELECTRON_SWR=1" in args:
                s = "electron_swr"
            else:
                if "-DACORN_SWR=1" in args:
                    s = "bbc_swr"
                    if "-DACORN_SCREEN_HOLE=1" not in args:
                        s += "_shr"
                else:
                    s = "tube"
            if "-DVMEM=1" not in args:
                s += "_novmem"
            if "-DACORN_SWR_SMALL_DYNMEM=1" in args:
                s += "_smalldyn"
            elif "-DACORN_SWR_MEDIUM_DYNMEM=1" in args:
                s += "_mediumdyn"
            s += "_" + ourhex(start_addr)
            return s

        if cmd_args.preload_opt and "-DVMEM=1" in args:
            args += ["-DPREOPT=1"]
        Executable.__init__(self, "ozmoo.asm", leafname, version_maker, start_addr, args)
        if "ACORN_RELOCATABLE" not in self.labels:
            self.truncate_at("end_of_routines")
        self.swr_dynmem = 0
        if "VMEM" in self.labels:
            self._patch_vmem()

    def _patch_vmem(self):
        # We use a strict greater than comparison in the next line because the
        # *Ozmoo code* fits fine with data_start equal to the start of screen
        # RAM. Whether we can get away with no main RAM free for dynamic memory
        # is another matter, handled by subsequent tests here and at run time.
        if "ACORN_SWR" in self.labels and self.labels["data_start"] > (0x8000 - self.min_screen_hole_size()):
            # The Ozmoo code itself won't fit in memory below the screen RAM.
            # This is most likely if --max-mode=0 is specified by the user; in
            # other modes there's usually enough room with PAGE=&E00.
            raise GameWontFit("not enough free RAM for Ozmoo code")

        # Can we fit the nonstored pages into memory?
        nonstored_pages_up_to = self.labels["story_start"] + nonstored_pages * bytes_per_page
        if nonstored_pages_up_to > self.max_pseudo_ramtop():
            raise GameWontFit("not enough free RAM for game's dynamic memory")
        if "ACORN_SWR" in self.labels:
            # SFTODO: This (and its symbol passed through to loader.bas and its use in loader.bas) is now a bit inconsistent - it is not "dynmem in SWR" in all cases. We only actually use it for medium and big models, and different ways for each of them. Maybe rename it something generic and just comment in the loader what it means in the model we're looking at.
            if "ACORN_SWR_SMALL_DYNMEM" in self.labels:
                self.swr_dynmem = 0 # not used
            elif "ACORN_SWR_MEDIUM_DYNMEM" in self.labels:
                self.swr_dynmem = nonstored_pages_up_to - 0x8000
                assert 0 <= self.swr_dynmem <= 16 * 1024
            else:
                assert "ACORN_SWR_BIG_DYNMEM" in self.labels
                self.swr_dynmem = nonstored_pages * bytes_per_page

        # On a second processor build, we must also have at least
        # min_vmem_blocks for swappable memory. For sideways RAM builds we need
        # to check at run time if we have enough main/sideways RAM for swappable
        # memory.
        if "ACORN_SWR" not in self.labels:
            nsmv_up_to = nonstored_pages_up_to + min_vmem_blocks * bytes_per_vmem_block
            if nsmv_up_to > self.max_pseudo_ramtop():
                raise GameWontFit("not enough free RAM for any swappable memory")

        # Generate initial virtual memory map. We just populate the entire table; if the
        # game is smaller than this we will just never use the other entries.
        vmap_z_l_offset = self.labels['initial_vmap_z_l'] - self.labels['program_start']
        vmap_z_h_offset = self.labels['vmap_z_h'        ] - self.labels['program_start']
        vmap_max_size = self.labels['vmap_max_size']
        assert self._asm_output[vmap_z_l_offset:vmap_z_l_offset+vmap_max_size] == b'V'*vmap_max_size
        assert self._asm_output[vmap_z_h_offset:vmap_z_h_offset+vmap_max_size] == b'V'*vmap_max_size
        blocks = cmd_args.preload_config[:] if cmd_args.preload_config is not None else []
        for i in range(vmap_max_size):
            if i not in blocks:
                blocks.append(i)
        blocks = blocks[:vmap_max_size]
        # We use 0 as an "invalid" address in vmap; since a Z-address &0000xx will always
        # be in dynamic memory, we will never get to the point of trying to load this
        # block via the virtual memory code. In practice it shouldn't matter but we always
        # specify a timestamp of 0 (the oldest possible timestamp) when using invalid_addr
        # in a vmap entry, so the entry will be replaced by something useful ASAP in the
        # unexpected case that the entry is used at runtime.
        # SFTODO: Probably fine, but (after, if I do, I tinker with the sort
        # code during init preload and the use of dummy entries there) this
        # might be better changed.
        invalid_addr = 0
        invalid_timestamp = 0
        for i, block_index in enumerate(blocks):
            timestamp = int(max_timestamp + ((float(i) / vmap_max_size) * (min_timestamp - max_timestamp))) & ~vmem_highbyte_mask
            if cmd_args.preload_opt:
                # Most of the vmap will be ignored, but we have to have at least one entry
                # and by making it an invalid addr we don't need to worry about loading
                # any "suggested" blocks.
                addr, timestamp = invalid_addr, invalid_timestamp
            else:
                addr = (nonstored_pages + block_index * pages_per_vmem_block) >> 1
            if ((addr >> 8) & ~vmem_highbyte_mask) != 0:
                # This vmap entry is useless; the current Z-machine version can't contain
                # such a block.
                # SFTODO: Warn? It's harmless but it means we could have clawed back a few
                # bytes by shrinking vmap_max_size.
                addr, timestamp = invalid_addr, invalid_timestamp
            vmap_entry = (timestamp << 8) | addr
            self._asm_output[vmap_z_l_offset + i] = vmap_entry & 0xff
            self._asm_output[vmap_z_h_offset + i] = (vmap_entry >> 8) & 0xff

    # SFTODO: This function is perhaps slightly mis-named. It returns the
    # smallest screen hole we can have for this build *ignoring* the prospect
    # that we might actually have shadow RAM even though we can cope without
    # it. It should do the right thing, it is just the name that's slightly
    # less than ideal.
    def min_screen_hole_size(self):
        if "ACORN_SCREEN_HOLE" in self.labels:
            adjusted_max_mode = min(cmd_args.max_mode, 6) if "ACORN_ELECTRON_SWR" in self.labels else cmd_args.max_mode
            return 0x8000 - himem_by_mode(adjusted_max_mode)
        return 0

    def max_pseudo_ramtop(self):
        if "ACORN_SWR" in self.labels:
            result = 0x8000 if "ACORN_SWR_SMALL_DYNMEM" in self.labels else 0xc000
            if "ACORN_SWR_MEDIUM_DYNMEM" not in self.labels:
                result -= self.min_screen_hole_size()
            return result
        else:
            return self.labels["flat_ramtop"]

    def max_nonstored_pages(self):
        return (self.max_pseudo_ramtop() - self.labels["story_start"]) // bytes_per_page

    # Return the size of the binary, ignoring any relocation data (which isn't
    # important for the limited use we make of the return value).
    def size(self):
        return len(self._asm_output)

    def rebuild_at(self, start_addr):
        return make_ozmoo_executable(self.leafname, start_addr, self.args)

    def add_loader_symbols(self, symbols):
        Executable.add_loader_symbols(self, symbols)
        symbols[self.leafname + "_MAX_PAGE"] = basic_int(self.start_addr)
        symbols[self.leafname + "_RELOCATABLE"] = "TRUE" if "ACORN_RELOCATABLE" in self.labels else "FALSE"
        symbols[self.leafname + "_SWR_DYNMEM_MODEL"] = "0" if "ACORN_SWR_SMALL_DYNMEM" in self.labels else "1" if "ACORN_SWR_MEDIUM_DYNMEM" in self.labels else "2" if "ACORN_SWR_BIG_DYNMEM" in self.labels else "-1"
        symbols[self.leafname + "_SWR_DYNMEM"] = basic_int(self.swr_dynmem)
        # We need *_SWR_MAIN_RAM_FREE because we can't infer *_SWR_MAIN_RAM_FREE
        # by comparing MAX_PAGE with actual PAGE, because we cap the value of
        # MAX_PAGE and there may be RAM free after the executable even when
        # PAGE=MAX_PAGE. We use 0x8000 in the calculation here as the loader will
        # apply an adjustment for the actual screen RAM size; it seems silly to
        # have both this code and the loader apply compensating adjustments for
        # min_screen_hole_size().
        if "ACORN_SWR_SMALL_DYNMEM" in self.labels:
            nonstored_pages_up_to = self.labels["story_start"] + nonstored_pages * bytes_per_page
            swr_main_ram_free = 0x8000 - nonstored_pages_up_to
            assert swr_main_ram_free >= 0
            # Because story_start is 512 byte-aligned and nonstored_pages is rounded up to be
            # a multiple of pages_per_vmem_block (2), swr_main_ram_free will always be a multiple
            # of 512. This isn't hugely important, but it makes reasoning about memory in the
            # loader slightly more comfortable.
            assert swr_main_ram_free % 512 == 0
            symbols[self.leafname + "_SWR_MAIN_RAM_FREE"] = basic_int(swr_main_ram_free)
        elif "ACORN_SWR_MEDIUM_DYNMEM" in self.labels:
            symbols[self.leafname + "_SWR_MAIN_RAM_FREE"] = basic_int(0x8000 - self.labels["vmem_start"])
        elif "ACORN_SWR_BIG_DYNMEM" in self.labels:
            # Here we assume we have the maximum 16K of sideways RAM available
            # for dynamic memory. (If we don't, the loader will take that into
            # account.) We model the sideways RAM being used in preference to
            # main RAM for dynamic memory, so as to leave as much main RAM free
            # for screen RAM as possible. (As always, we are just modelling
            # things so we can predict whether the executable will crash when we
            # run it and avoid doing that. The executable itself is responsible
            # for allocating RAM in reality.)
            min_main_ram_used = max(nonstored_pages * bytes_per_page - 0x4000, 0)
            swr_main_ram_free = (0x8000 - self.labels["story_start"]) - min_main_ram_used
            assert swr_main_ram_free >= 0
            symbols[self.leafname + "_SWR_MAIN_RAM_FREE"] = basic_int(swr_main_ram_free)

    def binary(self):
        # It's important to check self._binary isn't None so we don't compress
        # something we already compressed.
        if self._binary is not None:
            return self._binary
        binary = Executable.binary(self)
        if not cmd_args.no_exe_compression:
            binary_filename = make_temp("binary")
            with open(binary_filename, "wb") as f:
                f.write(binary)
            compressed_binary_filename = make_temp("binary.lzsa2")
            safe_distance = compress_lzsa(binary_filename, compressed_binary_filename, [])
            new_load_addr = self.load_addr + safe_distance
            extra_args = ["-DDECOMPRESS_TO=$%x" % (self.load_addr & 0xffff)]
            if self.leafname == "OZMOO2P":
                extra_args += ["-DTUBE=1"]
                # I can't see any way to get acme to generate a hex version of the
                # DECOMPRESS_TO value, so do it like this.
                with open(make_temp("go.asm"), "w") as f:
                    f.write('!text "GO %X", 13' % (self.load_addr & 0xffff))
            e = Executable("acorn-binary-lzsa.asm", "X", None, new_load_addr & 0xffff, extra_args)
            # print("QXX", self.leafname, hex(self.load_addr), hex(new_load_addr), hex(len(e.binary())))
            # The executable mustn't load into memory at 0x8000 or above as
            # there's no RAM there (except on a second processor). We'd *prefer*
            # it to fit below the screen memory to avoid ugly loading, but
            # that's not essential. If this happens *_max_start_addr can be
            # tweaked to try to prevent it, although if PAGE is high enough, it
            # may be unavoidable, particularly on the Electron with no shadow
            # RAM.
            assert (new_load_addr & 0xffff) + len(e.binary()) <= 0x8000
            self.load_addr = new_load_addr
            self.exec_addr = new_load_addr + os.path.getsize(compressed_binary_filename)
            binary = e.binary()
        self._binary = binary
        return self._binary


def make_ozmoo_executable(leafname, start_addr, args, report_failure_prefix = None):
    try:
        # It's not really necessary to include leafname in cache_key, but let's
        # play it safe.
        cache_key = (leafname, start_addr, tuple(args))
        e = make_ozmoo_executable._cache.get(cache_key)
        if e is not None:
            return e
        e = OzmooExecutable(leafname, start_addr, args)
        make_ozmoo_executable._cache[cache_key] = e
        return e
    except GameWontFit as e:
        if report_failure_prefix is not None:
            warn("Game is too large for %s: %s" % (report_failure_prefix, str(e)))
        return None
make_ozmoo_executable._cache = {}


# Build an Ozmoo executable which loads at whichever of initial_start_addr and
# initial_start_addr+256 gives the least wasted space. Because Ozmoo uses
# 512-byte alignment internally while PAGE has 256-byte alignment, one of these
# builds will have 256 bytes less of padding before data_start - that's the one
# we want to use, as it avoids wasting memory if PAGE happens to have the right
# 512-byte alignment, and if PAGE has the opposite alignment we will "waste" 256
# bytes before program_start to get the right alignment instead of wasting 256
# bytes on internal alignment, so we're no worse off. (The space below PAGE is
# not necessarily wasted either, since it may be used as shadow RAM cache on
# some machines.)
def make_optimally_aligned_executable(leafname, initial_start_addr, args, report_failure_prefix):
    base_executable = make_ozmoo_executable(leafname, initial_start_addr, args, report_failure_prefix)
    if base_executable is None:
        return None
    alternate_executable = make_ozmoo_executable(leafname, initial_start_addr + 256, args)
    # If alternate_executable is None:
    # - We know base_executable has the the optimal 512-byte alignment, otherwise
    #   there's no reason alternate_executable failed to build.
    # - Even if base_executable could have sub-optimal alignment (which it can't),
    #   we prefer a sub-optimal successful build to an optimal failed one.
    if alternate_executable is not None and alternate_executable.size() < base_executable.size():
        return alternate_executable
    return base_executable


# Build an Ozmoo executable which loads at the highest possible address; we pick
# an address which means it will work on machines with relatively high values of
# PAGE if possible. The executable will relocate itself down if PAGE isn't as
# high as the worst case we assume here.
def make_highest_possible_executable(leafname, args, report_failure_prefix):
    assert "-DACORN_RELOCATABLE=1" in args
    assert "-DACORN_SWR=1" in args

    e_low = make_optimally_aligned_executable(leafname, 0xe00, args, report_failure_prefix)
    # If we can't build successfully with a start of 0xe00 we can't ever manage
    # it.
    if e_low is None:
        return None
    assert e_low.start_addr in (0xe00, 0xf00)
    # There's no point loading really high, and doing a totally naive
    # calculation may cause us to load so high there's no room for the
    # relocation data before &8000, so we never load much higher than
    # max_start_addr.
    # SFTODO: This logic is duplicated in make_best_model_executable(), we
    # should pull max_start_addr() out as a function.
    max_start_addr = electron_max_start_addr if "-DACORN_ELECTRON_SWR=1" in args else bbc_max_start_addr
    if not same_double_page_alignment(e_low.start_addr, max_start_addr):
        max_start_addr += 256
    if "-DACORN_SWR_MEDIUM_DYNMEM=1" not in args:
        surplus_nonstored_pages = e_low.max_nonstored_pages() - nonstored_pages
        assert surplus_nonstored_pages >= 0
        assert surplus_nonstored_pages % 2 == 0
        max_start_addr = min(e_low.start_addr + surplus_nonstored_pages * bytes_per_page, max_start_addr)
    else:
        main_ram_vmem = 0x8000 - e_low.labels["vmem_start"]
        main_ram_vmem -= e_low.min_screen_hole_size()
        assert main_ram_vmem >= 0
        assert main_ram_vmem % (2 * bytes_per_page) == 0
        max_start_addr = min(e_low.start_addr + main_ram_vmem, max_start_addr)

    # If we build at max_start_addr, we might end up with data_start so high it
    # overlaps screen RAM in builds which support no-shadow configurations. I
    # added this code because I thought it was needed; in practice this
    # situation never occurs now I've fixed the original bug, but I think in
    # principle this is a risk, so having written the code I'll leave it in with
    # an assert so I know if it ever gets triggered.
    data_start_high = e_low.labels["data_start"] + (max_start_addr - e_low.start_addr)
    min_himem = 0x8000 - e_low.min_screen_hole_size()
    data_start_excess = max(data_start_high - min_himem, 0)
    # SFTODONOW TCO assert data_start_excess == 0 # just to see if this ever occurs, if it does do a sanity check and then delete the assert so following code can fix things up
    data_start_excess = 512 * divide_round_up(data_start_excess, 512)
    max_start_addr -= data_start_excess
    assert max_start_addr >= e_low.start_addr

    assert same_double_page_alignment(max_start_addr, e_low.start_addr)
    e = make_ozmoo_executable(leafname, max_start_addr, args, report_failure_prefix)
    assert e is None or e.max_nonstored_pages() >= nonstored_pages
    return e


# SFTODO: Since the Electron executable handles both shadow and non-shadow machines, *if* even a big model executable will require shadow RAM, should we retry small and medium models on the assumption we *do* have shadow RAM before settling on big model? I don't know if this case is possible, though without thinking too carefully I suspect it is possible but relatively unlikely.
def make_best_model_executable(leafname, args, report_failure_prefix):
    # Calculate adjusted_small_dynmem_page_threshold; it doesn't make sense to refuse to
    # build using the small model because it requires assuming PAGE>=max_start_addr.
    if "-DACORN_ELECTRON_SWR=1" in args:
        max_start_addr = electron_max_start_addr
    else:
        max_start_addr = bbc_max_start_addr
    adjusted_small_dynmem_page_threshold = min(small_dynmem_page_threshold, max_start_addr)

    small_e = None
    if (not (cmd_args.force_medium_dynmem or cmd_args.force_big_dynmem)) and ("-DACORN_SCREEN_HOLE=1" not in args or cmd_args.try_support_32k):
        small_e = make_highest_possible_executable(leafname, args + small_dynmem_args, None)
        # Some systems may have PAGE too high to run small_e, but those systems
        # would be able to run the game if built with the big dynamic memory model.
        # small_dynmem_page_threshold determines whether we're willing to prevent a system
        # running the game in order to get the benefits of the small dynamic memory
        # model.
        if small_e is not None:
            if small_e.start_addr >= adjusted_small_dynmem_page_threshold:
                info(init_cap(report_failure_prefix) + " executable uses small dynamic memory model and requires " + page_le(small_e.start_addr))
                return small_e
        msg = init_cap(report_failure_prefix) + " executable can't use small dynamic memory model as it "
        if small_e is None:
            info(msg + "won't fit even with PAGE=&E00")
        else:
            info(msg + "would require " + page_le(small_e.start_addr))

    # We don't do a medium build for the shadow executable. The medium memory
    # model doesn't have that big an advantage over the big model when we have
    # shadow RAM - the main one I've seen is the possibly artificial one from
    # reducing the amount of copying via bounce buffer when we're thrashing the
    # disc on machines which are tight on RAM. By not using the medium model, we
    # avoid *requiring* at least one bank of sideways RAM, which is useful as a
    # B+ or Integra-B actually has a fair bit of RAM (up to 19K of spare shadow
    # RAM in mode 7 and the private 12K) available even if it has no sideways
    # RAM. SFTODO: Is that entirely true? I think the basic point is sound, but the advantage only exists if the machine happens to be able to fit dynmem in main RAM with its particular PAGE. OK, I think on a B+ the private 12K *is* acceptable, but on an Integra-B we will insist on one bank of real SWR as the first 1K of private 12K is used by IBOS and this makes it unsuitable for dynmem. - I SHOULD PERHAPS DO SOME TIMINGS OF MEDDYN VS BIGDYN BEFORE REWRITING THIS COMMENT, AND IT MAY ALSO FORCE ME TO RECONSIDER
    medium_e = None
    if cmd_args.force_medium_dynmem or ("-DACORN_SCREEN_HOLE=1" in args and not cmd_args.force_big_dynmem):
        if nonstored_pages * bytes_per_page <= 16 * 1024:
            medium_e = make_highest_possible_executable(leafname, args + medium_dynmem_args, None)
            if medium_e is not None:
                info(init_cap(report_failure_prefix) + " executable uses medium dynamic memory model and requires " + page_le(medium_e.start_addr))
                return medium_e
        else:
            info(init_cap(report_failure_prefix) + " executable can't use medium dynamic memory model as the game's dynamic memory is >16K")
            if cmd_args.force_medium_dynmem:
                return None

    # Note that we don't respect small_dynmem_page_threshold when generating a big
    # dynamic memory executable; unlike the above decision about whether or not
    # to use the small dynamic memory model, we're not trading off performance
    # against available main RAM - if a system has PAGE too high to run the big
    # dynamic memory executable we generate, it just can't run the game at all
    # and there's nothing we can do about it.
    big_e = make_highest_possible_executable(leafname, args, report_failure_prefix)
    if big_e is not None:
        info(init_cap(report_failure_prefix) + " executable uses big dynamic memory model and requires " + page_le(big_e.start_addr))
    return big_e


# If cmd_args.extra_build_at is not None, rebuild e at that address. This
# executable isn't used any further, but it's useful for debugging to be able to
# force it to happen in order to have acme output for the actual address the
# code will run at after relocation and/or introduction of shadow RAM cache.
def extra_build_wrapper(e):
    if e is not None and cmd_args.extra_build_at is not None:
        e.rebuild_at(cmd_args.extra_build_at)
    return e


def make_electron_swr_executable():
    leafname = "OZMOOE"
    args = ozmoo_base_args + swr_args + relocatable_args + ["-DACORN_ELECTRON_SWR=1", "-DACORN_SCREEN_HOLE=1"]
    # SFTODO: Not sure if this is a good idea or not - it will slightly harm performance on some machines. If it *does* stay, factor out the duplicate code with make_shr_swr_executable.
    if not cmd_args.no_shadow_vmem:
        args += ["-DACORN_SHADOW_VMEM=1", "-DACORN_RECOMMENDED_SHADOW_CACHE_PAGES=%d" % cmd_args.recommended_shadow_cache_pages]
    return extra_build_wrapper(make_best_model_executable(leafname, args, "Electron"))


def make_bbc_swr_executable():
    leafname = "OZMOOB"
    args = ozmoo_base_args + swr_args + relocatable_args + bbc_args + ["-DACORN_SCREEN_HOLE=1"]
    return extra_build_wrapper(make_best_model_executable(leafname, args, "BBC B sideways RAM"))


def make_shr_swr_executable():
    leafname = "OZMOOSH"
    args = ozmoo_base_args + swr_args + relocatable_args + bbc_args
    if not cmd_args.no_shadow_vmem:
        args += ["-DACORN_SHADOW_VMEM=1", "-DACORN_RECOMMENDED_SHADOW_CACHE_PAGES=%d" % cmd_args.recommended_shadow_cache_pages]
    return extra_build_wrapper(make_best_model_executable(leafname, args, "shadow+sideways RAM"))


def make_tube_executables():
    leafname = "OZMOO2P"
    args = ozmoo_base_args + tube_args
    # The next check just saves a build which clearly can't succeed; the threshold could
    # be tightened up a bit but the more "realistic" the threshold the more chance there
    # is of subsequent code changes optimising things more than this code expects and
    # missing out on a chance to use a no-vmem build.
    if game_pages <= 64 * 4:
        tube_no_vmem = make_ozmoo_executable(leafname, tube_start_addr, args)
        if game_pages <= tube_no_vmem.max_nonstored_pages():
            info("Game is small enough to run without virtual memory on second processor")
            return [tube_no_vmem]
    args += vmem_args
    if not cmd_args.no_tube_cache:
        args += ["-DACORN_TUBE_CACHE=1"]
        args += ["-DACORN_TUBE_CACHE_MIN_TIMESTAMP=%d" % min_timestamp]
        args += ["-DACORN_TUBE_CACHE_MAX_TIMESTAMP=%d" % max_timestamp]
    # We don't pay attention to cmd_args.extra_build_at here; tube builds have a fixed
    # address anyway.
    tube_vmem = make_ozmoo_executable(leafname, tube_start_addr, args, "second processor")
    if tube_vmem is None:
        return None
    info("Game will be run using virtual memory on second processor")
    if cmd_args.no_tube_cache:
        return [tube_vmem]
    return [make_cache_executable(), tube_vmem]


def make_shaddrv_executable():
    args = ["-DACORN_SHADOW_VMEM=1"]
    if cmd_args.no_integra_b_private_ram:
       args += ["-DACORN_IGNORE_INTEGRA_B_PRIVATE_RAM=1"]
    e = Executable("acorn-shadow-driver.asm", "SHADDRV", None, 0x900, args)
    assert e.start_addr + len(e.binary()) <= 0xb00
    # SFTODO: Is putting these not-strictly-common things into common_labels a hack?
    common_labels.update({k:v for (k,v) in e.labels.items() if k.startswith("shadow_state") or k == "private_ram_in_use"})
    return e


def make_findswr_executable():
    return Executable("acorn-findswr.asm", "FINDSWR", None, 0x900, ["-DACORN_SWR=1"])


def make_insv_executable():
    # SFTODO: At the moment this lives at &A00; this doesn't conflict with FINDSWR because
    # the loader runs FINDSWR first. This feels like a shame; it wouldn't be too hard to
    # move the data FINDSWR leaves in page &9 into pages &4/5 and then pages &9/&A would
    # (ignoring this code) be free during gameplay and might be usable as vmem cache or
    # something. However, right now &A00 is free and I need somewhere to put this code.
    # It used to live in the buffers for sound channels 0-2 but it's too big since I added
    # Copy key support; it's just possible it could be squeezed down if I dropped some
    # niceties like preserving A on exit even when Copy is pressed, but I'm not too
    # optimistic. I thought I could put it in the CFS/RFS workspace at &380 but IBOS
    # uses that.
    workspace_start = 0x900
    workspace_end = 0xb00
    e = Executable("acorn-insv.asm", "INSV", None, workspace_start, ["-DUSE_HISTORY=1"])
    init = e.labels['init']
    assert e.start_addr + len(e.binary()) <= workspace_end
    e.exec_addr = host | init
    return e


def make_fast_hw_scroll_executable():
    # SFTODO: Location of this is an utter hack right now
    workspace_end = himem_by_mode(6)
    workspace_start = workspace_end - 0x400
    e = Executable("acorn-scroll.asm", "FASTSCR", None, workspace_start, ["-DACORN_HW_SCROLL_FAST=1", "-DACORN_SHADOW_VMEM=1"])
    init = e.labels['init']
    assert e.start_addr + len(e.binary()) <= workspace_end
    e.exec_addr = host | init
    loader_symbols["fast_scroll_load_addr"] = basic_int(workspace_start)
    return e


def make_turbo_test_executable():
    return Executable("acorn-turbo-test.asm", "TURBO", None, 0xd0, turbo_supported_args)


def make_cache_executable():
    # The load address here is chosen to be as high as possible while still
    # satisfying the assertion in acorn-cache.asm that there's a minimum amount
    # of free main RAM available for cache.
    return Executable("acorn-cache.asm", "CACHE2P", None, cache_start_addr, relocatable_args + ["-DACORN_TUBE_CACHE=1", "-DACORN_SWR=1", "-DACORN_SHADOW_VMEM=1"])


def make_boot():
    # SFTODO: Get rid of the VDU 21/VDU 6 stuff? I'm trying to hide the mildly
    # disconcerting error from running TURBO on a non-turbo second processor, but
    # maybe this is asking for trouble if there's a disc error or something of
    # the sort. On the other hand, the *EXEC file will plough on regardless if
    # any error occurs (other than a read error in !BOOT itself) and the MODE 135
    # before running LOADER will hide errors anyway.
    boot = [
        '*BASIC',
        'VDU 21',
        '*DIR $',
        '*FX21'
    ]
    if not cmd_args.no_tube and not cmd_args.no_turbo:
        # SFTODO: I don't really like this, but since running TURBO leaves us at the
        # supervisor prompt if run on a non-turbo second processor, I don't see much
        # alternative to running it from !BOOT in this way. (We can't just try
        # setting the turbo bit at &FEF0 in the loader and testing for turbo-style
        # paging, because the ReCo6502Mini uses that address for its speed control.)
        boot += [
            'IF PAGE<&E00 THEN */TURBO',
            '*BASIC',
        ]
    if cmd_args.splash_image:
        boot += [
            'VDU 6:CHAIN "PRELOAD"'
        ]
    else:
        boot += [
            'VDU 6:MODE 135',
            'CHAIN "LOADER"',
        ]
    return File("!BOOT", 0, 0, "\r".join(boot).encode("ascii") + b"\r")


# The build file does not contain all the possibly useful information, but it's
# a start. Examples of information not included:
# - whether basictool or beebasm was used to tokenise BASIC (although if crunching
#   is not disabled, it's easy to infer which was used by looking at the loader)
# - the version of basictool/beebasm/acme used for the build
def make_build_file():
    # In an attempt to avoid the minor privacy leak of including pathnames on the
    # build system in the build file, and to keep the size down, we convert them
    # to just the basename.
    data = ""
    for name, version in sorted(tool_versions.items()):
        data += "%s\r" % version
    for arg in sys.argv:
        if arg.startswith('-'):
            i = arg.find('=')
            if i != -1:
                arg = arg[:i+1] + os.path.basename(arg[i+1:])
            data += arg
        else:
            data += os.path.basename(arg)
        data += "\r"
    return File("BUILD", 0, 0, data.encode("ascii"))


def substitute_text(s, d, f):
    return substitute(s.encode("ascii"), {k.encode("ascii"): v.encode("ascii") for k, v in d.items()}, lambda x: f(x.decode("ascii")).encode("ascii")).decode("ascii")


def substitute(s, d, f):
    c = re.split(b"(\$\{|\})", s)
    result = b""
    i = 0
    while i < len(c):
        if c[i] == b"${":
            k = bytes(c[i+1])
            if k not in d:
                die("Unknown substitution: " + k.decode("ascii"))
            result += f(d[k])
            assert c[i+2] == b"}"
            i += 3
        else:
            result += c[i]
            i += 1
    return result


def crunch_line(line, crunched_symbols):
    def crunch_symbol(symbol):
        if symbol == "":
            return ""
        crunched_symbol = crunched_symbols.get(symbol, None)
        if crunched_symbol is None:
            i = len(crunched_symbols)
            crunched_symbol = ""
            while True:
                crunched_symbol += chr(ord("a") + (i % 26))
                i //= 26
                if i == 0:
                    break
            crunched_symbols[symbol] = crunched_symbol
        return crunched_symbol
    symbol = ""
    result = ""
    in_quote = False
    for c in line:
        if c == '"':
            in_quote = not in_quote
        if not in_quote and ((c >= "a" and c <="z") or c == "_"):
            symbol += c
        else:
            result += crunch_symbol(symbol) + c
            symbol = ""
    result += crunch_symbol(symbol)
    return result


def make_text_basic(template, symbols):
    # This isn't all that user-friendly and it makes some assumptions about what
    # the BASIC code will look like. I think this is OK, as it's not a general
    # tool - it's specifically designed to work with the Ozmoo loader.
    with open(template, "r") as f:
        if_results = []
        loader = []
        crunched_symbols = {}
        for line in f.readlines():
            line = line[:-1].strip()
            i = line.find(":REM ")
            if i == -1:
                i = line.find("REM ")
            if i != -1:
                line = line[:i]
            i = line.find("\\")
            if i != -1:
                # \ comments are terminated by a :, but we know we won't use that.
                line = line[:i]
            if line in ("", ":", "REM"):
                pass
            elif line.startswith("!ifdef") or line.startswith("!ifndef"):
                c = line.split(" ")
                assert len(c) == 3
                assert c[2] == "{"
                if_results.append(c[1] in symbols)
                if line.startswith("!ifndef"):
                    if_results[-1] = not if_results[-1]
            elif line.startswith("}"):
                assert len(if_results) > 0
                c = line.split(" ")
                if len(c) == 1:
                    if_results.pop(-1)
                elif len(c) == 3:
                    assert c[1] == "else" and c[2] == "{"
                    if_results[-1] = not if_results[-1]
                else:
                    assert False
            elif all(if_results):
                line = substitute_text(line, symbols, basic_string)
                if not cmd_args.no_loader_crunch and cmd_args.force_beebasm:
                    line = crunch_line(line, crunched_symbols)
                loader.append(line)
    return "\n".join(loader) + "\n"


def make_tokenised_basic(name, text_basic):
    filename_text = make_temp("%s.bas" % name)
    with open(filename_text, "w") as f:
        f.write(text_basic)
    if cmd_args.force_beebasm:
        filename_beebasm = make_temp("%s.beebasm" % name)
        filename_ssd = make_temp("%s.ssd" % name)
        with open(filename_beebasm, "w") as f:
            f.write('putbasic "%s", "%s"\n' % (filename_text, name.upper()))
        run_and_check([
            "beebasm",
            "-i", filename_beebasm,
            "-do", filename_ssd
        ], lambda x: b"no SAVE command" not in x)
        # Since it's the only file on the .ssd, we can get the tokenised BASIC
        # simply by chopping off the first two sectors. We peek the length out
        # of one of those sectors first.
        with open(filename_ssd, "rb") as f:
            tokenised_basic = bytearray(f.read())
            length = ((((tokenised_basic[0x10e] >> 4) & 0x3) << 16) |
                    (tokenised_basic[0x10d] << 8) | tokenised_basic[0x10c])
            tokenised_basic = tokenised_basic[512:512+length]
    else:
        filename_tokenised = make_temp("%s.tok" % name)
        run_and_check([
            "basictool",
            "-ts" if cmd_args.no_loader_crunch else "-tsp",
            filename_text,
            filename_tokenised
        ])
        with open(filename_tokenised, "rb") as f:
            tokenised_basic = f.read()
    return File(name.upper(), host | 0x1900, host | 0x8023, tokenised_basic)


def make_tokenised_cache_test():
    symbols = {}
    symbols.update({k: basic_string(v) for k, v in common_labels.items()})
    cache_text_basic = make_text_basic(os.path.join(ozmoo_base_dir, "templates", "cache-test.bas"), symbols)
    return make_tokenised_basic("cachtst", cache_text_basic)


def make_tokenised_loader(symbols):
    # TODO: Maybe some/all of the population of symbols should be moved into this function?
    if cmd_args.splash_image is not None:
        symbols["SPLASH"] = "1"
    symbols.update({k: basic_string(v) for k, v in common_labels.items()})
    symbols["MIN_VMEM_BYTES"] = basic_int(min_vmem_blocks * bytes_per_vmem_block)
    symbols["RECOMMENDED_SHADOW_CACHE_PAGES"] = basic_int(cmd_args.recommended_shadow_cache_pages)
    loader_text_basic = make_text_basic(os.path.join(ozmoo_base_dir, "templates", "loader.bas"), symbols)
    return make_tokenised_basic("loader", loader_text_basic)


def splash_screen_address():
    return himem_by_mode(cmd_args.splash_mode)

def himem_by_mode(mode):
    return {
        0: 0x3000,
        1: 0x3000,
        2: 0x3000,
        3: 0x4000,
        4: 0x5800,
        5: 0x5800,
        6: 0x6000,
        7: 0x7c00}[mode]


def mode_colours(mode):
    return {
       0: 2,
       1: 4,
       2: 16,
       3: 2,
       4: 2,
       5: 4,
       6: 2,
       7: 2}[mode]


def splash_mode_colours():
    return mode_colours(cmd_args.splash_mode)


def make_tokenised_preloader(loader, splash_start_addr):
    symbols = {
        "splash_mode": basic_string(cmd_args.splash_mode),
        "splash_max_colour": basic_string(splash_mode_colours() - 1),
        "splash_start_address": basic_string(splash_start_addr),
        "loader_size": basic_string(len(loader.binary()))
    }
    if cmd_args.splash_wait != 0:
        symbols["splash_wait"] = basic_string(cmd_args.splash_wait * 100)
    if cmd_args.splash_palette is None:
        symbols["set_splash_palette"] = "VDU 20"
    else:
        symbols["set_splash_palette"] = "VDU " + "".join("19,%d,%d;0;" % (i, j) for i, j in enumerate(cmd_args.splash_palette))
    preloader_text_basic = make_text_basic(os.path.join(ozmoo_base_dir, "templates", "preloader.bas"), symbols)
    return make_tokenised_basic("preload", preloader_text_basic)


def compress_lzsa(input_filename, output_filename, extra_args):
    test_executable("lzsa")
    safe_distance_list = [None]
    def lzsa_filter(line):
        if line.startswith(b"Safe distance:"):
            i = line.index(b":")
            safe_distance_list[0] = int(line[i+1:].split(b"(")[0])
        return True
    run_and_check(["lzsa", "-v", "-f", "2", "-r"] + extra_args + ["--prefer-ratio", input_filename, output_filename], output_filter=lzsa_filter)
    return safe_distance_list[0]


def make_splash_executable():
    if splash_screen_address() + os.path.getsize(cmd_args.splash_image) > 0x8000:
        die("Splash image is too large; is it a raw mode %d screen dump?" % cmd_args.splash_mode)
    # Do a trial build of acorn-splash.asm with a zero-length file to determine
    # the size of the machine code; this is a bit OTT, but why not?
    compressed_data_filename = make_temp("splash.lzsa2")
    with open(compressed_data_filename, "wb") as f:
        pass
    e = Executable("acorn-splash.asm", "SPLASH", None, 0x1000, ["-DSPLASH_SCREEN_ADDRESS=$1000"])
    splash_code_size = len(e.binary())
    safe_distance = compress_lzsa(cmd_args.splash_image, compressed_data_filename, ["-b"])
    splash_start_addr = 0x8000 - os.path.getsize(compressed_data_filename) - safe_distance - splash_code_size
    return Executable("acorn-splash.asm", "SPLASH", None, splash_start_addr, ["-DSPLASH_SCREEN_ADDRESS=$%x" % splash_screen_address()])


def title_from_filename(filename, remove_the_if_longer_than):
    title = os.path.basename(os.path.splitext(filename)[0])
    # This logic has been copied from make.rb.
    camel_case = re.search("[a-z]", title) and re.search("[A-Z]", title) and not re.search(" |_", title)
    if camel_case:
        title = re.sub("([a-z])([A-Z])", r"\1 \2", title)
        title = re.sub("A([A-Z])", r"A \1", title)
    title = re.sub("_+", " ", title)
    title = re.sub("(^ +)|( +)$", "", title)
    if remove_the_if_longer_than is not None and len(title) > remove_the_if_longer_than:
        title = re.sub("^(the|a) (.*)$", r"\2", title, flags=re.IGNORECASE)
    if re.search("^[a-z]", title):
        title = title.capitalize()
    if remove_the_if_longer_than is not None and len(title) > remove_the_if_longer_than:
        title = title[:remove_the_if_longer_than]
    return title


def parse_args():
    parser = argparse.ArgumentParser(description="Build an Acorn disc image to run a Z-machine game using %s." % (best_effort_version,))
    # SFTODO: Might be good to add an option for setting -DUNSAFE=1 for maximum performance, but I probably don't want to be encouraging that just yet.
    if version_txt is not None:
        parser.add_argument("--version", action="version", version=best_effort_version)
    parser.add_argument("-v", "--verbose", action="count", help="be more verbose about what we're doing (can be repeated)")
    parser.add_argument("-2", "--double-sided", action="store_true", help="generate a double-sided disc image (implied if IMAGEFILE has a .dsd or .adl extension)")
    parser.add_argument("-a", "--adfs", action="store_true", help="generate an ADFS disc image (implied if IMAGEFILE has a .adf or .adl extension)")
    parser.add_argument("input_file", metavar="ZFILE", help="Z-machine game filename (input)")
    parser.add_argument("output_file", metavar="IMAGEFILE", nargs="?", default=None, help="Acorn DFS/ADFS disc image filename (output)")

    group = parser.add_argument_group("optional splash screen arguments")
    group.add_argument("--splash-image", metavar="SCREENFILE", type=str, help="use screen dump SCREENFILE as a splash screen")
    group.add_argument("--splash-mode", metavar="N", type=int, help="use mode N for the splash screen")
    group.add_argument("--splash-palette", metavar="N,N,...", type=str, help="set physical colours for splash screen")
    group.add_argument("--splash-wait", metavar="N", type=int, help="show the splash screen for N seconds (0 means 'wait for any key')")

    group = parser.add_argument_group("optional title page arguments")
    group.add_argument("--default-mode", metavar="N", type=int, default=None, help="default to mode N if possible")
    group.add_argument("--auto-start", action="store_true", help="don't wait for SPACE on title page")
    group.add_argument("--custom-title-page", metavar="P", type=str, help="use custom title page P, where P is a filename of mode 7 screen data or an edit.tf URL")
    group.add_argument("--title", metavar="TITLE", type=str, help="set title for use on title page")
    group.add_argument("--subtitle", metavar="SUBTITLE", type=str, help="set subtitle for use on title page")

    group = parser.add_argument_group("optional in-game appearance arguments")
    group.add_argument("-7", "--no-mode-7-status", action="store_true", help="disable coloured status line in mode 7")
    group.add_argument("--no-mode-7-input", action="store_true", help="disable coloured input in mode 7")
    group.add_argument("--default-fg-colour", metavar="N", type=str, help="set the default foreground colour (0-7) for modes 0-6")
    group.add_argument("--default-bg-colour", metavar="N", type=str, help="set the default background colour (0-7) for modes 0-6")
    group.add_argument("--default-mode-7-status-colour", metavar="N", type=str, help="set the default colour (1-7) for the mode 7 status line")
    group.add_argument("--default-mode-7-input-colour", metavar="N", type=str, help="set the default colour (1-7) for mode 7 player input")
    group.add_argument("-4", "--only-40-column", action="store_true", help="only run in 40 column modes")
    group.add_argument("-8", "--only-80-column", action="store_true", help="only run in 80 column modes")
    group.add_argument("--min-mode", metavar="N", type=int, help="set the minimum allowed screen mode (0-7)")
    group.add_argument("--max-mode", metavar="N", type=int, help="set the maximum allowed screen mode (0-7)")

    group = parser.add_argument_group("optional advanced user arguments") # SFTODO: tweak description
    group.add_argument("-p", "--pad", action="store_true", help="pad disc image file to full size")
    group.add_argument("-b", "--benchmark", action="store_true", help="enable the built-in benchmark (implies -d)")
    # SFTODO: Rename "--force-osrdch" to "--no-timed-input"? This (slightly) hints that it can be used to optimise Z4+ games which don't use timed input, but it (slightly) fails to hint that it's a good workaround for people with emulators that work better with OSRDCH.
    group.add_argument("--force-osrdch", action="store_true", help="read keyboard with OSRDCH (will break timed games)")
    group.add_argument("--min-history", metavar="N", type=int, help="allocate at least N bytes for command history")
    group.add_argument("--history-upper-case", action="store_true", help="show command history in upper case")
    group.add_argument("--leave-caps-lock-alone", action="store_true", help="don't force lower case input by default")
    group.add_argument("--on-quit-command", metavar="COMMAND", type=str, help="execute COMMAND when game quits")
    group.add_argument("--on-quit-command-silent", metavar="COMMAND", type=str, help="execute COMMAND invisibly when game quits")
    group.add_argument("--recommended-shadow-cache-pages", metavar="N", type=int, help="try to allocate N pages for shadow cache")
    group.add_argument("--electron-only", action="store_true", help="only support the Electron")
    group.add_argument("--bbc-only", action="store_true", help="only support the BBC B/B+/Master")
    group.add_argument("--no-tube", action="store_true", help="don't support second processor")
    group.add_argument("--max-page", metavar="ADDR", type=str, help="assume PAGE<=ADDR")
    group.add_argument("-o", "--preload-opt", action="store_true", help="build in preload optimisation mode (implies -d)")
    group.add_argument("-c", "--preload-config", metavar="PREOPTFILE", type=str, help="build with specified preload configuration previously created with -o")
    group.add_argument("--interpreter-num", metavar="N", type=int, help="set the interpreter number (0-19, defaults to 2 for Beyond Zork and 8 otherwise)")
    group.add_argument("--no-cursor-editing", action="store_true", help="pass cursor keys through when reading a line from keyboard")
    group.add_argument("--no-history", action="store_true", help="disable command history")
    group.add_argument("-f", "--function-keys", action="store_true", help="pass function keys through to the game")
    group.add_argument("--force-beebasm", action="store_true", help="use beebasm to tokenise BASIC")
    group.add_argument("--force-basictool", action="store_true", help="use basictool to tokenise BASIC")
    group.add_argument("--no-build-file", action="store_true", help="disable creation of build file on generated disc image")
    # SFTODO: Not an ideal argument name but it will do for now.
    group.add_argument("--nfs-install-only", action="store_true", help="generate a disc image for installation to NFS")
    group.add_argument("--disc-title", metavar="DISCTITLE", type=str, help="set disc image title")
    group.add_argument("--try-support-32k", action="store_true", help="try to support unexpanded 32K machines")
    group.add_argument("--no-slow-hw-scroll", action="store_true", help="disable use of slow hardware scroll")
    group.add_argument("--no-fast-hw-scroll", action="store_true", help="disable use of fast hardware scroll")
    group.add_argument("--x-for-examine", action="store_true", help="interpret 'x' as 'examine' (Z1-4 games only)")
    group.add_argument("--undo", action="store_true", help="enable use of undo during gameplay")
    group.add_argument("--no-special-game-check", action="store_true", help="disable special treatment of some games")

    group = parser.add_argument_group("optional advanced/developer arguments (not normally needed)")
    group.add_argument("--never-defer-output", action="store_true", help="never defer output during the build")
    group.add_argument("-d", "--debug", action="store_true", help="build a debug version")
    group.add_argument("--print-swaps", action="store_true", help="print virtual memory swaps (implies -d)")
    group.add_argument("--trace", action="store_true", help="enable tracing (implies -d)")
    group.add_argument("--trace-floppy", action="store_true", help="trace disc access (implies -d)")
    group.add_argument("--trace-vm", action="store_true", help="trace virtual memory (implies -d)")
    group.add_argument("--speed", action="store_true", help="enable speed printing (implies -d)")
    group.add_argument("--no-dynmem-adjust", action="store_true", help="disable dynamic memory adjustment")
    group.add_argument("--fake-read-errors", action="store_true", help="fake intermittent read errors")
    group.add_argument("--slow", action="store_true", help="use slow but shorter routines")
    # We don't have a --force-small-dynmem because we already prefer it whenever it can be
    # used; building with --max-page=0xe00 will maximise the opportunity to use it.
    # SFTODO: Should I add it just for completeness, and make it prevent generation of anything
    # except small dynmem builds as with force medium/big?
    group.add_argument("--force-medium-dynmem", action="store_true", help="force use of the medium dynamic memory model")
    group.add_argument("--force-big-dynmem", action="store_true", help="force use of the big dynamic memory model")
    group.add_argument("--waste-bytes", metavar="N", type=int, help="waste N bytes of main RAM")
    group.add_argument("--force-65c02", action="store_true", help="use 65C02 instructions on all machines")
    group.add_argument("--force-6502", action="store_true", help="use only 6502 instructions on all machines (implies --no-turbo)")
    group.add_argument("--no-tube-cache", action="store_true", help="disable host cache use on second processor")
    group.add_argument("--no-turbo", action="store_true", help="disable turbo (256K) second processor support")
    group.add_argument("--no-loader-crunch", action="store_true", help="don't crunch the BASIC loader")
    group.add_argument("--no-exe-compression", action="store_true", help="don't compress executables")
    group.add_argument("--no-shadow-vmem", action="store_true", help="disable use of spare shadow RAM as vmem cache")
    group.add_argument("--extra-build-at", metavar="ADDR", type=str, help="perform an extra build at ADDR")
    group.add_argument("--no-runtime-info", action="store_true", help="disable debug info at runtime")
    group.add_argument("--debug-assert", action="store_true", help="include debug assertion code")
    # Not copying the "perform all checks for runtime errors" description make.rb uses for its equivalent
    # of --check-errors, because it would suggest that things like --debug-assert are implied by it, and
    # they aren't.
    group.add_argument("--check-errors", action="store_true", help="enable runtime error checking")
    group.add_argument("--cache-test", action="store_true", help="include host cache test program")
    group.add_argument("--no-sd-card-reset", action="store_true", help="don't force an error to reset SD cards")
    group.add_argument("--no-data-in-stack", action="store_true", help="disable use of stack space for data")
    group.add_argument("--save-temps", action="store_true", help="don't remove temporary files on exit")
    group.add_argument("--no-integra-b-private-ram", action="store_true", help="never use Integra-B private RAM")

    cmd_args = parser.parse_args()

    cmd_args.verbose_level = 0 if cmd_args.verbose is None else cmd_args.verbose

    if cmd_args.only_40_column and cmd_args.only_80_column:
        die("--only-40-column and --only-80-column are incompatible")
    if cmd_args.only_40_column and cmd_args.min_mode:
        die("--only-40-column and --min-mode are incompatible")
    if cmd_args.only_80_column and cmd_args.min_mode:
        die("--only-80-column and --min-mode are incompatible")
    if cmd_args.only_40_column and cmd_args.max_mode:
        die("--only-40-column and --max-mode are incompatible")
    if cmd_args.only_80_column and cmd_args.max_mode:
        die("--only-80-column and --max-mode are incompatible")
    if cmd_args.only_40_column:
        cmd_args.min_mode = 4
    elif cmd_args.only_80_column:
        cmd_args.max_mode = 3
    if cmd_args.min_mode is None:
        cmd_args.min_mode = 0
    if cmd_args.max_mode is None:
        cmd_args.max_mode = 7
    if cmd_args.default_mode is None:
        cmd_args.default_mode = cmd_args.max_mode
    if cmd_args.force_65c02 and cmd_args.force_6502:
        die("--force-65c02 and --force-6502 are incompatible")
    if cmd_args.preload_opt and cmd_args.preload_config:
        die("--preload-opt and --preload-config are incompatible")
    if cmd_args.splash_image is not None or cmd_args.splash_mode is not None:
        if cmd_args.splash_image is None or cmd_args.splash_mode is None:
            die("--splash-image and --splash-mode must both be specified")
    if cmd_args.splash_palette and not cmd_args.splash_image:
        die("--splash-palette only works with --splash-image")
    if cmd_args.on_quit_command is not None and cmd_args.on_quit_command_silent is not None:
        die("--on-quit-command and --on-quit-command-silent are incompatible")

    def validate_colour(colour, default = None, mode_7 = False):
        if colour is None:
            colour = default
        colour = {
            "black": 0,
            "red": 1,
            "green": 2,
            "yellow": 3,
            "blue": 4,
            "magenta": 5,
            "cyan": 6,
            "white": 7,
        }.get(str(colour).lower(), colour)
        try:
            i = int(colour)
        except:
            die("Invalid colour '%s'; colours must be specified as physical colour numbers or names" % colour)
        min_colour = 1 if mode_7 else 0
        max_colour = 7 if mode_7 else 15
        if i is not None and (i < 0 or i > 7):
            die("Invalid colour number %d; must be in the range %d-%d" % (i, min_colour, max_colour))
        return d if i is None else i
    cmd_args.default_fg_colour = validate_colour(cmd_args.default_fg_colour, 7)
    cmd_args.default_bg_colour = validate_colour(cmd_args.default_bg_colour, 4)
    cmd_args.default_mode_7_status_colour = validate_colour(cmd_args.default_mode_7_status_colour, 6, True)
    cmd_args.default_mode_7_input_colour = validate_colour(cmd_args.default_mode_7_input_colour, 3, True)

    # Lower numbered modes need more RAM and all modes have different RAM
    # requirements. Since available RAM is out of our control, we have to be
    # able to deal with any possible mode being the min mode just because of
    # available RAM at runtime, so we can allow the user for force any mode
    # without creating extra work for ourselves.
    if cmd_args.min_mode not in (0, 3, 4, 6, 7):
        die("Invalid mode specified for --min-mode; must be one of 0/3/4/6/7")
    if cmd_args.max_mode not in (0, 3, 4, 6, 7):
        die("Invalid mode specified for --max-mode; must be one of 0/3/6/7")
    # Validate this last to avoid confusing errors if e.g. user specifies
    # --max-mode=5 and nothing else.
    if cmd_args.default_mode not in (0, 3, 4, 6, 7):
        die("Invalid mode specified for --default-mode; must be one of 0/3/4/6/7")
    if cmd_args.min_mode > cmd_args.max_mode:
        die("--max-mode cannot be smaller than --min-mode")
    if not (cmd_args.min_mode <= cmd_args.default_mode <= cmd_args.max_mode):
        die("--default_mode must be within the range of acceptable modes specified by --min-mode/--max-mode/--only-40-column/--only-80-column")

    cmd_args.splash_wait = 10 if cmd_args.splash_wait is None else cmd_args.splash_wait
    if cmd_args.splash_mode is not None:
        if cmd_args.splash_mode < 0 or cmd_args.splash_mode > 7:
            die("Invalid splash screen mode specified")
    if cmd_args.splash_palette is not None:
        cmd_args.splash_palette = [validate_colour(x) for x in cmd_args.splash_palette.split(",")]
        if len(cmd_args.splash_palette) > mode_colours(cmd_args.splash_mode):
            die("Too many colours in --splash-palette list for mode %d" % cmd_args.splash_mode)

    if cmd_args.force_6502:
        # The CMOS instructions are useful in a second processor build which
        # supports turbo second processors. Obviously it's possible to avoid
        # them, but there is a non-zero cost to doing so, so we just don't
        # support this. (--force-6502 is only supported for debugging and for
        # non-standard second processors which have had their CMOS CPU
        # replaced.)
        if not cmd_args.no_turbo:
            warn("Disabling turbo (256K) second processor support because --force-6502 has been specified")
        cmd_args.no_turbo = True

    if cmd_args.output_file is not None:
        _, user_extension = os.path.splitext(cmd_args.output_file)
        if user_extension.lower() == '.dsd':
            cmd_args.double_sided = True
        elif user_extension.lower() == '.adl':
            cmd_args.adfs = True
            cmd_args.double_sided = True
        elif user_extension.lower() == '.adf':
            cmd_args.adfs = True

    if cmd_args.interpreter_num is not None:
        if not (0 <= cmd_args.interpreter_num <= 19):
            die("Invalid interpreter number")

    if cmd_args.title is None:
        cmd_args.title = title_from_filename(cmd_args.input_file, 40)

    if cmd_args.max_page is not None:
        cmd_args.max_page = our_parse_int(cmd_args.max_page)

    if cmd_args.benchmark or cmd_args.preload_opt or cmd_args.trace or cmd_args.trace_floppy or cmd_args.trace_vm or cmd_args.speed or cmd_args.print_swaps:
        cmd_args.debug = True

    if cmd_args.extra_build_at is not None:
        cmd_args.extra_build_at = our_parse_int(cmd_args.extra_build_at)

    if cmd_args.recommended_shadow_cache_pages is not None:
        if cmd_args.recommended_shadow_cache_pages < 2:
            die("--recommended-shadow-cache-pages must be at least 2")
    else:
        cmd_args.recommended_shadow_cache_pages = 4

    if cmd_args.min_history is not None:
        if cmd_args.min_history < 16 or cmd_args.min_history > 255:
            die("Minimum history size must be between 16 and 255 bytes inclusive")
    if cmd_args.no_history and cmd_args.min_history is not None:
        die("--no-history and --min-history are incompatible")
    if not cmd_args.no_history and cmd_args.min_history is None:
        cmd_args.min_history = 48

    if cmd_args.no_history and cmd_args.history_upper_case:
        die("--no-history and --history-upper-case are incompatible")

    if cmd_args.force_medium_dynmem and cmd_args.force_big_dynmem:
        die("--force-medium-dynmem and --force-big-dynmem are incompatible")

    if cmd_args.force_beebasm and cmd_args.force_basictool:
        die("--force-beebasm and --force-basictool are incompatible")

    return cmd_args


def make_preload_blocks_list(config_filename):
    with open(config_filename, "rb") as f:
        preload_config = bytearray(f.read())
    blocks = []
    for i in range(len(preload_config) // 2):
        # We don't care about the timestamp on the entries in preload_config;
        # they are in order of insertion and that's what we're really interested
        # in. (This isn't the same as the order based on timestamp; a block
        # loaded early may of course be used again later and therefore have a
        # newer timestamp than another block loaded after it but never used
        # again.) Note that just as when we don't use preload_config, the
        # initial vmap entries will be assigned timestamps based on their order;
        # the timestamps in preload_config are ignored.
        addr = ((preload_config[i*2] & vmem_highbyte_mask) << 8) | preload_config[i*2 + 1]
        if addr == 0:
            # This is an zero address, which is in dynamic memory and wouldn't normally
            # be seen in vmap. We put one of these as the first entry when building the
            # preopt executable, which we just ignore.
            assert i == 0
            continue
        block_index = ((addr << 1) - nonstored_pages) // pages_per_vmem_block
        assert block_index >= 0
        blocks.append(block_index)
    return blocks


def make_disc_image():
    global ozmoo_base_args
    global bbc_args
    global tube_args
    ozmoo_base_args = [
        "-DACORN=1",
        "-DACORN_CURSOR_PASS_THROUGH=1",
        "-DSTACK_PAGES=%d" % stack_pages,
        "-DSPLASHWAIT=0",
        "-DACORN_INITIAL_NONSTORED_PAGES=%d" % nonstored_pages,
        "-DACORN_DYNAMIC_SIZE_BYTES=%d" % dynamic_size_bytes,
        "-DACORN_GAME_PAGES=%d" % game_pages,
        "-DACORN_LOADER_HIGHLIGHT_FG=%d" % loader_screen.highlight_fg,
        "-DZP_BYTES_TO_SAVE=%d" % zp_bytes_to_save,
    ]
    # SFTODO: Re-order these to match the --help output eventually
    if double_sided_dfs():
        if cmd_args.nfs_install_only:
            # SFTODO: It would probably be a good idea to allow a double-sided
            # NFS install build where the DATA file (uninterleaved) is put on
            # surface 2; this would allow games up to just under 200K to be
            # installed via DFS without needing stitching tools. But let's keep
            # it simple for now until the NFS install option has seen some real
            # use. Or maybe we could add a simple BASIC CPYDATA program which will
            # copy and de-interleave the data from a double-sided DFS disc.
            die("NFS install discs cannot be double-sided")
        ozmoo_base_args += ["-DACORN_DSD=1"]
    # SFTODO: I am not too happy with the ACORN_ADFS name here; I might prefer to use ACORN_OSWORD_7F for DFS and default to OSFIND/OSGBPB-for-game-data. But this will do for now while I get something working.
    if cmd_args.adfs or cmd_args.nfs_install_only:
        ozmoo_base_args += ["-DACORN_ADFS=1"]
    # SFTODO: assembly variable should be *ACORN_*MODE_7_STATUS
    if not cmd_args.no_mode_7_status:
        bbc_args += ["-DMODE_7_STATUS=1"]
        tube_args += ["-DMODE_7_STATUS=1"]
    if not cmd_args.no_mode_7_input:
        bbc_args += ["-DMODE_7_INPUT=1"]
        tube_args += ["-DMODE_7_INPUT=1"]
    if not cmd_args.no_turbo:
        tube_args += turbo_supported_args
    if not cmd_args.force_6502:
        tube_args += ["-DCMOS=1"]
    if cmd_args.interpreter_num:
        ozmoo_base_args += ["-DTERPNO=%d" % cmd_args.interpreter_num]
    if cmd_args.function_keys:
        ozmoo_base_args += ["-DACORN_FUNCTION_KEY_PASS_THROUGH=1"]
    if not cmd_args.no_cursor_editing:
        ozmoo_base_args += ["-DACORN_CURSOR_EDIT_READ=1"]
    if cmd_args.force_65c02:
        ozmoo_base_args += ["-DCMOS=1"]
    if cmd_args.benchmark:
        ozmoo_base_args += ["-DBENCHMARK=1"]
    if cmd_args.debug:
        ozmoo_base_args += ["-DDEBUG=1"]
    if cmd_args.trace:
        ozmoo_base_args += ["-DTRACE=1"]
    if cmd_args.trace_floppy:
        ozmoo_base_args += ["-DTRACE_FLOPPY=1"]
    if cmd_args.trace_vm:
        ozmoo_base_args += ["-DTRACE_VM=1"]
    if cmd_args.speed:
        ozmoo_base_args += ["-DPRINTSPEED=1"]
    if cmd_args.print_swaps:
        ozmoo_base_args += ["-DPRINT_SWAPS=1"]
    if cmd_args.no_dynmem_adjust:
        ozmoo_base_args += ["-DACORN_NO_DYNMEM_ADJUST=1"]
    if cmd_args.fake_read_errors:
        ozmoo_base_args += ["-DFAKE_READ_ERRORS=1"]
    if cmd_args.slow:
        ozmoo_base_args += ["-DSLOW=1"]
    if cmd_args.waste_bytes:
        ozmoo_base_args += ["-DWASTE_BYTES=%s" % cmd_args.waste_bytes]
    # Z1-3 don't support timed input, so we always use OSRDCH as the code is
    # slightly shorter.
    if cmd_args.force_osrdch or z_machine_version in (1, 2, 3):
        ozmoo_base_args += ["-DACORN_OSRDCH=1"]
    if cmd_args.on_quit_command:
        ozmoo_base_args += ["-DACORN_ON_QUIT_COMMAND=1"]
    if cmd_args.on_quit_command_silent:
        ozmoo_base_args += ["-DACORN_ON_QUIT_COMMAND=1"]
        ozmoo_base_args += ["-DACORN_ON_QUIT_COMMAND_SILENT=1"]
    if not cmd_args.no_history:
        ozmoo_base_args += ["-DUSE_HISTORY=%d" % cmd_args.min_history]
    if cmd_args.history_upper_case:
        ozmoo_base_args += ["-DHISTORY_UPPER_CASE=1"]
    if not cmd_args.no_runtime_info:
        ozmoo_base_args += ["-DACORN_SHOW_RUNTIME_INFO=1"]
    if cmd_args.debug_assert:
        ozmoo_base_args += ["-DACORN_DEBUG_ASSERT=1"]
    if cmd_args.check_errors:
        ozmoo_base_args += ["-DCHECK_ERRORS=1"]
    if cmd_args.min_mode <= 3:
        ozmoo_base_args += ["-DSUPPORT_80COL=1"]
    if not cmd_args.no_slow_hw_scroll:
        ozmoo_base_args += ["-DACORN_HW_SCROLL_SLOW=1"]
    if not cmd_args.no_fast_hw_scroll:
        ozmoo_base_args += ["-DACORN_HW_SCROLL_FAST=1"]
    if cmd_args.no_data_in_stack:
        ozmoo_base_args += ["-DACORN_NO_DATA_IN_STACK=1"]
    if cmd_args.undo:
        tube_args += ["-DUNDO=1", "-DUNDO_BUFFER_SIZE_BYTES=%d" % undo_buffer_size]
    if cmd_args.x_for_examine:
        if z_machine_version >= 5:
            die("--x-for-examine is not supported for Z-machine versions 5 and above")
        ozmoo_base_args += ["-DX_FOR_EXAMINE=1"]

    if z_machine_version in (1, 2, 3, 4, 5, 7, 8):
        ozmoo_base_args += ["-DZ%d=1" % z_machine_version]
    else:
        die("Unsupported Z-machine version: %d" % (z_machine_version,))

    if cmd_args.on_quit_command or cmd_args.on_quit_command_silent:
        command = cmd_args.on_quit_command
        if command is None:
            command = cmd_args.on_quit_command_silent
        with open(make_temp("on-quit-command.asm"), "w") as f:
            f.write("    !byte %s, cr ; %s\n" % (", ".join(str(ord(c)) for c in command), command))

    want_electron = True
    want_bbc_swr = True
    want_bbc_shr_swr = True
    want_tube = True
    if cmd_args.bbc_only:
        want_electron = False
    if cmd_args.electron_only:
        want_bbc_swr = False
        want_bbc_shr_swr = False
    if cmd_args.no_tube:
        want_tube = False
    # SFTODO: Test this, it may be useless/wrong
    #if cmd_args.only_80_column:
    #    want_bbc_swr = False # mode 7 only build
    if not any([want_electron, want_bbc_swr, want_bbc_shr_swr, want_tube]):
        die("All possible builds have been disabled by command line options, nothing to do!")

    # We work with "executable groups" instead of executables so we can keep related
    # executables together on the disc, although this is only useful for the second
    # processor build at the moment.
    ozmoo_variants = []
    have_electron = False
    if want_electron:
        e = make_electron_swr_executable()
        if e is not None:
            have_electron = True
            ozmoo_variants.append([e])
    if want_bbc_swr:
        e = make_bbc_swr_executable()
        if e is not None:
            ozmoo_variants.append([e])
    if want_bbc_shr_swr:
        e = make_shr_swr_executable()
        if e is not None:
            ozmoo_variants.append([e])
    turbo_test_executable = None
    if want_tube:
        if not cmd_args.no_turbo:
            turbo_test_executable = make_turbo_test_executable()
        tube_executables = make_tube_executables()
        if tube_executables is not None:
            ozmoo_variants.append(tube_executables)
    if len(ozmoo_variants) == 0:
        die("No builds succeeded, can't generate disc image.")

    # We sort the executable groups by descending order of size; this isn't really
    # important unless we're doing a double-sided DFS build (where we want to
    # distribute larger things first), but it doesn't hurt to do it in all cases.
    ozmoo_variants = sorted(ozmoo_variants, key=disc_size, reverse=True)

    # SFTODO: INCONSISTENT ABOUT WHETHER ALL-LOWER OR ALL-UPPER IN LOADER_SYMBOLS
    loader_symbols.update({
        "default_mode": basic_int(cmd_args.default_mode),
        "DEFAULT_FG_COLOUR": basic_int(cmd_args.default_fg_colour),
        "DEFAULT_BG_COLOUR": basic_int(cmd_args.default_bg_colour),
        "DEFAULT_M7_STATUS_COLOUR": basic_int(cmd_args.default_mode_7_status_colour),
        "DEFAULT_M7_INPUT_COLOUR": basic_int(cmd_args.default_mode_7_input_colour)
    })
    for executable_group in ozmoo_variants:
        for e in executable_group:
            e.add_loader_symbols(loader_symbols)
    loader_symbols["MIN_MODE"] = basic_int(cmd_args.min_mode)
    loader_symbols["MAX_MODE"] = basic_int(cmd_args.max_mode)
    if cmd_args.auto_start:
        loader_symbols["AUTO_START"] = basic_int(1)
    if cmd_args.leave_caps_lock_alone:
        loader_symbols["LEAVE_CAPS_LOCK_ALONE"] = basic_int(1)
    if cmd_args.undo and z_machine_version <= 4:
        loader_symbols["CTRL_U_UNDO"] = basic_int(1)
    loader_screen.add_loader_symbols(loader_symbols)

    for min_menu_mode in (0, 3, 4, 6, 7):
        for max_menu_mode in (0, 3, 4, 6, 7):
            if max_menu_mode <= min_menu_mode:
                pass
            possible_runtime_max_modes = [cmd_args.max_mode]
            if cmd_args.max_mode == 7 and have_electron:
                possible_runtime_max_modes.append(6)
            if cmd_args.min_mode <= min_menu_mode and max_menu_mode in possible_runtime_max_modes:
                loader_symbols["NEED_MODE_MENU_%d_TO_%d" % (min_menu_mode, max_menu_mode)] = "1"

    disc_contents = [boot_file]
    if cmd_args.cache_test:
        disc_contents.append(make_tokenised_cache_test())
    if turbo_test_executable is not None:
        disc_contents.append(turbo_test_executable)
    loader = make_tokenised_loader(loader_symbols)
    if cmd_args.splash_image:
        splash_executable = make_splash_executable()
        disc_contents += [make_tokenised_preloader(loader, splash_executable.load_addr & 0xffff), splash_executable]
    # SFTODO: It's borderline possible (maybe not right now, maybe never) that none of the executables
    # support shadow RAM (perhaps it's a tube-only game with host cache disabled) and we don't need
    # to build and include the shadow driver. Putting this note in just in case it's worth being
    # smarter about including it, but really the cases where it's not useful aren't all that likely
    # or interesting.
    disc_contents += [loader, shaddrv_executable, findswr_executable]
    if not cmd_args.no_history:
        disc_contents.append(make_insv_executable())
    if not cmd_args.no_fast_hw_scroll:
        disc_contents.append(fast_hw_scroll_executable)
    assert all(f is not None for f in disc_contents)
    if double_sided_dfs():
        disc2_contents = []
    for executable_group in ozmoo_variants:
        if double_sided_dfs() and disc_size(disc2_contents) < disc_size(disc_contents):
            disc2_contents.extend(executable_group)
        else:
            disc_contents.extend(executable_group)
    if double_sided_dfs():
        for f in disc2_contents:
            f.surface = 2
            f.add_loader_symbols(loader_symbols)
        for i in range(len(disc_contents)):
            if disc_contents[i].leafname == "LOADER":
                disc_contents[i] = make_tokenised_loader(loader_symbols)

    # SFTODO: This is a bit of a hack, may well be able to simplify/improve
    if not cmd_args.adfs:
        user_extensions = (".ssd", ".dsd")
        preferred_extension = ".dsd" if cmd_args.double_sided else ".ssd"
    else:
        user_extensions = (".adf", ".adl")
        preferred_extension = ".adl" if cmd_args.double_sided else ".adf"
    if cmd_args.output_file is None:
        output_file = os.path.basename(os.path.splitext(cmd_args.input_file)[0] + preferred_extension)
    else:
        user_prefix, user_extension = os.path.splitext(cmd_args.output_file)
        # If the user wants to call the file .img or something, we'll leave it alone.
        if user_extension.lower() in user_extensions and user_extension.lower() != preferred_extension.lower():
            warn("Changing extension of output from %s to %s" % (user_extension, preferred_extension))
            user_extension = preferred_extension
        output_file = user_prefix + user_extension

    # If the user explicitly specifies a disc title we warn if it's too long,
    # otherwise we silently truncate the title from the loader screen to make it
    # fit.
    max_disc_title_len = MAX_ADFS_DISC_TITLE_LEN if cmd_args.adfs else MAX_DFS_DISC_TITLE_LEN
    disc_title = cmd_args.disc_title if cmd_args.disc_title is not None else cmd_args.title[:max_disc_title_len]
    if len(disc_title) > max_disc_title_len:
        warn('Truncating disc title to %d characters: "%s"' % (max_disc_title_len, disc_title[:max_disc_title_len]))

    if not cmd_args.adfs:
        disc = DfsImage(disc_contents, disc_title, boot_option=0 if cmd_args.nfs_install_only else 3)
        if not cmd_args.double_sided:
            # Because we read multiples of pages_per_vmem_block at a time, the data file must
            # start at a corresponding sector in order to avoid a read ever straddling a track
            # boundary. (Some emulators - b-em 1770/8271, BeebEm 1770 - seem relaxed about this
            # and it will work anyway. BeebEm's 8271 emulation seems stricter about this, so
            # it's good for testing.)
            disc.add_pad_file(lambda sector: sector % pages_per_vmem_block == 0)
            disc.add_file(File("DATA", 0, 0, game_data))
            if not cmd_args.no_build_file:
                disc.add_file(make_build_file())
            DfsImage.write_ssd(disc, output_file)
        else:
            disc2 = DfsImage(disc2_contents, disc_title, boot_option=0) # 0 = no action
            # The game data must start on a track boundary at the same place on both surfaces.
            max_first_free_sector = max(disc.first_free_sector(), disc2.first_free_sector())
            def pad_predicate(sector):
                return sector >= max_first_free_sector and sector % DfsImage.sectors_per_track == 0
            disc .add_pad_file(pad_predicate)
            disc2.add_pad_file(pad_predicate)
            data = [bytearray(), bytearray()]
            spt = DfsImage.sectors_per_track
            bps = DfsImage.bytes_per_sector
            bpt = DfsImage.bytes_per_track
            for i in range(0, bytes_to_pages(len(game_data)), spt):
                data[(i % (2 * spt)) // spt].extend(game_data[i*bps:i*bps+bpt])
            disc .add_file(File("DATA", 0, 0, data[0]))
            disc2.add_file(File("DATA", 0, 0, data[1]))
            if not cmd_args.no_build_file:
                disc.add_file(make_build_file())
            DfsImage.write_dsd(disc, disc2, output_file)
    else:
        disc = AdfsImage(disc_contents, disc_title)
        # There are no alignment requirements for ADFS so we don't need a pad file..
        disc.add_file(File("DATA", 0, 0, game_data))
        disc.add_directory("SAVES")
        if not cmd_args.no_build_file:
            disc.add_file(make_build_file())
        if cmd_args.double_sided:
            disc.write_adl(output_file)
        else:
            disc.write_adf(output_file)
    info("Generated files: " + output_file)


ozmoo_base_dir = os.path.dirname(os.path.realpath(__file__))

defer_output = False
deferred_output = []
best_effort_version = "Ozmoo"
try:
    with open(os.path.join(ozmoo_base_dir, "version.txt"), "r") as f:
        version_txt = f.read().strip()
    best_effort_version += " " + version_txt
except IOError:
    version_txt = None

cmd_args = parse_args()

# It's OK to run and give --help etc output if the version.txt file can't be found,
# but we don't want to generate a disc image with a missing version.
if version_txt is None:
    die("Can't find version.txt")

min_acme_version = (0, 97)
min_beebasm_version = (1, 9)
min_basictool_version = (0, 6)

tool_versions = {}
prechecks()

loader_screen = LoaderScreen()

header_version = 0
header_release = 2
header_serial = 18
header_static_mem = 0xe
header_flags_2 = 0x10
header_flags_2_undo = 1 << 4
pages_per_vmem_block = 2 # SFTODO: rename back to vmem_block_pagecount to match (upstream) assembly constant?
bytes_per_page = 256
bytes_per_vmem_block = pages_per_vmem_block * bytes_per_page
min_vmem_blocks = 2 # absolute minimum, one for PC, one for data SFTODO: ALLOW USER TO SPECIFY ON CMD LINE?
min_timestamp = 0
max_timestamp = 0xe0 # initial tick value
zp_bytes_to_save = 0xd

# We track temporary files created and remove them one by one on exit, unless
# --save-temps is specified. We could just remove everything except CACHEDIR.TAG
# in the temporary directory, but this feels safer.
temp_files_to_delete = set()
temp_dir = os.path.join(ozmoo_base_dir, "temp")
if not os.path.exists(temp_dir):
    os.makedirs(temp_dir)
# Create a cache directory tag in temp. See https://bford.info/cachedir/
cache_tag_file = make_temp("CACHEDIR.TAG", auto_delete=False)
if not os.path.exists(cache_tag_file):
    with open(cache_tag_file, "w") as f:
        f.write("Signature: 8a477f597d28d172789f06886806bc55")
if not cmd_args.save_temps:
    atexit.register(delete_temp_files)

host = 0xffff0000
tube_start_addr = 0x700
small_dynmem_page_threshold = 0x2000
bbc_max_start_addr = 0x3000
# On the Electron, we'd like to avoid the executable overwriting the mode 6
# screen RAM and corrupting the loading screen if we can, so we pick a
# relatively low address which should be >=PAGE on nearly all systems.
# SFTODO: The fuzziness around interpretation of max_start_addr means in
# practice we may slip *two* pages above this, which isn't ideal.
# SFTODONOW: Now that a non-shadow machine *can* run in modes other than the
# minimal screen RAM (e.g. a B running in mode 6 instead of mode 7), should
# we try to avoid loading "extra high" on the B as well? Maybe lower
# bbc_max_start_addr to something like 0x2000? I believe the concern here is
# that when we restart the game in mode (say) 3, a higher pre-relocation load
# increases the chances of visible screen corruption. (On a BBC the initial
# load is fine, as we're in mode 7 showing the loaders creen.)
if not cmd_args.adfs:
    electron_max_start_addr = 0x1900
else:
    electron_max_start_addr = 0x1d00
if cmd_args.max_page is not None:
    small_dynmem_page_threshold = cmd_args.max_page
    bbc_max_start_addr = max(bbc_max_start_addr, cmd_args.max_page)
    electron_max_start_addr = max(electron_max_start_addr, cmd_args.max_page)
cache_start_addr = 0x2b00


common_labels = {}
loader_symbols = {}

with open(cmd_args.input_file, "rb") as f:
    game_data = bytearray(f.read())
z_machine_version = game_data[header_version]
if z_machine_version == 3:
    vmem_highbyte_mask = 0x00
elif z_machine_version == 8:
    vmem_highbyte_mask = 0x03
else:
    vmem_highbyte_mask = 0x01

game_pages = bytes_to_pages(len(game_data))
dynamic_size_bytes = read_be_word(game_data, header_static_mem)
nonstored_pages = bytes_to_pages(dynamic_size_bytes)
stack_pages = 4
assert nonstored_pages > 0
# We round nonstored_pages up to a multiple of pages_per_vmem_block; this is
# irrelevant but harmless for non-VMEM builds, and for VMEM builds it is
# essential as the vmem code requires that cached game data blocks start on
# pages_per_vmem_block (512-byte) boundaries within the game.
while nonstored_pages % pages_per_vmem_block != 0:
    nonstored_pages += 1
# The following points are technically irrelevant and very mildly harmful for
# non-VMEM builds, but in order to avoid extra complexity we always apply them
# anyway:
#
# - Lots of VMEM code assumes that there is at least one meaningful entry in
#   the vmap (for example, loops over the vmap in vmem.asm are written to
#   assume they will always iterate at least once). It is almost certain that
#   the game already has at least one 512-byte block of read-only data in
#   addition to its rounded-up nonstored_pages of dynamic memory, but we
#   deliberately pad the game if necessary to ensure this. This guarantees
#   vmap_{used,max}_entries >= 1.
#
# - We pad the game if necessary so it's a multiple of 512-bytes long, i.e. we
#   guarantee that game_pages % pages_per_vmem_block == 0. In VMEM builds,
#   everything is 512-byte aligned and so this isn't really harmful, and it
#   avoids introducing corner cases when we're using game_pages during
#   initialisation. (Non-VMEM builds only require story_start to be 256-byte
#   aligned, and it's therefore possible to have (say) 201 pages of memory free
#   with a 201 page game but end up insisting on using VMEM because we treat it
#   as a 202 page game.) SFTODO: Is this quite right? Didn't I switch to making non-VMEM builds meet the same 512-byte alignment anyway? Not sure right now.
#
# SFTODO: It would be possible - the complexity would mainly be in the build
# script, I beleive - to stop applying these (particularly the second) tweaks
# to non-VMEM builds. I really don't know if it's worth it though.
while ((game_pages < nonstored_pages + pages_per_vmem_block) or
       (game_pages % pages_per_vmem_block != 0)):
    game_data += bytearray(bytes_per_page)
    game_pages = bytes_to_pages(len(game_data))

if cmd_args.undo:
    if z_machine_version >= 5 and ((game_data[header_flags_2 + 1] & header_flags_2_undo) == 0):
        warn("Ignoring --undo as this version %d game does not support undo" % z_machine_version)
        cmd_args.undo = False

if cmd_args.undo:
    undo_buffer_size = round_up(round_up(dynamic_size_bytes, 256) + stack_pages * 256, 512)
    # We impose this maximum undo buffer size limit to match the upstream limits
    # for the Commodore 128. This precise value doesn't have any significance,
    # but we can't support anything much bigger than this, as we have ~48K of
    # free RAM for stack, story and vmem cache on a second processor, so if the
    # undo buffer gets close to half of this there's no RAM left over for
    # dynamic memory or virtual memory cache.
    max_undo_buffer_size = 19.25 * 1024
    if undo_buffer_size > max_undo_buffer_size:
        cmd_args.undo = False
        warn("Ignoring --undo option as the undo buffer would need %s and the maximum is %s" % (format_kb(undo_buffer_size), format_kb(max_undo_buffer_size)))

bbc_args = []
tube_args = []
vmem_args = [
    "-DVMEM=1",
    "-DACORN_VMEM_BLOCKS=%d" % divide_round_up(game_pages - nonstored_pages, pages_per_vmem_block),
]
swr_args = vmem_args + ["-DACORN_SWR=1"]
relocatable_args = ["-DACORN_RELOCATABLE=1"]
small_dynmem_args = ["-DACORN_SWR_SMALL_DYNMEM=1"]
medium_dynmem_args = ["-DACORN_SWR_MEDIUM_DYNMEM=1"]
turbo_supported_args = ["-DACORN_TURBO_SUPPORTED=1"]

if cmd_args.preload_config:
    cmd_args.preload_config = make_preload_blocks_list(cmd_args.preload_config)

check_if_special_game()

boot_file = make_boot()
shaddrv_executable = make_shaddrv_executable()
findswr_executable = make_findswr_executable()
if not cmd_args.no_fast_hw_scroll:
    fast_hw_scroll_executable = make_fast_hw_scroll_executable()

single_to_double_sided = False
if not cmd_args.never_defer_output:
    defer_output = True
while True:
    try:
        make_disc_image()
        break
    except DiscFull:
        if not cmd_args.double_sided:
            cmd_args.double_sided = True
            single_to_double_sided = True
            Executable.all_output_names = set()
            deferred_output = []
            warn("Generating a double-sided disc as the game won't fit otherwise")
        else:
            if single_to_double_sided:
                die("Game is too large for even a double-sided disc")
            else:
                die("Game is too large for a double-sided disc")
show_deferred_output()

# SFTODO: For debugging purposes, a "just build at PAGE=&xxx and give me a usable report with no relocation shenanigans" option would be handy. This is less critical now we have --show-debug-info and --extra-build-at, perhaps not worth it at all.

# SFTODO: The memory models should probably be small, medium and *LARGE*, now we have "medium".

# SFTODO: I am sometimes seeing mediumdyn a bit slower than bigmem, have a think in case I need to tweak build heuristics. (There's not much in it; I think the difference is largest on machines where the dynmem adjustment kicks in, since bigdyn gives this optimisation more headroom.)

# SFTODO: It might be good to add a README.acorn.md or whatever. Contents would include (but not be limited to):
# - notes on installing a game to an ADFS hard drive
# - notes on installing a game to NFS

# SFTODO: Beyond Zork doesn't seem to fit without a second processor any more - I'm sure it used to *just* fit with shadow RAM and PAGE at &E00. Check and see if this indicates bloat. (This *may* now be fixed in practice by not building with history support, but it would be good to check older versions and see if that's why or if there is some other source of bloat here.)

# SFTODO: In Beyond Zork with --no-cursor-editing, COPY keys acts like f2 (=STATUS). Not necessarily a big deal but suggests something is subtlely awry. (Might not actually be f2 technically; I am pressing f2 on my PC keyboard but that may be f1 or something really. I don't think that's the issue, just don't get too hung up on *f2* specifically; key point is that COPY acts like one of the f keys and there's no obvious reason it should.) (FWIW, f2 *does* also work without --no-cursor-editing.) - *OK*, I see what's happening, if not what the best fix is. We explicitly set --function-keys option for BZ. This sets things up so f0 returns 133(=ZSCII f1; there is no f0 in ZSCII), so COPY key's *FX4,1 return value of 135 is indistinguishable from f2. I suspect the "right" fix would be to have the function keys return some "spare" codes and use the input character translation to map those to ZSCII function key codes, then COPY wouldn't get mixed up. But this would - admittedly only in games using --function-keys - add about 16 (8 function keys - due to no need to change, we are I believe constrained by C64's 8 function keys when parsing termination chars) bytes to the translation table. That's probably OK if a game *does* care about function keys, but think about it.

# SFTODO: C64 make has a "-re" option to enable full error checking (CHECK_ERRORS), I should add something like that to Acorn build script.

# SFTODO: For the medium dynmem model at least, could we move the Z-machine stack into the 16K "dynmem" SWR bank (assuming there's room for it with the dynmem, of course)? In medium model I *think* this would be near trivial - the dynmem bank is always paged in - oh no, it probably isn't, but I haven't checked. If this could be done it would free up 1K of main RAM which might be enough to allow e.g. mode 3 on a PAGE=&1900 machine, and since the extra 1K of SWR used would (all else being equal) free up 1K of main RAM and both can be used as vmem cache, there would be no penalty in general for doing this. I suspect this isn't going to fly unless the 16K dynmem bank really is paged in all the time (at least when executing Z-machine code accessing Z-machine stack) or could be paged in in extra cases without adding lots of complexity. Another problem would be that we put discardable init code in the Z-machine stack, and we need somewhere to load that code, whereas if we're (say) in mode 3 and *just* have enough main RAM for the game without room for the Z-machine stack in there, we have nowhere to load the discardable init code. This could possibly be worked around by switching to (say) mode 7 when we do a restart - that would give us extra memory and we could then switch back to mode 3 after discardable init code has been discarded. Perhaps a bit fiddly but perhaps not. Still, worth thinking about. (Also worth noting that *as of this writing* - I haven't tried to optimise things to squeeze extra free RAM out - HH non benchmark needs PAGE<=&1400 on B-no-shadow, so adding &400 from this stack move to SWR wouldn't quite get us mode 3 on PAGE=&1900.)

# SFTODO: I experimented with manually hacking things to wrap the (basictool-crunched) loader with JGH's MCode utility, and then apply the lzsa executable compression we use for the main Ozmoo binaries. Initial LOADER was &226E bytes, crunched *RUNnable one was &15F6. This was hacky and it didn't quite work, so size could vary but not by much. That is a 36% reduction in size, or 3.1K in absolute terms. I don't totally trust b-em's drive emulation to be realistic, but with drive noises on on M128 with DFS, loading the original version took 1.40s, while loading and decompressing the *RUNnable lzsa-ed version took 1.17s, so 16% faster or 0.23 seconds absolute. Of course, it may be that on modern fast filing systems like MMFS or even with a Gotek, the faster read performance would mean the smaller amount of data to load wasn't enough to compensate for the decompression time overhead. My gut feeling is that this is kind of cool but maybe not worth it; I don't know though. It does add an extra layer of complication to the build and I do value users being able to tweak the loader on their actual machine when debugging. We could have a build option to not crunch the loader and even without it it "just" requires doing *RUN-BREAK-OLD:SAVE "X" and changing !BOOT to CHAIN "X" instead of *RUN LOADER to allow tweaks. (*MCODEd BASIC is directly LOADable, but that's not true of an lzsa crunched version, of course.)

# SFTODO: Just for the record (maybe put this elsewhere), when using high screen RAM modes on a machine with no shadow RAM, there is some temporary corruption on screen during "restart" because the executable loads high enough to be within the screen RAM. This works fine, it just looks a little ugly. It probably isn't worth worrying about, but we could potentially change to mode 7 first. That would add at least a little bit of non-deletable extra code (bigger if we try to be smart and only change to mode 7 if we *would* get screen corruption, or maybe the simpler test of only doing it if we're not in a shadow mode) which is probably slightly worse than this slightly ugly but fine behaviour, but think about it.
# SFTODONOW: Hmm, is there any risk that the progress bar updating during the load could corrupt game data on the screen that we're still using? It may be this is unlikely for all sorts of reasons, but worth thinking about. Gut feeling is that by the time we start updating the progress bar we've relocated down to PAGE and by definition the non-discardable code+Z-machine stack (and the code it initially contains) must fit safely below screen RAM, and it seems *likely* the discardable init code after the Z-machine stack will fit in main RAM below screen RAM too, but I don't know if it's guaranteed. Maybe the build system and/or loader should enforce this? But do think about it, it may in practice be a non-issue. Also as noted above would could temporarily switch to mode 6/7 during the restart, *if* that would help with this risk.

# SFTODO: It might be nice to *optionally* allow splash images to be supplied in PNG format and use something like PIL to convert them. I think the position here would be that make-acorn.py should do "non-lossy" conversions - the input image should be in a suitable resolution with suitable colours (we might do trivial quantising to the standard palette or something, just to avoid being excessively picky given things like gimp make it a bit hard to get "perfect" n-colour indexed PNGs) but no dithering, downscaling, upscaling, etc.

# SFTODO: Would it be worth having a "secret" key which if held down when booting causes the preloader to not bother showing the splash screen?

# SFTODO: Any prospect of optimising SWR paging on systems with only 1 bank? If we can easily detect this (either at runtime or by checking during discardable init and patching a handful of manageable places) we can possibly avoid all SWR paging because we know our bank is always paged in.

# SFTODO: Is there any prospect of shrinking the interpreter for Z3 (and maybe other low version) by allowing a build with no split screen support and clearing bit 5 of flags 1 on startup to reflect this? I don't know how easy it would be or how much code it would save, but maybe worth a try. I don't think we can detect whether a game wants screen splitting support or not so this would have to be a user-initiated thing.

# SFTODO: Have I lost ability to run HH in mode 3 on a B with PAGE=&1900, or did that not work before either?

# SFTODO: I should probably rationalise the naming between the acorn-foo.asm files for the little executable programs, the make-acorn variables which correspond to them and so forth - e.g. if the on-disc file is called "foo" the file should probably be called "acorn-foo.asm" not "acorn-fooble.asm", and associated macros etc should probably be consistent too.

# SFTODO: I suspet I have variants on this comment in other places, but it really would be nice to not have to special case the Electron. The lack of mode 7 may complicate things, but I suspect this is mainly a loader issue and the user *can* choose (if there's room) to run in mode 6 on a BBC B anyway. The main problem I think is the sideways RAM paging. I have done analysis in the past which if I interpret it correctly and it wasn't buggy showed about a 6% slowdown for jsr-rts overhead if all SWR paging was done via a subroutine. It may be possible that I could do some profiling to identify the "hot" sideways RAM paging calls (if there are any) and those which (again, hopefully) are pretty rare and have negligible performance impact. We could then maybe do "jsr fast_page:nop:...:nop" or "jsr slow_page" and have the fast_page one patch itself out with inlie code for the current machine when called. Assuming there *are* just a handful of actually-critical paging occurrences this would mostly hide the machine differences, though the binary would be a bit larger (especially on the BBC) which might hurt performance by lowering free RAM, and because the BBC sequence is shorter it would have to end with "jmp address-past-last-nop" which would add a 3 cycle penalty to the BBC compared to the current code, and on the fast path that might not really be acceptable. But it's a thought.

# SFTODO: Could we dynamically patch history_start and history_size (and history_end, if there are any, which there probably aren't) references during initialisation? This would allow us to put the history buffer in the wasted 256 bytes if the game's natural 512-byte alignment doesn't match PAGE alignment on a particular system. Perhaps this is a bad idea from a support/consistency point of view - the same game has different history buffer on different machines - but maybe it isn't.

# SFTODO: It might be nicer if the build system generated the "Sorry, this game can't run on..." message itself as a string. That way you wouldn't e.g. boot Trinity on a BBC B and be told it won't run on a BBC B, then rush over to your Master only to be told it won't run on that either. The build system could generate a message like "Sorry, this game will only run on ..." which is more helpful. Not a big deal, but a nice touch.
