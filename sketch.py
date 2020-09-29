from __future__ import print_function
import argparse
import base64
import copy
import hashlib
import os
import re
import subprocess
import sys


def die(s):
    show_deferred_output()
    print(s, file=sys.stderr)
    sys.exit(1)


def info(s):
    if cmd_args.verbose_level >= 1:
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
    return hex(i)[2:]


def basic_int(i):
    as_decimal = str(i)
    if i < 0:
        as_hex = "-&" + ourhex(-i).upper()
    else:
        as_hex = "&" + ourhex(i).upper()
    return as_decimal if len(as_decimal) < len(as_hex) else as_hex


def basic_string(value):
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, int):
        return basic_int(value)
    return value


def read_le_word(data, i):
    return data[i]*256 + data[i+1]


def write_le(data, i, v, n):
    while n > 0:
        data[i] = v & 0xff
        i += 1
        v >>= 8
        n -= 1


def divide_round_up(x, y):
    if x % y == 0:
        return x // y
    else:
        return (x // y) + 1


def bytes_to_blocks(x):
    return divide_round_up(x, bytes_per_block)


def pad_to(data, size):
    assert len(data) <= size
    return data + bytearray(size - len(data))


def pad_to_multiple_of(data, block_size):
    return pad_to(data, block_size * divide_round_up(len(data), block_size))


def disc_size(contents):
    return sum(bytes_to_blocks(len(f.binary())) for f in contents)


def same_double_page_alignment(lhs, rhs):
    return lhs % 512 == rhs % 512


def double_sided_dfs():
    return cmd_args.double_sided and not cmd_args.adfs


def test_executable(name):
    try:
        child = subprocess.Popen([name], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        child.wait()
    except:
        die("Can't execute '" + name + "'; is it on your PATH?")


def run_and_check(args, output_filter=None):
    if output_filter is None:
        output_filter = lambda x: True
    if cmd_args.verbose_level >= 2:
        print(" ".join(args))
    child = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    child.wait()
    child_output = [line for line in child.stdout.readlines() if output_filter(line)]
    # The child's stdout and stderr will both be output to our stdout, but that's not
    # a big deal.
    if (child.returncode != 0 or cmd_args.verbose_level >= 2) and len(child_output) > 0:
        print("".join(x.decode(encoding="ascii") for x in child_output))
    if child.returncode != 0:
        die("%s failed" % args[0])


# Generate a relatively clear error message if we can't find one of our tools,
# rather than failing on a complex build command.
def prechecks():
    test_executable("acme")
    test_executable("beebasm")
    # Check for a new enough beebasm. We parse the --help output, if we find
    # something that looks like a version we complain if it's too old; if in
    # doubt we don't generate an error.
    def beebasm_version_check(x):
        c = x.split()
        if len(c) >= 2 and c[0] == "beebasm":
            version = c[1]
            if version.startswith("1."):
                c = version.split(".")
                if len(c) >= 2:
                    subversion = re.findall("^\d+", c[1])
                    if len(subversion) > 0 and int(subversion[0]) < 9:
                        die("You need beebasm 1.09 or later to build this")
    run_and_check(["beebasm", "--help"], output_filter=beebasm_version_check)


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
            cmd_args.custom_title_page = "https://edit.tf/#0:GpPdSTUmRfqBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECAak91JNSZF-oECBAgQIECBAgQIECBAgQIECBAgQIECBAgQICaxYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsBpPdOrCqSakyL9QIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIEEyfBiRaSCfVqUKtRBTqQaVSmgkRaUVAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQTt_Lbh2IM2_llz8t_XdkQIECBAgQIECBAgQIECBAgQIECAHIy4cmXkgzb-WXPy39d2RAgQIECBAgQIECBAgQIECBAgQIAcjTn0bNOfR0QZt_LLn5b-u7IgQIECBAgQIECBAgQIECBAgAyNOfRs059HRBiw49eflv67siBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIEEyfBiRaSCfVqUKtRBFnRKaCRFpRUCBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAk906EGHF-oECBAgQIECBAgQIECBAgQIECBAgQIECBAgQICaxYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsB0N_fLyy5EGLygSe59qbPn_UCBAgQIECBAgQIECBAgQIECA"
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

    def __init__(self, contents, boot_option = 3): # 3 = *EXEC
        self.data = bytearray(2 * DfsImage.bytes_per_sector)
        sectors = DfsImage.tracks * DfsImage.sectors_per_track
        self.data[0x107] = sectors & 0xff
        self.data[0x106] = ((sectors >> 8) & 0x3) | (boot_option << 4)
        # SFTODO: DISC TITLE
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

    def __init__(self, contents, boot_option = 3): # 3 = *EXEC
        self.catalogue = []
        self.data = bytearray(AdfsImage.bytes_per_sector * 2)
        self.total_sectors = AdfsImage.tracks * AdfsImage.sectors_per_track
        if cmd_args.double_sided:
            self.total_sectors *= 2
        write_le(self.data, 0xfc, self.total_sectors, 2)
        self.data[0x1fd] = boot_option
        self.data += AdfsImage._make_directory("$")
        self.md5 = hashlib.md5()
        # SFTODO: DISC TITLE
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
    def _make_directory(name):
        data = bytearray(5 * AdfsImage.bytes_per_sector)
        data[0x001:0x005] = b"Hugo"
        data[0x005] = 0 # number of catalogue entries
        if name == "$":
            data[0x4cc] = ord("$")
            data[0x4d9] = ord("$")
        else:
            data[0x4cc:0x4d6] = (name.encode("ascii") + b"\r" + b"\0"*10)[:10]
            data[0x4d9:0x4ec] = (name.encode("ascii") + b"\r" + b"\0"*19)[:19]
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
    cache = {}

    def __init__(self, asm_filename, leafname, version_maker, start_addr, args):
        self.asm_filename = asm_filename
        self.leafname = leafname
        self.surface = 0 # may be overridden later if we're building double-sided DFS
        self.version_maker = version_maker
        self.start_addr = start_addr
        self.load_addr = start_addr
        if leafname != "OZMOO2P":
            self.load_addr |= host
        self.exec_addr = self.load_addr
        self.args = args
        self._relocations = None
        output_name = os.path.splitext(os.path.basename(asm_filename))[0].replace("-", "_")
        if version_maker is not None:
            output_name += "_" + version_maker(start_addr, args)
        else:
            output_name += "_" + ourhex(start_addr)

        # SFTODO: MOVE THIS CACHE LOGIC INTO OZMOOEXECUTABLE? WE DON'T NEED IT ANYWHERE ELSE, AND IT WOULD THEN CACHE THE RESULTS OF VMEM PATCHIG AND EVERYTHING, WHICH FEELS A BIT MORE ELEGANT EVEN IF IN PRACTICE IT'S HARMLESS TO REDO THIS WORK
        # Not all build parameters have to be reflected in the output name, but we
        # can't have two builds with different parameters using the same output
        # name.
        cache_key = (asm_filename, output_name)
        cache_definition = (start_addr, set(args))
        cache_entry = Executable.cache.get(cache_key, None)
        if cache_entry is not None:
            assert cache_entry[0] == cache_definition
            e = cache_entry[1]
            self.labels = e.labels
            self._binary = e._binary
            self._relocations = e._relocations
            return

        self._labels_filename = os.path.join("temp", "acme_labels_" + output_name)
        self._report_filename = os.path.join("temp", "acme_report_" + output_name)
        self._binary_filename = os.path.join("temp", output_name)
        os.chdir("asm")
        def up(path):
            return os.path.join("..", path)
        cpu = "65c02" if "-DCMOS=1" in args else "6502"
        run_and_check(["acme", "--cpu", cpu, "--format", "plain", "--setpc", "$" + ourhex(start_addr)] + self.args + ["-l", up(self._labels_filename), "-r", up(self._report_filename), "--outfile", up(self._binary_filename), asm_filename])
        os.chdir("..")
        self.labels = self._parse_labels()

        with open(self._binary_filename, "rb") as f:
            self._binary = bytearray(f.read())
        if "ACORN_RELOCATABLE" in self.labels:
            self.truncate_at("reloc_count")
        update_common_labels(self.labels)

        Executable.cache[cache_key] = (cache_definition, copy.deepcopy(self))

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
        if cmd_args.adfs:
            symbols[self.leafname + "_BINARY"] = self.leafname
        else:
            symbols[self.leafname + "_BINARY"] = ":%d.$.%s" % (self.surface, self.leafname)

    def truncate_at(self, label):
        self._binary = self._binary[:self.labels[label]-self.labels["program_start"]]

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
        return Executable._binary_diff(other._binary, self._binary)

    @staticmethod
    def _binary_diff(alternate, master):
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
        if "ACORN_RELOCATABLE" in self.labels:
            if self._relocations is None:
                self._relocations = self._make_relocations()
            binary = self._binary + self._relocations
        else:
            binary = self._binary
        # A second processor binary *could* extend past 0x8000 but in practice
        # it won't come even close.
        assert self.start_addr + len(binary) <= 0x8000
        return binary


class OzmooExecutable(Executable):
    def __init__(self, leafname, start_addr, args):
        def version_maker(start_addr, args):
            if "-DACORN_ELECTRON_SWR=1" in args:
                s = "electron_swr"
            else:
                if "-DACORN_SWR=1" in args:
                    s = "bbc_swr"
                    if "-DACORN_NO_SHADOW=1" not in args:
                        s += "_shr"
                else:
                    s = "tube"
            if "-DVMEM=1" not in args:
                s += "_novmem"
            if "-DACORN_SWR_SMALL_DYNMEM=1" in args:
                s += "_smalldyn"
            s += "_" + ourhex(start_addr)
            return s

        if "-DACORN_NO_SHADOW=1" not in args and "-DACORN_HW_SCROLL=1" not in args:
            args += ["-DACORN_HW_SCROLL=1"]
        if cmd_args.preload_opt and "-DVMEM=1" in args:
            args += ["-DPREOPT=1"]
        Executable.__init__(self, "ozmoo.asm", leafname, version_maker, start_addr, args)
        if "ACORN_RELOCATABLE" not in self.labels:
            self.truncate_at("end_of_routines_in_stack_space")
        self.swr_dynmem = 0
        if "VMEM" in self.labels:
            self._patch_vmem()

    def _patch_vmem(self):
        # Can we fit the nonstored blocks into memory?
        nonstored_blocks_up_to = self.labels["story_start"] + nonstored_blocks * bytes_per_block
        if nonstored_blocks_up_to > self.pseudo_ramtop():
            raise GameWontFit("not enough free RAM for game's dynamic memory")
        if "ACORN_SWR" in self.labels:
            # Note that swr_dynmem may be negative; this means there will be
            # some main RAM free after loading dynamic memory when loaded at the
            # build addr. For relocatable builds the loader will also take
            # account of the actual value of PAGE.
            self.swr_dynmem = nonstored_blocks_up_to - 0x8000
            assert self.swr_dynmem <= 16 * 1024

        # On a second processor build, we must also have at least
        # min_vmem_blocks for swappable memory. For sideways RAM builds we need
        # to check at run time if we have enough main/sideways RAM for swappable
        # memory.
        if "ACORN_SWR" not in self.labels:
            nsmv_up_to = nonstored_blocks_up_to + min_vmem_blocks * bytes_per_vmem_block
            if nsmv_up_to > self.pseudo_ramtop():
                raise GameWontFit("not enough free RAM for any swappable memory")

        # Generate initial virtual memory map. We just populate the entire table; if the
        # game is smaller than this we will just never use the other entries.
        vmap_offset = self.labels['vmap_z_h'] - self.labels['program_start']
        vmap_max_size = self.labels['vmap_max_size']
        assert self._binary[vmap_offset:vmap_offset+vmap_max_size*2] == b'V'*vmap_max_size*2
        blocks = cmd_args.preload_config[:] if cmd_args.preload_config is not None else []
        for i in range(vmap_max_size):
            if i not in blocks:
                blocks.append(i)
        blocks = blocks[:vmap_max_size]
        # vmap entries should normally addr a 512-byte aligned block; invalid_addr
        # is odd so it won't ever match when the virtual memory code is searching the map.
        invalid_addr = 0x1
        for i, block_index in enumerate(blocks):
            timestamp = int(max_timestamp + ((float(i) / vmap_max_size) * (min_timestamp - max_timestamp))) & ~vmem_highbyte_mask
            if cmd_args.preload_opt:
                # Most of the vmap will be ignored, but we have to have at least one entry
                # and by making it an invalid addr we don't need to worry about loading
                # any "suggested" blocks.
                addr = invalid_addr
            else:
                addr = nonstored_blocks + block_index * vmem_block_pagecount
            if ((addr >> 8) & ~vmem_highbyte_mask) != 0:
                # This vmap entry is useless; the current Z-machine version can't contain
                # such a block.
                # SFTODO: Warn? It's harmless but it means we could have clawed back a few
                # bytes by shrinking vmap_max_size.
                addr = 0
            vmap_entry = (timestamp << 8) | addr
            self._binary[vmap_offset + i + 0            ] = (vmap_entry >> 8) & 0xff
            self._binary[vmap_offset + i + vmap_max_size] = vmap_entry & 0xff

    def pseudo_ramtop(self):
        if "ACORN_SWR" in self.labels:
            return 0x8000 if "ACORN_SWR_SMALL_DYNMEM" in self.labels else 0xc000
        else:
            return self.labels["flat_ramtop"]

    def max_nonstored_blocks(self):
        return (self.pseudo_ramtop() - self.labels["story_start"]) // bytes_per_block

    # Return the size of the binary, ignoring any relocation data (which isn't
    # important for the limited use we make of the return value).
    def size(self):
        return len(self._binary)

    def rebuild_at(self, start_addr):
        return OzmooExecutable(self.leafname, start_addr, self.args)

    def add_loader_symbols(self, symbols):
        Executable.add_loader_symbols(self, symbols)
        symbols[self.leafname + "_MAX_PAGE"] = basic_int(self.start_addr)
        symbols[self.leafname + "_RELOCATABLE"] = "TRUE" if "ACORN_RELOCATABLE" in self.labels else "FALSE"
        symbols[self.leafname + "_SWR_DYNMEM"] = basic_int(self.swr_dynmem)


def make_ozmoo_executable(leafname, start_addr, args, report_failure_prefix = None):
    try:
        return OzmooExecutable(leafname, start_addr, args)
    except GameWontFit as e:
        if report_failure_prefix is not None:
            warn("Game is too large for %s: %s" % (report_failure_prefix, str(e)))
        return None


# Build an Ozmoo executable which loads at the highest possible address; we pick
# an address which means it will work on machines with relatively high values of
# PAGE if possible. The executable will relocate itself down if PAGE isn't as
# high as the worst case we assume here.
def make_highest_possible_executable(leafname, args, report_failure_prefix):
    assert "-DACORN_RELOCATABLE=1" in args

    # Because of Ozmoo's liking for 512-byte alignment and the variable 256-byte
    # value of PAGE:
    # - max_nonstored_blocks() can only return even values on a VMEM build.
    # - nonstored_blocks (i.e. for this specific game) will always be even.
    # - There are two possible start addresses 256 bytes apart which will
    #   generate the same value of max_nonstored_blocks(), as one will waste an
    #   extra 256 bytes on aligning story_start to a 512-byte boundary.
    # - We want to use the higher of those two possible start addresses, because
    #   it means we won't need to waste 256 bytes before the start of the code
    #   if PAGE happens to have the right alignment.
    e_e00 = make_ozmoo_executable(leafname, 0xe00, args, report_failure_prefix)
    # If we can't fit build successfully with a start of 0xe00 we can't ever
    # manage it.
    if e_e00 is None:
        return None
    surplus_nonstored_blocks = e_e00.max_nonstored_blocks() - nonstored_blocks
    assert surplus_nonstored_blocks >= 0
    assert surplus_nonstored_blocks % 2 == 0
    # There's no point loading really high, and doing a totally naive
    # calculation may cause us to load so high there's no room for the
    # relocation data before &8000, so we never load much higher than
    # max_start_addr.
    approx_max_start_addr = min(0xe00 + surplus_nonstored_blocks * bytes_per_block, max_start_addr)
    e = make_optimally_aligned_executable(leafname, approx_max_start_addr, args, report_failure_prefix, e_e00)
    assert e is not None
    if same_double_page_alignment(e.start_addr, 0xe00):
        assert e.size() == e_e00.size()
    else:
        assert e.size() == e_e00.size() - 256
    assert 0 <= e.max_nonstored_blocks() - nonstored_blocks
    return e


# Build an Ozmoo executable which loads at whichever of initial_start_addr
# and initial_start_addr+256 gives the least wasted space. If provided
# base_executable is a pre-built executable whcih shares the same double-page
# alignment as initial_start_addr; this may help avoid an unnecessary build.
def make_optimally_aligned_executable(leafname, initial_start_addr, args, report_failure_prefix, base_executable = None):
    if base_executable is None:
        base_executable = make_ozmoo_executable(leafname, initial_start_addr, args, report_failure_prefix)
        if base_executable is None:
            return None
    else:
        assert base_executable.asm_filename == "ozmoo.asm"
        assert same_double_page_alignment(base_executable.start_addr, initial_start_addr)
        assert base_executable.args == args
    alternate_executable = make_ozmoo_executable(leafname, initial_start_addr + 256, args)
    if alternate_executable is not None and alternate_executable.size() < base_executable.size():
        return alternate_executable
    else:
        if base_executable.start_addr == initial_start_addr:
            return base_executable
        else:
            return make_ozmoo_executable(leafname, initial_start_addr, args, report_failure_prefix)


def make_shr_swr_executable():
    leafname = "OZMOOSH"
    args = ozmoo_base_args + ozmoo_swr_args + relocatable_args

    small_e = None
    if not cmd_args.force_big_dynmem:
        small_e = make_highest_possible_executable(leafname, args + small_dynmem_args, None)
        # Some systems may have PAGE too high to run small_e, but those systems
        # would be able to run the game if built with the big dynamic memory model.
        # highest_expected_page determines whether we're willing to prevent a system
        # running the game in order to get the benefits of the small dynamic memory
        # model.
        if small_e is not None:
            if small_e.start_addr >= highest_expected_page:
                info("Shadow+sideways RAM executable uses small dynamic memory model")
                return small_e

    # Note that we don't respect highest_expected_page when generating a big
    # dynamic memory executable; unlike the above decision about whether or not
    # to use the small dynamic memory model, we're not trading off performance
    # against available main RAM - if a system has PAGE too high to run the big
    # dynamic memory executable we generate, it just can't run the game at all
    # and there's nothing we can do about it.
    big_e = make_highest_possible_executable(leafname, args, "shadow+sideways RAM")
    if big_e is not None:
        if small_e is not None and small_e.start_addr < highest_expected_page:
            info("Shadow+sideways RAM executable uses big dynamic memory model because small model would require PAGE<=&" + ourhex(small_e.start_addr))
        else:
            info("Shadow+sideways RAM executable uses big dynamic memory model out of necessity")
    return big_e


def make_bbc_swr_executable():
    # Because of the screen hole needed to work around not having shadow RAM,
    # this executable is not relocatable. (It would be possible to use the same
    # strategy as the Electron and generate a relocatable executable with no
    # screen hole, but that would limit dynamic memory to 16K, whereas using
    # ACORN_NO_SHADOW allows dynamic memory comparable to other BBC versions.)
    # SFTODO: We *could* potentially switch between the two strategies - if
    # dynmem required is <=16K, use an Electron-style approach with no screen
    # hole and relocatable code. OTOH, that would force use of at least 16K
    # SWR and I think there's some prospect that we could make a stab at
    # running small games with no SWR and I don't really like ruling that out.
    leafname = "OZMOOB"
    args = ozmoo_base_args + ozmoo_swr_args + ["-DACORN_NO_SHADOW=1"]
    if not cmd_args.force_big_dynmem:
        small_e = make_ozmoo_executable(leafname, bbc_swr_start_addr, args + small_dynmem_args)
        if small_e is not None:
            info("BBC B sideways RAM executable uses small dynamic memory model")
            return small_e
    big_e = make_ozmoo_executable(leafname, bbc_swr_start_addr, args, "BBC B sideways RAM")
    if big_e is not None:
        info("BBC B sideways RAM executable uses big dynamic memory model")
    return big_e


def make_electron_swr_executable():
    args = ozmoo_base_args + ozmoo_swr_args + relocatable_args + ["-DACORN_ELECTRON_SWR=1"]
    # On the Electron, no main RAM is used for dynamic RAM so there's no
    # disadvantage to loading high in memory as far as the game itself is
    # concerned. However, we'd like to avoid the executable overwriting the mode
    # 6 screen RAM and corrupting the loading screen if we can, so we pick a
    # relatively low addr which should be >=PAGE on nearly all systems.
    return make_optimally_aligned_executable("OZMOOE", 0x1d00, args, "Electron")


def make_tube_executables():
    leafname = "OZMOO2P"
    tube_args = ozmoo_base_args
    if not cmd_args.force_6502:
        tube_args += ["-DCMOS=1"]
    tube_no_vmem = make_ozmoo_executable(leafname, tube_start_addr, tube_args)
    if game_blocks <= tube_no_vmem.max_nonstored_blocks():
        info("Game is small enough to run without virtual memory on second processor")
        return [tube_no_vmem]
    tube_args += ["-DVMEM=1"]
    if not cmd_args.no_tube_cache:
        tube_args += ["-DACORN_TUBE_CACHE=1"]
        tube_args += ["-DACORN_TUBE_CACHE_MIN_TIMESTAMP=%d" % min_timestamp]
        tube_args += ["-DACORN_TUBE_CACHE_MAX_TIMESTAMP=%d" % max_timestamp]
    tube_vmem = make_ozmoo_executable(leafname, tube_start_addr, tube_args, "second processor")
    if tube_vmem is not None:
        info("Game will be run using virtual memory on second processor")
    if cmd_args.no_tube_cache:
        return [tube_vmem]
    return [make_cache_executable(), tube_vmem]


def make_findswr_executable():
    return Executable("acorn-findswr.asm", "FINDSWR", None, 0x900, [])


def make_cache_executable():
    # In practice the cache executable will only be run in mode 7, but we'll
    # position it to load just below the mode 0 screen RAM.
    return Executable("acorn-cache.asm", "CACHE2P", None, 0x2c00, relocatable_args)


def make_boot():
    boot = [
        '*BASIC',
        '*DIR $',
        'MODE 135',
        'CHAIN "LOADER"',
    ]
    return File("!BOOT", 0, 0, "\r".join(boot).encode("ascii") + b"\r")


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


def make_text_loader(symbols):
    # This isn't all that user-friendly and it makes some assumptions about what
    # the BASIC code will look like. I think this is OK, as it's not a general
    # tool - it's specifically designed to work with the Ozmoo loader.
    symbols.update({k: basic_string(v) for k, v in common_labels.items()})
    symbols["MIN_VMEM_BYTES"] = basic_int(min_vmem_blocks * bytes_per_vmem_block)
    with open("templates/loader-sketch.bas", "r") as f:
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
                if not cmd_args.no_loader_crunch:
                    line = crunch_line(line, crunched_symbols)
                loader.append(line)
    return "\n".join(loader)


def make_tokenised_loader(loader_symbols):
    loader_bas = os.path.join("temp", "loader.bas")
    with open(loader_bas, "w") as f:
        f.write(make_text_loader(loader_symbols))
    loader_beebasm = os.path.join("temp", "loader.beebasm")
    loader_ssd = os.path.join("temp", "loader.ssd")
    with open(loader_beebasm, "w") as f:
        f.write('putbasic "%s", "LOADER"\n' % loader_bas)
    run_and_check([
        "beebasm",
        "-i", loader_beebasm,
        "-do", loader_ssd
    ], lambda x: b"no SAVE command" not in x)
    # Since it's the only file on the .ssd, we can get the tokenised BASIC
    # simply by chopping off the first two sectors. We peek the length out
    # of one of those sectors first.
    with open(loader_ssd, "rb") as f:
        tokenised_loader = bytearray(f.read())
        length = ((((tokenised_loader[0x10e] >> 4) & 0x3) << 16) |
                  (tokenised_loader[0x10d] << 8) | tokenised_loader[0x10c])
        tokenised_loader = tokenised_loader[512:512+length]
    return File("LOADER", host | 0x1900, host | 0x8023, tokenised_loader)


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
    parser.add_argument("-p", "--pad", action="store_true", help="pad disc image file to full size")
    parser.add_argument("-7", "--no-mode-7-colour", action="store_true", help="disable coloured status line in mode 7")
    parser.add_argument("--default-mode", metavar="N", type=int, help="default to mode N if possible")
    parser.add_argument("--auto-start", action="store_true", help="don't wait for SPACE on title page")
    parser.add_argument("--custom-title-page", metavar="P", type=str, help="use custom title page P, where P is a filename of mode 7 screen data or an edit.tf URL")
    parser.add_argument("--title", metavar="TITLE", type=str, help="set title for use on title page")
    parser.add_argument("--subtitle", metavar="SUBTITLE", type=str, help="set subtitle for use on title page")
    parser.add_argument("-4", "--only-40-column", action="store_true", help="only run in 40 column modes")
    parser.add_argument("-8", "--only-80-column", action="store_true", help="only run in 80 column modes")
    parser.add_argument("--electron-only", action="store_true", help="only support the Electron")
    parser.add_argument("--bbc-only", action="store_true", help="only support the BBC B/B+/Master")
    parser.add_argument("--no-tube", action="store_true", help="don't support second processor")
    # SFTODO: --min-relocate-addr FROM make-acorn.py, OR NEW REPLACEMENT
    parser.add_argument("-o", "--preload-opt", action="store_true", help="build in preload optimisation mode (implies -d)")
    parser.add_argument("-c", "--preload-config", metavar="PREOPTFILE", type=str, help="build with specified preload configuration previously created with -o")
    parser.add_argument("input_file", metavar="ZFILE", help="Z-machine game filename (input)")
    parser.add_argument("output_file", metavar="IMAGEFILE", nargs="?", default=None, help="Acorn DFS/ADFS disc image filename (output)")
    group = parser.add_argument_group("advanced/developer arguments (not normally needed)")
    group.add_argument("--never-defer-output", action="store_true", help="never defer output during the build")
    group.add_argument("-d", "--debug", action="store_true", help="build a debug version")
    group.add_argument("-b", "--benchmark", action="store_true", help="enable the built-in benchmark (implies -d)")
    group.add_argument("--print-swaps", action="store_true", help="print virtual memory swaps (implies -d)")
    group.add_argument("--trace", action="store_true", help="enable tracing (implies -d)")
    group.add_argument("--trace-floppy", action="store_true", help="trace disc access (implies -d)")
    group.add_argument("--trace-vm", action="store_true", help="trace virtual memory (implies -d)")
    group.add_argument("--speed", action="store_true", help="enable speed printing (implies -d)")
    group.add_argument("--no-hole-check", action="store_true", help="disable screen hole check")
    group.add_argument("--no-dynmem-adjust", action="store_true", help="disable dynamic memory adjustment")
    group.add_argument("--fake-read-errors", action="store_true", help="fake intermittent read errors")
    group.add_argument("--slow", action="store_true", help="use slow but shorter routines")
    group.add_argument("--force-big-dynmem", action="store_true", help="disable automatic selection of small dynamic memory model where possible")
    group.add_argument("--waste-bytes", metavar="N", type=int, help="waste N bytes of main RAM")
    group.add_argument("--force-65c02", action="store_true", help="use 65C02 instructions on all machines")
    group.add_argument("--force-6502", action="store_true", help="use only 6502 instructions on all machines")
    group.add_argument("--no-tube-cache", action="store_true", help="disable host cache use on second processor")
    group.add_argument("--no-loader-crunch", action="store_true", help="don't crunch the BASIC loader")

    cmd_args = parser.parse_args()

    cmd_args.verbose_level = 0 if cmd_args.verbose is None else cmd_args.verbose

    if cmd_args.only_40_column and cmd_args.only_80_column:
        die("--only-40-column and --only-80-column are incompatible")
    if cmd_args.force_65c02 and cmd_args.force_6502:
        die("--force-65c02 and --force-6502 are incompatible")
    if cmd_args.preload_opt and cmd_args.preload_config:
        die("--preload-opt and --preload-config are incompatible")

    if cmd_args.output_file is not None:
        _, user_extension = os.path.splitext(args.output_file)
        if user_extension.lower() == '.dsd':
            cmd_args.double_sided = True
        elif user_extension.lower() == '.adl':
            cmd_args.adfs = True
            cmd_args.double_sided = True
        elif user_extension.lower() == '.adf':
            cmd_args.adfs = True

    if cmd_args.default_mode is not None:
        if cmd_args.default_mode not in (0, 3, 4, 6, 7):
            die("Invalid default mode specified")
    else:
        cmd_args.default_mode = 7

    if cmd_args.title is None:
        cmd_args.title = title_from_filename(cmd_args.input_file, 40)

    if cmd_args.benchmark or cmd_args.preload_opt or cmd_args.trace or cmd_args.trace_floppy or cmd_args.trace_vm or cmd_args.speed or cmd_args.print_swaps:
        cmd_args.debug = True

    return cmd_args


def make_preload_blocks_list(config_filename):
    with open(config_filename, "rb") as f:
        preload_config = bytearray(f.read())
    blocks = []
    for i in range(len(preload_config) // 2):
        # We don't care about the timestamp on the entries in preload_config;
        # they are in order of insertion and that's what we're really interested
        # in. (This isn't the *same* as the order based on timestamp; a block
        # loaded early may of course be used again later and therefore have a
        # newer timestamp than another block loaded after it but never used
        # again.) Note that just as when we don't use preload_config, the
        # initial vmap entries will be assigned timestamps based on their order;
        # the timestamp in preload_config are ignored.
        addr = ((preload_config[i*2] & vmem_highbyte_mask) << 8) | preload_config[i*2 + 1]
        if addr & 1 == 1:
            # This is an odd address so it's invalid; we expect to see one of
            # these as the first block and we just ignore it.
            assert i == 0
            continue
        block_index = (addr - nonstored_blocks) // vmem_block_pagecount
        assert block_index >= 0
        blocks.append(block_index)
    return blocks


def make_disc_image():
    global ozmoo_base_args
    ozmoo_base_args = [
        "-DACORN=1",
        "-DACORN_CURSOR_PASS_THROUGH=1",
        "-DSTACK_PAGES=4",
        "-DSMALLBLOCK=1",
        "-DSPLASHWAIT=0",
        "-DACORN_INITIAL_NONSTORED_BLOCKS=%d" % nonstored_blocks,
        "-DACORN_DYNAMIC_SIZE_BYTES=%d" % dynamic_size_bytes,
    ]
    # SFTODO: Re-order these to match the --help output eventually
    if double_sided_dfs():
        ozmoo_base_args += ["-DACORN_DSD=1"]
    # SFTODO: I am not too happy with the ACORN_ADFS name here; I might prefer to use ACORN_OSWORD_7F for DFS and default to OSFIND/OSGBPB-for-game-data. But this will do for now while I get something working.
    if cmd_args.adfs:
        ozmoo_base_args += ["-DACORN_ADFS=1"]
    # SFTODO: assembly variable should be *ACORN_*MODE_7_STATUS
    if not cmd_args.no_mode_7_colour:
        ozmoo_base_args += ["-DMODE_7_STATUS=1"]
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
    if cmd_args.no_hole_check:
        ozmoo_base_args += ["-DACORN_DISABLE_SCREEN_HOLE_CHECK=1"]
    if cmd_args.no_dynmem_adjust:
        ozmoo_base_args += ["-DACORN_NO_DYNMEM_ADJUST=1"]
    if cmd_args.fake_read_errors:
        ozmoo_base_args += ["-DFAKE_READ_ERRORS=1"]
    if cmd_args.slow:
        ozmoo_base_args += ["-DSLOW=1"]
    if cmd_args.waste_bytes:
        ozmoo_base_args += ["-DWASTE_BYTES=%s" % cmd_args.waste_bytes]

    if z_machine_version in (3, 4, 5, 8):
        ozmoo_base_args += ["-DZ%d=1" % z_machine_version]
    else:
        die("Unsupported Z-machine version: %d" % (z_machine_version,))

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
    if cmd_args.only_80_column:
        want_bbc_swr = False # mode 7 only build
        # Arguably we should set want_electron to False, but an Electron with
        # shadow RAM might exist and it would be able to run the game.
    if not any([want_electron, want_bbc_swr, want_bbc_shr_swr, want_tube]):
        die("All possible builds have been disabled by command line options, nothing to do!")

    # We work with "executable groups" instead of executables so we can keep related
    # executables together on the disc, although this is only useful for the second
    # processor build at the moment.
    ozmoo_variants = []
    if want_electron:
        e = make_electron_swr_executable()
        if e is not None:
            ozmoo_variants.append([e])
    if want_bbc_swr:
        e = make_bbc_swr_executable()
        if e is not None:
            ozmoo_variants.append([e])
    if want_bbc_shr_swr:
        e = make_shr_swr_executable()
        if e is not None:
            ozmoo_variants.append([e])
    if want_tube:
        ozmoo_variants.append(make_tube_executables())
    if len(ozmoo_variants) == 0:
        die("No builds succeeded, can't generate disc image.")

    # We sort the executable groups by descending order of size; this isn't really
    # important unless we're doing a double-sided DFS build (where we want to
    # distribute larger things first), but it doesn't hurt to do it in all cases.
    ozmoo_variants = sorted(ozmoo_variants, key=disc_size, reverse=True)

    # SFTODO: INCONSISTENT ABOUT WHETHER ALL-LOWER OR ALL-UPPER IN LOADER_SYMBOLS
    loader_symbols = {
        "default_mode": basic_int(cmd_args.default_mode),
    }
    for executable_group in ozmoo_variants:
        for e in executable_group:
            e.add_loader_symbols(loader_symbols)
    if cmd_args.only_40_column:
        loader_symbols["ONLY_40_COLUMN"] = basic_int(1)
    if cmd_args.only_80_column:
        loader_symbols["ONLY_80_COLUMN"] = basic_int(1)
    if not cmd_args.only_40_column and not cmd_args.only_80_column:
        loader_symbols["NO_ONLY_COLUMN"] = basic_int(1)
    if cmd_args.auto_start:
        loader_symbols["AUTO_START"] = 1
    loader_screen.add_loader_symbols(loader_symbols)

    disc_contents = [boot_file, make_tokenised_loader(loader_symbols), findswr_executable]
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
        assert disc_contents[1].leafname == "LOADER"
        disc_contents[1] = make_tokenised_loader(loader_symbols)

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

    if not cmd_args.adfs:
        disc = DfsImage(disc_contents)
        if not cmd_args.double_sided:
            # Because we read multiples of vmem_block_pagecount at a time, the data file must
            # start at a corresponding sector in order to avoid a read ever straddling a track
            # boundary. (Some emulators - b-em 1770/8271, BeebEm 1770 - seem relaxed about this
            # and it will work anyway. BeebEm's 8271 emulation seems stricter about this, so
            # it's good for testing.)
            disc.add_pad_file(lambda sector: sector % vmem_block_pagecount == 0)
            disc.add_file(File("DATA", 0, 0, game_data))
            DfsImage.write_ssd(disc, output_file)
        else:
            disc2 = DfsImage(disc2_contents, boot_option=0) # 0 = no action
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
            for i in range(0, bytes_to_blocks(len(game_data)), spt):
                data[(i % (2 * spt)) // spt].extend(game_data[i*bps:i*bps+bpt])
            disc .add_file(File("DATA", 0, 0, data[0]))
            disc2.add_file(File("DATA", 0, 0, data[1]))
            DfsImage.write_dsd(disc, disc2, output_file)
    else:
        disc = AdfsImage(disc_contents)
        # There are no alignment requirements for ADFS so we don't need a pad file..
        disc.add_file(File("DATA", 0, 0, game_data))
        disc.add_directory("SAVES")
        if cmd_args.double_sided:
            disc.write_adl(output_file)
        else:
            disc.write_adf(output_file)


defer_output = False
deferred_output = []
best_effort_version = "Ozmoo"
try:
    with open(os.path.join(os.path.dirname(sys.argv[0]), "version.txt"), "r") as f:
        version_txt = f.read().strip()
    best_effort_version += " " + version_txt
except IOError:
    version_txt = None

cmd_args = parse_args()

# It's OK to run and give --help etc output if the version.txt file can't be found,
# but we don't want to generate a disc image with a missing version.
if version_txt is None:
    die("Can't find version.txt")

prechecks()

loader_screen = LoaderScreen()

header_version = 0
header_static_mem = 0xe
vmem_block_pagecount = 2
bytes_per_block = 256
bytes_per_vmem_block = vmem_block_pagecount * bytes_per_block
min_vmem_blocks = 2 # absolute minimum, one for PC, one for data SFTODO: ALLOW USER TO SPECIFY ON CMD LINE?
min_timestamp = 0
max_timestamp = 0xe0 # initial tick value
highest_expected_page = 0x2000 # SFTODO: BEST VALUE? MAKE USER CONFIGURABLE ANYWAY. ALSO A BIT MISNAMED AS WE DON'T USE IT FOR EG THE BBC NO SHADOW EXECUTABLE

ozmoo_swr_args = ["-DVMEM=1", "-DACORN_SWR=1"]
relocatable_args = ["-DACORN_RELOCATABLE=1"]
small_dynmem_args = ["-DACORN_SWR_SMALL_DYNMEM=1"]

host = 0xffff0000
tube_start_addr = 0x600
# SFTODO: I think overriding these bbc_swr_start_addres on command line would be desirable, so users with &E00 filing systems but no shadow RAM can do a build which can take advantage of the extra main RAM on their machines - however, worth noting that fiddling with these addresses opens up lots of scope for the screen hole to break the assembly - maybe I would want to offer e00 as an option and that's it, that way I can (hopefully) pre-tweak the code to handle these three values (e00, 1900, 1d00) and that will be that.
if not cmd_args.adfs:
    bbc_swr_start_addr = 0x1900
else:
    # SFTODO: Should I be using 0x1f00? That's what a model B with DFS+ADFS
    # has PAGE at. Maybe stick with this for now and see if anyone has problems,
    # so we don't pay a small performance penalty unless there's some evidence
    # it's useful.
    bbc_swr_start_addr = 0x1d00
max_start_addr = 0x3000

common_labels = {}

with open(cmd_args.input_file, "rb") as f:
    game_data = bytearray(f.read())
z_machine_version = game_data[header_version]
if z_machine_version == 3:
    vmem_highbyte_mask = 0x01
elif z_machine_version == 8:
    vmem_highbyte_mask = 0x07
else:
    vmem_highbyte_mask = 0x03
game_blocks = bytes_to_blocks(len(game_data))
dynamic_size_bytes = read_le_word(game_data, header_static_mem)
nonstored_blocks = bytes_to_blocks(dynamic_size_bytes)
while nonstored_blocks % vmem_block_pagecount != 0:
    nonstored_blocks += 1
if cmd_args.preload_config:
    cmd_args.preload_config = make_preload_blocks_list(cmd_args.preload_config)

boot_file = make_boot()
findswr_executable = make_findswr_executable()

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
            Executable.cache = {}
            deferred_output = []
            warn("Generating a double-sided disc as the game won't fit otherwise")
        else:
            if single_to_double_sided:
                die("Game is too large for even a double-sided disc")
            else:
                die("Game is too large for a double-sided disc")
show_deferred_output()

# SFTODO: If disc space permits it would be good to include the build args in a BUILD file at the "end" of the disc. Try to "anonymise" this so it doesn't include any paths or filenames.
