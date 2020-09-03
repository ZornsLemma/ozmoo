# SFTODO: Add an option to optionally disable use of CMOS instructions even on second processor

# SFTODO: Probably add a trivial "pre-loader" (probably renaming LOADER to LOAD2 or similar)
# which just does MODE 135 and CHAINs the main loader, then can remove MODE 135 from !BOOT
# and this will avoid some minor awkwardness with Integra-B installations on hard drive.

# SFTODO: Perhaps be good to check for acme and beebasm (ideally version of beebasm too)
# on startup and generate a clear error if they're not found.

# SFTODO: It might be nice to support generating non-interleaved .adf ADFS images if the
# game fits on a medium disc. Even if I don't, it might be nice to warn we're generating
# a DFS image if the user gives an output file with a .adf extension. [A 640K image is
# nicer, though, as it has loads of space free for saves, and unlike DFS where it's
# trivial to format side 2 of a .ssd, expanding a 320K ADFS M to a 640K ADFS L is not
# quite so easy. Not rocket science, but a minor faff.]

# SFTODO: Would be nice to set the disc title on the SSD; there's a possibly
# helpful function in make.rb I can copy.

# SFTODO: No support for PREOPT yet

# SFTODO: Lots of magic constants around sector size and track size and so forth here

# SFTODO: Some uses of assert check things which are not internal errors

# SFTODO: It would be good if the loader could check PAGE against what we know the shadow RAM binary will cope with, so we can get a cleaner error in the loader rather than a "PAGE too high / Bad program" error when the loader actually tries to run it.

from __future__ import print_function
import argparse
import base64
import hashlib
import os
import re
import shutil
import subprocess
import sys

def die(s):
    print(s, file=sys.stderr)
    sys.exit(1)

def info(s):
    if verbose_level >= 1:
        print(s)

def warn(s):
    print("Warning: %s" % s, file=sys.stderr)

def substitute(lst, a, b):
    return [x.replace(a, b) for x in lst]

def ourhex(i):
    return hex(i)[2:]

def basichex(i):
    return "&" + hex(i)[2:].upper()

def our_parse_int(s):
    if s.startswith("$") or s.startswith("&"):
        return int(s[1:], 16)
    if s.startswith("0x"):
        return int(s[2:], 16)
    return int(s)

# "Validate" a teletext colour control code. This is mainly needed because it's easy to
# use a space in the template to indicate "white" for one of the colours, but a space
# won't do what we need it to do.
def colour(n):
    if 129 <= n <= 135:
        return n
    else:
        return 135

def run_and_check(args, output_filter=None):
    if output_filter is None:
        output_filter = lambda x: True
    if verbose_level >= 2:
        print(" ".join(args))
    child = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    child.wait()
    child_output = [line for line in child.stdout.readlines() if output_filter(line)]
    # The child's stdout and stderr will both be output to our stdout, but that's not
    # a big deal.
    if verbose_level >= 2 and len(child_output) > 0:
        print("".join(x.decode(encoding="ascii") for x in child_output))
    if child.returncode != 0:
        die("%s failed" % args[0])

def parse_labels(filename):
    labels = {}
    with open(filename, "r") as f:
        for line in f.readlines():
            line = line[:-1]
            components = line.split("=")
            value = components[1].strip()
            i = value.find(";")
            if i != -1:
                value = value[:i]
            labels[components[0].strip()] = int(value.strip().replace("$", "0x"), 0)
    return labels

def get_word(data, i):
    return data[i]*256 + data[i+1]

def divide_round_up(x, y):
    if x % y == 0:
        return x // y
    else:
        return (x // y) + 1

def bytes_to_blocks(x):
    return divide_round_up(x, 256)

# SFTODO: Do I need to do the three character switches the OS performs automatically? We will be outputting the mode 7 header/footer using PRINT not direct memory access.
def decode_edittf_url(url):
    i = url.index(b"#")
    s = url[i+1:]
    i = s.index(b":")
    s = s[i+1:]
    s += b"===="
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

def escape_basic_string(s):
    s = s.replace(b'"', b'";CHR$(34);"')
    return s


# SFTODO: MOVE/RENAME
class Executable(object):
    def __init__(self, version, start_address, extra_args):
        self.raw_version = version
        version = version.replace("START", ourhex(start_address))
        self.version = version
        self.start_address = start_address
        self.extra_args = extra_args[:]
        if "-DACORN_NO_SHADOW=1" not in self.extra_args:
            self.extra_args += ["-DACORN_HW_SCROLL=1"]
        if "-DCMOS=1" in acme_args1 + self.extra_args:
            cpu = "65c02"
        else:
            cpu = "6502"
        os.chdir("asm")
        run_and_check(substitute(acme_args1 + ["--cpu", cpu, "--setpc", "$" + ourhex(start_address)] + self.extra_args + acme_args2, "VERSION", version))
        os.chdir("..")
        self.binary_filename = "temp/ozmoo_" + version
        self.labels_filename = "temp/acme_labels_" + version + ".txt"
        self.labels = parse_labels(self.labels_filename)

        with open(self.binary_filename, "rb") as f:
            self.binary = f.read()
        if "ACORN_RELOCATABLE" in self.labels:
            truncate_label = "reloc_count"
        else:
            truncate_label = "end_of_routines_in_stack_space"
        self.binary = self.binary[:self.labels[truncate_label]-self.labels["program_start"]]

        if "VMEM" in self.labels:
            self.binary = Executable.patch_vmem(self.binary, self.labels)

    @staticmethod
    def patch_vmem(binary, labels):
        binary = bytearray(binary)
        vmem_block_pagecount = labels["vmem_block_pagecount"]
        vmap_max_size = labels["vmap_max_size"]

        if z_machine_version == 3:
            vmem_highbyte_mask = 0x01
        elif z_machine_version == 8:
            vmem_highbyte_mask = 0x07
        else:
            vmem_highbyte_mask = 0x03

        if "ACORN_SWR" in labels:
            pseudo_ramtop = 0xc000
        else:
            pseudo_ramtop = labels["flat_ramtop"]
        ozmoo_ram_blocks = (pseudo_ramtop - labels["story_start"]) / 256
        # SFTODO: Ideally these failures would result in us generating a disc which doesn't
        # support the system type we're currently patching the binary for, but which
        # does support others.
        if nonstored_blocks > ozmoo_ram_blocks:
            die("Not enough free RAM for game's dynamic memory")
        if game_blocks > nonstored_blocks:
            min_vmem_blocks = 2 # absolute minimum, one for PC, one for data
            if nonstored_blocks + min_vmem_blocks * vmem_block_pagecount > ozmoo_ram_blocks:
                # SFTODO: On an ACORN_SWR build, this is not necessarily a problem, but let's
                # keep the check in place for now, as if we fail to meet this condition we
                # would have to require at least two sideways RAM banks in order to run and
                # right now the loader doesn't check for that.
                die("Not enough free RAM for any swappable memory")

        # Generate initial virtual memory map. We just populate the entire table; if the
        # game is smaller than this we will just never use the other entries.
        vmap_offset = binary.index(b'VVVVVVVVVVVV')
        vmap_length = 0
        while chr(binary[vmap_offset + vmap_length]) == 'V':
            vmap_length += 1
        if vmap_length & 1 != 0:
            vmap_length -= 1
        assert vmap_length >= vmap_max_size * 2
        min_age = vmem_highbyte_mask + 1
        max_age = 0xff & ~vmem_highbyte_mask
        for i in range(vmap_max_size):
            age = int(max_age + ((float(i) / vmap_max_size) * (min_age - max_age))) & ~vmem_highbyte_mask
            addr = ((nonstored_blocks // vmem_block_pagecount) + i) * vmem_block_pagecount
            if ((addr >> 8) & ~vmem_highbyte_mask) != 0:
                # This vmap entry is useless; the current Z-machine version can't contain
                # such a block.
                # SFTODO: Warn? It's harmless but it means we could have clawed back a few
                # bytes by shrinking vmap_max_size.
                addr = 0
            vmap_entry = (age << 8) | addr
            binary[vmap_offset + i + 0            ] = (vmap_entry >> 8) & 0xff
            binary[vmap_offset + i + vmap_max_size] = vmap_entry & 0xff
        return binary

    def add_to_disc(self, disc, directory, name):
        high_order = 0 if self.start_address <= 0x800 else host
        disc.add_file(directory, name, high_order | self.start_address, high_order | self.start_address, self.binary)


best_effort_version = "Ozmoo"
try:
    with open(os.path.join(os.path.dirname(sys.argv[0]), "version.txt"), "r") as f:
        version_txt = f.read().strip()
    best_effort_version += " " + version_txt
except IOError:
    version_txt = None

parser = argparse.ArgumentParser(description="Build an Acorn disc image to run a Z-machine game using %s." % (best_effort_version,))
# SFTODO: Might be good to add an option for setting -DUNSAFE=1 for maximum performance, but I probably don't want to be encouraging that just yet.
if version_txt is not None:
    parser.add_argument("--version", action="version", version=best_effort_version)
parser.add_argument("-v", "--verbose", action="count", help="be more verbose about what we're doing (can be repeated)")
parser.add_argument("-2", "--double-sided", action="store_true", help="generate a double-sided disc image (implied if IMAGEFILE has a .dsd extension)")
parser.add_argument("-7", "--no-mode-7-colour", action="store_true", help="disable coloured status line in mode 7")
parser.add_argument("-p", "--pad", action="store_true", help="pad disc image file to full size")
parser.add_argument("--default-mode", metavar="N", type=int, help="default to mode N if possible")
parser.add_argument("--auto-start", action="store_true", help="don't wait for SPACE on title page")
parser.add_argument("--custom-title-page", metavar="P", type=str, help="use custom title page P, where P is a filename of mode 7 screen data or an edit.tf URL")
parser.add_argument("--title", metavar="TITLE", type=str, help="set title for use on title page")
parser.add_argument("--subtitle", metavar="SUBTITLE", type=str, help="set subtitle for use on title page")
parser.add_argument("--min-relocate-addr", metavar="ADDR", type=str, help="assume PAGE<=ADDR if it helps use the small memory model", default="0x1900") # SFTODO: RENAME THIS ARG
parser.add_argument("-a", "--adfs", action="store_true", help="generate an ADFS disc image (implied if IMAGEFILE has a .adl extension)")
parser.add_argument("input_file", metavar="ZFILE", help="Z-machine game filename (input)")
parser.add_argument("output_file", metavar="IMAGEFILE", nargs="?", default=None, help="Acorn DFS/ADFS disc image filename (output)")
group = parser.add_argument_group("developer-only arguments (not normally needed)")
group.add_argument("-d", "--debug", action="store_true", help="build a debug version")
group.add_argument("-b", "--benchmark", action="store_true", help="enable the built-in benchmark (implies -d)")
group.add_argument("--print-swaps", action="store_true", help="print virtual memory swaps (implies -d)")
group.add_argument("--trace", action="store_true", help="enable tracing (implies -d)")
group.add_argument("--speed", action="store_true", help="enable speed printing (implies -d)")
group.add_argument("--no-hole-check", action="store_true", help="disable screen hole check")
group.add_argument("--no-dynmem-adjust", action="store_true", help="disable dynamic memory adjustment")
group.add_argument("--fake-read-errors", action="store_true", help="fake intermittent read errors")
group.add_argument("--slow", action="store_true", help="use slow but shorter routines")
group.add_argument("--force-big-dynmem", action="store_true", help="disable automatic selection of small dynamic memory model where possible")
group.add_argument("--waste-bytes", metavar="N", type=int, help="waste N bytes of main RAM")
group.add_argument("--force-65c02", action="store_true", help="use 65C02 instructions on all machines")
# SFTODO: MORE
args = parser.parse_args()
verbose_level = 0 if args.verbose is None else args.verbose

# It's OK to run and given --help etc output if the version.txt file can't be found,
# but we don't want to generate a disc image with a missing version.
if version_txt is None:
    die("Can't find version.txt")

if args.output_file is not None:
    _, user_extension = os.path.splitext(args.output_file)
    if user_extension.lower() == '.dsd':
        args.double_sided = True
    elif user_extension.lower() == '.adl':
        args.adfs = True

if args.adfs and args.double_sided:
    # SFTODO: This might not continue to be true if I support ADFS M .adf images.
    warn("--double-sided has no effect for ADFS images")
    args.double_sided = False

header_version = 0

with open(args.input_file, "rb") as f:
    game_data = bytearray(f.read())
game_blocks = bytes_to_blocks(len(game_data))

acme_args1 = [
    "acme",
    "-DACORN=1",
    "-DACORN_CURSOR_PASS_THROUGH=1",
    "-DSTACK_PAGES=4",
    "-DSMALLBLOCK=1",
    "-DSPLASHWAIT=0"
]
acme_args2 = [
    "--format", "plain",
    # SFTODO: Should really use the OS-local path join character in next three lines, not '/'
    "-l", "../temp/acme_labels_VERSION.txt",
    "-r", "../temp/acme_report_VERSION.txt",
    "--outfile", "../temp/ozmoo_VERSION",
    "ozmoo.asm"
]

# SFTODO: I am not too happy with the ACORN_ADFS name here; I might prefer to use ACORN_OSWORD_7F for DFS and default to OSFIND/OSGBPB-for-game-data. But this will do for now while I get something working.
if args.adfs:
    acme_args1 += ["-DACORN_ADFS=1"]

z_machine_version = game_data[header_version]
if z_machine_version == 3:
    acme_args1 += ["-DZ3=1"]
elif z_machine_version == 5:
    acme_args1 += ["-DZ5=1"]
elif z_machine_version == 8:
    acme_args1 += ["-DZ8=1"]
else:
    die("Unsupported Z-machine version: %d" % (z_machine_version,))

debug = args.debug
if args.benchmark:
    debug = True
    acme_args1 += ["-DBENCHMARK=1"]
if args.trace:
    debug = True
    acme_args1 += ["-DTRACE=1"]
if args.speed:
    debug = True
    acme_args1 += ["-DPRINTSPEED=1"]
if args.print_swaps:
    debug = True
    acme_args1 += ["-DPRINT_SWAPS=1"]
if args.no_hole_check:
    acme_args1 += ["-DACORN_DISABLE_SCREEN_HOLE_CHECK=1"]
if args.no_dynmem_adjust:
    acme_args1 += ["-DACORN_NO_DYNMEM_ADJUST=1"]
if args.waste_bytes:
    acme_args1 += ["-DWASTE_BYTES=%s" % (args.waste_bytes,)]
if args.fake_read_errors:
    acme_args1 += ["-DFAKE_READ_ERRORS=1"]
if args.slow:
    acme_args1 += ["-DSLOW=1"]
if args.double_sided:
    acme_args1 += ["-DACORN_DSD=1"]
if not args.no_mode_7_colour:
    acme_args1 += ["-DMODE_7_STATUS=1"]
if args.force_65c02:
    acme_args1 += ["-DCMOS=1"]
if debug:
    acme_args1 += ["-DDEBUG=1"]

try:
    os.mkdir("temp")
except OSError:
    pass

host = 0xffff0000
tube_start_addr = 0x600
if not args.adfs:
    swr_start_addr = 0x1900
else:
    # SFTODO: Should I be using 0x1f00? That's what a model B with DFS+ADFS
    # has PAGE at. Maybe stick with this for now and see if anyone has problems,
    # so we don't pay a small performance penalty unless there's some evidence
    # it's useful.
    swr_start_addr = 0x1d00
# Shadow RAM builds will load at or just above some address X and relocate
# down to PAGE or just above. X must be at or above PAGE on the system the game
# is running on. Because the code relocates down, it's mostly harmless to have
# X higher than necessary. However, the decision to build the code for the big
# or small dynamic memory model must be made at build time and it must be based
# on the worst case, i.e. a system with PAGE at X where the code can't relocate
# down. We build a version to run at X=shr_swr_default_start_address *unless*
# that uses the big dynamic memory model and there's a lower
# X>=shr_swr_min_start_addr which will allow the small dynamic memory model to
# be used. One way to look at this is that we're saying "the game must run on
# any machine where PAGE>=shr_swr_min_start_addr; it would be nice to work on
# machines where PAGE>=shr_swr_default_start_addr but we're willing to not work
# on those machines if that enables us to use the small dynamic memory model."
# SFTODO: This comment may want tweaking, but I'll wait until I've finished
# fiddling with the code.
shr_swr_min_start_addr = our_parse_int(args.min_relocate_addr)
shr_swr_default_start_addr = max(0x2000, shr_swr_min_start_addr)

tube_no_vmem = Executable("tube_no_vmem", tube_start_addr, [])

# We take some constants from the ACME labels to avoid duplicating them both
# here and in constants.asm. We need to take them from a particular build, but
# these won't vary.
common_labels = tube_no_vmem.labels
header_static_mem = common_labels["header_static_mem"]

vmem_block_pagecount = 2 # SFTODO: This doesn't vary, but ideally we'd take it from labels
dynamic_size_bytes = get_word(game_data, header_static_mem)
nonstored_blocks = bytes_to_blocks(dynamic_size_bytes)
while nonstored_blocks % vmem_block_pagecount != 0:
    nonstored_blocks += 1



if args.custom_title_page is not None:
    if args.custom_title_page.startswith("http"):
        title_page_template = decode_edittf_url(args.custom_title_page)
    else:
        with open(args.custom_title_page, "rb") as f:
            title_page_template = f.read()
        if title_page_template.startswith(b"http"):
            title_page_template = decode_edittf_url(title_page_template)
        title_page_template = bytearray(title_page_template)
        for c in title_page_template:
            if c < 32:
                die("Invalid character found in custom title page")
else:
    title_page_template = decode_edittf_url(b"https://edit.tf/#0:GpPdSTUmRfqBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECAak91JNSZF-oECBAgQIECBAgQIECBAgQIECBAgQIECBAgQICaxYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsBpPdOrCqSakyL9QIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIEEyfBiRaSCfVqUKtRBTqQaVSmgkRaUVAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQTt_Lbh2IM2_llz8t_XdkQIECBAgQIECBAgQIECBAgQIECAHIy4cmXkgzb-WXPy39d2RAgQIECBAgQIECBAgQIECBAgQIAcjTn0bNOfR0QZt_LLn5b-u7IgQIECBAgQIECBAgQIECBAgAyNOfRs059HRBiw49eflv67siBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIEEyfBiRaSCfVqUKtRBFnRKaCRFpRUCBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAgQIECBAk906EGHF-oECBAgQIECBAgQIECBAgQIECBAgQIECBAgQICaxYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsWLFixYsB0N_fLyy5EGLygSe59qbPn_UCBAgQIECBAgQIECBAgQIECA")

if args.default_mode is not None:
    default_mode = args.default_mode
    if default_mode not in (0, 3, 4, 6, 7):
        die("Invalid default mode specified")
else:
    default_mode = 7

auto_start = "TRUE" if args.auto_start else "FALSE"
if args.title is not None:
    title = args.title
else:
    title = os.path.basename(os.path.splitext(args.input_file)[0])
    # This logic has been copied from make.rb.
    camel_case = re.search("[a-z]", title) and re.search("[A-Z]", title) and not re.search(" |_", title)
    if camel_case:
        print("Q", title)
        title = re.sub("([a-z])([A-Z])", r"\1 \2", title)
        print("Q", title)
        title = re.sub("A([A-Z])", r"A \1", title)
        print("Q", title)
    title = re.sub("_+", " ", title)
    title = re.sub("(^ +)|( +)$", "", title)
    # SFTODO: DO THE "REMOVE THE IF LONGER THAN" BIT - MAY WANT TO VARY THIS FOR TITLE SCRREN VS DISC TITLE
    if re.search("^[a-z]", title):
        title = title.capitalize()



# SFTODO: Move these two classes to top of file?
class DiscFull(Exception):
    pass


class DiscImage(object):
    # This will not cope with an arbitrary disc image as a template; because it's
    # freshly generated by beebasm we know all the files are nicely contiguous
    # at the start.
    def __init__(self, template = None):
        if template is None:
            self.data = bytearray(512)
            sectors = 80 * 10
            self.data[0x107] = sectors & 0xff
            self.data[0x106] = (sectors >> 8) & 0x3
        else:
            with open(template, "rb") as f:
                self.data = bytearray(f.read())

    def catalogue_offset(self, file_number):
        return 8 + file_number * 8

    def num_files(self):
        return self.data[0x105] // 8

    def length(self, file_number):
        o = 0x100 + self.catalogue_offset(file_number)
        return (((self.data[o+6] >> 4) & 0x3) << 16) | (self.data[o+5] << 8) | self.data[o+4]

    def start_sector(self, file_number):
        o = 0x100 + self.catalogue_offset(file_number)
        return ((self.data[o+6] & 0x3) << 8) + self.data[o+7]

    def first_free_sector(self):
        if self.num_files() == 0:
            return 2
        else:
            # The last file on the disc is always in the first catalogue entry.
            return self.start_sector(0) + bytes_to_blocks(self.length(0))

    def add_to_catalogue(self, directory, name, load_addr, exec_addr, length, start_sector):
        assert self.num_files() < 31
        assert len(directory) == 1
        assert len(name) <= 7
        if bytes_to_blocks(length) >= (80*10 - start_sector):
            raise DiscFull()
        self.data[0x105] += 1*8
        self.data[0x010:0x100] = self.data[0x008:0x0f8]
        self.data[0x110:0x200] = self.data[0x108:0x1f8]
        name = (name + " "*7)[:7] + directory
        self.data[0x008:0x010] = bytearray(name, "ascii")
        self.data[0x108] = load_addr & 0xff
        self.data[0x109] = (load_addr >> 8) & 0xff
        self.data[0x10a] = exec_addr & 0xff
        self.data[0x10b] = (exec_addr >> 8) & 0xff
        self.data[0x10c] = length & 0xff
        self.data[0x10d] = (length >> 8) & 0xff
        self.data[0x10e] = (
                (((exec_addr >> 16) & 0x3) << 6) |
                (((length >> 16) & 0x3) << 4) |
                (((load_addr >> 16) & 0x3) << 2) |
                ((start_sector >> 8) & 0x3))
        self.data[0x10f] = start_sector & 0xff

    def add_file(self, directory, name, load_addr, exec_addr, data):
        self.add_to_catalogue(directory, name, load_addr, exec_addr, len(data), self.first_free_sector())
        self.data += data
        pad = (256 - len(self.data) % 256) & 0xff
        self.data += bytearray(pad)

    def get_file(self, directory, name):
        name = bytearray((name + " "*7)[:7] + directory, "ascii")
        for i in range(self.num_files()):
            offset = 8 + i * 8
            if self.data[offset:offset+8] == name:
                def extend_address(addr):
                    if ((addr >> 16) & 3) == 3:
                        return addr | 0xffff0000
                    else:
                        return addr
                load_addr = extend_address(self.data[0x100+offset] | (self.data[0x101+offset] << 8) | (((self.data[0x106+offset] >> 2) & 0x3) << 16))
                exec_addr = extend_address(self.data[0x102+offset] | (self.data[0x103+offset] << 8) | (((self.data[0x106+offset] >> 6) & 0x3) << 16))
                length = self.data[0x104+offset] | (self.data[0x105+offset] << 8) | (((self.data[0x106+offset] >> 4) & 0x3) << 16)
                start_sector = self.data[0x107+offset] | ((self.data[0x106+offset] & 0x3) << 8)
                return (load_addr, exec_addr, self.data[start_sector*256:start_sector*256+length])
        return None

    def pad(self, predicate):
        pad_start_sector = self.first_free_sector()
        pad_length = 0
        while True:
            track = (pad_start_sector + pad_length) // 10
            sector = (pad_start_sector + pad_length) % 10
            if predicate(track, sector):
                break
            pad_length += 1
        if pad_length > 0:
            self.add_file("$", "PAD", 0, 0, bytearray(256 * pad_length))

    def lock_all(self):
        for i in range(self.num_files()):
            self.data[0x00f + i*8] |= 128

    def extend(self):
        self.data += b'\0' * 256 * (80 * 10 - self.first_free_sector())


# SFTODO: Move?
class AdfsImage(object):
    UNLOCKED = 0
    LOCKED = 1
    SUBDIRECTORY = 2

    def __init__(self):
        self.catalogue = []
        self.data = bytearray(256 * 2)
        self.total_sectors = 80 * 2 * 16
        self.data[0xfc] = self.total_sectors & 0xff
        self.data[0xfd] = (self.total_sectors >> 8) & 0xff
        self.data[0x1fd] = 3 # *EXEC !BOOT
        self.data += self.make_directory("$")
        self.md5 = hashlib.md5()

    def add_file(self, directory, name, load_addr, exec_addr, data):
        assert directory == "$"
        start_sector = len(self.data) // 256
        self.data += data
        pad = (256 - len(self.data) % 256) & 0xff
        self.data += bytearray(pad)
        if (len(self.data) // 256) > self.total_sectors:
            raise DiscFull()
        self.catalogue.append([name, load_addr, exec_addr, len(data), start_sector, self.UNLOCKED])
        self.md5.update(data)

    def add_directory(self, directory, name):
        assert directory == "$"
        start_sector = len(self.data) // 256
        self.data += self.make_directory(name)
        if (len(self.data) // 256) > self.total_sectors:
            raise DiscFull()
        self.catalogue.append([name, 0, 0, 0x500, start_sector, self.SUBDIRECTORY | self.LOCKED])

    @classmethod
    def make_directory(cls, name):
        data = bytearray(5 * 256)
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

    def lock_all(self):
        for entry in self.catalogue:
            entry[5] |= self.LOCKED

    def extend(self):
        self.data += b'\0' * (80 * 2 * 16 * 256 - len(self.data))

    def finalise(self):
        first_free_sector = len(self.data) // 256
        self.data[0] = first_free_sector & 0xff
        self.data[1] = (first_free_sector >> 8) & 0xff
        self.data[2] = (first_free_sector >> 16) & 0xff
        free_space_len = 80 * 2 * 16 - first_free_sector
        self.data[0x100] = free_space_len & 0xff
        self.data[0x101] = (free_space_len >> 8) & 0xff
        self.data[0x102] = (free_space_len >> 16) & 0xff
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
            if attributes & self.LOCKED:
                self.data[offset+2] |= 128
            if attributes & self.SUBDIRECTORY:
                self.data[offset+3] |= 128
            load_addr = entry[1]
            self.data[offset+0xa] = load_addr & 0xff
            self.data[offset+0xb] = (load_addr >> 8) & 0xff
            self.data[offset+0xc] = (load_addr >> 16) & 0xff
            self.data[offset+0xd] = (load_addr >> 24) & 0xff
            exec_addr = entry[2]
            self.data[offset+0xe] = exec_addr & 0xff
            self.data[offset+0xf] = (exec_addr >> 8) & 0xff
            self.data[offset+0x10] = (exec_addr >> 16) & 0xff
            self.data[offset+0x11] = (exec_addr >> 24) & 0xff
            length = entry[3]
            self.data[offset+0x12] = length & 0xff
            self.data[offset+0x13] = (length >> 8) & 0xff
            self.data[offset+0x14] = (length >> 16) & 0xff
            self.data[offset+0x15] = (length >> 24) & 0xff
            start_sector = entry[4]
            self.data[offset+0x16] = start_sector & 0xff
            self.data[offset+0x17] = (start_sector >> 8) & 0xff
            self.data[offset+0x18] = (start_sector >> 16) & 0xff
        self.md5.update(self.data[0:0x700])
        # Use a "random" disc ID which won't vary gratuitously from run to run.
        self.data[0x1fb] = self.md5.digest()[0]
        self.data[0x1fc] = self.md5.digest()[1]
        self.data[0xff] = self.checksum(self.data[0:0xff])
        self.data[0x1ff] = self.checksum(self.data[0x100:0x1ff])

    @classmethod
    def checksum(cls, data):
        c = 0
        s = 0
        for b in data[::-1]:
            s += b + c
            c = (s & 0x100) >> 8
            s = s & 0xff
        return s

# SFTODO: Move this function?
def max_game_blocks_main_ram(executable):
    return (executable.labels["flat_ramtop"] - executable.labels["story_start"]) // 256

# SFTODO: Move this function?
def add_findswr_executable(disc):
    load_address = 0x900
    os.chdir("asm")
    extra_args = []
    run_and_check(["acme", "--setpc", "$" + ourhex(load_address), "--cpu", "6502", "--format", "plain", "-l", "../temp/acme_labels_findswr.txt", "-r", "../temp/acme_report_findswr.txt", "--outfile", "../temp/findswr", "acorn-findswr.asm"])
    os.chdir("..")
    with open("temp/findswr", "rb") as f:
        disc.add_file("$", "FINDSWR", host | load_address, host | load_address, f.read())
            
# SFTODO: Move this function?
def make_tube_executable():
    if game_blocks <= max_game_blocks_main_ram(tube_no_vmem):
        info("Game is small enough to run without virtual memory on second processor")
        e = tube_no_vmem
    else:
        info("Game will be run using virtual memory on second processor")
        e = Executable("tube_vmem", tube_start_addr, ["-DVMEM=1", "-DCMOS=1"])
    return e

# SFTODO: Move this function?
def make_relocations(alternate, master):
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
    assert len(relocations) > 0
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

# SFTODO: Move this function?
def make_small_dynmem_executable(version, start_address, extra_args):
    if not args.force_big_dynmem:
        e = Executable(version.replace("_DYNMEMSIZE", "_sdyn"), start_address, extra_args + ["-DACORN_SWR_SMALL_DYNMEM=1"])
        if nonstored_blocks <= max_game_blocks_main_ram(e):
            return e
    return Executable(version.replace("_DYNMEMSIZE", ""), start_address, extra_args)

# SFTODO: MOVE THIS FUNCTION
def info_swr_dynmem(name, labels):
    if "ACORN_SWR_SMALL_DYNMEM" in labels:
        info("Dynamic memory fits in main RAM on " + name)
    else:
        # "may" because it will depend on PAGE at runtime.
        # SFTODO: But for the sideways-no-shadow build, we only work with one PAGE, so there's actually no "may" about it.
        info("Sideways RAM may be used for dynamic memory on " + name)

# SFTODO: Move this function?
# SFTODO: I think this is OK but it could probably do with a review when I can come to it fresh
def make_swr_shr_executable():
    extra_args = ["-DVMEM=1", "-DACORN_SWR=1", "-DACORN_RELOCATABLE=1"]

    # We consider two possible start addresses one page apart in a couple of places here;
    # this is because the requirement that story_start be double page-aligned on VMEM
    # builds means one alignment will generate a smaller binary than the other as it will
    # have one less page of padding than the other. We want the smaller one because that
    # way we won't waste any RAM on machines where PAGE has the same alignment, and on
    # machines with an oppositely-aligned PAGE the relocation code will waste one page
    # to maintain the required alignment (which is no worse than having it inserted into
    # the binary by the build process).

    # 0xe00 is an arbitrary address - the relocation means we can relocate to any address
    # lower than the high version of the executable - but it's a good choice because it
    # means we can use the ACME labels/report directly for debugging on a Master.
    low_start_address_options = (0xe00, 0xe00 + 0x100)
    low_candidate = None
    for start_address in low_start_address_options:
        e = make_small_dynmem_executable("swr_shr_vmem_DYNMEMSIZE_START", start_address, extra_args)
        if low_candidate is None or len(e.binary) < len(low_candidate.binary):
            low_candidate = e
    assert low_candidate is not None

    high_candidate = None
    info_shown = False
    if "ACORN_SWR_SMALL_DYNMEM" in low_candidate.labels:
        surplus_nonstored_blocks = max_game_blocks_main_ram(low_candidate) - nonstored_blocks
        # We already picked the optimally double page-aligned version of the binary, so we don't
        # want to adjust by an odd number of pages.
        if surplus_nonstored_blocks % 2 != 0:
            surplus_nonstored_blocks -= 1
        adjusted_high_start_address = low_candidate.start_address + surplus_nonstored_blocks * 0x100
        if adjusted_high_start_address >= shr_swr_min_start_addr:
            high_candidate = make_small_dynmem_executable("swr_shr_vmem_DYNMEMSIZE_START", adjusted_high_start_address, extra_args)
            assert "ACORN_SWR_SMALL_DYNMEM" in high_candidate.labels
            # If the game has very small dynamic memory requirements, we might actually use
            # an address higher than shr_swr_default_start_addr.
            info("Adjusting sideways+shadow RAM build start address to &%s to allow dynamic memory to just fit into main RAM" % (ourhex(high_candidate.start_address),))
            info_shown = True

    if high_candidate is None:
        # We can't generate a valid high candidate with the small dynamic memory model;
        # we may or may not have been able to do so for the low candidate, but since the
        # two need to agree we must use the large dynamic memory model for both.
        if "ACORN_SWR_SMALL_DYNMEM" in low_candidate.labels:
            low_candidate = None
            for start_address in low_start_address_options:
                e = Executable("swr_shr_vmem_START", start_address, extra_args)
                if low_candidate is None or len(e.binary) < len(low_candidate.binary):
                    low_candidate = e
        assert low_candidate is not None

        # SFTODO: In principle rather than use shr_swr_default_start_addr we could pick an
        # address which just leaves n (=2?) pages of sideways RAM free in the first bank
        # for virtual memory. This would be similar to what we do in the small dynmem case
        # where we load as high as we can.
        # SFTODO: For games with a very large dynamic memory requirement, we might need to
        # allow lowering shr_swr_default_start_addr to make them fit at all. Obviously this
        # could be done simply by editing the constant value in this script, but allowing
        # an automatic adjustment using shr_swr_min_start_addr would be friendlier. However,
        # it's probably not worth worrying about this until a problematic game turns up.
        high_start_address = shr_swr_default_start_addr + (low_candidate.start_address % 0x200)
        high_candidate = Executable("swr_shr_vmem_START", high_start_address, extra_args)

    if not info_shown:
        info_swr_dynmem("sideways+shadow RAM build", high_candidate.labels)
        info("Sideways+shadow RAM build will run at %s address" % ("even" if high_candidate.start_address % 0x200 == 0 else "odd"))

    relocations = make_relocations(low_candidate.binary, high_candidate.binary)
    high_candidate.binary += relocations
    return high_candidate


# SFTODO: Move this function?
def make_bbc_swr_executable():
    e = make_small_dynmem_executable("swr_vmem_DYNMEMSIZE", swr_start_addr, ["-DVMEM=1", "-DACORN_SWR=1", "-DACORN_NO_SHADOW=1"])
    info_swr_dynmem("BBC sideways RAM build", e.labels)
    return e


# SFTODO: Move this function?
# SFTODO: Use debugger to make sure Electron binary does relocate itself down
# SFTODO: Some code duplication with make_swr_shr_executable?
def make_electron_swr_executable():
    extra_args = ["-DVMEM=1", "-DACORN_SWR=1", "-DACORN_RELOCATABLE=1", "-DACORN_ELECTRON=1", "-DACORN_SAVE_RESTORE_OSFIND=1"]

    # 0xe00 is an arbitrary address - the relocation means we can relocate to any address
    # lower than the high version of the executable - but it's a good choice because it
    # means we can use the ACME labels/report directly for debugging with an E00 DFS.
    low_start_address_options = (0xe00, 0xe00 + 0x100)
    low_candidate = None
    for start_address in low_start_address_options:
        e = Executable("electron_swr_vmem_START", start_address, extra_args)
        if low_candidate is None or len(e.binary) < len(low_candidate.binary):
            low_candidate = e
    assert low_candidate is not None

    # SFTODO: HARD-CODED VALUE
    high_start_address = 0x1d00
    if (high_start_address - low_candidate.start_address) % 0x200 != 0:
        high_start_address += 0x100
    assert (high_start_address - low_candidate.start_address) % 0x200 == 0
    high_candidate = Executable("electron_swr_vmem_START", high_start_address, extra_args)
    info_swr_dynmem("Electron sideways RAM build", high_candidate.labels)
    relocations = make_relocations(low_candidate.binary, high_candidate.binary)
    high_candidate.binary += relocations
    return high_candidate

# SFTODO: We now have the possibility to disable certain builds by command line, or to allow the generated game to simply not support certain builds if they failed. (Care with the latter; we probably do want to require --no-hole-check to explicitly push on with that, but if a game is simply too big for some configurations we should disable them. Probably wait until such a game turns up before implementing this.0

tube_executable = make_tube_executable()
# SFTODO: IF we didn't support tube, tube_detected would be a PROCdie() call.
tube_detected = 'hw$="Second processor":binary$="OZMOO2P":max_page%=&800:any_mode%=TRUE:GOTO 1000'

swr_shr_executable = make_swr_shr_executable()
swr_shr_detected = 'binary$="OZMOOSH":max_page%%=%s:any_mode%%=TRUE:GOTO 1000' % (basichex(swr_shr_executable.start_address),)

bbc_swr_executable = make_bbc_swr_executable()
bbc_swr_detected = 'binary$="OZMOOB":max_page%%=%s:any_mode%%=FALSE:GOTO 1000' % (basichex(bbc_swr_executable.start_address),)

electron_swr_executable = make_electron_swr_executable()
electron_swr_detected = 'binary$="OZMOOE":max_page%%=%s:any_mode%%=FALSE:GOTO 1000' % (basichex(electron_swr_executable.start_address),)


# SFTODO: Move this into a function, probably some of the title parsing stuff above too
with open("templates/loader.bas", "r") as loader_template:
    # Python 3 won't allow the output to be a text file, because it contains top-bit-set
    # mode 7 control codes. We therefore have to open it as binary and use os.linesep to
    # get the line endings right for the platform we're on.
    linesep = os.linesep.encode("ascii")
    with open("temp/loader.bas", "wb") as loader:
        # SFTODO: Slightly hacky but feeling my way here
        space_line = None
        first_loader_line = None
        last_loader_line = None
        normal_fg_colour = 135
        header_fg_colour = 131
        highlight_fg_colour = 131
        highlight_bg_colour = 129
        start_adjust = 0
        for line in loader_template:
            assert line[-1] == '\n'
            line = line[:-1]
            if line.startswith("REM ${BANNER}"):
                loader.write("IF host_os%=0 THEN GOTO 500" + linesep)
                header = bytearray()
                footer = bytearray()
                for i in range(0, len(title_page_template) // 40):
                    banner_line = title_page_template[i*40:(i+1)*40]
                    if b"LOADER OUTPUT STARTS HERE" in banner_line:
                        if first_loader_line is None:
                            first_loader_line = i + start_adjust
                    elif b"LOADER OUTPUT ENDS HERE" in banner_line:
                        last_loader_line = i
                        continue
                    banner_line = banner_line.replace(b"${TITLE}", title.encode("ascii"))
                    if b"${SUBTITLE}" in banner_line:
                        if args.subtitle is not None:
                            banner_line = banner_line.replace(b"${SUBTITLE}", args.subtitle.encode("ascii"))
                        else:
                            start_adjust = -1
                            continue
                    banner_line = banner_line.replace(b"${OZMOO}", best_effort_version.encode("ascii"))
                    banner_line = (banner_line + b" "*40)[:40]
                    if b"Normal foreground" in banner_line:
                        normal_fg_colour = colour(banner_line[0])
                    elif b"Header foreground" in banner_line:
                        header_fg_colour = colour(banner_line[0])
                    elif b"Highlight foreground" in banner_line:
                        highlight_fg_colour = colour(banner_line[0])
                    elif b"Highlight background" in banner_line:
                        highlight_bg_colour = colour(banner_line[0])
                    elif b"${SPACE}" in banner_line:
                        space_line = i
                        banner_line = b" "*40
                    if first_loader_line is None:
                        header += banner_line
                    elif last_loader_line is not None:
                        footer += banner_line
                if space_line is None:
                    space_line = 24
                # SFTODO: Need to handle various things not being set, either by giving error or using semi-sensible defaults
                while len(footer) > 0 and footer.startswith(b" "*40):
                    footer = footer[40:]
                    last_loader_line += 1
                loader_lines = last_loader_line + 1 - first_loader_line
                min_loader_lines = 12 # SFTODO: 11 is currently enough, but I want to keep one "spare"
                if loader_lines < min_loader_lines:
                    die("Title page needs at least %d lines for the loader; there are only %d." % (min_loader_lines, loader_lines))
                if len(footer) > 0:
                    print_command = b"PRINTTAB(0,%d);" % (last_loader_line + 1,)
                    scroll_adjust = False
                    for i in range(0, len(footer) // 40):
                        footer_line = footer[i*40:(i+1)*40]
                        if last_loader_line + 1 + i == 24:
                            if footer_line[-1] == ord(' '):
                                footer_line = footer_line[:-1]
                            else:
                                scroll_adjust = True
                        loader.write(b"%s\"%s\";%s" % (print_command, escape_basic_string(footer_line), linesep))
                        print_command = b"PRINT"
                    if scroll_adjust:
                        # We printed at the bottom right character of the screen and the OS will
                        # have automatically scrolled it, so we need to force a scroll down to
                        # fix this.
                        loader.write(b"VDU30,11" + linesep)
                    else:
                        loader.write(b"VDU30" + linesep)
                    for i in range(0, len(header) // 40):
                        header_line = header[i*40:(i+1)*40]
                        if header_line == b" "*40:
                            loader.write(b"PRINT" + linesep)
                        else:
                            loader.write(b"PRINT\"%s\";%s" % (escape_basic_string(header_line), linesep))
                loader.write("first_loader_line=%d%s" % (first_loader_line, linesep))
                loader.write("last_loader_line=%d%s" % (last_loader_line, linesep))
                loader.write("space_line=%d%s" % (space_line, linesep))
                loader.write("GOTO 600" + linesep)
                # SFTODO: Some of this code could probably just be written inline in the template rather than emitted here
                loader.write("500")
                loader.write("VDU 23,128,0;0,255,255,0,0;" + linesep)
                loader.write("FOR i%=129 TO 159:VDU 23,i%,0;0;0;0;:NEXT" + linesep)
                loader.write('PRINTTAB(1,23);STRING$(39,CHR$128);" Powered by %s";%s' % (best_effort_version.encode("ascii"), linesep))
                loader.write("VDU 30" + linesep)
                loader.write('PRINT " %s"%s' % (title.encode("ascii"), linesep))
                loader.write('PRINT " ";STRING$(39,CHR$128);' + linesep)
                if args.subtitle is not None:
                    loader.write('PRINT " %s"%s' % (args.subtitle.encode("ascii"), linesep))
                    loader.write("first_loader_line=4" + linesep)
                else:
                    loader.write("first_loader_line=3" + linesep)
                loader.write("last_loader_line=22" + linesep)
                loader.write("space_line=22" + linesep)
                loader.write("600")
            else:
                line = line.replace("${TUBEDETECTED}", tube_detected)
                line = line.replace("${BBCSHRSWRDETECTED}", swr_shr_detected)
                line = line.replace("${BBCSWRDETECTED}", bbc_swr_detected)
                line = line.replace("${ELECTRONSWRDETECTED}", electron_swr_detected)
                line = line.replace("${DEFAULTMODE}", str(default_mode))
                # SFTODO DELETE line = line.replace("${SWRMAXPAGE}", basichex(swr_executable.start_address))
                # SFTODO DELETE line = line.replace("${SHRMAXPAGE}", basichex(swr_shr_executable.start_address))
                line = line.replace("${AUTOSTART}", auto_start)
                line = line.replace("${NORMALFG}", str(normal_fg_colour))
                line = line.replace("${HEADERFG}", str(header_fg_colour))
                line = line.replace("${HIGHLIGHTFG}", str(highlight_fg_colour))
                line = line.replace("${HIGHLIGHTBG}", str(highlight_bg_colour))
                if line.startswith("REM"):
                    continue
                rem_index = line.find(":REM")
                if rem_index != -1:
                    line = line[:rem_index]
                if line == "" or line == ":":
                    continue
                loader.write(line.encode("ascii") + linesep)

run_and_check([
    "beebasm",
    "-i", "templates/base.beebasm",
    "-do", "temp/base.ssd",
    "-opt", "3"
], lambda x: b"no SAVE command" not in x)

# SFTODO: If we're building a double-sided game it might be nice if at least one of the
# Ozmoo executables could be put on the second surface, using some of the otherwise wasted
# space for the pad file.
if not args.adfs:
    disc = DiscImage("temp/base.ssd") # SFTODO: RENAME DfsImage?
    if args.double_sided:
        disc2 = DiscImage()
else:
    template_ssd = DiscImage("temp/base.ssd")
    boot = template_ssd.get_file("$", "!BOOT")
    loader = template_ssd.get_file("$", "LOADER")
    disc = AdfsImage()
    disc.add_file("$", "!BOOT", boot[0], boot[1], boot[2])
    disc.add_file("$", "LOADER", loader[0], loader[1], loader[2])

add_findswr_executable(disc)
tube_executable.add_to_disc(disc, "$", "OZMOO2P")
# SFTODO: If we do start putting one of the Ozmoo executables on the second surface
# for a double-sided game, this is probably the one to pick - it's going to be at least
# slightly larger due to the relocations, and the second surface has slightly more free
# space as it doesn't have !BOOT and LOADER on, never mind the fact it has the other
# two Ozmoo executables.
swr_shr_executable.add_to_disc(disc, "$", "OZMOOSH")
bbc_swr_executable.add_to_disc(disc, "$", "OZMOOB")
electron_swr_executable.add_to_disc(disc, "$", "OZMOOE")


# SFTODO: It would be nice if we automatically expanded to a double-sided disc if
# necessary, but since this alters the binaries we build it's a bit fiddly and I don't
# think it's a huge problem.
if not args.adfs:
    if not args.double_sided:
        # Because we read multiples of vmem_block_pagecount at a time, the data file must
        # start at a corresponding sector in order to avoid a read ever straddling a track
        # boundary. (Some emulators - b-em 1770/8271, BeebEm 1770 - seem relaxed about this
        # and it will work anyway. BeebEm's 8271 emulation seems stricter about this, so
        # it's good for testing.)
        disc.pad(lambda track, sector: sector % vmem_block_pagecount == 0)
        try:
            disc.add_file("$", "DATA", 0, 0, game_data)
        except DiscFull:
            die("Game won't fit on a single-sided disc, try specifying --double-sided")
    else:
        # The game data must start on a track boundary at the same place on both surfaces.
        disc.pad(lambda track, sector: sector == 0)
        disc2.pad(lambda track, sector: track * 10 + sector == disc.first_free_sector())
        data = [bytearray(), bytearray()]
        for i in range(0, bytes_to_blocks(len(game_data)), 10):
            data[(i % 20) // 10].extend(game_data[i*256:i*256+10*256])
        try:
            disc.add_file("$", "DATA", 0, 0, data[0])
            disc2.add_file("$", "DATA", 0, 0, data[1])
        except DiscFull:
            die("Game won't fit on a double-sided disc")
else:
    # There are no alignment requirements for ADFS.
    disc.add_file("$", "DATA", 0, 0, game_data)

disc.lock_all()
if args.double_sided:
    disc2.lock_all()

# SFTODO: This is a bit of a hack, may well be able to simplify/improve
if not args.adfs:
    preferred_extension = ".dsd" if args.double_sided else ".ssd"
    if args.output_file is None:
        output_file = os.path.basename(os.path.splitext(args.input_file)[0] + preferred_extension)
    else:
        user_prefix, user_extension = os.path.splitext(args.output_file)
        # If the user wants to call the file .img or something, we'll leave it alone.
        if user_extension.lower() in (".ssd", ".dsd") and user_extension.lower() != preferred_extension.lower():
            warn("Changing extension of output from %s to %s" % (user_extension, preferred_extension))
            user_extension = preferred_extension
        output_file = user_prefix + user_extension
else:
    preferred_extension = ".adl"
    if args.output_file is None:
        output_file = os.path.basename(os.path.splitext(args.input_file)[0] + preferred_extension)
    else:
        output_file = args.output_file

if args.adfs:
    # We could put this on right at the start, but it feels a bit neater to have it
    # after all the game data on the disc.
    disc.add_directory("$", "SAVES")
    disc.finalise()

if args.pad:
    disc.extend()
    if args.double_sided:
        disc2.extend()

# SFTODO: Move this
def get_track(data, start, size):
    return (data[start:start+size] + bytearray(size))[:size]

with open(output_file, "wb") as f:
    if not args.adfs:
        if not args.double_sided:
            f.write(disc.data)
        else:
            track_size = 256 * 10
            for track in range(80):
                i = track * track_size
                if i >= len(disc.data) and i >= len(disc2.data):
                    break
                f.write(get_track(disc.data, i, track_size))
                f.write(get_track(disc2.data, i, track_size))
    else:
        max_track = min(divide_round_up(len(disc.data), 16 * 256), 80)
        track_size = 256 * 16
        for track in range(max_track):
            for surface in range(2):
                f.write(get_track(disc.data, (surface*80 + track) * track_size, track_size))
