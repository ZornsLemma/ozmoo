# SFTODO: Perhaps be good to check for acme and beebasm (ideally version of beebasm too)
# on startup and generate a clear error if they're not found.

# SFTODO: Would be nice to set the disc title on the SSD; there's a possibly
# helpful function in make.rb I can copy.

# SFTODO: No support for PREOPT yet

# SFTODO: Lots of magic constants around sector size and track size and so forth here

# SFTODO: Some uses of assert check things which are not internal errors

from __future__ import print_function
import argparse
import os
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

def bytes_to_blocks(x):
    if x & 0xff != 0:
        return int(x / 256) + 1
    else:
        return int(x / 256)

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
        os.chdir("asm")
        run_and_check(substitute(acme_args1 + ["--setpc", "$" + ourhex(start_address)] + self.extra_args + acme_args2, "VERSION", version))
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
        vmap_offset = binary.index(b'VVVVVVVVV')
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


best_effort_version = "Ozmoo"
try:
    with open(os.path.join(os.path.dirname(sys.argv[0]), "version.txt"), "r") as f:
        version_txt = f.read().strip()
    best_effort_version += " " + version_txt
except IOError:
    version_txt = None

parser = argparse.ArgumentParser(description="Build an Acorn disc image to run a Z-machine game using %s." % (best_effort_version,))
if version_txt is not None:
    parser.add_argument("--version", action="version", version=best_effort_version)
parser.add_argument("-v", "--verbose", action="count", help="be more verbose about what we're doing (can be repeated)")
parser.add_argument("-2", "--double-sided", action="store_true", help="generate a double-sided disc image (implied if IMAGEFILE has a .dsd extension)")
parser.add_argument("-7", "--no-mode-7-colour", action="store_true", help="disable coloured status line in mode 7")
parser.add_argument("-p", "--pad", action="store_true", help="pad disc image file to full size")
parser.add_argument("input_file", metavar="ZFILE", help="Z-machine game filename (input)")
parser.add_argument("output_file", metavar="IMAGEFILE", nargs="?", default=None, help="Acorn DFS disc image filename (output)")
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
    "--cpu", "6502",
    "--format", "plain",
    # SFTODO: Should really use the OS-local path join character in next three lines, not '/'
    "-l", "../temp/acme_labels_VERSION.txt",
    "-r", "../temp/acme_report_VERSION.txt",
    "--outfile", "../temp/ozmoo_VERSION",
    "ozmoo.asm"
]

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
if debug:
    acme_args1 += ["-DDEBUG=1"]

try:
    os.mkdir("temp")
except OSError:
    pass

host = 0xffff0000
tube_start_addr = 0x600
swr_start_addr = 0x1900
# Shadow RAM builds will load at or just above shr_swr_start_addr and relocate
# down to PAGE or just above. shr_swr_start_addr must be high enough to
# acommodate the maximum PAGE the binary will support. It is still better if
# this isn't too high as it will increase the worst case main RAM free, which
# may allow ACORN_SWR_SMALL_DYNMEM to be used.
shr_swr_start_addr = 0x1000 # SFTODO TEMP HACK SHOULD BE 0x2000

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




with open("templates/loader.bas", "r") as loader_template:
    with open("temp/loader.bas", "w") as loader:
        for line in loader_template:
            line = line.replace("${OZMOOVERSION}", best_effort_version)
            loader.write(line)

run_and_check([
    "beebasm",
    "-i", "templates/base.beebasm",
    "-do", "temp/base.ssd",
    "-opt", "3"
], lambda x: b"no SAVE command" not in x)


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


# SFTODO: Move this function?
def max_game_blocks_main_ram(executable):
    return (executable.labels["flat_ramtop"] - executable.labels["story_start"]) // 256
            
# SFTODO: Move this function?
def add_tube_executable(ssd):
    if game_blocks <= max_game_blocks_main_ram(tube_no_vmem):
        info("Game is small enough to run without virtual memory on second processor")
        e = tube_no_vmem
    else:
        info("Game will be run using virtual memory on second processor")
        e = Executable("tube_vmem", tube_start_addr, ["-DVMEM=1"])
    ssd.add_file("$", "OZMOO2P", tube_start_addr, tube_start_addr, e.binary)

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
def info_no_swr_dynmem(name, labels):
    if "ACORN_SWR_SMALL_DYNMEM" in labels:
        info("Dynamic memory fits in main RAM on " + name)
    else:
        # "may" because it will depend on PAGE at runtime.
        info("Sideways RAM may be used for dynamic memory on " + name)

# SFTODO: Move this function?
def add_swr_shr_executable(ssd):
    candidate = None
    extra_args = ["-DVMEM=1", "-DACORN_SWR=1", "-DACORN_RELOCATABLE=1"]
    # Because VMEM builds require story_start to be double-page aligned, the executable will be quite
    # different depending on whether it starts at an even or odd page and the size of padding required
    # to make that start address work with a double-page alignment for story_start. We pick whichever
    # of an odd or even start address gives the smallest executable, and preserve that alignment
    # (rounding up the relocation target address on the system we're running on necessary) when
    # we relocate down. This avoids wasting a double page, as could happen if we arbitarily picked
    # a start address which needed an extra page of padding and then the system we're running on
    # has an oppositely aligned ideal relocation target.
    for start_address in (shr_swr_start_addr, shr_swr_start_addr + 0x100):
        e = make_small_dynmem_executable("swr_shr_vmem_DYNMEMSIZE_START", start_address, ["-DVMEM=1", "-DACORN_SWR=1", "-DACORN_RELOCATABLE=1"])
        if candidate is None or len(e.binary) < len(candidate.binary):
            candidate = e
    assert candidate is not None

    info_no_swr_dynmem("sideways+shadow RAM build", candidate.labels)
    info("Sideways+shadow RAM build will run at %s address" % ("even" if candidate.start_address % 0x200 == 0 else "odd"))

    high_executable = candidate
    high_start_address = high_executable.start_address
    low_start_address = 0xe00 + (high_start_address % 0x200)
    low_executable = Executable(high_executable.raw_version, low_start_address, high_executable.extra_args)
    relocations = make_relocations(low_executable.binary, high_executable.binary)
    # SFTODO: If we do start putting one of the Ozmoo executables on the second surface
    # for a double-sided game, this is probably the one to pick - it's going to be at least
    # slightly larger due to the relocations, and the second surface has slightly more free
    # space as it doesn't have !BOOT and LOADER on, never mind the fact it has the other
    # two Ozmoo executables.
    ssd.add_file("$", "OZMOOSH", host | high_start_address, host | high_start_address, high_executable.binary + relocations)


# SFTODO: Move this function?
def add_swr_executable(ssd):
    e = make_small_dynmem_executable("swr_vmem_DYNMEMSIZE", swr_start_addr, ["-DVMEM=1", "-DACORN_SWR=1", "-DACORN_NO_SHADOW=1"])
    info_no_swr_dynmem("sideways RAM build", e.labels)
    ssd.add_file("$", "OZMOOSW", host | swr_start_addr, host | swr_start_addr, e.binary)


# SFTODO: If we're building a double-sided game it might be nice if at least one of the
# Ozmoo executables could be put on the second surface, using some of the otherwise wasted
# space for the pad file.
ssd = DiscImage("temp/base.ssd")
if args.double_sided:
    ssd2 = DiscImage()

add_tube_executable(ssd)
add_swr_shr_executable(ssd)
add_swr_executable(ssd)


# SFTODO: It would be nice if we automatically expanded to a double-sided disc if
# necessary, but since this alters the binaries we build it's a bit fiddly and I don't
# think it's a huge problem.
if not args.double_sided:
    # Because we read multiples of vmem_block_pagecount at a time, the data file must
    # start at a corresponding sector in order to avoid a read ever straddling a track
    # boundary. (Some emulators - b-em 1770/8271, BeebEm 1770 - seem relaxed about this
    # and it will work anyway. BeebEm's 8271 emulation seems stricter about this, so
    # it's good for testing.)
    ssd.pad(lambda track, sector: sector % vmem_block_pagecount == 0)
    try:
        ssd.add_file("$", "DATA", 0, 0, game_data)
    except DiscFull:
        die("Game won't fit on a single-sided disc, try specifying --double-sided")
else:
    # The game data must start on a track boundary at the same place on both surfaces.
    ssd.pad(lambda track, sector: sector == 0)
    ssd2.pad(lambda track, sector: track * 10 + sector == ssd.first_free_sector())
    data = [bytearray(), bytearray()]
    for i in range(0, bytes_to_blocks(len(game_data)), 10):
        data[(i % 20) // 10].extend(game_data[i*256:i*256+10*256])
    try:
        ssd.add_file("$", "DATA", 0, 0, data[0])
        ssd2.add_file("$", "DATA", 0, 0, data[1])
    except DiscFull:
        die("Game won't fit on a double-sided disc")

ssd.lock_all()
if args.double_sided:
    ssd2.lock_all()

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

if args.pad:
    ssd.extend()
    if args.double_sided:
        ssd2.extend()

with open(output_file, "wb") as f:
    if not args.double_sided:
        f.write(ssd.data)
    else:
        track_size = 256 * 10
        for track in range(80):
            i = track * track_size
            if i >= len(ssd.data) and i >= len(ssd2.data):
                break
            f.write(ssd.data[i:i+track_size])
            f.write(ssd2.data[i:i+track_size])
