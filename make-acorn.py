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
        print("".join(child_output))
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

parser = argparse.ArgumentParser(description="Build an Acorn disc image to run a Z-machine game using Ozmoo.")
parser.add_argument("-v", "--verbose", action="count", help="be more verbose about what we're doing (can be repeated)")
parser.add_argument("-2", "--double-sided", action="store_true", help="generate a double-sided disc image (implied if IMAGEFILE has a .dsd extension)")
parser.add_argument("input_file", metavar="ZFILE", help="Z-machine game filename (input)")
parser.add_argument("output_file", metavar="IMAGEFILE", nargs="?", default=None, help="Acorn DFS disc image filename (output)")
group = parser.add_argument_group("developer-only arguments (not normally needed)")
group.add_argument("-d", "--debug", action="store_true", help="build a debug version")
group.add_argument("-b", "--benchmark", action="store_true", help="enable the built-in benchmark (implies -d)")
group.add_argument("--trace", action="store_true", help="enable tracing (implies -d)")
group.add_argument("--speed", action="store_true", help="enable speed printing (implies -d)")
# SFTODO: MORE
args = parser.parse_args()
verbose_level = 0 if args.verbose is None else args.verbose

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
    "--setpc", "$600",
    "-DACORN=1",
    "-DSTACK_PAGES=4",
    "-DSMALLBLOCK=1",
    "-DSPLASHWAIT=0"
]
acme_args2 = [
    "--cpu", "6502",
    "--format", "plain",
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
if args.double_sided:
    acme_args1 += ["-DACORN_DSD=1"]
if debug:
    acme_args1 += ["-DDEBUG=1"]

try:
    os.mkdir("temp")
except OSError:
    pass
os.chdir("asm")
run_and_check(substitute(acme_args1 + acme_args2, "VERSION", "no_vmem"))
run_and_check(substitute(acme_args1 + ["-DVMEM=1"] + acme_args2, "VERSION", "vmem"))
os.chdir("..")

labels_no_vmem = parse_labels("temp/acme_labels_no_vmem.txt")
ramtop_no_vmem = labels_no_vmem["ramtop"]
assert ramtop_no_vmem >= 0x7c00 # SFTODO: ready to catch this changing to an address not a value
max_game_blocks_no_vmem = (ramtop_no_vmem - labels_no_vmem["story_start"]) / 256
# SFTODO: If we put the binary on the disc ourselves - which isn't a big deal -
# instead of letting beebasm do it, we wouldn't need to do the file copying here.
if game_blocks <= max_game_blocks_no_vmem:
    info("Game is small enough to run without virtual memory")
    labels = labels_no_vmem
    shutil.copyfile("temp/ozmoo_no_vmem", "temp/ozmoo")
else:
    info("Game will be run using virtual memory")
    labels = parse_labels("temp/acme_labels_vmem.txt")
    shutil.copyfile("temp/ozmoo_vmem", "temp/ozmoo")

run_and_check([
    "beebasm",
    "-i", "templates/base.beebasm",
    "-do", "temp/base.ssd",
    "-opt", "3"
], lambda x: b"no SAVE command" not in x)

header_static_mem = labels["header_static_mem"]
double_sided = "ACORN_DSD" in labels
use_vmem = "VMEM" in labels
if use_vmem:
    vmem_block_pagecount = labels["vmem_block_pagecount"]
    vmap_max_size = labels["vmap_max_size"]


class DiscFull(object):
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
        return (self.data[o+5] << 8) + self.data[o+4]

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


ssd = DiscImage("temp/base.ssd")
if double_sided:
    ssd2 = DiscImage()

if use_vmem:
    if z_machine_version == 3:
        vmem_highbyte_mask = 0x01
    elif z_machine_version == 8:
        vmem_highbyte_mask = 0x07
    else:
        vmem_highbyte_mask = 0x03

    dynamic_size_bytes = get_word(game_data, header_static_mem)
    nonstored_blocks = bytes_to_blocks(dynamic_size_bytes)
    while nonstored_blocks % vmem_block_pagecount != 0:
        nonstored_blocks += 1
    ozmoo_ram_blocks = (labels["ramtop"] - labels["story_start"]) / 256
    if nonstored_blocks > ozmoo_ram_blocks:
        die("Not enough free RAM for game's dynamic memory")
    if game_blocks > nonstored_blocks:
        min_vmem_blocks = 2 # absolute minimum, one for PC, one for data
        if nonstored_blocks + min_vmem_blocks * vmem_block_pagecount > ozmoo_ram_blocks:
            die("Not enough free RAM for any swappable memory")

    # Generate initial virtual memory map. We just populate the entire table; if the
    # game is smaller than this we will just never use the other entries. We patch
    # this directly into the ozmoo binary.
    vmap_offset = ssd.data.index(b'VVVVVVVVV')
    vmap_length = 0
    while chr(ssd.data[vmap_offset + vmap_length]) == 'V':
        vmap_length += 1
    if vmap_length & 1 != 0:
        vmap_length -= 1
    assert vmap_length >= vmap_max_size * 2
    for i in range(vmap_max_size):
        high = (256 - 8 * (i // 4) - 32) & ~vmem_highbyte_mask
        low = ((nonstored_blocks // vmem_block_pagecount) + i) * vmem_block_pagecount
        # If this assertion fails, we need to be setting the low bits of high with
        # the high bits of low. :-)
        assert low & 0xff == low
        ssd.data[vmap_offset + i + 0            ] = high
        ssd.data[vmap_offset + i + vmap_max_size] = low

# SFTODO: It would be nice if we automatically expanded to a double-sided disc if
# necessary, but since this alters the binaries we build it's a bit fiddly and I don't
# think it's a huge problem.
if not double_sided:
    # SFTODO: I thought this padding would be necessary, but it seems not to be even if
    # I deliberately force a "bad" alignment.
    if False:
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
        data[(i % 20) / 10].extend(game_data[i*256:i*256+10*256])
    try:
        ssd.add_file("$", "DATA", 0, 0, data[0])
        ssd2.add_file("$", "DATA", 0, 0, data[1])
    except DiscFull:
        die("Game won't fit on a double-sided disc")

ssd.lock_all()
if double_sided:
    ssd2.lock_all()

preferred_extension = ".dsd" if double_sided else ".ssd"
if args.output_file is None:
    output_file = os.path.basename(os.path.splitext(args.input_file)[0] + preferred_extension)
else:
    user_prefix, user_extension = os.path.splitext(args.output_file)
    # If the user wants to call the file .img or something, we'll leave it alone.
    if user_extension.lower() in (".ssd", ".dsd") and user_extension.lower() != preferred_extension.lower():
        warn("Changing extension of output from %s to %s" % (user_extension, preferred_extension))
        user_extension = preferred_extension
    output_file = user_prefix + user_extension

with open(output_file, "wb") as f:
    if not double_sided:
        f.write(ssd.data)
    else:
        track_size = 256 * 10
        for track in range(80):
            i = track * track_size
            if i >= len(ssd.data) and i >= len(ssd2.data):
                break
            f.write(ssd.data[i:i+track_size])
            f.write(ssd2.data[i:i+track_size])
