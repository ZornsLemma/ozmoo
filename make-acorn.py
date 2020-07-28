# SFTODO: Do proper argument parsing etc

# SFTODO: Would be nice to set the disc title on the SSD; there's a possibly
# helpful function in make.rb I can copy.

# SFTODO: No support for PREOPT yet

# SFTODO: No support for all-RAM non-VMEM builds yet

from __future__ import print_function
import os
import sys

# SFTODO: Can we take some/all of these values from the labels dictionary? That
# would be much neater and safer.
ramtop = 0xf800
header_version = 0
header_static_mem = 0xe
vm_page_blocks = 2 # VM pages are 2x256 byte blocks
vmap_max_size = 102

labels = {}
with open("temp/acme_labels.txt", "r") as f:
    for line in f.readlines():
        line = line[:-1]
        components = line.split("=")
        value = components[1].strip()
        i = value.find(";")
        if i != -1:
            value = value[:i]
        labels[components[0].strip()] = int(value.strip().replace("$", "0x"), 0)

with open(sys.argv[1], "rb") as f:
    game_data = bytearray(f.read())
output_name = os.path.basename(os.path.splitext(sys.argv[1])[0] + ".ssd")


# This will not cope with an arbitrary disc image; because it's freshly generated
# by beebasm we know all the files are nicely contiguous at the start.
class DiscImage(object):
    def __init__(self, template):
        with open(template, "rb") as f:
            self.data = bytearray(f.read())

    def catalogue_offset(self, file_number):
        return 8 + file_number * 8

    def num_files(self):
        return self.data[0x105] / 8

    def length(self, file_number):
        o = 0x100 + self.catalogue_offset(file_number)
        return (self.data[o+5] << 8) + self.data[o+4]

    def start_sector(self, file_number):
        o = 0x100 + self.catalogue_offset(file_number)
        return ((self.data[o+6] & 0x3) << 8) + self.data[o+7]

    def first_free_sector(self):
        # The last file on the disc is always in the first catalogue entry.
        return self.start_sector(0) + bytes_to_blocks(self.length(0))

    def add_to_catalogue(self, directory, name, load_addr, exec_addr, length, start_sector):
        assert self.num_files() < 31
        assert len(directory) == 1
        assert len(name) <= 7
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

    def lock_all(self):
        for i in range(self.num_files()):
            self.data[0x00f + i*8] |= 128


def get_word(data, i):
    return data[i]*256 + data[i+1]

def bytes_to_blocks(x):
    if x & 0xff != 0:
        return int(x / 256) + 1
    else:
        return int(x / 256)


ssd = DiscImage("temp/base.ssd")

max_preload_blocks = (ramtop - labels["story_start"]) / 256

game_blocks = bytes_to_blocks(len(game_data))
while game_blocks * 256 > len(game_data):
    game_data.append(0)


z_machine_version = game_data[header_version]
if z_machine_version == 3:
    vmem_highbyte_mask = 0x01
elif z_machine_version == 8:
    vmem_highbyte_mask = 0x07
else:
    vmem_highbyte_mask = 0x03

dynamic_size = get_word(game_data, header_static_mem)
nonstored_blocks = bytes_to_blocks(dynamic_size)
# Virtual memory works in 512 byte (=2 block) chunks
if nonstored_blocks & 1 != 0:
    nonstored_blocks += 1

if nonstored_blocks > max_preload_blocks:
    die("Not enough free RAM for dynamic memory")
if game_blocks > nonstored_blocks:
    if nonstored_blocks + vm_page_blocks > max_preload_blocks:
        die("Not enough free RAM for any swappable memory")
    # SFTODO: One VM block of swappable memory is not really going to
    # work. We should probably emit a warning unless there are at
    # least n blocks of swappable memory. I suspect you need at least
    # 2 VM blocks=4 pages of RAM, one VM block for the PC and one VM
    # block for data access. That would thrash like hell but I think
    # in principle it should work, and might be a good torture test.

# SFTODO: We mostly don't need preload_blocks now, but I'll keep it for now as it
# is how we can decide whether we need VMEM or not.
if game_blocks <= max_preload_blocks:
    preload_blocks = game_blocks
    # SFTODO: We don't need virtual memory support in this case. I'm not sure
    # we "should" do anything - maybe warn, so the user can choose to do a
    # RAM-only build (which will perform better)? Note that if we eventually
    # build discs which have both 2P and non-2P support, the 2P version may be
    # RAM-only and the non-2P one probably will use VM.
    # SFTODO: When/if this takes care of the whole build without make-acorn.sh,
    # we can of course go back and rebuild a non-VM-capable version of ozmoo;
    # we might not even make the user specify whether they want VM or not.
    print("Warning: virtual memory is not needed")
else:
    preload_blocks = max_preload_blocks

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
    low = ((nonstored_blocks // vm_page_blocks) + i) * vm_page_blocks
    # If this assertion fails, we need to be setting the low bits of high with
    # the high bits of low. :-)
    assert low & 0xff == low
    ssd.data[vmap_offset + i + 0            ] = high
    ssd.data[vmap_offset + i + vmap_max_size] = low

first_free_sector = ssd.first_free_sector()
# SFTODO: I thought this padding would be necessary, but it seems not to be even if
# I deliberately force a "bad" alignment.
if False:
    pad_sectors = 0
    while (first_free_sector + pad_sectors) & (vm_page_blocks - 1) != 0:
        # readblocks will fail if we try to read a VM block which straddles a track
        # boundary, so make sure the game data starts on a multiple of the block
        # size.
        pad_sectors += 1
    if pad_sectors != 0:
        ssd.data.extend(pad_sectors * 256 * b'\0')
        ssd.add_to_catalogue("$", "PAD", 0, 0, pad_sectors * 256, first_free_sector)
        first_free_sector += pad_sectors
ssd.data.extend(game_data)
ssd.add_to_catalogue("$", "DATA", 0, 0, game_blocks * 256, first_free_sector)
ssd.lock_all()

with open(output_name, "wb") as f:
    f.write(ssd.data)
