# SFTODO: Do proper argument parsing etc

# SFTODO: Would be nice to set the disc title on the SSD; there's a possibly
# helpful function in make.rb I can copy.

# SFTODO: No support for PREOPT yet

# SFTODO: No support for all-RAM non-VMEM builds yet

from __future__ import print_function
import os
import sys

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
    # least n blocks of swappable memory.

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
    ssd.data[vmap_offset + i + 0            ] = high
    ssd.data[vmap_offset + i + vmap_max_size] = low

# We're going to add two files which share the same data on the disc. This is a
# bit naughty but it means we can avoid duplicating any data. We put the longer
# of the two files first in the catalogue as that seems least likely to cause
# problems if the disc is written to by DFS.
# SFTODO: Might be worth experimenting with writing to the disc using dfferent
# DFSes. *COMPACT on DFS 2.24 seems to undo the duplication, but (not tested)
# not actually corrupt the data. Not ideal. I suppose I *could* just make
# PRELOAD and DATA disjoint and have readblocks use the right one. However,
# if we wanted two different-sized PRELOADS (e.g. for a 2P or SWR disc) that
# wouldn't work. We could have *three* separate disjoint files. Gets a bit
# messy in a different way though. Actually since we could guarantee the
# disjoint files followed each other, readblocks wouldn't need to do
# anything clever (except in the currently hypothetical case of an interleaved
# double-sided disc), it would just use the start of PRELOAD as its base.
# I don't think this helps the two-different-sized PRELOADs issue. OTOH a SWR
# PRELOAD would probably be a dozen-ish K due to limited main RAM size on non-2P
# so the duplication isn't too big a deal. Still rather avoid it though.
# SFTODO: *Maybe* we should only use this trick if we would otherwise run out
# of space on the disc, and duplicate otherwise? OTOH, assuming it isn't risky
# to save onto a disc with this trick, avoiding the duplication will leave more
# space for saved games.
first_free_sector = ssd.first_free_sector()
ssd.data.extend(game_data)
ssd.add_to_catalogue("$", "PRELOAD", 0, 0, preload_blocks * 256, first_free_sector)
ssd.add_to_catalogue("$", "DATA", 0, 0, game_blocks * 256, first_free_sector)

with open(output_name, "wb") as f:
    f.write(ssd.data)
