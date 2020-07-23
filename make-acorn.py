# SFTODO: Do proper argument parsing etc

# SFTODO: Would be nice to set the disc title on the SSD; there's a possibly
# helpful function in make.rb I can copy.

# SFTODO: No support for PREOPT yet

# SFTODO: No support for all-RAM non-VMEM builds yet

from __future__ import print_function
import sys

ramtop = 0xf800

header_static_mem = 0xe

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
    game_data = f.read()


# This will not cope with an arbitrary disc image; because it's freshly generated
# by beebasm we know all the files are nicely contiguous at the start.
class DiscImage(object):
    def __init__(self, template):
        with open(template, "rb") as f:
            self.data = f.read()


def get_word(data, i):
    return ord(data[i])*256 + ord(data[i+1])


ssd = DiscImage("temp/base.ssd")

max_preload_blocks = (ramtop - labels["story_start"]) / 256

dynamic_size = get_word(game_data, header_static_mem)
nonstored_blocks = int(dynamic_size / 256)
if dynamic_size & 0xff != 0:
    nonstored_blocks += 1
# Virtual memory works in 512 byte (=2 block) chunks
if nonstored_blocks & 1 != 0:
    nonstored_blocks += 1

if nonstored_blocks > max_preload_blocks:
    die("Not enough free RAM for dynamic memory")
# SFTODO: If (as is very likely, but not necessarily guaranteed) the game as a whole is larger
# than nonstored_blocks, we need *at least* one free block of RAM beyond that, and in
# reality at least a handful, for use for paging in static/high memory. We should die if we
# don't have even one more free block and warn if we only have a few.

preload_blocks = min(nonstored_blocks, max_preload_blocks)

# TODO: WE NEED TO GENERATE THE VM CONFIG, SAVE IT TO A "CONFIG" FILE ON DISC AND
# HAVE THE VM CODE (IT CAN BE IN DISCARDABLE INIT) OSFILE "*LOAD" CONFIG INTO THE VM
# SPACE. THE VM THEN NEEDS TO OSFILE "*LOAD" THE PRELOAD FILE INTO THE STORY SPACE
# (AGAIN, CAN BE IN DISCARDABLE INIT)

# SFTODO: WE NEED TO PUT THE GAME DATA ON THE DISC *AND* GENERATE AN OVERLAPPING
# CATALOG ENTRY CALLED "PRELOAD" WHICH INCLUDES THE FIRST preload_blocks BLOCKS
# OF THAT FILE.
