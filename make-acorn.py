# SFTODO: Do proper argument parsing etc

# SFTODO: Would be nice to set the disc title on the SSD; there's a possibly
# helpful function in make.rb I can copy.

# SFTODO: No support for PREOPT yet

# SFTODO: No support for all-RAM non-VMEM builds yet

from __future__ import print_function
import sys

ramtop = 0xf800
header_static_mem = 0xe
vm_page_blocks = 2 # VM pages are 2x256 byte blocks

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

def bytes_to_blocks(x):
    if x & 0xff != 0:
        return int(x / 256) + 1
    else:
        return int(x / 256)


ssd = DiscImage("temp/base.ssd")

max_preload_blocks = (ramtop - labels["story_start"]) / 256

game_blocks = bytes_to_blocks(len(game_data))

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
else:
    preload_blocks = max_preload_blocks

# TODO: WE NEED TO GENERATE THE VM CONFIG, SAVE IT TO A "CONFIG" FILE ON DISC AND
# HAVE THE VM CODE (IT CAN BE IN DISCARDABLE INIT) OSFILE "*LOAD" CONFIG INTO THE VM
# SPACE. THE VM THEN NEEDS TO OSFILE "*LOAD" THE PRELOAD FILE INTO THE STORY SPACE
# (AGAIN, CAN BE IN DISCARDABLE INIT)

# SFTODO: WE NEED TO PUT THE GAME DATA ON THE DISC *AND* GENERATE AN OVERLAPPING
# CATALOG ENTRY CALLED "PRELOAD" WHICH INCLUDES THE FIRST preload_blocks BLOCKS
# OF THAT FILE.
