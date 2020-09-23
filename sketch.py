# SFTODO: I am thinking that for SWR builds the build script is not directly responsible for ensuring free memory for vmem paging - a) *if* the build is relocatable a lower PAGE counts towards making this available b) having more SWR than the nominal bare minimum avoids the problem. So the build script needs to communicate a) if the build is reloctable b) the min SWR *excluding* swappable memory required at the "default"/maximum PAGE for that executable to the loader, and it will be responsible for deciding if the game can run or not.

from __future__ import print_function
import copy
import os
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

def substitute(lst, a, b): # SFTODO: DELETE IF NOT USED
    return [x.replace(a, b) for x in lst]

def ourhex(i):
    return hex(i)[2:]

def get_word(data, i):
    return data[i]*256 + data[i+1]

def divide_round_up(x, y):
    if x % y == 0:
        return x // y
    else:
        return (x // y) + 1

def bytes_to_blocks(x):
    return divide_round_up(x, 256)

# SFTODO: Make this a member of Executable?
def max_nonstored_blocks(executable):
    return (executable.pseudo_ramtop() - executable.labels["story_start"]) // 256

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
    if (child.returncode != 0 or verbose_level >= 2) and len(child_output) > 0:
        print("".join(x.decode(encoding="ascii") for x in child_output))
    if child.returncode != 0:
        die("%s failed" % args[0])

# SFTODO: MAKE A MEMBER OF EXECUTABLE?
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

class GameWontFit(Exception):
    pass
        
            
# SFTODO: In a few places I am doing set(extra_args) - this is fine if all the elements stand alone like "-DFOO=1", but if there are multi-element entries ("--setpc", "$0900") I will need to do something different. I am not sure if this will be an issue or not.
class Executable(object):
    cache = {}

    def __init__(self, asm_filename, version_maker, start_address, extra_args):
        self.asm_filename = asm_filename
        self.version_maker = version_maker
        self.start_address = start_address
        self.extra_args = extra_args
        self._relocations = None
        # SFTODO: Should really use the OS-local path join character in next few lines, not '/'
        output_name = os.path.splitext(os.path.basename(asm_filename))[0].replace("-", "_")
        if version_maker is not None:
            output_name += "_" + version_maker(start_address, extra_args)
        else:
            output_name += "_" + ourhex(start_address)

        # SFTODO: MOVE THIS CACHE LOGIC INTO OZMOOEXECUTABLE? WE DON'T NEED IT ANYWHERE ELSE, AND IT WOULD THEN CACHE THE RESULTS OF VMEM PATCHIG AND EVERYTHING, WHICH FEELS A BIT MORE ELEGANT EVEN IF IN PRACTICE IT'S HARMLESS TO REDO THIS WORK
        # Not all build parameters have to be reflected in the output name, but we
        # can't have two builds with different parameters using the same output
        # name.
        cache_key = (asm_filename, output_name)
        cache_definition = (start_address, set(extra_args))
        cache_entry = Executable.cache.get(cache_key, None)
        if cache_entry is not None:
            assert cache_entry[0] == cache_definition
            e = cache_entry[1]
            self.labels = e.labels
            self._binary = e._binary
            self._relocations = e._relocations
            return

        self._labels_filename = "temp/acme_labels_" + output_name
        self._report_filename = "temp/acme_report_" + output_name
        self._binary_filename = "temp/" + output_name
        os.chdir("asm")
        # SFTODO: Should really use the OS-local path join character in next few lines, not '/'
        output_prefix = "../"
        cpu = "65c02" if "-DCMOS=1" in extra_args else "6502"
        run_and_check(["acme", "--cpu", cpu, "--format", "plain", "--setpc", "$" + ourhex(start_address)] + self.extra_args + ["-l", output_prefix + self._labels_filename, "-r", output_prefix + self._report_filename, "--outfile", output_prefix + self._binary_filename, asm_filename])
        os.chdir("..")
        self.labels = parse_labels(self._labels_filename)

        with open(self._binary_filename, "rb") as f:
            self._binary = bytearray(f.read())
        if "ACORN_RELOCATABLE" in self.labels:
            self.truncate_at("reloc_count")

        Executable.cache[cache_key] = (cache_definition, copy.deepcopy(self))

    def rebuild_at(self, start_address):
        print("SFTODO BASEEXECUTABLE REBUILD_AT")
        return Executable(self.asm_filename, self.version_maker, start_address, self.extra_args)

    def truncate_at(self, label):
        self._binary = self._binary[:self.labels[label]-self.labels["program_start"]]

    def _make_relocations(self):
        assert "ACORN_RELOCATABLE" in self.labels
        other_start_address = 0xe00
        if "ACORN_RELOCATE_WITH_DOUBLE_PAGE_ALIGNMENT" in self.labels:
            other_start_address += (self.start_address - other_start_address) % 0x200
        # SFTODO: If the two addresses are the same the relocation is pointless, can we avoid it? I think the build may fail if this is the case, need to test it at least works even if it is pointless
        assert other_start_address <= self.start_address
        other = self.rebuild_at(other_start_address)
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

    def binary(self):
        if "ACORN_RELOCATABLE" in self.labels:
            if self._relocations is None:
                self._relocations = self._make_relocations()
            return self._binary + self._relocations
        else:
            return self._binary

class OzmooExecutable(Executable):
    def __init__(self, start_address, extra_args):
        def version_maker(start_address, extra_args):
            if "-DACORN_ELECTRON_SWR=1" in extra_args:
                s = "electron_swr"
            else:
                if "-DACORN_SWR=1" in extra_args:
                    s = "bbc_swr"
                    if "-DACORN_NO_SHADOW=1" not in extra_args:
                        s += "_shr"
                else:
                    s = "tube"
            if "-DVMEM=1" not in extra_args:
                s += "_novmem"
            if "-DACORN_SWR_SMALL_DYNMEM=1" in extra_args:
                s += "_smalldyn"
            s += "_" + ourhex(start_address)
            return s

        Executable.__init__(self, "ozmoo.asm", version_maker, start_address, extra_args)

        if "ACORN_RELOCATABLE" not in self.labels:
            self.truncate_at("end_of_routines_in_stack_space")

        self.swr_dynmem = 0
        if "VMEM" in self.labels:
            self.patch_vmem()

    def patch_vmem(e): # SFTODO: RENAME ARG TO 'SELF'
        assert e.asm_filename == "ozmoo.asm"

        vmem_block_pagecount = e.labels["vmem_block_pagecount"]
        bytes_per_vmem_block = vmem_block_pagecount * 256
        assert bytes_per_vmem_block == 512

        if z_machine_version == 3:
            vmem_highbyte_mask = 0x01
        elif z_machine_version == 8:
            vmem_highbyte_mask = 0x07
        else:
            vmem_highbyte_mask = 0x03

        # Can we fit the nonstored blocks into memory?
        nonstored_blocks_up_to = e.labels["story_start"] + nonstored_blocks * bytes_per_block
        if nonstored_blocks_up_to > e.pseudo_ramtop():
            raise GameWontFit("Not enough free RAM for game's dynamic memory")
        if "ACORN_SWR" in e.labels:
            # Note that swr_dynmem may be negative; this means the game may run
            # (albeit badly) with no sideways RAM at all. (For relocatable builds
            # the loader will adjust it anyway, to take account of actual PAGE
            # versus maximum page the executable was built for.)
            e.swr_dynmem = nonstored_blocks_up_to - 0x8000
            assert "ACORN_SWR_SMALL_DYNMEM" in e.labels or e.swr_dynmem > 0
            assert e.swr_dynmem <= 16 * 1024

        # On a second processor build, we must also have at least
        # min_vmem_blocks for swappable memory. On sideways RAM builds we leave
        # checking this to the loader - the dynamic memory size has a big
        # influence on what we can run at any given start address no matter how
        # much sideways RAM we have, because we can use at most 16K of it for
        # dynamic memory, but other banks of sideways RAM can be used for
        # swappable memory.
        min_vmem_blocks = 2 # absolute minimum, one for PC, one for data SFTODO: ALLOW USER TO SPECIFY ON CMD LINE
        if "ACORN_SWR" not in e.labels:
            nsmv_up_to = nonstored_blocks_up_to + min_vmem_blocks * bytes_per_vmem_block
            if nsmv_up_to > e.pseudo_ramtop():
                raise GameWontFit("Not enough free RAM for any swappable memory")

        # Generate initial virtual memory map. We just populate the entire table; if the
        # game is smaller than this we will just never use the other entries.
        vmap_offset = e.labels['vmap_z_h'] - e.labels['program_start']
        vmap_max_size = e.labels['vmap_max_size']
        assert e._binary[vmap_offset:vmap_offset+vmap_max_size*2] == b'V'*vmap_max_size*2
        blocks = []
        # SFTODO: We will do this work for every single executable; it's not in practice a
        # big deal but it's a bit inelegant.
        if False: # SFTODO if args.preload_config is not None:
            with open(args.preload_config, "rb") as f:
                preload_config = bytearray(f.read())
            for i in range(len(preload_config) // 2):
                # We don't care about the timestamp on the entries in preload_config; they are in
                # order of insertion and that's what we're really interested in. (This isn't
                # the *same* as the order based on timestamp; a block loaded early may of course
                # be used again later and therefore have a newer timestamp than another block
                # loaded after it but never used again.) Note that just as when we don't use
                # preload_config, the initial vmap entries will be assigned timestamps based
                # on their order; the timestamp in preload_config are ignored.
                addr = ((preload_config[i*2] & vmem_highbyte_mask) << 8) | preload_config[i*2 + 1]
                if addr & 1 == 1:
                    # This is an odd address so it's invalid; we expect to see one of these
                    # as the first block and we just ignore it.
                    assert i == 0
                    continue
                block_index = (addr - nonstored_blocks) // vmem_block_pagecount
                assert block_index >= 0
                blocks.append(block_index)
        # SFTODONOW: SHOULDN'T THIS RANGE START AT nonstored_blocks NOT 0??? IT SHOULD STILL HAVE vmap_max_size *ENTRIES*. - NO, THINK ABOUT IT FRESH BUT THIS IS PROBABLY CORRECT - WE ADD NONSTORED_BLOCKS WHEN CALCULATING ADDR, AND WE SUBTRACT IT WHEN EXTRACING BLOCK INDEX FROM ADDR ABOVE
        for i in range(vmap_max_size):
            if i not in blocks:
                blocks.append(i)
        blocks = blocks[:vmap_max_size]
        #print("Q", blocks) SFTODO TEMP
        #import random # SFTODO TEMP
        #random.seed(42) # SFTODO TEMP
        #random.shuffle(blocks) # SFTODO TEMP
        #print("Q", blocks) # SFTODO TEMP
        # vmap entries should normally address a 512-byte aligned block; invalid_address
        # is odd so it won't ever match when the virtual memory code is searching the map.
        invalid_address = 0x1
        for i, block_index in enumerate(blocks):
            timestamp = int(max_timestamp + ((float(i) / vmap_max_size) * (min_timestamp - max_timestamp))) & ~vmem_highbyte_mask
            if False: # SFTODO args.preload_opt:
                # Most of the vmap will be ignored, but we have to have at least one entry
                # and by making it an invalid address we don't need to worry about loading
                # any "suggested" blocks.
                addr = invalid_address
            else:
                # SFTODO: Simplify this to "nonstored_blocks + block_index * vmem_block_pagecount"?
                addr = ((nonstored_blocks // vmem_block_pagecount) + block_index) * vmem_block_pagecount
            if ((addr >> 8) & ~vmem_highbyte_mask) != 0:
                # This vmap entry is useless; the current Z-machine version can't contain
                # such a block.
                # SFTODO: Warn? It's harmless but it means we could have clawed back a few
                # bytes by shrinking vmap_max_size.
                addr = 0
            vmap_entry = (timestamp << 8) | addr
            e._binary[vmap_offset + i + 0            ] = (vmap_entry >> 8) & 0xff
            e._binary[vmap_offset + i + vmap_max_size] = vmap_entry & 0xff

    def pseudo_ramtop(self):
        if "ACORN_SWR" in self.labels:
            return 0x8000 if "ACORN_SWR_SMALL_DYNMEM" in self.labels else 0xc000
        else:
            return self.labels["flat_ramtop"]

    # Return the size of the binary, ignoring any relocation data (which isn't
    # important for the limited use we make of the return value).
    def size(self):
        return len(self._binary)

    def rebuild_at(self, start_address):
        print("SFTODO OZMOOEXECUTABLE REBUILD_AT")
        return OzmooExecutable(start_address, self.extra_args)


def make_ozmoo_executable(start_address, extra_args):
    try:
        return OzmooExecutable(start_address, extra_args)
    except GameWontFit:
        return None




bytes_per_block = 256 # SFTODO MOVE
# SFTODO: PROPER DESCRIPTION - THIS RETURNS A "MAXIMALLY HIGH" BUILD WHICH USES SMALL DYNAMIC MEMORY, OR NONE - NOTE THAT IF THE RETURNED BUILD RUNS AT (SAY) 0x1000, IT MAY NOT BE ACCEPTABLE BECAUSE IT WON'T RUN ON A "TYPICAL" B OR B+ - SO WE NEED TO TAKE SOME USER PREFERENCE INTO ACCOUNT
def make_highest_possible_executable(extra_args):
    # Because of Ozmoo's liking for 512-byte alignment and the variable 256-byte value of PAGE:
    # - max_nonstored_blocks() can only return even values
    # - There are two possible start addresses 256 bytes apart which will generate the same
    #   value of max_nonstored_blocks(), as one will waste an extra 256 bytes on aligning
    #   story_start to a 512-byte boundary.
    # - We want to use the higher of those two possible start addresses, because it means we
    #   won't need to waste 256 bytes before the start of the code if PAGE happens to have the
    #   right alignment.
    e_e00 = make_ozmoo_executable(0xe00, extra_args)
    # If we can't fit build successfully with a start of 0xe00 we can't ever
    # manage it.
    if e_e00 is None:
        return None
    # SFTODO: WE ALSO MUST NOT LOAD SO HIGH THAT THE BINARY (AND DON'T FORGET IT WILL HAVE AN AS-YET-UNDETERMINED NUMBER OF BYTES OF RELOC DATA APPENDED TOO) DOESN'T FIT BELOW $8000!
    surplus_nonstored_blocks = max_nonstored_blocks(e_e00) - nonstored_blocks
    assert surplus_nonstored_blocks >= 0
    # An extra 256 byte block is useless to us, so round down to a multiple of 512 bytes.
    surplus_nonstored_blocks &= ~0x1
    # There's no point loading really high, and doing a totally naive
    # calculation may cause us to load so high there's no room for the
    # relocation data before &8000, so we never load higher than
    # max_start_address.
    approx_max_start_address = min(0xe00 + surplus_nonstored_blocks * bytes_per_block, max_start_address)
    e = make_optimally_aligned_executable(approx_max_start_address, extra_args, e_e00)
    assert e is not None
    if (e.start_address & 0x100) == (0xe00 & 0x100):
        assert e.size() == e_e00.size()
    else:
        assert e.size() < e_e00.size()
    assert 0 <= max_nonstored_blocks(e) - nonstored_blocks
    return e




def make_optimally_aligned_executable(initial_start_address, extra_args, base_executable = None):
    if base_executable is None:
        base_executable = make_ozmoo_executable(initial_start_address, extra_args)
        if base_executable is None:
            return None
    else:
        assert base_executable.asm_filename == "ozmoo.asm"
        assert (base_executable.start_address & 0x1ff) == (initial_start_address & 0x1ff)
        assert base_executable.extra_args == extra_args
    # If the alignment works out appropriately, we may have the same amount of available RAM
    # with less wasted alignment by building one page past initial_start_address.
    alternate_executable = make_ozmoo_executable(initial_start_address + 0x100, extra_args)
    if alternate_executable is not None and alternate_executable.size() < base_executable.size():
        return alternate_executable
    else:
        if base_executable.start_address == initial_start_address:
            return base_executable
        else:
            return make_ozmoo_executable(initial_start_address, extra_args)



def make_shr_swr_executable():
    # SFTODO: I should maybe (everywhere) just say "args" not "extra args", unless Executable or whatever is going to force some args in all the time
    extra_args = ozmoo_base_args + ozmoo_swr_args + relocatable_args

    small_e = make_highest_possible_executable(extra_args + small_dynmem_args)
    # Some systems may have PAGE too high to run small_e, but those systems
    # would be able to run the game if built with the big dynamic memory model.
    # highest_expected_page determines whether we're willing to prevent a system
    # running the game in order to get the benefits of the small dynamic memory
    # model.
    if small_e is not None:
        if small_e.start_address >= highest_expected_page:
            info("Shadow+sideways RAM executable uses small dynamic memory model")
            return small_e

    # Note that we don't respect highest_expected_page when generating a big
    # dynamic memory executable; unlike the above decision about whether or not
    # to use the small dynamic memory model, we're not trading off performance
    # against available main RAM - if a system has PAGE too high to run the big
    # dynamic memory executable we generate, it just can't run the game at all
    # and there's nothing we can do about it.
    big_e = make_highest_possible_executable(extra_args)
    if big_e is not None:
        if small_e is not None and small_e.start_address < highest_expected_page:
            info("Shadow+sideways RAM executable uses big dynamic memory model because small model would require PAGE<=" + ourhex2(small_e.start_address))
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
    extra_args = ozmoo_base_args + ozmoo_swr_args + ["-DACORN_NO_SHADOW=1"]
    small_e = make_ozmoo_executable(0x1900, extra_args + small_dynmem_args) # SFTODO: CONSTANT ADDRESS
    if small_e is not None:
        return small_e
    return make_ozmoo_executable(0x1900, extra_args) # SFTODO: CONSTANT ADDRESS

def make_electron_swr_executable():
    # SFTODO: Duplication here with make_shr_swr_executable() extra_args
    extra_args = ozmoo_base_args + ozmoo_swr_args + relocatable_args + ["-DACORN_ELECTRON_SWR=1"]
    # On the Electron, no main RAM is used for dynamic RAM so there's no
    # disadvantage to loading high in memory as far as the game itself is
    # concerned. However, we'd like to avoid the executable overwriting the mode
    # 6 screen RAM and corrupting the loading screen if we can, so we pick a
    # relatively low address which should be >=PAGE on nearly all systems.
    return make_optimally_aligned_executable(0x1d00, extra_args)

def make_tube_executable():
    tube_args = ozmoo_base_args
    if True: # SFTODO: IF CMOS NOT DISABLED ENTIRELY BY CMD LINE ARG
        tube_args += ["-DCMOS=1"]
    tube_no_vmem = make_ozmoo_executable(tube_start_address, tube_args)
    if game_blocks <= max_nonstored_blocks(tube_no_vmem):
        info("Game is small enough to run without virtual memory on second processor")
        return tube_no_vmem
    info("Game will be run using virtual memory on second processor")
    tube_args += ["-DVMEM=1"]
    if True: # SFTODO not args.no_tube_cache:
        tube_args += ["-DACORN_TUBE_CACHE=1"]
        tube_args += ["-DACORN_TUBE_CACHE_MIN_TIMESTAMP=%d" % min_timestamp]
        tube_args += ["-DACORN_TUBE_CACHE_MAX_TIMESTAMP=%d" % max_timestamp]
    return make_ozmoo_executable(tube_start_address, tube_args)

def make_findswr_executable():
    return Executable("acorn-findswr.asm", None, 0x900, [])

def make_cache_executable():
    # In practice the cache executable will only be run in mode 7, but we'll position it to load just below the mode 0 screen RAM.
    return Executable("acorn-cache.asm", None, 0x2c00, ["-DACORN_RELOCATABLE=1"])


header_version = 0
header_static_mem = 0xe
vmem_block_pagecount = 2
min_timestamp = 0
max_timestamp = 0xe0 # initial tick value
verbose_level = 2 # SFTODO TEMP HACK SHOULD BE PARSED FROM ARGS
highest_expected_page = 0x2000 # SFTODO: BEST VALUE? MAKE USER CONFIGURABLE ANYWAY. ALSO A BIT MISNAMED AS WE DON'T USE IT FOR EG THE BBC NO SHADOW EXECUTABLE

ozmoo_swr_args = ["-DVMEM=1", "-DACORN_SWR=1"]
relocatable_args = ["-DACORN_RELOCATABLE=1"]
small_dynmem_args = ["-DACORN_SWR_SMALL_DYNMEM=1"]

host = 0xffff0000
tube_start_address = 0x600
max_start_address = 0x4000

with open(sys.argv[1], "rb") as f : # SFTODO with open(args.input_file, "rb") as f:
    game_data = bytearray(f.read())
game_blocks = bytes_to_blocks(len(game_data))
dynamic_size_bytes = get_word(game_data, header_static_mem)
nonstored_blocks = bytes_to_blocks(dynamic_size_bytes)
while nonstored_blocks % vmem_block_pagecount != 0:
    nonstored_blocks += 1

ozmoo_base_args = [ # SFTODO: MOVE THIS?
    "-DACORN=1",
    "-DACORN_CURSOR_PASS_THROUGH=1",
    "-DSTACK_PAGES=4",
    "-DSMALLBLOCK=1",
    "-DSPLASHWAIT=0",
    "-DACORN_INITIAL_NONSTORED_BLOCKS=%d" % nonstored_blocks,
    "-DACORN_DYNAMIC_SIZE_BYTES=%d" % dynamic_size_bytes,
]

z_machine_version = game_data[header_version]
# SFTODO: This is wrong/incomplete - fairly sure we support 4 and 7 too
if z_machine_version == 3:
    ozmoo_base_args += ["-DZ3=1"]
elif z_machine_version == 5:
    ozmoo_base_args += ["-DZ5=1"]
elif z_machine_version == 8:
    ozmoo_base_args += ["-DZ8=1"]
else:
    die("Unsupported Z-machine version: %d" % (z_machine_version,))

e = make_electron_swr_executable()
if e is not None:
    print(ourhex(e.start_address))
else:
    print(None)
e = make_bbc_swr_executable()
print(ourhex(e.start_address), e.swr_dynmem)
e = make_shr_swr_executable()
print(ourhex(e.start_address), e.swr_dynmem)
e = make_tube_executable()
print(ourhex(e.start_address))
e = make_findswr_executable()
print(ourhex(e.start_address))
e = make_cache_executable()
print(ourhex(e.start_address))
print(len(e.binary()))
