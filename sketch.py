from __future__ import print_function
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

def template_substitute(template, args):
    d = {}
    for arg in args:
        if arg.startswith("-D"):
            c = arg.split("=", 1)
            k = c[0][2:]
            v = c[1]
            assert k not in d
            d[k] = v
    c = template.split("${")
    result = c[0]
    for s in c[1:]:
        i = s.find("}")
        if i != -1:
            tail = s[i+1:]
            s = s[:i]
        else:
            tail = ""
        # s now contains the contents of a ${...} item in the template
        if ":" in s:
            parts = s.split(":")
            if parts[0] in d:
                result += parts[1]
            else:
                result += parts[2]
        else:
            assert s in d
            result += d[s]
        result += tail
    return result


class GameWontFit(Exception):
    pass
        
            
# SFTODO: In a few places I am doing set(extra_args) - this is fine if all the elements stand alone like "-DFOO=1", but if there are multi-element entries ("--setpc", "$0900") I will need to do something different. I am not sure if this will be an issue or not.
# SFTODO: I think an Executable object should have a min_swr property which would be 0 for tube builds or (maybe) small games which can run without SWR on a non-2P, 16 for most games and 32 for games which use all of first bank for dynmem. using K not bank count to ease things if I do ever support using e.g. private 12K on B+ where things aren't a multiple of 16.
class Executable(object):
    # SFTODO: version is not a basename it is a version string
    def __init__(self, asm_filename, version, start_address, extra_args):
        self.asm_filename = asm_filename
        self.version = version
        self.start_address = start_address
        self.extra_args = extra_args
        self._relocations = None
        basename = os.path.splitext(asm_filename)[0]
        # SFTODO: Should really use the OS-local path join character in next few lines, not '/'
        self._labels_filename = "temp/acme_labels_%s_%s" % (basename, version)
        self._report_filename = "temp/acme_report_%s_%s" % (basename, version)
        self._binary_filename = "temp/%s_%s" % (basename, version)
        os.chdir("asm")
        # SFTODO: Should really use the OS-local path join character in next few lines, not '/'
        output_prefix = "../"
        run_and_check(["acme", "--cpu", "6502", "--format", "plain", "--setpc", "$" + ourhex(start_address)] + self.extra_args + ["-l", output_prefix + self._labels_filename, "-r", output_prefix + self._report_filename, "--outfile", output_prefix + self._binary_filename, asm_filename])
        os.chdir("..")
        self._labels = parse_labels(self._labels_filename)

        with open(self._binary_filename, "rb") as f:
            self._binary = f.read()
        if "ACORN_RELOCATABLE" in self._labels:
            truncate_label = "reloc_count"
        else:
            truncate_label = "end_of_routines_in_stack_space"
        self._binary = self._binary[:self._labels[truncate_label]-self._labels["program_start"]]

        # SFTODO
        #if "VMEM" in self._labels:
        #    self._binary = Executable.patch_vmem(self._binary, self._labels)

    # SFTODO: Can/should we just automatically do the "other" build to make the relocations when we're asked for the binary?
    def add_relocations(self, other):
        assert "ACORN_RELOCATABLE" in self._labels
        assert self.asm_filename == other.asm_filename
        assert self.start_address != other.start_address
        assert set(self.extra_args) == set(other.extra_args)
        return bytearray([]) # SFTODO! self._relocations = SFTODO

    def binary(self):
        if "ACORN_RELOCATABLE" in self._labels:
            assert self._relocations is not None
            return self._binary + self._relocations
        else:
            return self._binary


# SFTODO: PATCH_VMEM IS A BIG SOURCE OF NOT-NECESSARILY-FATAL ERRORS, WHAT IS GOING TO CALL THAT AND HOW WILL I HANDLE THIS FAILING? I THINK THIS IS THE ONLY LEGIT REASON FOR FAILING TO BUILD AN EXECUTABLE, THOUGH DO NOTE THAT IN SOME CASES (NOT SURE JUST NOW) IT MAY BE LEGIT FOR A BUILD DOING EXPERIMENTALLY TO FAIL ON THESE GROUPS, WE WOULD THEN JUST TWEAK PARAMS TO DO ANOTHER BUILD FOR THAT TARGET MACHINE
def make_executable(asm_filename, start_address, extra_args):
    assert isinstance(start_address, int)

    version_templates = {
        "ozmoo": "${ACORN_ELECTRON:_electron:_bbc}${ACORN_SWR:_swr:_tube}${ACORN_NO_SHADOW:_noshadow:}${VMEM::_novmem}${ACORN_SWR_SMALL_DYNMEM:_smalldyn:}_${STARTADDRESS}"
    }

    basename = os.path.splitext(asm_filename)[0]
    assert basename in version_templates
    version = template_substitute(version_templates[basename], ["-DSTARTADDRESS=%s" % ourhex(start_address)] + extra_args)

    # Not all build parameters have to be reflected in the version string, but
    # we can't have two builds with different parameters using the same version
    # string.
    cache_key = (asm_filename, version)
    definition = (start_address, set(extra_args))
    cache_entry = make_executable.cache.get(cache_key, None)
    if cache_entry is not None:
        assert cache_entry[0] == definition
        return cache_entry[1]

    # SFTODO: THIS NEEDS TO END UP RETURNING "NONE" IF IT'S A SMALLDYN BUILD AND GAME WON'T FIT IN MAIN RAM
    try:
        e = Executable(asm_filename, version, start_address, extra_args)
    except GameWontFit:
        e = None
    make_executable.cache[cache_key] = (definition, e)
    return e
make_executable.cache = {}



block_size_bytes = 256 # SFTODO MOVE
# SFTODO: PROPER DESCRIPTION - THIS RETURNS A "MAXIMALLY HIGH" BUILD WHICH USES SMALL DYNAMIC MEMORY, OR NONE - NOTE THAT IF THE RETURNED BUILD RUNS AT (SAY) 0x1000, IT MAY NOT BE ACCEPTABLE BECAUSE IT WON'T RUN ON A "TYPICAL" B OR B+ - SO WE NEED TO TAKE SOME USER PREFERENCE INTO ACCOUNT
def make_highest_possible_executable():
    # Because of Ozmoo's liking for 512-byte alignment and the variable 256-byte value of PAGE:
    # - max_game_blocks_main_ram() can only return even values
    # - There are two possible start addresses 256 bytes apart which will generate the same
    #   value of max_game_blocks_main_ram(), as one will waste an extra 256 bytes on aligning
    #   story_start to a 512-byte boundary.
    # - We want to use the higher of those two possible start addresses, because it means we
    #   won't need to waste 256 bytes before the start of the code if PAGE happens to have the
    #   right alignment.
    e_e00 = make_executable("ozmoo.asm", 0xe00, extra_args)
    # If we can't fit dynamic memory into main RAM with a start of 0xe00 we
    # can't ever manage it.
    if e_e00 is None:
        return None
    surplus_nonstored_blocks = max_game_blocks_main_ram(e_e00) - nonstored_blocks
    assert surplus_nonstored_blocks >= 0
    # An extra 256 byte block is useless to us, so round down to a multiple of 512 bytes.
    surplus_nonstored_blocks &= ~0x1
    approx_max_start_address = 0xe00 + surplus_nonstored_blocks * block_size_bytes
    e = make_optimally_aligned_executable("ozmoo.asm", approx_max_start_address, extra_args, e_e00)
    assert e is not None
    if (e.start_address & 0x100) == (0xe00 & 0x100):
        assert e.size() == e_e00.size()
    else:
        assert e.size() < e_e00.size()
    assert 0 <= max_game_ram_blocks_main_ram(e) - nonstored_blocks <= 1
    return e




def make_optimally_aligned_executable(initial_start_address, extra_args, base_executable = None):
    if base_executable is None:
        base_executable = make_executable("ozmoo.asm", initial_start_address, extra_args)
    else:
        assert base_executable.asm_filename == "ozmoo.asm"
        assert (base_executable.start_address & 0x1ff) == (initial_start_address & 0x1ff)
        assert base_executable.extra_args == extra_args
    # If the alignment works out appropriately, we may have the same amount of available RAM
    # with less wasted alignment by building one page past initial_start_address.
    alternate_executable = make_executable("ozmoo.asm", initial_start_address + 0x100, extra_args)
    if alternate_executable.size() < base_executable.size():
        return alternate_executable
    else:
        if base_executable.start_address == initial_start_address:
            return base_executable
        else:
            return make_executable("ozmoo.asm", initial_start_address, extra_args)



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
    small_e = make_executable("ozmoo.asm", 0x1900, extra_args + small_dynmem_args) # SFTODO: CONSTANT ADDRESS
    if small_e is not None:
        return small_e
    return make_executable("ozmoo.asm", 0x1900, extra_args) # SFTODO: CONSTANT ADDRESS

def make_electron_swr_executable():
    # SFTODO: Duplication here with make_shr_swr_executable() extra_args
    extra_args = ozmoo_base_args + ozmoo_swr_args + relocatable_args + ["-DACORN_ELECTRON_SWR=1"]
    # On the Electron, no main RAM is used for dynamic RAM so there's no
    # disadvantage to loading high in memory as far as the game itself is
    # concerned. However, we'd like to avoid the executable overwriting the mode
    # 6 screen RAM and corrupting the loading screen if we can, so we pick a
    # relatively low address which should be >=PAGE on nearly all systems.
    return make_optimally_aligned_executable(0x1d00, extra_args)
    

header_version = 0
header_static_mem = 0xe
vmem_block_pagecount = 2
min_timestamp = 0
max_timestamp = 0xe0 # initial tick value
verbose_level = 2 # SFTODO TEMP HACK SHOULD BE PARSED FROM ARGS

ozmoo_swr_args = ["-DVMEM=1", "-DACORN_SWR=1"]
relocatable_args = ["-DACORN_RELOCATABLE=1"]
small_dynmem_args = ["-DACORN_SWR_SMALL_DYNMEM=1"]

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

e = make_electron_swr_executable()
