import os

def ourhex(i):
    return hex(i)[2:]

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
        
            
# SFTODO: In a few places I am doing set(extra_args) - this is fine if all the elements stand alone like "-DFOO=1", but if there are multi-element entries ("--setpc", "$0900") I will need to do something different. I am not sure if this will be an issue or not.
# SFTODO: I think an Executable object should have a min_swr property which would be 0 for tube builds or (maybe) small games which can run without SWR on a non-2P, 16 for most games and 32 for games which use all of first bank for dynmem. using K not bank count to ease things if I do ever support using e.g. private 12K on B+ where things aren't a multiple of 16.
class Executable(object):
    def __init__(self, asm_filename, output_basename, start_address, extra_args):
        self.output_basename = output_basename
        self.relocations = None
        pass # SFTODO

    # SFTODO: Can/should we just automatically do the "other" build to make the relocations when we're asked for the binary?
    def add_relocations(self, other):
        assert "ACORN_RELOCATABLE" in self.labels
        assert self.asm_filename == other.asm_filename
        assert self.start_address != other.start_address
        assert set(self.extra_args) == set(other.extra_args)
        self.relocations = SFTODO

    def binary(self):
        if "ACORN_RELOCATABLE" in self.labels:
            # SFTODO: *Something* in this class needs to handle truncating at reloc_count
            assert self.relocations is not None
            return self.acme_output + self.relocations
        else:
            return self.acme_output


# SFTODO: PATCH_VMEM IS A BIG SOURCE OF NOT-NECESSARILY-FATAL ERRORS, WHAT IS GOING TO CALL THAT AND HOW WILL I HANDLE THIS FAILING? I THINK THIS IS THE ONLY LEGIT REASON FOR FAILING TO BUILD AN EXECUTABLE, THOUGH DO NOTE THAT IN SOME CASES (NOT SURE JUST NOW) IT MAY BE LEGIT FOR A BUILD DOING EXPERIMENTALLY TO FAIL ON THESE GROUPS, WE WOULD THEN JUST TWEAK PARAMS TO DO ANOTHER BUILD FOR THAT TARGET MACHINE
def make_executable(asm_filename, start_address, extra_args):
    assert isinstance(start_address, int)

    filename_templates = {
        "ozmoo": "ozmoo${ACORN_ELECTRON:_electron:_bbc}${ACORN_SWR:_swr:_tube}${ACORN_NO_SHADOW:_noshadow:}${VMEM::_novmem}${ACORN_SWR_SMALL_DYNMEM:_smalldyn:}_${STARTADDRESS}"
    }

    basename = os.path.splitext(asm_filename)[0]
    assert basename in filename_templates
    output_basename = template_substitute(filename_templates[basename], ["-DSTARTADDRESS=%s" % ourhex(start_address)] + extra_args)

    # Not all build parameters have to be reflected in the filename, but we
    # can't have two builds with different parameters using the same filename.
    definition = (start_address, set(extra_args))
    cache_entry = make_executable.cache.get(output_basename, None)
    if cache_entry is not None:
        assert cache_entry[0] == definition
        return cache_entry[1]

    # SFTODO: THIS NEEDS TO END UP RETURNING "NONE" IF IT'S A SMALLDYN BUILD AND GAME WON'T FIT IN MAIN RAM
    try:
        e = Executable(asm_filename, output_basename, start_address, extra_args)
    except GameWontFit:
        e = None
    make_executable.cache[output_basename] = (definition, e)
    return e
make_executable.cache = {}



block_size_bytes = 256 # SFTODO MOVE
# SFTODO: PROPER DESCRIPTION - THIS RETURNS A "MAXIMALLY HIGH" BUILD WHICH USES SMALL DYNAMIC MEMORY, OR NONE - NOTE THAT IF THE RETURNED BUILD RUNS AT (SAY) 0x1000, IT MAY NOT BE ACCEPTABLE BECAUSE IT WON'T RUN ON A "TYPICAL" B OR B+ - SO WE NEED TO TAKE SOME USER PREFERENCE INTO ACCOUNT
def make_small_dynmem():
    extra_args = extra_args + ["-DACORN_SWR_SMALL_DYNMEM=1"]
    # Because of Ozmoo's liking for 512-byte alignment and the variable 256-byte value of PAGE:
    # - max_game_blocks_main_ram() can only return even values
    # - There are two possible start addresses 256 bytes apart which will generate the same
    #   value of max_game_blocks_main_ram(), as one will waste an extra 256 bytes on aligning
    #   story_start to a 512-byte boundary.
    # - We want to use the higher of those two possible start addresses, because it means we
    #   won't need to waste 256 bytes before the start of the code if PAGE happens to have the
    #   right alignment.
    small_dyn_e00 = make_executable("ozmoo.asm", 0xe00, small_dyn_extra_args)
    # If we can't fit dynamic memory into main RAM with a start of 0xe00 we
    # can't ever manage it.
    if small_dyn_e00 is None:
        return None
    surplus_nonstored_blocks = max_game_blocks_main_ram(small_dyn_e00) - nonstored_blocks
    assert surplus_nonstored_blocks >= 0
    # An extra 256 byte block is useless to us, so round down to a multiple of 512 bytes.
    surplus_nonstored_blocks &= ~0x1
    approx_max_start_address = 0xe00 + surplus_nonstored_blocks * block_size_bytes
    # If the alignment works out appropriately, we may have the same amount of available RAM
    # with less wasted alignment by building one page past approx_max_start_address.
    small_dyn = make_executable("ozmoo.asm", approx_max_start_address + 0x100, small_dyn_extra_args)
    if small_dyn is not None:
        assert small_dyn.size() != small_dyn_e00.size()
        if small_dyn.size() < small_dyn_e00.size():
            return small_dyn
    small_dyn = make_executable("ozmoo.asm", approx_max_start_adress, small_dyn_extra_args)
    assert small_dyn is not None
    assert small_dyn.size() == small_dyn_e00.size()
    return small_dyn



def make_shr_swr_executable():
    # SFTODO: I should maybe (everywhere) just say "args" not "extra args", unless Executable or whatever is going to force some args in all the time
    extra_args = ozmoo_base_args + ["-DVMEM=1", "-DACORN_SWR=1", "-DACORN_RELOCATABLE=1"]
    small_dyn_executable = make_small_dynmem_executable(extra_args)
    if small_dyn_executable is not None and small_dyn_executable.start_address < max_supported_page:
        info("Shadow+sideways RAM executable can't use small dynamic memory model as it would require PAGE<=%s" % ourhex2(small_dyn_executable.start_address))
        small_dyn_executable = None
    if small_dyn_executable is not None:
        return small_dyn_executable
    return make_executable("ozmoo.asm", 0x2500, extra_args)
    

    

e = make_executable("ozmoo.asm", 0x900, [])
print e.output_basename
e = make_executable("ozmoo.asm", 0x900, ["-DACORN_SWR=1"])
print e.output_basename
e = make_executable("ozmoo.asm", 0x900, ["-DACORN_SWR=1", "-DACORN_NO_SHADOW=1", "-DVMEM=1"])
print e.output_basename
#e = make_executable("ozmoo.asm", 0x900, ["-DACORN_SWR=1", "-DACORN_ELECTRON=1", "-DACORN_SWR_SMALL_DYNMEM=1", "-DVMEM=1"])
#print e.output_basename
e = make_executable("ozmoo.asm", 0x900, ["-DACORN_SWR=1", "-DACORN_ELECTRON=1", "-DACORN_SWR_SMALL_DYNMEM=1", "-DVMEM=1", "-DFOO=1"])
print e.output_basename
