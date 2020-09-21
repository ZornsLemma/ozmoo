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
        
            
class Executable(object):
    def __init__(self, asm_filename, output_basename, start_address, extra_args):
        self.output_basename = output_basename
        pass # SFTODO


def make_executable(asm_filename, start_address, extra_args):
    assert isinstance(start_address, int)

    filename_templates = {
        "ozmoo": "ozmoo${ACORN_ELECTRON:_electron:}${ACORN_SWR:_swr:_tube}${ACORN_NO_SHADOW:_noshadow:}${VMEM::_novmem}${ACORN_SWR_SMALL_DYNMEM:_smalldyn:}_${STARTADDRESS}"
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

    try:
        e = Executable(asm_filename, output_basename, start_address, extra_args)
    except: # SFTODO: catch specific exception types
        e = None
    make_executable.cache[output_basename] = (definition, e)
    return e
make_executable.cache = {}

    

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
