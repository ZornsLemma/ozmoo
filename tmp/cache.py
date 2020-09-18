import random

vmap_local_entries = 60
vmap_cache_entries = 50
vmap_total_entries = vmap_local_entries + vmap_cache_entries

# vmap here doesn't simulate the "low" bits containing address. Note that
# the same timestamp can appear more than once.
vmap = [x // 2 for x in range(vmap_total_entries)]
original_vmap = vmap
random.shuffle(vmap)

# cutover is supplied by build script in reality, to avoid doing this
# sort in Ozmoo code.
sorted_vmap = sorted(vmap, reverse=True)
# SFTODO: POSS WANT VMAP_LOCAL_ETNRIES-1, I AM STILL SKETCHING THIS OUT
cutover = sorted_vmap[vmap_local_entries]
sorted_vmap = None # stop accidental use

cache = []
from_index = 0
to_index = 0

while to_index < vmap_local_entries:
    vmap[to_index] = vmap[from_index]
    # load_blocks_from_index(to_index)
    if vmap[to_index] < cutover and len(cache) < vmap_cache_entries:
        cache.append(vmap[to_index])
    else:
        to_index += 1
    from_index += 1

last_index = vmap_local_entries - 1
while from_index < vmap_total_entries:
    cache.append(vmap[last_index])
    assert len(cache) < vmap_cache_entries
    vmap[last_index] = vmap[from_index]
    # load_blocks_from_index(last_index)
    from_index += 1
    
print("cache", cache)
local_vmap = vmap[0:vmap_local_entries]
print("local vmap", local_vmap)


assert len(cache) <= vmap_cache_entries
assert sorted(local_vmap + cache) == sorted(original_vmap)
assert len(set(cache).intersection(set(local_vmap))) == 0
