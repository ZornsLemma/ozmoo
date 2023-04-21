IF PAGE<&800 THEN PRINT "Sorry, tube only!":END

old_at%=@%
ON ERROR PROCerror

*/FINDSWR
*/CACHE2P
mode%=0

REM The Ozmoo executable always forces the shadow mode bit on when initialising
REM the cache, so we do the same. We don't want to chase phantom bugs or quirks
REM caused by running on a machine with shadow RAM but not using a shadow mode.
REM SFTODO: Maybe change this later. It might be nice - if it works - to be
REM able to test non-shadow and shadow cases without actually switching to a
REM machine with/without shadow RAM. But for now let's keep it as close to real
REM use as possible.
MODE mode% OR 128

REM In order to help catch bugs that only occurs when the cache is not fully
REM populated, we periodically restart.
pass%=0
REPEAT
CLS

A%=&88:X%=mode% OR 128:host_cache_size_vmem_blocks%=(USR(&FFF4) AND &FF00) DIV &100
PRINT "Host cache size (blocks): ";host_cache_size_vmem_blocks%;" / Pass: ";pass%
max_call_count%=1000+RND(50)*100

expected_hit_ratio=host_cache_size_vmem_blocks%/game_blocks%

DIM osword_block% 64

REM For now we don't try to model exactly what the cache will be doing. Instead,
REM we request game data blocks at random and track the cache hit ratio - since we
REM are requesting at random, if the cache is working properly the hit ratio will
REM approximate the ratio of cache size to game data size. The cache starts empty
REM which will drag the hit ratio down at first but that effect should wear off over
REM time.
REM SFTODO: It might not be a bad idea to record the last n blocks we offered for
REM smallish n and check that we *do* get a cache hit if the block we request at random
REM is one of those last n blocks.

block_size%=512:REM bytes

game_blocks%=489

local_cache_blocks%=10
DIM local_cache% local_cache_blocks%*block_size%
DIM local_cache_id%(local_cache_blocks%-1)
FOR I%=0 TO local_cache_blocks%-1
REM SFTODO: It would be better to pick a random set of initial blocks, but we'd
REM need to avoid duplicates. (Duplicates might work, but wouldn't be realistic and
REM might cause confusion while debugging.)
local_cache_id%(I%)=I%+10
PROCcreate_block(local_cache_id%(I%),local_cache%+I%*block_size%)
NEXT
REM Just sanity check the newly created blocks...
FOR I%=0 TO local_cache_blocks%-1:PROCcheck_block(local_cache_id%(I%),local_cache%+I%*block_size%):NEXT

hit_count%=0:call_count%=0
REPEAT

REM Pick a game block we don't already have in our local cache at random.
REM SFTODO: We might get away with picking a game block and not caring if it's in
REM our local cache, but it seems like it might cause confusion when we see
REM "invalid" state (the same block in host and local cache) during debugging.
REPEAT
REM SFTODONOW: I think there's a bug - here on in the actual cache code - where
REM wanted_block%=0 early on, probably before the cache gets fully populated. (I
REM don't believe this can ever occur in Ozmoo, because block 0 is always dynamic
REM memory - but if there is a bug in cache code, be good to fix it anyway.)
wanted_block%=RND(game_blocks%)-1
already_have%=FALSE
FOR I%=0 TO local_cache_blocks%-1
IF local_cache_id%(I%)=wanted_block% THEN already_have%=TRUE
NEXT
UNTIL NOT already_have%

local_cache_block_to_evict%=RND(local_cache_blocks%)-1
local_addr%=local_cache%+local_cache_block_to_evict%*block_size%

osword_block%?0=12
osword_block%?1=12
osword_block%!2=local_addr%
osword_block%!6=local_cache_id%(local_cache_block_to_evict%):REM SFTODO: Do something to test "&FFFF=no block offered" case?
osword_block%?8=&FF:REM SFTODO: Do something to test timestamp hints?
osword_block%!9=wanted_block%
A%=&E0:X%=osword_block%:Y%=osword_block% DIV 256:CALL &FFF1
call_count%=call_count%+1
hit%=((osword_block%?11)=0)
REM PRINT "Offer block ID ";local_cache_id%(local_cache_block_to_evict%);" (local entry ";local_cache_block_to_evict%;"), want block ID ";wanted_block%;": ";:IF hit% THEN PRINT "hit" ELSE PRINT "miss"
REM To detect the cache corrupting arbitrary memory, we check all the local blocks.
IF hit% THEN hit_count%=hit_count%+1 ELSE PROCcreate_block(wanted_block%,local_addr%)
local_cache_id%(local_cache_block_to_evict%)=wanted_block%
FOR I%=0 TO local_cache_blocks%-1:PROCcheck_block(local_cache_id%(I%),local_cache%+I%*block_size%):NEXT

current_hit_ratio=hit_count%/call_count%:@%=old_at%
IF call_count% MOD 100=0 THEN @%=&20300:PRINT "Expected hit ratio ";expected_hit_ratio;", current hit ratio ";current_hit_ratio:@%=old_at%

UNTIL call_count%>=max_call_count%

REM We need to pause if this happens so the users gets a chance to see it.
IF expected_hit_ratio<(current_hit_ratio*0.8) OR expected_hit_ratio>(current*hit_ratio*1.2) THEN PRINT "Poor current hit ratio; press any key to continue":OSCLI "FX21":IF GET

pass%=pass%+1
UNTIL FALSE
END

REM SFTODO: We could use less repetitive fake data, so we are more likely to
REM catch corrupt data coming back for one reason or another, but this will do for
REM now.
DEF PROCcreate_block(block_num%, addr%)
LOCAL I%
FOR I%=0 TO 511 STEP 4
addr%!I%=block_num%
NEXT
ENDPROC

DEF PROCcheck_block(block_num%,addr%)
LOCAL I%
FOR I%=0 TO 511 STEP 4
IF addr%!I%<>block_num% THEN PRINT "PROCcheck_block(";block_num%;",&";STR$~addr%;") failed!":END
NEXT
ENDPROC

DEF PROCerror
@%=old_at%
REPORT:PRINT " at line ";ERL
END
