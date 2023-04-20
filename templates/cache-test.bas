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
A%=&88:X%=mode% OR 128:host_cache_size_vmem_blocks%=(USR(&FFF4) AND &FF00) DIV &100
PRINT "Host cache size (blocks): ";host_cache_size_vmem_blocks%

DIM osword_block% 64

REM For now we don't try to model exactly what the cache will be doing. Instead,
REM we request game data blocks at random and track the cache hit ratio - since we
REM are requesting at random, if the cache is working properly the hit ratio will
REM approximate the ratio of cache size to game data size. The cache starts empty
REM which will drag the hit ratio down at first but that effect should wear off over
REM time.

block_size%=512:REM bytes

game_blocks%=489

local_cache_blocks%=10
DIM local_cache% local_cache_blocks%*block_size%
DIM local_cache_id%(local_cache_blocks%-1)
FOR I%=0 TO local_cache_blocks%-1:local_cache_id%(I%)=RND(game_blocks%)-1:PROCcreate_block(local_cache_id%(I%),local_cache%+I%*block_size%):NEXT

hit_count%=0:call_count%=0
REPEAT

REM Pick a game block we don't already have in our local cache at random.
REM SFTODO: We might get away with picking a game block and not caring if it's in
REM our local cache, but it seems like it might cause confusion when we see
REM "invalid" state (the same block in host and local cache) during debugging.
REPEAT
wanted_block%=RND(game_blocks%)-1
already_have%=FALSE
FOR I%=0 TO local_cache_blocks%-1
IF local_cache_id%(I%)=wanted_block% THEN already_have%=TRUE
NEXT
UNTIL NOT already_have%

local_cache_block_to_evict%=RND(local_cache_blocks%)-1

osword_block%?0=12
osword_block%?1=12
osword_block%!2=local_cache%+local_cache_block_to_evict%*block_size%
osword_block%!6=local_cache_id%(local_cache_block_to_evict%):REM SFTODO: Do something to test "&FFFF=no block offered" case?
osword_block%?8=&FF:REM SFTODO: Do something to test timestamp hints?
osword_block%!9=wanted_block%
A%=&E0:X%=osword_block%:Y%=osword_block% DIV 256:CALL &FFF1
call_count%=call_count%+1
hit%=((osword_block%?11)=0)
local_addr%=local_cache%+local_cache_block_to_evict%*block_size%
IF hit% THEN PROCcheck_block(wanted_block%,local_addr%):hit_count%=hit_count%+1 ELSE PROCcreate_block(wanted_block%,local_addr%)
local_cache_id%(local_cache_block_to_evict%)=wanted_block%

IF call_count% MOD 100=0 THEN @%=&20200:PRINT "Expected hit ratio ";host_cache_size_vmem_blocks%/game_blocks%;", current hit ratio ";hit_count%/call_count%:@%=old_at%

UNTIL FALSE
END

REM SFTODO: We could use less repetitive fake data, so we are more likely to
REM catch corrupt data coming back for one reason or another, but this will do for
REM now.
DEF PROCcreate_block(block_num%, addr%)
FOR I%=0 TO 511 STEP 4
addr%!I%=block_num%
NEXT
ENDPROC

DEF PROCcheck_block(block_num%,addr%)
FOR I%=0 TO 511 STEP 4
IF addr%!I%<>block_num% THEN PRINT "PROCcheck_block(";block_num%;",&";STR$~addr%;") failed!":END
NEXT
ENDPROC

DEF PROCerror
@%=old_at%
REPORT:PRINT " at line ";ERL
END
