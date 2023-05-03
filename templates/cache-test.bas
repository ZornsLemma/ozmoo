IF PAGE>&800 THEN PRINT "Sorry, tube only!":END

old_at%=@%
ON ERROR PROCerror

mode%=0
REM The Ozmoo executable always forces the shadow mode bit on when initialising
REM the cache, so we do the same. We don't want to chase phantom bugs or quirks
REM caused by running on a machine with shadow RAM but not using a shadow mode.
REM SFTODO: Maybe change this later. It might be nice - if it works - to be
REM able to test non-shadow and shadow cases without actually switching to a
REM machine with/without shadow RAM. But for now let's keep it as close to real
REM use as possible.
MODE mode% OR 128

DIM osword_block% 64

integra_b%=FNusr_osbyte_x(&49,&FF,0)=&49
IF integra_b% THEN host_os%=1 ELSE host_os%=FNusr_osbyte_x(0,1,0)

*/SHADDRV
*/FINDSWR

private_ram_bank%=0
IF integra_b% THEN private_ram_bank%=64
IF host_os%=2 THEN private_ram_bank%=128
ram_banks%=FNpeek(${ram_bank_count}):
IF private_ram_bank%<>0 AND ram_banks%<${max_ram_bank_count} THEN PROCpoke(${ram_bank_list}+ram_banks%,private_ram_bank%):ram_banks%=ram_banks%+1:PROCpoke(${ram_bank_count},ram_banks%)

PROCpoke(${cache_screen_mode},mode% AND &7F)
*/CACHE2P

track_offers%=10:REM 0 disables offer tracking
IF track_offers%>0 THEN DIM recent_offers%(track_offers%-1)

block_size%=512:REM bytes

local_cache_blocks%=10
DIM local_cache% local_cache_blocks%*block_size%
DIM local_cache_id%(local_cache_blocks%-1)

REM In order to help catch bugs that only occurs when the cache is not fully
REM populated, we periodically restart.
pass%=0
REPEAT
CLS

game_blocks%=489

IF track_offers%>0 THEN FOR I%=0 TO track_offers%-1:recent_offers%(I%)=-1:NEXT
recent_offers_ptr%=0

REM X is no longer used by this OSBYTE; we set it to a defined but wrong value
REM to make it more obvious if it's used by mistake.
A%=&88:X%=(mode%+2) AND 7:host_cache_size_vmem_blocks%=(USR(&FFF4) AND &FF00) DIV &100
PRINT "Host cache size (blocks): ";host_cache_size_vmem_blocks%;" / Pass: ";pass%
max_call_count%=1000+RND(20)*100

IF track_offers%>host_cache_size_vmem_blocks% THEN PRINT "track_offers% can't be larger than the host cache size":END

expected_hit_ratio=host_cache_size_vmem_blocks%/game_blocks%

REM We don't try to model exactly what the cache will be doing. Instead,
REM we request game data blocks at random and track the cache hit ratio - since we
REM are requesting at random, if the cache is working properly the hit ratio will
REM approximate the ratio of cache size to game data size. The cache starts empty
REM which will drag the hit ratio down at first but that effect should wear off over
REM time. If track_offers%>0, we do track that many of our most recent offers (i.e.
REM we track a subset of what the cache ought to hold) and make sure that if we don't
REM get a cache hit, it wasn't for one of those blocks we know should have been there.

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
IF NOT hit% AND track_offers%>0 THEN PROCcheck_not_recent_offer(wanted_block%)
IF track_offers%>0 THEN recent_offers%(recent_offers_ptr%)=local_cache_id%(local_cache_block_to_evict%):recent_offers_ptr%=(recent_offers_ptr%+1) MOD track_offers%
REM PRINT "Offer block ID ";local_cache_id%(local_cache_block_to_evict%);" (local entry ";local_cache_block_to_evict%;"), want block ID ";wanted_block%;": ";:IF hit% THEN PRINT "hit" ELSE PRINT "miss"
REM To detect the cache corrupting arbitrary memory, we check all the local blocks.
IF hit% THEN hit_count%=hit_count%+1 ELSE PROCcreate_block(wanted_block%,local_addr%)
local_cache_id%(local_cache_block_to_evict%)=wanted_block%
FOR I%=0 TO local_cache_blocks%-1:PROCcheck_block(local_cache_id%(I%),local_cache%+I%*block_size%):NEXT

current_hit_ratio=hit_count%/call_count%:@%=old_at%
IF call_count% MOD 100=0 THEN @%=&20300:PRINT "Expected hit ratio ";expected_hit_ratio;", current hit ratio ";current_hit_ratio:@%=old_at%

UNTIL call_count%>=max_call_count%

REM We need to pause if this happens so the users gets a chance to see it.
IF expected_hit_ratio<(current_hit_ratio*0.8) OR expected_hit_ratio>(current_hit_ratio*1.2) THEN PRINT "Poor current hit ratio; press any key to continue":OSCLI "FX21":IF GET

pass%=pass%+1
UNTIL FALSE
END

REM SFTODO: We could use less repetitive fake data, so we are more likely to
REM catch corrupt data coming back for one reason or another, but this will do for
REM now.
DEF PROCcreate_block(block_num%, addr%)
LOCAL I%
FOR I%=0 TO 511 STEP 4
addr%!I%=block_num%*I%
NEXT
ENDPROC

DEF PROCcheck_block(block_num%,addr%)
LOCAL I%
FOR I%=0 TO 511 STEP 4
IF addr%!I%<>(block_num%*I%) THEN PRINT "PROCcheck_block(";block_num%;",&";STR$~addr%;") failed!":END
NEXT
ENDPROC

DEF PROCcheck_not_recent_offer(wanted_block%)
LOCAL I%
FOR I%=0 TO track_offers%-1
IF recent_offers%(I%)=wanted_block% THEN PRINT "PROCcheck_not_recent_offer(";wanted_block%;") failed!":END
NEXT
ENDPROC

DEF PROCerror
@%=old_at%
REPORT:PRINT " at line ";ERL
END

DEF FNpeek(addr):!osword_block%=&FFFF0000 OR addr:A%=5:X%=osword_block%:Y%=osword_block% DIV 256:CALL &FFF1:=osword_block%?4

DEF PROCpoke(addr,val):!osword_block%=&FFFF0000 OR addr:osword_block%?4=val:A%=6:X%=osword_block%:Y%=osword_block% DIV 256:CALL &FFF1:ENDPROC

DEF FNusr_osbyte_x(A%,X%,Y%)=(USR&FFF4 AND &FF00) DIV &100
