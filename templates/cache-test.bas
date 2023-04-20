IF PAGE<&800 THEN PRINT "Sorry, tube only!":END
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
A%=&88:X%=mode% OR 128:host_cache_size_vmem_blocks%=(USR(&FFF4) AND &FF00) DIV &100
PRINT host_cache_size_vmem_blocks%
