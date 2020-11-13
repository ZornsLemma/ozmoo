ON ERROR MODE 7:PROCerror
MODE ${splash_mode}:VDU 23,1,0;0;0;0;
*FX229,1
*FX4,1
true_himem=HIMEM:HIMEM=(TOP+512) AND &FF00
REM If we don't have enough RAM to run the splash screen executable, don't run
REM it!
REM TODO: This check won't detect a very high PAGE in the host if we're running
REM on a second processor. This isn't a likely problem but not ideal.
IF HIMEM>=${splash_start_address} THEN GOTO 2000

A%=0:X%=1:host_os=(USR&FFF4 AND &FF00) DIV &100

REM We didn't request a shadow mode when we changed mode above, but some
REM systems may force it on (e.g. a BBC machine where the user has done
REM *SHADOW, or an Electron with a Master RAM board in shadow mode). We
REM know this has happened because host_himem will be &8000; this isn't
REM affected if we're running on a second processor.
A%=&85:X%=${splash_mode}:host_himem=(USR&FFF4 AND &FFFF00) DIV &100
IF host_himem<&8000 THEN GOTO 1000

REM We've had shadow RAM forced on, so we probably can't show the splash screen.
REM We make an exception for the Electron with a MRB in shadow mode. In order to
REM err on the side of caution (there might be another shadow RAM expansion for
REM the Electron which forces shadow modes on all the time), we check for the
REM precise value at &27F given in the MRB documentation.
IF host_os<>0 THEN GOTO 2000
A%=&EF:X%=0:Y%=&FF:shadow_state=(USR&FFF4 AND &FF00) DIV &100
IF shadow_state<>&80 THEN GOTO 2000

1000FOR colour=0 TO ${splash_max_colour}:VDU 19,colour,0;0;:NEXT
*/SPLASH
${set_splash_palette}
*FX21
!ifdef splash_wait {
K%=INKEY(${splash_wait})
} else {
K%=GET
}

2000HIMEM=true_himem
REM We'd like to leave the splash screen on display while the loader is loaded.
REM We can't do this if the loader wouldn't fit in the current screen mode.
change_now=FALSE
IF (PAGE+${loader_size})>=(HIMEM-&100) THEN change_now=TRUE
REM On an Electron or BBC B, any shadow RAM will be provided by a third party
REM and it *may* be the case that memory from &3000 upwards will be corrupted by
REM changing from a non-shadow mode (as we are probably in at the moment) into a
REM shadow mode (as the loader will do), so we change into a shadow mode now if
REM that's a possibility.
tube=PAGE<&E00
A%=&85:X%=135:potential_himem=(USR&FFF4 AND &FFFF00) DIV &100
IF NOT tube AND (host_os=0 OR host_os=1) AND potential_himem=&8000 AND (PAGE+${loader_size})>=&2F00 THEN change_now=TRUE
IF change_now THEN MODE 135:VDU 23,1,0;0;0;0;
CHAIN "LOADER"
END

DEF PROCerror:REPORT:PRINT" at line ";ERL:*FX229
*FX4
END
