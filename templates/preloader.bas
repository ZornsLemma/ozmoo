ON ERROR MODE 7:PROCerror
MODE ${splash_mode}:VDU 23,1,0;0;0;0;
*FX229,1
*FX4,1
FOR C%=0 TO ${splash_max_colour}:VDU 19,C%,0;0;:NEXT
*LOAD SPLASH
VDU 20
*FX21
!ifdef splash_wait {
K%=INKEY(${splash_wait})
} else {
K%=GET
}

REM We'd like to leave the splash screen on display while the loader is loaded.
REM We can't do this if the loader wouldn't fit in the current screen mode.
change_now=FALSE
IF (PAGE+${loader_size})>=(HIMEM-&100) THEN change_now=TRUE
REM On an Electron or BBC B, any shadow RAM will be provided by a third-party and
REM it *may* be the case that memory from &3000 upwards will be corrupted by
REM changing from a non-shadow mode (as we are in at the moment) into a shadow
REM mode (as the loader will do), so we change into a shadow mode now if that's
REM a possibility.
tube=PAGE<&E00
A%=0:X%=1:host_os=(USR&FFF4 AND &FF00) DIV &100
A%=&85:X%=135:potential_himem=(USR&FFF4 AND &FFFF00) DIV &100
IF NOT tube AND (host_os=0 OR host_os=1) AND potential_himem=&8000 AND (PAGE+${loader_size})>=&2F00 THEN change_now=TRUE
IF change_now THEN MODE 135:VDU 23,1,0;0;0;0;
CHAIN "LOADER"
END

DEF PROCerror:REPORT:PRINT" at line ";ERL:*FX229
*FX4
END
