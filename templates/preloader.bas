ON ERROR MODE 7:PROCerror
MODE ${splash_mode}:VDU 23,1,0;0;0;0;
*FX229,1
*FX4,1

A%=0:X%=1:host_os=(USR&FFF4 AND &FF00) DIV &100
A%=&85:X%=${splash_mode}:host_himem=(USR&FFF4 AND &FFFF00) DIV &100
tube=PAGE<&E00
mrb=FALSE

REM We didn't request a shadow mode when we changed mode above, but some
REM systems may force it on (e.g. a BBC machine where the user has done
REM *SHADOW, or an Electron with a Master RAM board in shadow mode). We
REM know this has happened because host_himem will be &8000; this isn't
REM affected if we're running on a second processor.
IF host_himem<&8000 THEN GOTO 1000

REM We've had shadow RAM forced on, so we probably can't show the splash screen.
REM We make an exception for the Electron with a MRB in shadow mode and no
REM second processor. In order to err on the side of caution (there might be
REM another shadow RAM expansion for the Electron), we check for the precise
REM value at &27F given in the MRB documentation. (We don't support a second
REM processor here because it seems unlikely the special MRB OS routine at &FBFD
REM will work over the tube.)
IF tube OR host_os<>0 AND ?&27F<>&80 THEN GOTO 2000

REM We're on an Electron with a Master RAM board in shadow mode and no second
REM processor.
mrb=TRUE
DIM code 256
ptr=&70
FOR opt=0 TO 2 STEP 2
P%=code
[OPT opt
LDA #0:STA ptr
LDA #${splash_screen_address} DIV 256:STA ptr+1
.loop
LDY #0:LDA (ptr),Y
BIT rts:LDX ptr:LDY ptr+1:JSR &FBFD
INC ptr:BNE loop
INC ptr+1:BPL loop
.rts
RTS
]
NEXT

1000FOR C%=0 TO ${splash_max_colour}:VDU 19,C%,0;0;:NEXT
*LOAD SPLASH
IF mrb THEN CALL code
VDU 20
*FX21
!ifdef splash_wait {
K%=INKEY(${splash_wait})
} else {
K%=GET
}

2000
REM We'd like to leave the splash screen on display while the loader is loaded.
REM We can't do this if the loader wouldn't fit in the current screen mode.
change_now=FALSE
IF (PAGE+${loader_size})>=(HIMEM-&100) THEN change_now=TRUE
REM On an Electron or BBC B, any shadow RAM will be provided by a third party and
REM it *may* be the case that memory from &3000 upwards will be corrupted by
REM changing from a non-shadow mode (as we are in at the moment) into a shadow
REM mode (as the loader will do), so we change into a shadow mode now if that's
REM a possibility.
A%=&85:X%=135:potential_himem=(USR&FFF4 AND &FFFF00) DIV &100
IF NOT tube AND (host_os=0 OR host_os=1) AND potential_himem=&8000 AND (PAGE+${loader_size})>=&2F00 THEN change_now=TRUE
IF change_now THEN MODE 135:VDU 23,1,0;0;0;0;
CHAIN "LOADER"
END

DEF PROCerror:REPORT:PRINT" at line ";ERL:*FX229
*FX4
END
