integra_b=FALSE
ON ERROR GOTO 100
integra_b=FNusr_osbyte_x(&49,&FF,0)=&49
100ON ERROR MODE 7:PROCerror

MODE ${splash_mode}:VDU 23,1,0;0;0;0;
*FX229,1
*FX4,1

REM See comment in loader.bas for more on this.
*EXEC
CLOSE #0

tube=PAGE<&E00
host_os=FNusr_osbyte_x(0,1,0)

REM Lower HIMEM to avoid the SPLASH executable corrupting anything. This is
REM unnecessary but harmless if we're running on a second processor; the
REM SPLASH executable runs on the host.
true_himem=HIMEM:HIMEM=(TOP+512) AND &FF00

REM If running the SPLASH executable is going to trample on things, don't!
IF NOT tube AND HIMEM>=${splash_start_address} THEN GOTO 2000
IF tube AND FNusr_osbyte_x(&B3,0,&FF)*256+&600>=${splash_start_address} THEN GOTO 2000

REM We didn't request a shadow mode when we changed mode above, but some
REM systems may force it on (e.g. a BBC machine where the user has done
REM *SHADOW, or an Electron with a Master RAM board in shadow mode). We
REM know this has happened because OSBYTE &85 will return &8000; this isn't
REM affected if we're running on a second processor.
REM SFTODO: I haven't tested it yet, but I suspect the Watford 32K shadow RAM
REM card for the BBC B will always return &8000 and we won't be able to display the
REM splash screen. We could work around this, although we'd probably need to be
REM installing the shadow driver here instead of in the loader in order to avoid
REM problems knowing which OSBYTE to use.
IF FNmode_himem(${splash_mode})<&8000 THEN GOTO 1000

REM We've had shadow RAM forced on, so we probably can't show the splash screen.
REM We make an exception for the Electron with a MRB in shadow mode; the SPLASH
REM executable contains special code to transfer the image into shadow RAM
REM using the MRB OS routine. In order to err on the side of caution (there
REM might be another shadow RAM expansion for the Electron which forces shadow
REM modes on all the time), we check for the precise value at &27F given in the
REM MRB documentation.
IF host_os<>0 THEN GOTO 2000
IF FNusr_osbyte_x(&EF,0,&FF)<>&80 THEN GOTO 2000

1000FOR colour=0 TO ${splash_max_colour}:VDU 19,colour,0;0;:NEXT
*/SPLASH
${set_splash_palette}
*FX21
!ifdef splash_wait {
key=INKEY(${splash_wait})
} else {
key=GET
}

2000HIMEM=true_himem
REM We'd like to leave the splash screen on display while the loader is loaded.
REM We can't do this if the loader wouldn't fit in the current screen mode.
change_now=FALSE
IF PAGE+${loader_size}>=HIMEM-&100 THEN change_now=TRUE
REM On an Electron or BBC B, any shadow RAM will be provided by a third party
REM and it *may* be the case that memory from &3000 upwards will be corrupted by
REM changing from a non-shadow mode (as we are probably in at the moment) into a
REM shadow mode (as the loader will do), so we change into a shadow mode now if
REM that's a possibility. The Integra-B can fake the value of host_os but still
REM retains the possibility of this corruption (it depends on the SHX setting)
REM so we test for it explicitly.
REM SFTODO: Maybe we should add "AND HIMEM<>&8000" to the following line?
REM That would avoid doing an unnecessary mode change where shadow RAM is permanently
REM forced on but we take whatever steps are necessary to load the image into video
REM RAM - currently this is just the Electron MRB shadow case, but in principle it
REM could happen on other machines so better to handle it via this generic kind of test.
IF NOT tube AND (host_os<=1 OR integra_b) AND FNmode_himem(135)=&8000 AND PAGE+${loader_size}>=&2F00 THEN change_now=TRUE
IF change_now THEN MODE 135:VDU 23,1,0;0;0;0;
CHAIN "LOADER"
END

DEF FNusr_osbyte_x(A%,X%,Y%)=(USR&FFF4 AND &FF00) DIV &100

REM Note that if we're running on the tube this still returns the value of HIMEM
REM in the host, so it should successfully detect cases where shadow RAM is forced
REM on even then.
DEF FNmode_himem(X%):A%=&85:=(USR&FFF4 AND &FFFF00) DIV &100

DEF PROCerror:REPORT:PRINT" at line ";ERL:*FX229
*FX4
END

REM SFTODO: Should we run FINDSWR from the preloader? That way the user gets to
REM admire the title screen while it's run - it doesn't take long, but it's not
REM nothing. We could deduct the time it takes to run it from any splash_wait
REM time as well.
