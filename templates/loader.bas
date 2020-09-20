REM The loader uses some of the resident integer variables (or at least the
REM corresponding memory) to communicate with the Ozmoo executable. This
REM means it's probably least error prone to avoid using resident integer
REM variables gratuitously in this code.
REM SFTODO: It would be nice if the loader and build system could work
REM together to allow the user to *optionally* specify a high-res title
REM screen before we go into the mode 7 loader.
REM SFTODO: In principle the build system could communicate size of
REM "nonstored_blocks" to this code (it's hard for it to modify it
REM directly, but it could set a resident integer variable in !BOOT)
REM and it could also pass on the value of story_start, then this code
REM (with some fiddling to allow for relocation - but this code tells
REM where to relocate to, so it's possible) could tell the user if
REM the game fits entirely in RAM on their machine. It could even set
REM a flag to communicate this to the Ozmoo binary and prevent it
REM mentioning removing game disc for save. It would still need to
REM request/check for the binary on RESTART, so it would need code
REM for that and maybe RESTART counts as "playing" the game, so the
REM user shouldn't be told here in the first place.
*FX229,1
:
REM On an Integra-B, we may have problems selecting shadow mode from this
REM large program which may be using memory above &3000 if we're currently
REM in a non-shadow mode. Normally !BOOT selects a shadow mode to avoid
REM this problem, but we do this as a fallback (e.g. if we've been copied
REM to a hard drive and our !BOOT isn't in use any more).
A%=&85:X%=135:potential_himem%=(USR&FFF4 AND &FFFF00) DIV &100
IF potential_himem%=&8000 AND HIMEM<&8000 THEN MODE 135:CHAIN "LOADER"
:
REM The following need to be kept consistent with asm/constants.asm
REM SFTODO: Should we make make-acorn.py substitute them in?
relocate_target=&408
fg_colour=&409
bg_colour=&40A
screen_mode=&40B
ram_bank_count=&904
ram_bank_list=&905
filename_data=&42F
filename_size=49
:
MODE 135
VDU 23,1,0;0;0;0;
?fg_colour=7
?bg_colour=4
A%=0:X%=1:host_os%=USR(&FFF4) DIV &100 AND &FF
IF host_os%=0 THEN VDU 19,0,?bg_colour,0;0,19,7,?fg_colour,0;0
*FX4,1
DIM block% 256
REM ${BANNER} - make-acorn.py will add banner printing code here
ON ERROR PROCerror
REM SFTODO: Note that for Z3 games, anything shown on the top line of
REM the screen will remain present occupying the not-yet-displayed
REM status line until the game starts. This means that if any disc
REM errors occur during the initial loading, the screen may scroll
REM but the top line won't. This isn't a big deal but for the nicest
REM possible appearance in this admittedly unlikely situation either
REM clear the top line before running the Ozmoo executable or make
REM sure it has something that looks OK on its own. (For example,
REM *not* the top half of some double-height text.)
:
shadow%=(potential_himem%=&8000)
tube%=(PAGE<&E00)
PROCdetect_swr:REM will die if 0 banks found
IF tube% THEN ${TUBEDETECTED}
REM SFTODO: Hypothetical AQR support might kick in here and if found (maybe we ask the user for permission) we'd select the relevant binary and GOTO 1000, otherwise we'd carry on
IF shadow% AND host_os%<>0 THEN ${BBCSHRSWRDETECTED}
IF host_os%<>0 THEN ${BBCSWRDETECTED}
IF host_os%=0 THEN ${ELECTRONSWRDETECTED}
1000PRINTTAB(0,first_loader_line);CHR$${HEADERFG};"Hardware detected:"
IF tube% THEN PRINT CHR$${NORMALFG};"  Second processor"
IF shadow% THEN PRINT CHR$${NORMALFG};"  Shadow RAM"
IF swr% THEN PRINT CHR$${NORMALFG};"  ";swr$
IF NOT tube% THEN ?relocate_target=FNrelocate_to DIV 256
IF PAGE>max_page% THEN PROCdie("Sorry, PAGE must be <=&"+STR$~max_page%+".")
start_mode%=${DEFAULTMODE}
IF host_os%=0 AND start_mode%=7 THEN start_mode%=6
auto%=${AUTOSTART}
IF host_os%=0 THEN mode_key$="0346" ELSE mode_key$="03467"
mode_x%=FNmode_x(start_mode%)
mode_y%=FNmode_y(start_mode%)
IF NOT any_mode% THEN mode_key$="" ELSE IF NOT auto% THEN PROCmode_menu
PRINT'CHR$${HEADERFG};"In-game controls:"
controls_vpos%=VPOS
PROCupdate_controls
IF NOT auto% THEN PRINTTAB(0,space_line);CHR$${NORMALFG};"Press SPACE/RETURN to start the game...";
REPEAT
*FX21
IF auto% THEN key$=" " ELSE key$=GET$
IF host_os%=0 AND key$=CHR$(2) THEN ?bg_colour=(?bg_colour+1) MOD 8:VDU 19,0,?bg_colour,0;0
IF host_os%=0 AND key$=CHR$(6) THEN ?fg_colour=(?fg_colour+1) MOD 8:VDU 19,7,?fg_colour,0;0
IF INSTR(mode_key$,key$)<>0 THEN PROCmenu_to_mode(VAL(key$)) ELSE IF ASC(key$)>=136 AND ASC(key$)<=139 THEN PROCmenu_cursor(ASC(key$))
UNTIL key$=" " OR key$=CHR$(13)
?screen_mode=FNmode_from_menu
IF ?screen_mode=7 THEN ?fg_colour=6
VDU 28,0,space_line,39,space_line,12,26,31,0,space_line,${NORMALFG}
PRINT "Loading, please wait...";
${TUBECACHE}
fs%=FNfs
IF fs%<>4 THEN path$=FNpath
REM Select user's home directory on NFS
IF fs%=5 THEN *DIR
REM On non-DFS, select a SAVES directory if it exists but don't worry if it doesn't.
ON ERROR GOTO 2000
IF fs%=4 THEN PROCoscli("DIR S") ELSE *DIR SAVES
2000ON ERROR PROCerror
IF fs%=4 THEN filename$="/"+binary$ ELSE filename$=path$+".DATA"
IF LEN(filename$)>(filename_size-1) THEN PROCdie("Game data path too long")
REM We do this last, as it uses a lot of resident integer variable space and this reduces
REM the chances of it accidentally getting corrupted.
$filename_data=filename$
*FX4,0
REM SFTODO: Should test with BASIC I at some point, probably work fine but galling to do things like PROCoscli and still not work on BASIC I!
IF fs%=4 THEN PROCoscli($filename_data) ELSE PROCoscli("/"+path$+"."+binary$)
END
:
DEF PROCdetect_swr
swr%=FALSE
*/FINDSWR
swr_type=&903
c%=FNpeek(ram_bank_count)
IF c%=0 AND NOT tube% THEN PROCdie("Sorry, no free sideways RAM or second  "+CHR$${NORMALFG}+"processor detected.")
IF c%=0 THEN ENDPROC
IF FNpeek(swr_type)>2 THEN  PROCdie("Sorry, only ROMSEL-controlled sideways "+CHR$${NORMALFG}+"RAM currently supported.")
swr$=STR$(16*c%)+"K sideways RAM (bank"
IF c%>1 THEN swr$=swr$+"s"
swr$=swr$+" &"
FOR i%=0 TO c%-1
swr$=swr$+STR$~FNpeek(ram_bank_list+i%)
NEXT
swr$=swr$+")"
swr%=TRUE
ENDPROC
:
DEF FNrelocate_to
REM SFTODO: We could potentially be more aggressive, relocating down to &1100 or &1300
REM (we'd need to set a resident integer variable in !BOOT or something so the build
REM script could communicate which is appropriate) on a B or B+. However, a) I suspect
REM doing so is incompatible with shadow RAM on a B, where the third party shadow RAM
REM will have workspace probably at &1900, so we'd probably want to avoid doing that
REM if we're on a B and shadow RAM is available b) I have in the past had problems with
REM the SRAM utilities writing (legitimately, if annoyingly) to their part of the "DFS"
REM workspace and corrupting the RAM I've used below &1900 on OS errors, so this might
REM be error prone or require some careful setup work here to disable such things. For
REM now just play it safe.
REM SFTODO: If the next line is "=PAGE", beebasm seems to tokenise it incorrectly.
dummy%=PAGE
=dummy%
:
DEF PROCmode_menu
DIM m$(1,1)
m$(0,0)="0) 80x32"
m$(1,0)="4) 40x32"
m$(0,1)="3) 80x25"
m$(1,1)="6) 40x25"
PRINT'CHR$${HEADERFG};"Screen mode:";CHR$${NORMALFG};"(hit 0/3/4/6";
IF host_os%<>0 THEN PRINT "/7";
PRINT " to change)"
mode_menu_vpos%=VPOS
IF host_os%<>0 THEN PRINT CHR$${NORMALFG};"  0) 80x32    4) 40x32    7) 40x25"'CHR$${NORMALFG};"  3) 80x25    6) 40x25       teletext" ELSE FOR y%=0 TO 1:FOR x%=0 TO 1:PRINTTAB(3+x%*20,mode_menu_vpos%+y%);m$(x%,y%):NEXT:NEXT
vpos%=VPOS
PROChighlight_mode_menu(mode_x%,mode_y%,TRUE)
PRINTTAB(0,vpos%);
old_mode_x%=mode_x%:old_mode_y%=mode_y%
ENDPROC
:
DEF PROCmenu_cursor(key%)
IF host_os%=0 THEN max_x%=1 ELSE max_x%=2
IF key%=136 AND mode_x%>0 THEN mode_x%=mode_x%-1
IF key%=137 AND mode_x%<max_x% THEN mode_x%=mode_x%+1
IF key%=138 AND mode_y%<1 THEN mode_y%=mode_y%+1
IF key%=139 AND mode_y%>0 THEN mode_y%=mode_y%-1
PROCupdate_mode_menu
ENDPROC
:
DEF PROCmenu_to_mode(mode%)
mode_x%=FNmode_x(mode%)
mode_y%=FNmode_y(mode%)
PROCupdate_mode_menu
ENDPROC
:
DEF PROCupdate_mode_menu
IF mode_x%=old_mode_x% AND mode_y%=old_mode_y% THEN ENDPROC
IF mode_x%=2 AND old_mode_x%=2 THEN ENDPROC
PROChighlight_mode_menu(old_mode_x%,old_mode_y%,FALSE)
PROChighlight_mode_menu(mode_x%,mode_y%,TRUE)
old_mode_x%=mode_x%:old_mode_y%=mode_y%
PROCupdate_controls
ENDPROC
:
DEF PROChighlight_mode_menu(mode_x%,mode_y%,on%)
LOCAL x%,width%,start_y%,end_y%,y%:REM SFTODO OUT OF DATE
IF host_os%=0 THEN PROChighlight_mode_menu_electron(mode_x%,mode_y%,on%):ENDPROC
IF mode_x%=2 THEN start_y%=0:end_y%=1:width%=0 ELSE start_y%=mode_y%:end_y%=mode_y%:width%=13
x%=mode_x%*12
FOR y%=start_y% TO end_y%
PRINTTAB(x%,mode_menu_vpos%+y%);
IF on% THEN VDU ${HIGHLIGHTBG},157,${HIGHLIGHTFG} ELSE PRINT CHR$${NORMALFG};"  ";
PRINTTAB(x%+width%,mode_menu_vpos%+y%);
IF width%>0 AND on% THEN VDU 156,${NORMALFG}
IF width%>0 AND NOT on% THEN PRINT " ";
NEXT
ENDPROC
:
DEF PROChighlight_mode_menu_electron(mode_x%,mode_y%,on%)
IF on% THEN COLOUR 135:COLOUR 0 ELSE COLOUR 128:COLOUR 7
PRINTTAB(1+mode_x%*20,mode_menu_vpos%+mode_y%);"  ";m$(mode_x%,mode_y%);"  ";
COLOUR 128:COLOUR 7
ENDPROC
:
DEF FNmode_x(mode%)
IF mode%=0 OR mode%=3 THEN =0
IF mode%=4 OR mode%=6 THEN =1
=2
:
DEF FNmode_y(mode%)
IF mode%=0 OR mode%=4 OR mode%=7 THEN =0
=1
:
DEF FNmode_from_menu
IF mode_x%=2 THEN =7
IF mode_x%=1 THEN =4+2*mode_y%
=0+3*mode_y%
:
DEF PROCupdate_controls
REM SFTODO: We shouldn't mention CTRL-F at all in mode 7 if the build script has turned mode 7 colour off
PRINTTAB(0,controls_vpos%);CHR$${NORMALFG};"  CTRL-F: ";
IF mode_x%=2 THEN PRINT "change status line colour" ELSE PRINT "change foreground colour "'CHR$${NORMALFG};"  CTRL-B: change background colour"
IF binary$<>"OZMOOB" THEN PRINT CHR$${NORMALFG};"  CTRL-S: change scrolling mode   "
IF mode_x%=2 THEN PRINT STRING$(40, " ");
ENDPROC
:
DEF PROCerror
PROCclear
REPORT:PRINT " at line ";ERL
PROCcleanup
END
:
DEF PROCdie(message$)
PROCclear
PRINT CHR$${NORMALFG};message$'
PROCcleanup
END
:
DEF PROCclear
REM SFTODO: If we don't detect SWR/2P, this gives slightly ugly output. But I would like to leave the HW detected line present if we die for some other reason, as it's informative (e.g. max_page varies with build chosen).
VDU 28,0,last_loader_line,39,first_loader_line+3,12
ENDPROC
:
DEF PROCcleanup
VDU 23,1,1,0;0;0;0;
*FX4,0
*FX229,0
ENDPROC
:
DEF PROCoscli($block%)
LOCAL X%,Y%
X%=block%:Y%=block% DIV 256:CALL &FFF7
ENDPROC
:
REM SFTODO: WE DON'T NEED THIS ON DFS
DEF FNpath
LOCAL path$,A%,X%,Y%,name%,name$,drive$
DIM data% 256
path$=""
REPEAT
block%!1=data%
A%=6:X%=block%:Y%=block% DIV 256:CALL &FFD1
name%=data%+1+?data%
name%?(1+?name%)=13
name$=FNstrip($(name%+1))
path$=name$+"."+path$
REM On Econet, you can't do *DIR ^ when in the root.
IF name$<>"$" AND name$<>"&" THEN *DIR ^
UNTIL name$="$" OR name$="&"
path$=LEFT$(path$,LEN(path$)-1)
?name%=13
drive$=FNstrip($(data%+1))
IF drive$<>"" THEN path$=":"+drive$+"."+path$
PROCoscli("DIR "+path$)
=path$
:
DEF FNstrip(s$)
s$=s$+" "
REPEAT:s$=LEFT$(s$,LEN(s$)-1):UNTIL RIGHT$(s$,1)<>" "
=s$
:
DEF FNfs
LOCAL A%,Y%
A%=0:Y%=0:=USR(&FFDA) AND &FF
:
DEF FNpeek(addr%)
!block%=&FFFF0000 OR addr%
A%=5:X%=block%:Y%=block% DIV 256:CALL &FFF1
=block%?4
