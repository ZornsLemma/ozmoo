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
MODE 135
VDU 23,1,0;0;0;0;
ON ERROR PROCerror
*FX4,1
DIM block% 256
REM ${BANNER} - make-acorn.py will add banner printing code here
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
REM The following need to be kept consistent with asm/constants.asm
relocate_target=&408
fg_colour=&409
bg_colour=&40A
screen_mode=&40B
ram_bank_count=&410
ram_bank_list=&411
max_ram_bank_count=9:REM 255*0.5K for VM plus 16K for dynamic memory
game_data_filename=&42F
game_data_filename_size=32
:
shadow%=(HIMEM>=&8000)
tube%=(PAGE<&E00)
A%=0:X%=1:host_os%=USR(&FFF4) DIV &100 AND &FF
IF tube% THEN ${TUBEDETECTED}
*/FINDSWR
PROCdetect_swr:REM will die if 0 banks found
REM SFTODO: Hypothetical AQR support might kick in here and if found (maybe we ask the user for permission) we'd select the relevant binary and GOTO 1000, otherwise we'd carry on
IF shadow% AND host_os%<>0 THEN ${BBCSHRSWRDETECTED}
IF host_os%<>0 THEN ${BBCSWRDETECTED}
IF host_os%=0 THEN ${ELECTRONSWRDETECTED}
REM SFTODO: I'm assuming the mode selection and default colour code below knows how to handle the Electron (no mode 7) rather than us having extra options here
1000PRINT CHR$${HEADERFG};"Hardware detected:"'CHR$${NORMALFG};"  ";hw$
IF NOT tube% THEN ?relocate_target=FNrelocate_to DIV 256
IF PAGE>max_page% THEN PROCdie("Sorry, PAGE must be <=&"+STR$~max_page%+".")
mode%=${DEFAULTMODE}
auto%=${AUTOSTART}
mode_key$="03467"
mode_y%=0
IF NOT (tube% OR shadow%) THEN mode%=7:mode_key$="" ELSE IF NOT auto% THEN PROCmode_menu(mode%)
IF host_os%=0 AND mode%=7 THEN mode%=6:REM SFTODO HACK FOR ELECTRON
PRINT'CHR$${HEADERFG};"In-game controls:"
controls_vpos%=VPOS
PROCupdate_controls(mode%)
IF NOT auto% THEN PRINTTAB(0,${SPACELINE});CHR$${NORMALFG};"Press SPACE to start the game...";
REPEAT
*FX21
IF auto% THEN key$=" " ELSE key$=GET$
IF ASC(key$)>=136 AND ASC(key$)<=139 THEN key$=FNupdate_cursor(ASC(key$))
IF INSTR(mode_key$,key$)<>0 AND mode%<>VAL(key$) THEN PROCupdate_mode_menu(mode%,VAL(key$)):mode%=VAL(key$):PROCupdate_controls(mode%)
UNTIL key$=" "
?screen_mode=mode%
IF mode%=7 THEN ?fg_colour=6 ELSE ?fg_colour=7
?bg_colour=4
VDU 28,0,${SPACELINE},39,${SPACELINE},12,26,31,0,${SPACELINE},${NORMALFG}
PRINT "Loading, please wait...";
fs%=FNfs
IF fs%=4 THEN path$=":0.$" ELSE path$=FNpath
REM Select user's home directory on NFS
IF fs%=5 THEN *DIR
REM On non-DFS, select a SAVES directory if it exists but don't worry if it doesn't.
ON ERROR GOTO 2000
IF fs%=4 THEN PROCoscli("DIR S") ELSE *DIR SAVES
2000ON ERROR PROCerror
game_data_path$=path$+".DATA"
IF LEN(game_data_path$)>(game_data_filename_size-1) THEN PROCdie("Game data path too long")
REM We do this last, as it uses a lot of resident integer variable space and this reduces
REM the chances of it accidentally getting corrupted.
$game_data_filename=game_data_path$
*FX4,0
REM SFTODO: Should test with BASIC I at some point, probably work fine but galling to do things like PROCoscli and still not work on BASIC I!
PROCoscli("/"+path$+"."+binary$)
END
:
DEF PROCdetect_swr
*/FINDSWR
swr_banks=&903
swr_type=&904
swr_test=&905
IF ?swr_banks=0 THEN PROCdie("Sorry, no free sideways RAM or second  "+CHR$${NORMALFG}+"processor detected.")
IF ?swr_type>2 THEN  PROCdie("Sorry, only ROMSEL-controlled sideways "+CHR$${NORMALFG}+"RAM currently supported.")
REM We don't trust ?swr_banks because ROM write through can make it misleading.
REM Instead we take the first max_ram_bank_count banks with a non-0 count in
REM swr_test.
c%=0
FOR i%=0 TO 15
IF i%?swr_test>0 AND c%<max_ram_bank_count THEN ram_bank_list?c%=i%:c%=c%+1
NEXT
?ram_bank_count=c%
hw$=STR$(16*?ram_bank_count)+"K sideways RAM (bank"
IF c%>1 THEN hw$=hw$+"s"
hw$=hw$+" &"
FOR i%=0 TO c%-1
hw$=hw$+STR$~(ram_bank_list?i%)
NEXT
hw$=hw$+")"
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
DEF PROCmode_menu(mode%)
PRINT'CHR$${HEADERFG};"Screen mode:";CHR$${NORMALFG};"(hit 0/3/4/6/7 to change)"
mode_menu_vpos%=VPOS
PRINT CHR$${NORMALFG};"  0) 80x32    4) 40x32    7) 40x25"
PRINT CHR$${NORMALFG};"  3) 80x25    6) 40x25       teletext"
vpos%=VPOS
PROChighlight_mode_menu(mode%,TRUE)
PRINTTAB(0,vpos%);
ENDPROC
:
DEF FNupdate_cursor(key%)
IF key%=136 AND mode_x%>0 THEN mode_x%=mode_x%-1
IF key%=137 AND mode_x%<2 THEN mode_x%=mode_x%+1
IF key%=138 AND mode_y%<1 THEN mode_y%=mode_y%+1
IF key%=139 AND mode_y%>0 THEN mode_y%=mode_y%-1
IF mode_x%=0 AND mode_y%=0 THEN ="0"
IF mode_x%=0 AND mode_y%=1 THEN ="3"
IF mode_x%=1 AND mode_y%=0 THEN ="4"
IF mode_x%=1 AND mode_y%=1 THEN ="6"
="7"
:
DEF PROCupdate_mode_menu(old_mode%,new_mode%)
PROChighlight_mode_menu(old_mode%,FALSE)
PROChighlight_mode_menu(new_mode%,TRUE)
ENDPROC
:
DEF PROChighlight_mode_menu(mode%,on%)
LOCAL x%,width%,start_y%,end_y%,y%
IF mode%=4 OR mode%=6 THEN x%=12:mode_x%=1 ELSE IF mode%=7 THEN x%=24:mode_x%=2 ELSE x%=0:mode_x%=0
IF mode%=0 OR mode%=4 THEN mode_y%=0
IF mode%=3 OR mode%=6 THEN mode_y%=1
IF mode%=0 OR mode%=4 OR mode%=7 THEN start_y%=mode_menu_vpos% ELSE start_y%=mode_menu_vpos%+1
IF mode%=7 THEN end_y%=start_y%+1:width%=0 ELSE end_y%=start_y%:width%=13
FOR y%=start_y% TO end_y%
PRINTTAB(x%,y%);
IF on% THEN VDU ${HIGHLIGHTBG},157,${HIGHLIGHTFG} ELSE PRINT CHR$${NORMALFG};"  ";
PRINTTAB(x%+width%,y%);
IF width%>0 AND on% THEN VDU 156,${NORMALFG}
IF width%>0 AND NOT on% THEN PRINT " ";
NEXT
ENDPROC
:
DEF PROCupdate_controls(mode%)
REM SFTODO: We shouldn't mention CTRL-F at all if the build script has turned mode 7 colour off
PRINTTAB(0,controls_vpos%);CHR$${NORMALFG};"  CTRL-F: ";
IF mode%=7 THEN PRINT "change status line colour" ELSE PRINT "change foreground colour "'CHR$${NORMALFG};"  CTRL-B: change background colour"
IF shadow% OR tube% THEN PRINT CHR$${NORMALFG};"  CTRL-S: change scrolling mode   "
IF mode%=7 THEN PRINT STRING$(40, " ");
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
VDU 28,0,${LASTLOADERLINE},39,${FIRSTLOADERLINE}+3,12
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
