REM SFTODO: Check SFTODOs etc in original loader as some of them may still be valid and need transferring across here

REM The loader uses some of the resident integer variables (or at least the
REM corresponding memory) to communicate with the Ozmoo executable. This
REM means it's probably least error prone to avoid using resident integer
REM variables gratuitously in this code.

REM As this code is not performance-critical, I have used real variables instead
REM of integer variables most of the time to shorten things slightly by avoiding
REM constant use of "%".

REM SFTODO: It would be nice if the loader and build system could work
REM together to allow the user to *optionally* specify a high-res title
REM screen before we go into the mode 7 loader.

*FX229,1
*FX4,1
ON ERROR PROCerror

REM On an Integra-B, we may have problems selecting shadow mode from this
REM large program which may be using memory above &3000 if we're currently
REM in a non-shadow mode. Normally !BOOT selects a shadow mode to avoid
REM this problem, but we do this as a fallback (e.g. if we've been copied
REM to a hard drive and our !BOOT isn't in use any more).
A%=&85:X%=135:potential_himem=(USR&FFF4 AND &FFFF00) DIV &100
IF potential_himem=&8000 AND HIMEM<&8000 THEN MODE 135:CHAIN "LOADER"

fg_colour=${fg_colour}
bg_colour=${bg_colour}
screen_mode=${screen_mode}

A%=0:X%=1:host_os=(USR&FFF4 AND &FF00) DIV &100:electron=host_os=0
MODE 135:VDU 23,1,0;0;0;0;
?fg_colour=7:?bg_colour=4
IF electron THEN VDU 19,0,?bg_colour,0;0,19,7,?fg_colour,0;0
DIM block% 256
REM SFTODO: SET UP HEADER AND FOOTER
VDU 28,0,22,39,8:REM SFTODO TEMPORARY, TO SIMULATE BANNER

normal_fg=134:REM SFTODO: SHOULD BE SET VIA A SUBSTITUTION - TEMP 134 NOT 135 TO MAKE IT OBVIOUS IF I FORGET IT!
header_fg=131:REM SFTODO: SHOULD BE SET VIA A SUBSTITUTION
highlight_fg=132:REM SFTODO: SHOULD BE SET VIA A SUBSTITUTION
highlight_bg=129:REM SFTODO: SHOULD BE SET VIA A SUBSTITUTION
electron_space=0
IF electron THEN normal_fg=0:header_fg=0:electron_space=32

shadow=potential_himem=&8000
tube=PAGE<&E00
PROCdetect_swr

PRINT CHR$header_fg;"Hardware detected:"
vpos=VPOS
IF tube THEN PRINT CHR$normal_fg;"  Second processor"
IF shadow THEN PRINT CHR$normal_fg;"  Shadow RAM"
IF swr$<>"" THEN PRINT CHR$normal_fg;"  ";swr$
IF vpos=VPOS THEN PRINT CHR$normal_fg;"  None"
PRINT

REM The tube build works on both the BBC and Electron, so we check that first.
!ifdef OZMOO2P_BINARY {
IF tube THEN binary$="${OZMOO2P_BINARY}":GOTO 2000
} else {
IF tube THEN PROCunsupported_machine("a second processor")
}
!ifdef ONLY_80_COLUMN {
IF NOT shadow THEN PROCunsupported_machine("a machine without shadow RAM or a second processor")
}
!ifdef OZMOOE_BINARY {
IF electron THEN binary$="${OZMOOE_BINARY}":max_page=${OZMOOE_MAX_PAGE}:relocatable=${OZMOOE_RELOCATABLE}:swr_dynmem_needed=${OZMOOE_SWR_DYNMEM}:GOTO 1000
} else {
IF electron THEN PROCunsupported_machine("an Electron")
}
REM SFTODO: Not just here - if I am able to run some games with no SWR, I should probably take SWR out of the "plain B" and "shadow+sideways RAM" build names (which would affect the new make-acorn.py script too, just as an internal naming thing)
!ifdef OZMOOSH_BINARY {
IF shadow THEN binary$="${OZMOOSH_BINARY}":max_page=${OZMOOSH_MAX_PAGE}:relocatable=${OZMOOSH_RELOCATABLE}:swr_dynmem_needed=${OZMOOSH_SWR_DYNMEM}:GOTO 1000
} else {
REM OZMOOB_BINARY only works on a model B because of the mode-7-at-&3C00 trick,
REM so if we don't have OZMOOSH_BINARY we must refuse to work on anything
REM else.
IF host_os<>1 THEN PROCunsupported_machine("a BBC B+/Master")
}
!ifdef OZMOOB_BINARY {
binary$="${OZMOOB_BINARY}":max_page=${OZMOOB_MAX_PAGE}:relocatable=${OZMOOB_RELOCATABLE}:swr_dynmem_needed=${OZMOOB_SWR_DYNMEM}:GOTO 1000
} else {
REM SFTODO: Next line is misleading, depending on the other build options we may mean "a BBC B without shadow RAM", but it will depend on options.
PROCunsupported_machine("a BBC B")
}

REM For builds which can use sideways RAM, we need to check if we have enough
REM main RAM and/or sideways RAM to run successfully.
REM SFTODO: WE SHOULD SHOW HARDWARE DETECTION EARLIER THAN THIS, SO USER CAN SEE WHAT SWR WE DETECTED BEFORE WE COMPLAIN WE DON'T HAVE ENOUGH, IN CASE OF DETECTION PROBLEMS
REM SFTODO: We shouldn't emit this block of code if we *only* support tube.
REM SFTODO THIS WON'T DO THE RIGHT THING ON ELECTRON, WHERE MAIN RAM CAN SUBSTITUTE FOR VMEM BUT NOT DYNMEM
REM The use of 'p' in the next line is to work around a beebasm bug.
REM (https://github.com/stardot/beebasm/issues/45)
1000IF PAGE>max_page THEN PROCdie("Sorry, you need PAGE<=&"+STR$~max_page+"; it is &"+STR$~PAGE+".")
IF relocatable THEN extra_main_ram=max_page-PAGE:p=PAGE:?${ozmoo_relocate_target}=p DIV 256 ELSE extra_main_ram=0
swr_dynmem_needed=swr_dynmem_needed-&4000*?${ram_bank_count}
REM On the BBC extra_main_ram will reduce the need for sideways RAM for dynamic
REM memory, but on the Electron it is used as swappable memory only.
vmem_needed=${MIN_VMEM_BYTES}-extra_main_ram
IF electron AND swr_dynmem_needed>0 THEN PROCdie_ram(swr_dynmem_needed+FNmax(vmem_needed,0),"sideways RAM")
mem_needed=swr_dynmem_needed+vmem_needed
IF mem_needed>0 THEN PROCdie_ram(mem_needed,"main or sideways RAM")
REM SFTODO: If we have >=MIN_VMEM_BYTES but not >=PREFERRED_MIN_VMEM_BYTES we should maybe show a warning

REM SFTODO: If we had nested !ifdef support, in the AUTO_START case we could just
REM avoid emitting all the code between the GOTO 3000 and line 3000.
!ifdef AUTO_START {
2000IF tube OR shadow THEN ?screen_mode=${default_mode} ELSE ?screen_mode=7+electron
mode_keys_vpos=VPOS:PROCshow_mode_keys:GOTO 3000
} else {
2000IF NOT (tube OR shadow) THEN ?screen_mode=7+electron:mode_keys_vpos=VPOS:PROCshow_mode_keys:REPEAT:key=GET:UNTIL key=32 OR key=13:GOTO 3000
}

DIM mode_x(8),mode_y(8)
REM It's tempting to derive mode_list$ from the contents of menu$, but it's more
REM trouble than it's worth, because it's shown (with inserted "/" characters)
REM on screen and for neatness we want it to be sorted into numerical order.
!ifdef ONLY_80_COLUMN {
max_x=1
max_y=0
DIM menu$(max_x,max_y),menu_x(max_x)
menu$(0,0)="0) 80x32"
menu$(1,0)="3) 80x25"
mode_list$="03"
}
!ifdef ONLY_40_COLUMN {
max_x=1
max_y=1
DIM menu$(max_x,max_y),menu_x(max_x)
IF electron THEN max_y=0:menu$(0,0)="4) 40x32":menu$(1,0)="6) 40x25":mode_list$="46" ELSE menu$(0,0)="4) 40x32":menu$(0,1)="6) 40x25":menu$(1,0)="7) 40x25   ":menu$(1,1)="   teletext":mode_list$="467"
}
!ifdef NO_ONLY_COLUMN {
max_x=2
max_y=1
DIM menu$(max_x,max_y),menu_x(max_x)
menu$(0,0)="0) 80x32"
menu$(0,1)="3) 80x25"
menu$(1,0)="4) 40x32"
menu$(1,1)="6) 40x25"
menu$(2,0)="7) 40x25   "
menu$(2,1)="   teletext"
IF electron THEN max_x=1:mode_list$="0346" ELSE mode_list$="03467"
}
REM The y loop here is done in reverse as VAL(" ") is 0 and we want to get the
REM second line of the mode 7 entry over with before it can corrupt the mode 0
REM entry, which will always be in the first line if it's present.
FOR y=max_y TO 0 STEP -1:FOR x=0 TO max_x:mode=VALLEFT$(menu$(x,y),1):mode_x(mode)=x:mode_y(mode)=y:NEXT:NEXT
PRINT CHR$header_fg;"Screen mode:";CHR$normal_fg;CHR$electron_space;"(hit ";:sep$="":FOR i=1 TO LEN(mode_list$):PRINT sep$;MID$(mode_list$,i,1);:sep$="/":NEXT:PRINT " to change)"
menu_top_y=VPOS
IF max_x=2 THEN gutter=0 ELSE gutter=5
FOR y=0 TO max_y:PRINTTAB(0,menu_top_y+y);CHR$normal_fg;:FOR x=0 TO max_x:menu_x(x)=POS:PRINT SPC2;menu$(x,y);SPC(2+gutter);:NEXT:NEXT
mode_keys_vpos=menu_top_y+max_y+2
mode$="${default_mode}":IF INSTR(mode_list$,mode$)=0 THEN mode$=RIGHT$(mode_list$,1)
x=mode_x(VALmode$):y=mode_y(VALmode$):PROChighlight(x,y,TRUE)
REPEAT
old_x=x:old_y=y
key=GET
IF key=136 AND x>0 THEN x=x-1
IF key=137 AND x<max_x THEN x=x+1
IF key=138 AND y<max_y THEN y=y+1
IF key=139 AND y>0 THEN y=y-1
REM We don't set y if mode 7 is selected by pressing "7" so subsequent movement
REM with cursor keys remembers the old y position.
key$=CHR$key:IF INSTR(mode_list$,key$)<>0 THEN x=mode_x(VALkey$):IF NOT FNis_mode_7(x) THEN y=mode_y(VALkey$)
IF x<>old_x OR (y<>old_y AND NOT FNis_mode_7(x)) THEN PROChighlight(old_x,old_y,FALSE):PROChighlight(x,y,TRUE)
UNTIL key=32 OR key=13

3000
IF ?screen_mode=7 THEN ?fg_colour=6
REM SFTODO: SHOW "LOADING, PLEASE WAIT"
!ifdef CACHE2P_BINARY {
IF tube THEN */${CACHE2P_BINARY}
}
fs=FNfs
IF fs<>4 THEN path$=FNpath
REM Select user's home directory on NFS
IF fs=5 THEN *DIR
REM On non-DFS, select a SAVES directory if it exists but don't worry if it doesn't.
ON ERROR GOTO 4000
IF fs=4 THEN PROCoscli("DIR S") ELSE *DIR SAVES
4000ON ERROR PROCerror
REM On DFS this is actually a * command, not a filename, hence the leading "/" (="*RUN").
IF fs=4 THEN filename$="/"+binary$ ELSE filename$=path$+".DATA"
IF LENfilename$>=${filename_size} THEN PROCdie("Game data path too long")
REM We do this last, as it uses a lot of resident integer variable space and this reduces
REM the chances of it accidentally getting corrupted.
filename_data=${game_data_filename_or_restart_command}
$filename_data=filename$
*FX4,0
REM SFTODO: Should test with BASIC I at some point, probably work fine but galling to do things like PROCoscli and still not work on BASIC I!
VDU 26:REM GET RID OF THIS IF I NEVER DO VDU 28
IF fs=4 THEN PROCoscli($filename_data) ELSE PROCoscli("/"+path$+"."+binary$)
END

DEF PROCerror:CLS:REPORT:PRINT" at line ";ERL:PROCfinalise

DEF PROCdie(message$)
REM SFTODO: This needs to print nicely on screen preserving any hardware detected output, word-wrapping and using the normal fg colour if we're in mode 7.
PROCpretty_print(normal_fg,message$)
PRINT
REM Fall through to PROCfinalise
DEF PROCfinalise
*FX229,0
*FX4,0
END

REM This is not a completely general pretty-print routine, e.g. it doesn't make
REM any attempt to handle words which are longer than the screen width. It's
REM good enough for our needs.
REM
REM colour should be 0 or a teletext colour control code.
REM
REM The current X text cursor position will be used as the left margin for the
REM output; if colour<>0 there will be an additional one character indent.
DEF PROCpretty_print(colour,message$)
prefix$=CHR$colour+STRING$(POS," ")
i=1
VDU colour
REPEAT
space=INSTR(message$," ",i+1)
IF space=0 THEN word$=MID$(message$,i) ELSE word$=MID$(message$,i,space-i)
new_pos=POS+LENword$
IF new_pos<40 THEN PRINT word$;" "; ELSE IF new_pos=40 THEN PRINT word$; ELSE PRINT'prefix$;word$;" ";
IF POS=0 AND space<>0 THEN PRINT prefix$;
i=space+1
UNTIL space=0
IF POS<>0 THEN PRINT
ENDPROC

DEF PROCdetect_swr
*/FINDSWR
REM We use FNpeek here because FINDSWR runs on the host and we may be running on
REM a second processor.
swr_banks=FNpeek(${ram_bank_count}):swr$=""
IF FNpeek(${swr_type})>2 THEN swr$="("+STR$(swr_banks*16)+"K unsupported sideways RAM)"
IF swr_banks=0 THEN ENDPROC
swr$=STR$(swr_banks*16)+"K sideways RAM (bank":IF swr_banks>1 THEN swr$=swr$+"s"
swr$=swr$+" &":FOR i%=0 TO swr_banks-1:swr$=swr$+STR$~FNpeek(${ram_bank_list}+i%):NEXT:swr$=swr$+")"
ENDPROC

DEF PROCunsupported_machine(machine$):PROCdie("Sorry, this game won't run on "+machine$+".")
DEF PROCdie_ram(amount,ram_type$):PROCdie("Sorry, you need at least "+STR$(amount/1024)+"K more "+ram_type$+".")

DEF PROChighlight(x,y,on)
IF on AND FNis_mode_7(x) THEN ?screen_mode=7 ELSE IF on THEN ?screen_mode=VAL(menu$(x,y))
IF on THEN PROCshow_mode_keys
IF electron THEN PROChighlight_internal_electron(x,y,on):ENDPROC
IF FNis_mode_7(x) THEN PROChighlight_internal(x,0,on):y=1
DEF PROChighlight_internal(x,y,on)
REM We put the "normal background" code in at the right hand side first before
REM (maybe) putting a "coloured backgroudn" code in at the left hand side to try
REM to reduce visual glitches.
IF x<2 THEN PRINTTAB(menu_x(x)+3+LENmenu$(x,y),menu_top_y+y);CHR$normal_fg;CHR$156;
PRINTTAB(menu_x(x)-1,menu_top_y+y);
IF on THEN PRINT CHR$highlight_bg;CHR$157;CHR$highlight_fg ELSE PRINT "  ";CHR$normal_fg
ENDPROC
DEF PROChighlight_internal_electron(x,y,on)
PRINTTAB(menu_x(x),menu_top_y+y);
IF on THEN COLOUR 135:COLOUR 0 ELSE COLOUR 128:COLOUR 7
PRINT SPC(2);menu$(x,y);SPC(2);
COLOUR 128:COLOUR 7
ENDPROC

DEF PROCshow_mode_keys
mode_7_no_hw_scroll=NOT (shadow OR tube OR electron)
!ifndef MODE_7_STATUS {
IF mode_7_no_hw_scroll THEN ENDPROC
}
mode_keys_last_max_y=mode_keys_last_max_y:REM set variable to 0 if it doesn't exist
IF mode_keys_last_max_y=0 THEN PRINTTAB(0,mode_keys_vpos);CHR$header_fg;"In-game controls:" ELSE PRINTTAB(0,mode_keys_vpos+1);
REM The odd indentation on the next few lines is so a) it's easy to see all the
REM different possible output lines have the same length and will completely
REM obliterate each other b) the build script will strip off the extra
REM indentation as it's at the start of the line.
!ifdef MODE_7_STATUS {
         IF ?screen_mode=7 THEN PRINT CHR$normal_fg;"  CTRL-F: change status line colour"
}
        IF ?screen_mode<>7 THEN PRINT CHR$normal_fg;"  CTRL-F: change foreground colour "
        IF ?screen_mode<>7 THEN PRINT CHR$normal_fg;"  CTRL-B: change background colour "
IF NOT mode_7_no_hw_scroll THEN PRINT CHR$normal_fg;"  CTRL-S: change scrolling mode    "
REM Clear any additional rows which we used last time but haven't used this time.
IF VPOS<mode_keys_last_max_y THEN PRINT SPC(40*(mode_keys_last_max_y-VPOS));
mode_keys_last_max_y=VPOS
ENDPROC

DEF FNis_mode_7(x)=LEFT$(menu$(x,0),1)="7"

DEF PROCoscli($block%):X%=block%:Y%=X%DIV256:CALL&FFF7:ENDPROC

DEF FNpeek(addr%):!block%=&FFFF0000 OR addr%:A%=5:X%=block%:Y%=block% DIV 256:CALL &FFF1:=block%?4

DEF FNfs:A%=0:Y%=0:=USR&FFDA AND &FF

DEF FNmax(a,b):IF a<b THEN =b ELSE =a
