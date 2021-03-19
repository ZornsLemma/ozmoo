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

REM SFTODO: Note that for Z3 games, anything shown on the top line of
REM the screen will remain present occupying the not-yet-displayed
REM status line until the game starts. This means that if any disc
REM errors occur during the initial loading, the screen may scroll
REM but the top line won't. This isn't a big deal but for the nicest
REM possible appearance in this admittedly unlikely situation either
REM clear the top line before running the Ozmoo executable or make
REM sure it has something that looks OK on its own. (For example,
REM *not* the top half of some double-height text.)

*FX229,1
*FX4,1

integra_b=FALSE
ON ERROR GOTO 100
integra_b=FNusr_osbyte_x(&49,&FF,0)=&49
100

ON ERROR PROCerror

REM We need to ensure the !BOOT file is cleanly closed. (SFTODO: Why, exactly?
REM But I think we do.) We can't do CLOSE #0 in the last line of !BOOT because
REM it doesn't work (sometimes; it's maddeningly variable) on a Master. We don't
REM want to assume BASIC 2, so we can't do OSCLI "EXEC":CHAIN "BLAH" in !BOOT,
REM so we do this instead. I don't know if the CLOSE #0 line is needed but it
REM certainly doesn't hurt. See the discussion at
REM https://stardot.org.uk/forums/viewtopic.php?f=54&t=19324 for some more
REM details.
*EXEC
CLOSE #0

A%=&85:X%=135:potential_himem=(USR&FFF4 AND &FFFF00) DIV &100
!ifndef SPLASH {
    REM With third-party shadow RAM on an Electron or BBC B, we *may* experience
    REM corruption of memory from &3000 upwards when changing between shadow and
    REM non-shadow modes. Normally !BOOT selects a shadow mode to avoid this
    REM problem, but as a fallback (e.g. if we've been copied to a hard drive and
    REM our !BOOT isn't in use any more) we re-execute ourself after switching to
    REM shadow mode if we're not already in a shadow mode. This line of the program
    REM will almost certainly be below &3000 so won't be corrupted by the switch.
    IF potential_himem=&8000 AND HIMEM<&8000 THEN MODE 135:CHAIN "LOADER"
} else {
    REM If we have a splash screen, the preloader has taken care of these issues.
    REM We might be very tight for memory though; if we are, change to mode 135
    REM early.
    IF HIMEM-TOP<512 THEN MODE 135:VDU 23,1,0;0;0;0;
}

REM In a few places in the loader (not the Ozmoo binary) we assume printing at
REM the bottom right of the screen will cause a scroll. Override any "No Scroll"
REM configuration on a Master to make this assumption valid.
VDU 23,16,0,254,0;0;0;

fg_colour=${fg_colour}
bg_colour=${bg_colour}
!ifdef MODE_7_PROMPT {
?${prompt_colour}=3:REM SFTODONOW: Default should be settable at build time
}
screen_mode=${screen_mode}
DIM block% 256
A%=0:X%=1:host_os=(USR&FFF4 AND &FF00) DIV &100
IF integra_b THEN host_os=1:REM override Integra-B OS version faking
electron=host_os=0

REM Do the hardware detection (which is slightly slow, especially the sideways RAM
REM detection as that requires running a separate executable) before we change
REM screen mode; this way if there's a splash screen it's visible during this delay.
REM
REM FINDSWR will play around with the user VIA as it probes for Solidisk-style
REM sideways RAM. It does its best to reset things afterwards, but it's not enough
REM for (at least) the TurboMMC filing system. We therefore deliberately generate and
REM ignore an error afterwards, which has the side effect of forcing a reset (and is
REM harmless if this wasn't necessary). See
REM https://stardot.org.uk/forums/viewtopic.php?p=311977#p311977 for more on this.
*/FINDSWR
ON ERROR GOTO 500
*INFO XYZZY1
500ON ERROR PROCerror
shadow=potential_himem=&8000
shadow_extra$=""
tube=PAGE<&E00
REM SFTODO: We should get rid of this line and PROCdetect_turbo itself if turbo is
REM not supported via build options.
IF tube THEN PROCdetect_turbo
private_ram_in_use=FALSE:REM SFTODONOW: We really should be detecting this independently of assemble_shadow_driver in a tube-safe way but this will do for now
!ifdef ACORN_SHADOW_VMEM {
IF shadow AND NOT tube THEN PROCassemble_shadow_driver
}
PROCdetect_swr

MODE 135:VDU 23,1,0;0;0;0;
?fg_colour=${DEFAULT_FG_COLOUR}:?bg_colour=${DEFAULT_BG_COLOUR}
IF electron THEN VDU 19,0,?bg_colour,0;0,19,7,?fg_colour,0;0
IF electron THEN PROCelectron_header_footer ELSE PROCbbc_header_footer

normal_fg=${NORMAL_FG}:normal_graphics_fg=normal_fg+16:header_fg=${HEADER_FG}:highlight_fg=${HIGHLIGHT_FG}:highlight_bg=${HIGHLIGHT_BG}:electron_space=0
IF electron THEN normal_fg=0:normal_graphics_fg=32:header_fg=0:electron_space=32

REM We always report sideways RAM, even if it's irrelevant (e.g. we're on a
REM second processor and the game fits entirely in RAM or the host cache isn't
REM enabled), as it seems potentially confusing if we sometimes apparently
REM fail to detect sideways RAM.
PRINT CHR$header_fg;"Hardware detected:"
vpos=VPOS
IF tube THEN PRINT CHR$normal_fg;"  Second processor (";tube_ram$;")"
IF shadow THEN PRINT CHR$normal_fg;"  Shadow RAM ";shadow_extra$
IF swr$<>"" THEN PRINT CHR$normal_fg;"  ";swr$
IF vpos=VPOS THEN PRINT CHR$normal_fg;"  None"
PRINT
die_top_y=VPOS

PROCchoose_version_and_check_ram

!ifdef SPLASH {
    REM Flush the keyboard buffer to reduce confusion if the user held down SPACE
    REM or RETURN for a while in order to dismiss the loader screen.
    *FX21
}

!ifdef AUTO_START {
    IF tube OR shadow THEN ?screen_mode=${default_mode} ELSE ?screen_mode=7+electron
    mode_keys_vpos=VPOS:PROCshow_mode_keys
} else {
    IF tube OR shadow THEN PROCmode_menu ELSE ?screen_mode=7+electron:mode_keys_vpos=VPOS:PROCshow_mode_keys:PROCspace:REPEAT UNTIL FNhandle_common_key(GET)
}

IF ?screen_mode=7 THEN ?fg_colour=${DEFAULT_M7_STATUS_COLOUR}
PRINTTAB(0,space_y);CHR$normal_fg;"Loading:";:pos=POS:PRINT "                               ";
REM Leave the cursor positioned ready for the loading progress indicator.
PRINTTAB(pos,space_y);CHR$normal_graphics_fg;
VDU 23,255,-1;-1;-1;-1;:REM block UDG for progress indicator in modes 0-6
!ifdef CACHE2P_BINARY {
    IF tube THEN */${CACHE2P_BINARY}
}
IF NOT tube THEN ?${ozmoo_relocate_target}=FNcode_start DIV 256
fs=FNfs
IF fs<>4 THEN path$=FNpath
REM Select user's home directory on NFS
IF fs=5 THEN *DIR
REM On non-DFS, select a SAVES directory if it exists but don't worry if it doesn't.
ON ERROR GOTO 1000
IF fs=4 THEN PROCoscli("DIR S") ELSE *DIR SAVES
1000ON ERROR PROCerror
REM On DFS this is actually a * command, not a filename, hence the leading "/" (="*RUN").
IF fs=4 THEN filename$="/"+binary$ ELSE filename$=path$+".DATA"
IF LENfilename$>=${filename_size} THEN PROCdie("Game data path too long")
REM We do this last, as it uses a lot of resident integer variable space and this reduces
REM the chances of it accidentally getting corrupted.
filename_data=${game_data_filename_or_restart_command}
$filename_data=filename$
*FX4,0
REM SFTODO: Should test with BASIC I at some point, probably work fine but galling to do things like PROCoscli and still not work on BASIC I!
IF fs=4 THEN PROCoscli($filename_data) ELSE PROCoscli("/"+path$+"."+binary$)
END

DEF PROCerror:CLS:REPORT:PRINT" at line ";ERL:PROCfinalise

DEF PROCdie(message$)
VDU 28,0,space_y,39,die_top_y,12
PROCpretty_print(normal_fg,message$)
PRINT
REM Fall through to PROCfinalise
DEF PROCfinalise
*FX229,0
*FX4,0
END

DEF PROCelectron_header_footer
VDU 23,128,0;0,255,255,0,0;
PRINTTAB(0,23);STRING$(40,CHR$128);"${OZMOO}";
IF POS=0 THEN VDU 30,11 ELSE VDU 30
PRINT "${TITLE}";:IF POS>0 THEN PRINT
PRINTSTRING$(40,CHR$128);
!ifdef SUBTITLE {
    PRINT "${SUBTITLE}";:IF POS>0 THEN PRINT
}
PRINT:space_y=22
ENDPROC

DEF PROCbbc_header_footer
PRINTTAB(0,${FOOTER_Y});:${FOOTER}
IF POS=0 THEN VDU 30,11 ELSE VDU 30
${HEADER}
PRINTTAB(0,${MIDDLE_START_Y});:space_y=${SPACE_Y}
ENDPROC

DEF PROCchoose_version_and_check_ram
REM The tube build works on both the BBC and Electron, so we check that first.
!ifdef OZMOO2P_BINARY {
    IF tube THEN binary$="${OZMOO2P_BINARY}":ENDPROC
} else {
    IF tube THEN PROCunsupported_machine("a second processor")
}
PROCchoose_non_tube_version

REM SFTODO: Review all the following fresh; I think it's right but I got seriously
REM agitated trying to write it.

REM For builds which can use sideways RAM, we need to check if we have enough
REM main RAM and/or sideways RAM to run successfully.
REM The use of 'p' in the next line is to work around a beebasm bug.
REM (https://github.com/stardot/beebasm/issues/45)
IF PAGE>max_page THEN PROCdie("Sorry, you need PAGE<=&"+STR$~max_page+"; it is &"+STR$~PAGE+".")
extra_main_ram=max_page-PAGE

REM Small dynamic memory model builds must have enough main RAM free for dynamic
REM memory, but the build system takes care of this by knowing the worst-case
REM start of screen RAM and choosing max_page accordingly. SFTODO: This won't be
REM true once we allow runtime choice of screen mode on non-shadow systems; the
REM loader will have to be involved in the decision.

REM At this point we have three different kinds of memory available:
REM - extra_main_ram bytes free in main RAM
REM - flexible_swr bytes of normal, contiguous sideways RAM
REM - vmem_only_swr bytes of non-contiguous sideways RAM
IF integra_b THEN vmem_only_swr=${integra_b_private_ram_size} ELSE vmem_only_swr=0
flexible_swr=swr_size-vmem_only_swr

IF medium_dynmem THEN PROCcheck_ram_medium_dynmem:ENDPROC

REM Dynamic memory can come from a combination of main RAM and flexible_swr. For this
REM calculation we prefer to take it from flexible_swr so we can use the result to
REM determine the available main RAM for shadow vmem cache if that's enabled.
flexible_swr=flexible_swr-swr_dynmem_needed
IF flexible_swr<0 THEN extra_main_ram=extra_main_ram+flexible_swr:flexible_swr=0
PROCsubtract_ram(${MIN_VMEM_BYTES})
IF extra_main_ram<0 THEN PROCdie_ram(-extra_main_ram,"main or sideways RAM")
!ifdef ACORN_SHADOW_VMEM {
    REM SFTODO: I think this is right, but think about it fresh!
    free_main_ram=extra_main_ram
}
ENDPROC

DEF PROCcheck_ram_medium_dynmem
REM For the medium dynamic memory model, we *must* have enough flexible_swr for the
REM game's dynamic memory; nothing else can substitute.
flexible_swr=flexible_swr-swr_dynmem_needed
PROCsubtract_ram(${MIN_VMEM_BYTES})
REM SFTODO: The errors we generate here are true but because they're separate it's
REM possible a user would fix one, try again and get another. extra_main_ram<0 is
REM very unlikely so this is probably OK.
IF flexible_swr<0 THEN PROCdie_ram(-flexible_swr,"sideways RAM")
IF extra_main_ram<0 THEN PROCdie_ram(-extra_main_ram,"main RAM")
!ifdef ACORN_SHADOW_VMEM {
    REM SFTODO: I think this is right, but think about it fresh!
    REM SFTODO: Can I just use extra_main_ram directly and get rid of free_main_ram?
    free_main_ram=extra_main_ram
}
ENDPROC

REM Subtract n bytes in total from vmem_only_swr, flexible_swr and extra_main_ram,
REM preferring to take from them in that order. Only extra_main_ram will be allowed
REM to go negative as a result of this subtraction. We prefer this order because
REM vmem_only_swr is the least valuable memory type and we want to maximise
REM extra_main_ram in case it can be used as shadow vmem cache.
DEF PROCsubtract_ram(n)
IF vmem_only_swr>0 THEN d=FNmin(n,vmem_only_swr):vmem_only_swr=vmem_only_swr-d:n=n-d
IF flexible_swr>0 THEN d=FNmin(n,flexible_swr):flexible_swr=flexible_swr-d:n=n-d
extra_main_ram=extra_main_ram-n
ENDPROC

DEF FNcode_start
REM 'p' is used to work around a beebasm tokenisation bug.
REM (https://github.com/stardot/beebasm/issues/45)
p=PAGE
!ifndef ACORN_SHADOW_VMEM {
    =p
} else {
    REM If we have spare shadow RAM after the screen uses what it needs, we can
    REM use it as virtual memory cache. We need some cache pages in main RAM
    REM to do this, which we create by relocating to an address higher than PAGE;
    REM the executable then notices this space and uses it.
    REM SFTODO: This logic may not be ideal, see how things work out.
    IF NOT shadow THEN =p
    IF NOT shadow_driver THEN =p
    REM In mode 0, all shadow RAM is used for the screen.
    IF ?screen_mode=0 THEN =p
    REM SFTODO: If we're in a large-ish screen mode, should we reduce the number
    REM of pages of shadow cache? 1K of cache for 4K of shadow RAM in mode 3, for
    REM example, feels a little excessive when we'd use the same 1K to back 19K
    REM of shadow RAM in mode 7.
    shadow_cache=FNmin(${RECOMMENDED_SHADOW_CACHE_PAGES}*256,free_main_ram)
    REM The shadow cache must not overlap with shadow RAM.
    REM SFTODO: Strictly speaking this could be allowed on machines where we use
    REM an API call to transfer data instead of paging shadow RAM into the
    REM memory map, but it's not a very likely case anyway.
    IF p+shadow_cache>=&3000 THEN shadow_cache=&3000-p
    REM The shadow cache must have at least two pages, since one might be locked
    REM while it contains the Z-machine PC and we need another one available
    REM when that happens.
    IF shadow_cache<512 THEN shadow_cache=0
    =p+shadow_cache
}

!ifdef ACORN_SHADOW_VMEM {
    DEF FNmin(a,b)
    IF a<b THEN =a ELSE =b

    REM SFTODO: I could probably make use of this function in quite a few other places.
    DEF FNusr_osbyte_x(A%,X%,Y%)=(USR&FFF4 AND &FF00) DIV &100
}

DEF PROCchoose_non_tube_version
!ifdef ONLY_80_COLUMN {
    IF NOT shadow THEN PROCunsupported_machine("a machine without shadow RAM or a second processor")
}
!ifdef OZMOOE_BINARY {
    IF electron THEN binary$="${OZMOOE_BINARY}":max_page=${OZMOOE_MAX_PAGE}:swr_dynmem_needed=${OZMOOE_SWR_DYNMEM}:medium_dynmem=${OZMOOE_SWR_MEDIUM_DYNMEM}:ENDPROC
} else {
    IF electron THEN PROCunsupported_machine("an Electron")
}
REM SFTODO: Should I make the loader support some sort of line-continuation character, then I could split up some of these very long lines?
!ifdef OZMOOSH_BINARY {
    IF shadow THEN binary$="${OZMOOSH_BINARY}":max_page=${OZMOOSH_MAX_PAGE}:swr_dynmem_needed=${OZMOOSH_SWR_DYNMEM}:medium_dynmem=${OZMOOSH_SWR_MEDIUM_DYNMEM}:ENDPROC
} else {
    REM If - although I don't believe this is currently possible - we don't have
    REM OZMOOSH_BINARY but we do have OZMOOB_BINARY, we can run OZMOOB_BINARY on any
    REM BBC.
    !ifndef OZMOOB_BINARY {
        IF host_os<>1 THEN PROCunsupported_machine("a BBC B+/Master")
    }
}
!ifdef OZMOOB_BINARY {
    binary$="${OZMOOB_BINARY}":max_page=${OZMOOB_MAX_PAGE}:swr_dynmem_needed=${OZMOOB_SWR_DYNMEM}:medium_dynmem=${OZMOOB_SWR_MEDIUM_DYNMEM}
} else {
    !ifdef OZMOOSH_BINARY {
        PROCunsupported_machine("a BBC B without shadow RAM")
    } else {
        PROCunsupported_machine("a BBC B")
    }
}
ENDPROC

!ifndef AUTO_START {
    DEF PROCmode_menu
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
    x=mode_x(VALmode$):y=mode_y(VALmode$):PROChighlight(x,y,TRUE):PROCspace
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
    UNTIL FNhandle_common_key(key)
    ENDPROC

    DEF FNhandle_common_key(key)
    IF electron AND key=2 THEN ?bg_colour=(?bg_colour+1) MOD 8:VDU 19,0,?bg_colour,0;0
    IF electron AND key=6 THEN ?fg_colour=(?fg_colour+1) MOD 8:VDU 19,7,?fg_colour,0;0
    =key=32 OR key=13

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
}

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

DEF PROCdetect_turbo
REM SFTODO: Can/should we detect this via modified A register with OSBYTE &84 instead? (We probably still need to enable turbo mode first.)
REM SFTODO: Once the Ozmoo executable has started poking non-0 into page 3, will BASIC crash on a soft-break? I suspect the re-enter language will disable turbo mode, but test this.
!&70=block%:?block%=0
P%=block%+1
[OPT 0
\ Set all banks to 0 to start with before turning turbo mode on.
\ (The Ozmoo executable relies on this, and if we didn't do it BASIC might crash
\ when its (zp),y accesses start accessing random banks once we've turned on
\ turbo mode.)
LDY #0:TYA
.loop
STA &300,Y
DEY:BNE loop
\ Try to turn turbo mode on.
LDA #&80:STA &FEF0
\ See if we do actually have multiple 64K banks available.
LDA #1:STA &371:STA (&70),Y
STY &371:LDA (&70),Y
\ Note that &371 is left as 0, so all banks are still 0.
RTS
]
turbo=(USR(block%+1) AND &FF)=0
IF turbo THEN tube_ram$="256K" ELSE tube_ram$="64K"
!ifdef ACORN_TURBO_SUPPORTED {
    ?${is_turbo}=turbo:REM we only care about bit 7 being set
}
ENDPROC

!ifdef ACORN_SHADOW_VMEM {
DEF PROCassemble_shadow_driver
REM SFTODO: This might be handled differently eventually, perhaps a binary like
REM FINDSWR runs at &900, checks the hardware and installs the right driver,
REM but this will do for now. (Doing it via a separate binary would allow us to
REM install this code in the host even if we're on a second processor, so CACHE2P
REM can use it to access shadow RAM.)
REM SFTODO: All this code will bloat the LOADER program, which makes it more
REM likely PRELOAD (if present) will have to change to mode 7 before doing
REM CHAIN "LOADER", which is mildly ugly. For now this is OK, but this is another
REM reason to consider an alternate way of installing the correct shadow RAM
REM driver.
shadow_driver=TRUE
IF integra_b THEN PROCassemble_shadow_driver_integra_b:ENDPROC
REM SFTODO: I don't think it would be hard to support Watford/Aries shadow RAM
REM on a BBC B, but unless/until I have an emulator which supports this or a
REM user willing to test on real hardware I'm not going to write code and hope
REM it works. As it stands Ozmoo will probably run in screen-only shadow RAM
REM mode on a Watford/Aries machine.
IF electron AND FNusr_osbyte_x(&EF,0,&FF)=&80 THEN PROCassemble_shadow_driver_electron_mrb:ENDPROC
IF host_os=2 THEN PROCassemble_shadow_driver_bbc_b_plus:ENDPROC
IF host_os>=3 THEN PROCassemble_shadow_driver_master:ENDPROC
shadow_driver=FALSE:shadow_extra$="(screen only)"
ENDPROC
REM SFTODO: Should we set shadow_extra$ to "(screen only)" or some other distinctive string if the game fits in memory but we have too little main RAM free to have a shadow cache? It might be prudent to offer some clue this is happening, because (e.g.) having ADFS+DFS vs DFS might be enough to tip us from one state to the other and performance would drop dramatically because we've suddenly lost access to the spare shadow RAM

DEF PROCassemble_shadow_driver_electron_mrb
FOR opt%=0 TO 2 STEP 2
P%=${shadow_ram_copy}
[OPT opt%
CMP #&30:BCS copy_from_shadow
\ We're copying to shadow RAM.
STA lda_abs_x+2
LDX #0
.copy_to_shadow_loop
.lda_abs_x
LDA &FF00,X \ patched
BIT our_rts:JSR &FBFD \ write to shadow RAM
INX
BNE copy_to_shadow_loop
.our_rts
RTS
.copy_from_shadow
\ We're copying from shadow RAM.
STY sta_abs_x+2:TAY
LDX #0
.copy_from_shadow_loop
CLV:JSR &FBFD \ read from shadow RAM
.sta_abs_x
STA &FF00,X \ patched
INX
BNE copy_from_shadow_loop
RTS
]
NEXT
ENDPROC

DEF PROCassemble_shadow_driver_integra_b
REM SFTODO: We could probably fiddle directly with the Integra-B hardware
REM registers to optimise this a little.
FOR opt%=0 TO 2 STEP 2
P%=${shadow_ram_copy}
[OPT opt%
STA lda_abs_y+2:STY sta_abs_y+2
\ SFTODONOW: DO THE PAGING DIRECTLY RATHER THAN GOING VIA OSBYTE - WE'RE ALREADY POKING THE HW DIRECTLY SO MIGHT AS WELL BE AS FAST AS POSS!
LDA #&6C:LDX #1:JSR &FFF4 \ page in shadow RAM
LDY #0
.copy_loop
.lda_abs_y
LDA &FF00,Y \ patched
.sta_abs_y
STA &FF00,Y \ patched
DEY
BNE copy_loop
LDA #&6C:LDX #0:JSR &FFF4 \ page out shadow RAM
RTS
]
NEXT
ENDPROC

DEF PROCassemble_shadow_driver_bbc_b_plus
REM Determine if the private 12K is free on a B+ by checking for any extended
REM vectors pointing into it.
REM SFTODO: We should probably do something similar (remember it's >=64 not 128 - we could use same code for both really) on Integra-B (although we may always find there is such a vector pointing into part of the private RAM we don't touch) - think about it anyway - OK, FWIW under normal circumstances no extended vectors point into bank 64+ on Integra-B, even after I enable printer buffer with "*BUFFER 0" - I suspect this is handled via the stub in page 8 rather than an extended vector - but in principle some other code *might* set an extended vector up to point into that RAM
REM SFTODO: This may or may not be acceptable in practice, but I'd really rather
REM not have to ask the user about using the private 12K. If SWMMFS+ is in use
REM but is *not* the current filing system, this won't detect it and there might
REM be "Sum?" errors or worse on BREAK. CTRL-BREAK should fix this. Have a play
REM around with this on an emulator at some point.
private_ram_in_use=FALSE
extended_vector_table=&D9F
FOR vector=0 TO 26
IF extended_vector_table?(vector*3+2)>=128 THEN private_ram_in_use=TRUE
NEXT
IF private_ram_in_use THEN PROCassemble_shadow_driver_bbc_b_plus_os:ENDPROC
REM The private 12K is free, so we can use this much faster implementation which
REM takes advantage of the ability of code running at &Axxx in the 12K private
REM RAM to access shadow RAM directly.
shadow_copy_private_ram=&AF00
FOR opt%=0 TO 2 STEP 2
P%=${shadow_ram_copy}
[OPT opt%
LDX &F4:STX lda_imm_bank+1
LDX #128:STX &F4:STX &FE30
JMP shadow_copy_private_ram
.stub_finish
.lda_imm_bank
LDA #0 \ patched
STA &F4:STA &FE30
RTS
]
O%=block%:P%=shadow_copy_private_ram
shadow_copy_low_ram=O%
[OPT opt%+4
STA lda_abs_y+2:STY sta_abs_y+2
LDY #0
.copy_loop
.lda_abs_y
LDA &FF00,Y \ patched
.sta_abs_y
STA &FF00,Y \ patched
DEY
BNE copy_loop
JMP stub_finish
]
shadow_copy_low_ram_end=O%
P%=O%
[OPT opt%
.copy_to_private_ram
LDA &F4:STA &70
LDA #128:STA &F4:STA &FE30
LDY #shadow_copy_low_ram_end-shadow_copy_low_ram-1
.copy_to_private_ram_loop
LDA shadow_copy_low_ram,Y:STA shadow_copy_private_ram,Y
DEY:CPY #&FF:BNE copy_to_private_ram_loop
LDA &70:STA &F4:STA &FE30
RTS
]
NEXT
CALL copy_to_private_ram
ENDPROC

DEF PROCassemble_shadow_driver_bbc_b_plus_os
REM SFTODO: Not sure I like this string, but I think it's better to leave the
REM default case not saying anything (as opposed to "fast") and therefore I
REM don't really like to call this case "slow". It's also proably bad
REM marketing. :-) So I'm aiming for a more factual description here.
shadow_extra$="(via OS)"
FOR opt%=0 TO 2 STEP 2
P%=${shadow_ram_copy}
[OPT opt%
CMP #&30:BCS copy_from_shadow
\ We're copying to shadow RAM.
STA lda_abs_y+2:STY &D7
LDY #0:STY &D6
.copy_to_shadow_loop
.lda_abs_y
LDA &FF00,Y \ patched
JSR &FFB3 \ OSWRSC, preserves Y - equivalent to STA (&D6),Y - note Y is used!
INY
BNE copy_to_shadow_loop
RTS
.copy_from_shadow
\ We're copying from shadow RAM.
STA &F7:STY sta_abs+2
LDY #0:STY &F6
.copy_from_shadow_loop
JSR &FFB9 \ OSRDSC, ignores and corrupts Y
.sta_abs
STA &FF00 \ patched
INC &F6
INC sta_abs+1
BNE copy_from_shadow_loop
RTS
]
NEXT
ENDPROC

DEF PROCassemble_shadow_driver_master
FOR opt%=0 TO 2 STEP 2
P%=${shadow_ram_copy}
[OPT opt%
STA lda_abs_y+2:STY sta_abs_y+2
LDA #4:TSB &FE34 \ page in shadow RAM
LDY #0
.copy_loop
.lda_abs_y
LDA &FF00,Y \ patched
.sta_abs_y
STA &FF00,Y \ patched
DEY
BNE copy_loop
LDA #4:TRB &FE34 \ page out shadow RAM
RTS
]
NEXT
ENDPROC
}

DEF PROCdetect_swr
REM We use FNpeek here because FINDSWR runs on the host and we may be running on
REM a second processor.
swr_banks=FNpeek(${ram_bank_count}):swr$=""
REM SFTODO: For now we only detect and include private RAM if we're not running on
REM a second processor. We'd need to use a PROCpoke() instead of ? to write safely
REM to host memory, but that's not a big deal. More to the point is that CACHE2P
REM doesn't know how to skip the IBOS workspace on an Integra-B, so we'll just
REM avoid this altogether for the moment.
swr_adjust=0
IF NOT tube THEN PROCdetect_private_ram
IF FNpeek(${swr_type})>2 THEN swr$="("+STR$(swr_banks*16)+"K unsupported sideways RAM)"
swr_size=&4000*FNpeek(${ram_bank_count})-swr_adjust
IF swr_banks=0 THEN ENDPROC
REM SFTODONOW: Maybe a bit confusing that we call it "private RAM" here but sideways RAM if we have real sideways RAM to go with it - also as per TODO above we may not actually have the full 12K, and while it's maybe confusing to say "11.5K private RAM" we also don't want the user adding up their memory and finding it doesn't come out right - arguably we *can* say 12K private RAM (at least on B+, not sure about Integra-B) because we *do* have it all, it's just we set aside the last 512 bytes for other uses, but still for Ozmoo
IF swr_size<=12*1024 THEN swr$="12K private RAM":ENDPROC
REM SFTODO: With the possibility of having ".5" in the number, we can probably wrap around at the right hand edge of the screen if we have 7 or 8 sideways RAM banks. ("Wrapping" includes printing in the rightmost column and having an extra line feed.)
swr$=STR$(swr_size/1024)+"K sideways RAM (bank":IF swr_banks>1 THEN swr$=swr$+"s"
swr$=swr$+" &":FOR i=0 TO swr_banks-1:bank=FNpeek(${ram_bank_list}+i)
IF bank>=64 THEN bank$="+" ELSE bank$=STR$~bank
swr$=swr$+bank$:NEXT:swr$=swr$+")"
ENDPROC

DEF PROCdetect_private_ram
IF swr_banks<${max_ram_bank_count} AND integra_b THEN swr_banks?${ram_bank_list}=64:swr_banks=swr_banks+1:?${ram_bank_count}=swr_banks:swr_adjust=16*1024-${integra_b_private_ram_size}
IF swr_banks<${max_ram_bank_count} AND host_os=2 THEN IF NOT private_ram_in_use THEN swr_banks?${ram_bank_list}=128:swr_banks=swr_banks+1:?${ram_bank_count}=swr_banks:swr_adjust=16*1024-${b_plus_private_ram_size}
ENDPROC

DEF PROCunsupported_machine(machine$):PROCdie("Sorry, this game won't run on "+machine$+".")
DEF PROCdie_ram(amount,ram_type$):PROCdie("Sorry, you need at least "+STR$(amount/1024)+"K more "+ram_type$+".")

DEF PROCshow_mode_keys
mode_keys_last_max_y=mode_keys_last_max_y:REM set variable to 0 if it doesn't exist
IF mode_keys_last_max_y=0 THEN PRINTTAB(0,mode_keys_vpos);CHR$header_fg;"In-game controls:" ELSE PRINTTAB(0,mode_keys_vpos+1);
REM The odd indentation on the next few lines is so a) it's easy to see all the
REM different possible output lines have the same length and will completely
REM obliterate each other b) the build script will strip off the extra
REM indentation as it's at the start of the line.
                                PRINT CHR$normal_fg;"  SHIFT:  show next page of text"
!ifdef MODE_7_STATUS {
         IF ?screen_mode=7 THEN PRINT CHR$normal_fg;"  CTRL-F: change status line colour"
}
!ifdef MODE_7_PROMPT {
REM SFTODO: "P" and "prompt" are maybe less than ideal here; the *prompt* isn't coloured,
REM it's the user's input which is coloured. Not sure if this is a problem, but maybe
REM CTRL-I would be better, and change the wording, and the name of the make-acorn.py
REM option, and MODE_7_PROMPT itself...
         IF ?screen_mode=7 THEN PRINT CHR$normal_fg;"  CTRL-P: change prompt colour     "
}
        IF ?screen_mode<>7 THEN PRINT CHR$normal_fg;"  CTRL-F: change foreground colour "
        IF ?screen_mode<>7 THEN PRINT CHR$normal_fg;"  CTRL-B: change background colour "
REM SFTODO: Should the next one be conditional on ACORN_HW_SCROLL?
        IF ?screen_mode<>7 THEN PRINT CHR$normal_fg;"  CTRL-S: change scrolling mode    "
REM Clear any additional rows which we used last time but haven't used this time.
IF VPOS<mode_keys_last_max_y THEN PRINT SPC(40*(mode_keys_last_max_y-VPOS));
mode_keys_last_max_y=VPOS
ENDPROC

DEF PROCspace
PRINTTAB(0,space_y);CHR$normal_fg;"Press SPACE/RETURN to start the game...";
ENDPROC

DEF FNis_mode_7(x)=LEFT$(menu$(x,0),1)="7"

DEF PROCoscli($block%):X%=block%:Y%=X%DIV256:CALL&FFF7:ENDPROC

DEF FNpeek(addr):!block%=&FFFF0000 OR addr:A%=5:X%=block%:Y%=block% DIV 256:CALL &FFF1:=block%?4

DEF FNfs:A%=0:Y%=0:=USR&FFDA AND &FF

REM SFTODO: FNpath AND FNstrip CAN BE OMITTED IF THIS IS A DFS BUILD (THOUGH ULTIMATELY I REALLY MEAN "OSWORD 7F", AS IT MAY BE I WANT TO BUILD DFS-WITH-OSGBPB FOR INSTALL ON NFS INSTEAD OF HAVING TO VIA ADFS)
DEF FNpath
DIM data% 256
path$=""
REPEAT
block%!1=data%
A%=6:X%=block%:Y%=block% DIV 256:CALL &FFD1
name=data%+1+?data%
name?(1+?name)=13
name$=FNstrip($(name+1))
path$=name$+"."+path$
REM On Econet, you can't do *DIR ^ when in the root.
REM SFTODO: You can't always do *DIR ^ on Econet; can/should I try to work around this?
IF name$<>"$" AND name$<>"&" THEN *DIR ^
UNTIL name$="$" OR name$="&"
path$=LEFT$(path$,LEN(path$)-1)
?name=13
drive$=FNstrip($(data%+1))
IF drive$<>"" THEN path$=":"+drive$+"."+path$
PROCoscli("DIR "+path$)
=path$

DEF FNstrip(s$)
s$=s$+" "
REPEAT:s$=LEFT$(s$,LEN(s$)-1):UNTIL RIGHT$(s$,1)<>" "
=s$

DEF FNmax(a,b):IF a<b THEN =b ELSE =a
