REM SFTODO: Check SFTODOs etc in original loader as some of them may still be valid and need transferring across here

REM The loader uses some of the resident integer variables (or at least the
REM corresponding memory) to communicate with the Ozmoo executable. This
REM means it's probably least error prone to avoid using resident integer
REM variables gratuitously in this code.

REM As this code is not performance-critical, I have used real variables instead
REM of integer variables most of the time to shorten things slightly by avoiding
REM constant use of "%".

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

!ifndef LEAVE_CAPS_LOCK_ALONE {
    REM Some games - e.g. Freefall - don't like read_char returning upper case
    REM characters. They don't like it on frotz either, but on frotz Caps Lock
    REM will usually not be on by default. Force Caps Lock off on Acorn Ozmoo;
    REM if the user wants to turn it back on that is their choice (just as it
    REM is on frotz).
    *FX202,48
}

integra_b=FALSE
REM SFTODO: We don't actually need this ON ERROR, because OSBYTE *doesn't* throw for unrecognised OSBYTE.
ON ERROR GOTO 100
integra_b=FNusr_osbyte_x(&49,&FF,0)=&49
100ON ERROR PROCerror

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

!ifndef SPLASH {
    REM With third-party shadow RAM on an Electron or BBC B, we *may* experience
    REM corruption of memory from &3000 upwards when changing between shadow and
    REM non-shadow modes. Normally !BOOT selects a shadow mode to avoid this
    REM problem, but as a fallback (e.g. if we've been copied to a hard drive and
    REM our !BOOT isn't in use any more) we re-execute ourself after switching to
    REM shadow mode if we're not already in a shadow mode. This line of the program
    REM will almost certainly be below &3000 so won't be corrupted by the switch.
    REM (I am not sure, but I think this is also useful on tube; the host cache
    REM might have its memory corrupted by the change from non-shadow to shadow
    REM mode after the initial cache population.)
    IF FNhimem_for_mode(135)=&8000 AND HIMEM<&8000 THEN MODE 135:CHAIN "LOADER"
} else {
    REM If we have a splash screen, the preloader has taken care of these issues.
    REM We might be very tight for memory though; if we are, change to mode 135
    REM early.
    IF HIMEM-TOP<512 THEN MODE 135:VDU 23,1,0;0;0;0;
}

REM Allow calling PROCdie() before things have been properly set up.
die_top_y=0:space_y=24:normal_fg=0

REM In a few places in the loader (not the Ozmoo binary) we assume printing at
REM the bottom right of the screen will cause a scroll. Override any "No Scroll"
REM configuration on a Master to make this assumption valid.
VDU 23,16,0,254,0;0;0;

DIM block% 256
host_os=FNusr_osbyte_x(0,1,0)
REM If we're on an Integra-B in OSMODE 0, it's as if we're on a standard model B.
REM We need to avoid detecting the private RAM, because the non-shadow-RAM model B
REM executable we'll choose to run doesn't know how to handle it.
IF integra_b AND host_os=1 THEN integra_b=FALSE
REM If we're on an Integra-B in some other OSMODE, override its faking of the OS
REM version; we don't want to use our code to access the B+ or Master shadow RAM
REM hardware, for example.
IF integra_b THEN host_os=1
electron=host_os=0

REM Do the hardware detection (which is slightly slow, especially the sideways RAM
REM detection as that requires running a separate executable) before we change
REM screen mode; this way if there's a splash screen it's visible during this delay.
REM SFTODO: Since we might have to change to mode 7 to load the loader, actually
REM *RUNning these hardware detection executables from the preloader might be nicer.

*/SHADDRV

REM FINDSWR will play around with the user VIA as it probes for Solidisk-style
REM sideways RAM. It does its best to reset things afterwards, but it's not enough
REM for (at least) the TurboMMC filing system. We therefore deliberately generate and
REM ignore an error afterwards, which has the side effect of forcing a reset (and is
REM harmless if this wasn't necessary). See
REM https://stardot.org.uk/forums/viewtopic.php?p=311977#p311977 for more on this.
*/FINDSWR
REM PROCpoke(${ram_bank_count},0):REM SFTODONOW TEMP HACK
!ifndef NO_SD_CARD_RESET {
    ON ERROR GOTO 500
    REM If we're on a real floppy, the disc will still be spinning after running
    REM FINDSWR so there's shouldn't be any performance penalty to doing this *INFO.
    *INFO XYZZY1
    500ON ERROR PROCerror
}

shadow_state=FNpeek(${shadow_state})
shadow=shadow_state<>${shadow_state_none}

tube=PAGE<&E00
!ifdef OZMOO2P_BINARY {
    IF tube THEN PROCdetect_turbo
} else {
    tube_ram$=""
}
PROCdetect_swr

MODE 135:VDU 23,1,0;0;0;0;
REM SFTODO: Instead of assuming an Electron doesn't have mode 7, we could test
REM to see if we're in mode 7 (as opposed to 6) here and use the mode 7 code if so.
REM This *might* allow an Electron with a mode 7 expansion to use the mode 7 menu
REM and run games in mode 7. We'd need to change some uses of "electron" to a new
REM "has_mode_7" (or "no_mode_7") variable, of course. Without an emulator or
REM someone willing to test this on real hardware it may be best not to try this, at
REM least without reading up on the various Electron mode 7 implementations. In
REM particular, if they provide mode 7 but HIMEM is lower than &7C00, all sorts of
REM assumptions in the build system/loader are likely to be violated with odd
REM results. Of course, we could check that too - if we are in mode 7 but HIMEM<&7C00,
REM explicitly switch back to mode 6, otherwise set the has_mode_7 flag.

REM We don't want to write to any of the resident variable workspace earlier than this
REM because we might trample on P%/O% when assembling code.
fg_colour=${fg_colour}
bg_colour=${bg_colour}
!ifdef MODE_7_INPUT {
?${input_colour}=${DEFAULT_M7_INPUT_COLOUR}
}
screen_mode=${screen_mode}
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
IF tube THEN PRINT CHR$normal_fg;"  Second processor";tube_ram$
IF shadow THEN PRINT CHR$normal_fg;"  Shadow RAM ";FNshadow_extra
IF swr$<>"" THEN PRINT CHR$normal_fg;"  ";swr$
IF vpos=VPOS THEN PRINT CHR$normal_fg;"  None"
PRINT
die_top_y=VPOS

REM A DFS build won't work on another filing system because (among other things)
REM it uses OSWORD &7F to read game data. All sorts of oddness can occur if you
REM try this, so let's at least make it obvious what's going on. If you find this
REM comment by grepping the source for the error message below, you probably
REM want to build with --adfs.
fs=FNfs
!ifndef ACORN_ADFS {
IF fs<>4 THEN PROCdie("Sorry, this game will only work on DFS.")
}

PROCchoose_version_and_check_ram

!ifdef SPLASH {
    REM Flush the keyboard buffer to reduce confusion if the user held down SPACE
    REM or RETURN for a while in order to dismiss the loader screen.
    *FX21
}

?screen_mode=FNmin(FNmax(${default_mode},min_mode),max_mode)
!ifdef AUTO_START {
    mode_keys_vpos=VPOS:PROCshow_mode_keys
} else {
    IF min_mode<>max_mode THEN PROCmode_menu ELSE mode_keys_vpos=VPOS:PROCshow_mode_keys:PROCspace:REPEAT UNTIL FNhandle_common_key(GET)
}

IF ?screen_mode=7 THEN ?fg_colour=${DEFAULT_M7_STATUS_COLOUR}
PRINTTAB(0,space_y);CHR$normal_fg;"Loading:";:pos=POS:PRINT "                               ";
REM Leave the cursor positioned ready for the loading progress indicator.
PRINTTAB(pos,space_y);CHR$normal_graphics_fg;
REM half-block and block UDGs for progress indicator in modes 0-6
VDU 23,181,240,240,240,240,240,240,240,240
VDU 23,255,-1;-1;-1;-1;
!ifdef screen_mode_host {
    PROCpoke(${screen_mode_host},?${screen_mode})
}
!ifdef ACORN_HW_SCROLL_CUSTOM {
    REM SFTODO: Change this line when this executable supports Electron
    REM The FASTSCR executable will examine the current hardware and set SFTODOWHAT to indicate whether we can actually support fast hardware scrolling on this machine. We need to copy that to SFTODOWHERE so the Ozmoo executable itself can see this and act accordingly.
    */FASTSCR
    ?${use_custom_hw_scroll}=FNpeek(${fast_scroll_status_host})
}
REM INSV will be placed on the disc before CACHE2P, so by running them in this
REM order we reduce the chances of the drive head moving backwards.
!ifdef USE_HISTORY {
    */INSV
}
!ifdef CACHE2P_BINARY {
    IF tube THEN */${CACHE2P_BINARY}
}
REM If there are no non-tube builds, ozmoo_relocate_target won't be defined.
!ifdef ozmoo_relocate_target {
    IF NOT tube THEN ?${ozmoo_relocate_target}=FNcode_start DIV 256
}
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

DEF FNshadow_extra
IF shadow_state=${shadow_state_screen_only} THEN ="(screen only)
IF shadow_state=${shadow_state_integra_b} THEN ="(Integra-B)"
IF shadow_state=${shadow_state_mrb} THEN ="(Master RAM board)"
IF shadow_state=${shadow_state_b_plus_os} THEN ="(via OS)"
IF shadow_state=${shadow_state_watford} THEN ="(Watford)"
IF shadow_state=${shadow_state_aries} THEN ="(Aries)"
REM Not all shadow states have an "extra" message; for example, on a Master,
REM shadow RAM is shadow RAM and there's no need for any qualification.
=""

DEF PROCchoose_version_and_check_ram
min_mode=${MIN_MODE}
max_mode=${MAX_MODE}
IF electron AND max_mode=7 THEN max_mode=6

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
REM This is perhaps a bit verbose and/or vague, but to be helpful we suggest
REM changes other than somehow lowering PAGE when running on a machine with no
REM shadow RAM. We don't try to calculate this properly (i.e. repeat the
REM mode-specific tests with HIMEM=&8000) as it's painful enough doing it once. It's
REM certainly feasible that a game which can run in mode 7 would fail to run on a
REM machine with no shadow RAM and succeed on one with shadow RAM, but it's a fairly
REM unlikely case, and we avoid making the suggestion in that case because adding
REM shadow RAM is only equivalent to lowering PAGE by &400, which may well not be
REM enough. In all other modes, shadow RAM more than compensates for any realistic
REM PAGE>&E00 compared to having PAGE=&E00.
extra$=""
!ifdef OZMOO2P_BINARY {
    IF max_mode<7 AND NOT shadow THEN extra$=" Alternatively, adding shadow RAM or a second processor will probably allow the game to run."
} else {
    IF max_mode<7 AND NOT shadow THEN extra$=" Alternatively, adding shadow RAM will probably allow the game to run."
}
IF PAGE>max_page THEN PROCdie("Sorry, you need PAGE<=&"+STR$~max_page+"; it is &"+STR$~PAGE+"."+extra$)

REM At this point we have three different kinds of memory available:
REM - extra_main_ram bytes free in main RAM; this varies with screen mode on
REM   non-shadow systems, so is calculated later for each mode we consider
REM - flexible_swr_ro bytes of sideways RAM which can be used as dynamic memory or vmem cache
REM - vmem_only_swr bytes of sideways RAM which can be used only as vmem cache
vmem_only_swr=swr_size-flexible_swr_ro

REM If we have shadow RAM, all modes have the same (0K) screen RAM requirement so we
REM only need to do the test once for mode 0. Remember the screen RAM isn't the only
REM factor here - we need enough main and/or sideways RAM to run successfully, so we
REM still need to do the test. If we don't have shadow RAM, we test the modes in
REM increasing order of RAM requirement and stop testing once we experience a
REM failure.
REM SFTODO: We could do the tests in the other order; which is faster (hopefully
REM mostly imperceptibly) will depend on how many modes are acceptable compared to
REM how many aren't.
REM SFTODO: I THINK (NOT EXPLICITLY TIMED IT) THIS IS A SMIDGE SLOW, MAYBE SEE IF I CAN IMPROVE IT
IF shadow THEN die_if_not_ok=TRUE:ok=FNmode_ok(0):ENDPROC
RESTORE 3000
3000DATA 7,6,4,3,0
ok=TRUE
REPEAT
READ mode
REM We're a bit inconsistent about passing values to FNmode_ok() and its children vs using globals.
die_if_not_ok=(shadow OR mode=max_mode)
IF mode>=${MIN_MODE} AND mode<=max_mode THEN ok=FNmode_ok(mode):IF ok THEN min_mode=mode
UNTIL mode<=${MIN_MODE} OR NOT ok
ENDPROC

DEF FNmode_ok(mode)
REM We may have shadow RAM even if we don't require it (the Electron executable
REM currently handles both types of system), so we check the potential value of
REM HIMEM in the shadow version of the mode we're interested in, if it exists.
screen_ram=&8000-FNhimem_for_mode(128+mode)
extra_main_ram=swr_main_ram_free+(max_page-PAGE)-screen_ram
REM flexible_swr may be modified during the decision making process, so reset it each time.
flexible_swr=flexible_swr_ro
IF swr_dynmem_model=0 THEN =FNmode_ok_small_dynmem
IF swr_dynmem_model=1 THEN =FNmode_ok_medium_dynmem
=FNmode_ok_big_dynmem

REM SFTODONOW I NEED TO TEST EVERY SINGLE CASE IN THESE MODE OK CHECKS :-/

DEF FNmode_ok_small_dynmem
REM SFTODO: Is it confusing that main_ram_shortfall and any_ram_shortfall are "positive for not enough" whereas we are generally tracking available RAM and using "negative for not enough"?
REM We must have main RAM for the dynamic memory. The build system checked that
REM the binary would have enough main RAM when built at the maximum value of PAGE
REM and with the assumed screen RAM, so we're OK for dynamic memory iff
REM extra_main_ram>=0. In order to report the requirements correctly if we don't
REM have enough, we take a (tweaked) copy of extra_main_ram before trying to
REM allocate ${MIN_VMEM_BYTES}.
main_ram_shortfall=-FNmin(extra_main_ram,0)
PROCsubtract_ram(${MIN_VMEM_BYTES})
IF extra_main_ram>=0 THEN =TRUE
any_ram_shortfall=(-extra_main_ram)-main_ram_shortfall
=FNmaybe_die_ram(main_ram_shortfall,"main RAM",any_ram_shortfall,"main+sideways RAM")

DEF FNmode_ok_medium_dynmem
REM For the medium dynamic memory model, we *must* have enough flexible_swr for the
REM game's dynamic memory; main RAM can't be used as dynamic memory.
REM SFTODOTESTDONE: Although you have to force it (we won't by default use medium dynmem on shadow-capable build), I have verified that this correctly works for games with <=11.5K dynmem on a B+64K and correctly refuses to run with more dynmem, and that those games that refuse do run if you add a 16K SWR bank
flexible_swr=flexible_swr-swr_dynmem_needed
PROCsubtract_ram(${MIN_VMEM_BYTES})
=FNmaybe_die_ram(-flexible_swr,"sideways RAM",-extra_main_ram,"main+sideways RAM")

DEF FNmode_ok_big_dynmem
REM Dynamic memory can come from a combination of main RAM and flexible_swr. For this
REM calculation we prefer to take it from flexible_swr so we can use the result to
REM determine the available main RAM for shadow vmem cache if that's enabled.
flexible_swr=flexible_swr-swr_dynmem_needed
IF flexible_swr<0 THEN extra_main_ram=extra_main_ram+flexible_swr:flexible_swr=0
PROCsubtract_ram(${MIN_VMEM_BYTES})
=FNmaybe_die_ram(-extra_main_ram,"main+sideways RAM",0,"")

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
    REM If we don't have shadow RAM or a shadow driver, we don't need any cache.
    IF shadow_state<${shadow_state_first_driver} THEN =p
    REM In mode 0, all shadow RAM is used for the screen.
    IF ?screen_mode=0 THEN =p
    REM If we're in (say) shadow mode 3, we only have 4K of spare shadow RAM and
    REM it's tempting to reduce the number of pages of shadow cache. This would
    REM sometimes help on machines which don't have that much RAM for virtual
    REM memory cache, but it runs the risk of creating bad worst-case behaviour.
    REM Imagine a game has 4 "hot" pages of read-only memory, all of which happen
    REM to end up in shadow RAM - if we've reduced the shadow cache from 4 pages
    REM to 3 pages we will suffer a big performance hit as we repeatedly copy data
    REM into the shadow cache. This might not be all that likely, and the
    REM probability of it happening goes down as the size of spare shadow RAM
    REM reduces, but it's always possible. It therefore seems safest to avoid
    REM adjusting the size of shadow cache depending on the size of spare shadow
    REM RAM. (In principle we could allow the user to specify a minimum shadow
    REM cache size which we'd never shrink below, but in reality the user is not
    REM going to know a safe minimum value. There is already some risk of this
    REM happening anyway - perhaps the user tests on a machine where an extra page
    REM of shadow cache is added to the default because of the alignment of PAGE,
    REM and that makes all the difference, or perhaps the user's precise memory
    REM setup avoids hot read-only memory being loaded into shadow RAM on their
    REM machine.)
    shadow_cache=FNmin(${RECOMMENDED_SHADOW_CACHE_PAGES}*256,extra_main_ram)
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
!ifdef OZMOOE_BINARY {
    IF electron THEN binary$="${OZMOOE_BINARY}":max_page=${OZMOOE_MAX_PAGE}:swr_dynmem_model=${OZMOOE_SWR_DYNMEM_MODEL}:swr_dynmem_needed=${OZMOOE_SWR_DYNMEM}:swr_main_ram_free=${OZMOOE_SWR_MAIN_RAM_FREE}:ENDPROC
} else {
    IF electron THEN PROCunsupported_machine("an Electron")
}
REM SFTODO: Should I make the loader support some sort of line-continuation character, then I could split up some of these very long lines?
!ifdef OZMOOSH_BINARY {
    IF shadow THEN binary$="${OZMOOSH_BINARY}":max_page=${OZMOOSH_MAX_PAGE}:swr_dynmem_model=${OZMOOSH_SWR_DYNMEM_MODEL}:swr_dynmem_needed=${OZMOOSH_SWR_DYNMEM}:swr_main_ram_free=${OZMOOSH_SWR_MAIN_RAM_FREE}:ENDPROC
} else {
    REM If - although I don't believe this is currently possible - we don't have
    REM OZMOOSH_BINARY but we do have OZMOOB_BINARY, we can run OZMOOB_BINARY on any
    REM BBC.
    !ifndef OZMOOB_BINARY {
        IF host_os<>1 THEN PROCunsupported_machine("a BBC B+/Master")
    }
}
!ifdef OZMOOB_BINARY {
    binary$="${OZMOOB_BINARY}":max_page=${OZMOOB_MAX_PAGE}:swr_dynmem_model=${OZMOOB_SWR_DYNMEM_MODEL}:swr_dynmem_needed=${OZMOOB_SWR_DYNMEM}:swr_main_ram_free=${OZMOOB_SWR_MAIN_RAM_FREE}
} else {
    !ifdef OZMOOSH_BINARY {
        REM SFTODO: Should we also (if OZMOO2P_BINARY is defined) suggest a sedcond processor as an option, as we do in the PAGE-too-high-no-shadow case?
        PROCunsupported_machine("a BBC B without shadow RAM")
    } else {
        PROCunsupported_machine("a BBC B")
    }
}
ENDPROC

!ifndef AUTO_START {
    DEF PROCmode_menu

    REM SFTODO: Is this code a bit slow? All of it, not the RESTORE logic. I
    REM would really quite like to write the menu handling in machine code, but
    REM unless I assemble the code at runtime (which might be slower than this?) it
    REM means either *LOADing another file off the disc or appending the code to the
    REM end of this BASIC program, which is brittle and likely to stop users being
    REM able to help me debug stuff by tweaking the loader.
    !ifdef NEED_MODE_MENU_0_TO_7 {
        IF min_mode=0 AND max_mode=7 THEN RESTORE 10000
    }
    !ifdef NEED_MODE_MENU_3_TO_7 {
        IF min_mode=3 AND max_mode=7 THEN RESTORE 10500
    }
    !ifdef NEED_MODE_MENU_4_TO_7 {
        IF min_mode=4 AND max_mode=7 THEN RESTORE 11000
    }
    !ifdef NEED_MODE_MENU_6_TO_7 {
        IF min_mode=6 AND max_mode=7 THEN RESTORE 11500
    }
    !ifdef NEED_MODE_MENU_0_TO_6 {
        IF min_mode=0 AND max_mode=6 THEN RESTORE 12000
    }
    !ifdef NEED_MODE_MENU_3_TO_6 {
        IF min_mode=3 AND max_mode=6 THEN RESTORE 12500
    }
    !ifdef NEED_MODE_MENU_4_TO_6 {
        IF min_mode=4 AND max_mode=6 THEN RESTORE 13000
    }
    !ifdef NEED_MODE_MENU_0_TO_4 {
        IF min_mode=0 AND max_mode=4 THEN RESTORE 13500
    }
    !ifdef NEED_MODE_MENU_0_TO_3 {
        IF min_mode=0 AND max_mode=3 THEN RESTORE 14000
    }
    !ifdef NEED_MODE_MENU_3_TO_4 {
        IF min_mode=3 AND max_mode=4 THEN RESTORE 14500
    }
    mode_list$=""
    READ n:n=n-1
    REM We don't bother with a second dimension to highlight_left_[xy], because
    REM the cells line up vertically.
    DIM mode_x(7),mode_y(7),cell_x(n),cell_y(n),text$(2,1),highlight_left_x(2),highlight_right_x(2),mode(2,1),min_x(1),max_y(2)
    max_x=0:max_y=0:min_x(1)=9:REM Logically we should set min_x(0)=9 too, but in practice the default 0 works.
    FOR i=0 TO n
    READ x,y,text$(x,y)
    mode$=LEFT$(text$(x,y),1)
    IF mode$<>" " THEN mode_list$=mode_list$+mode$:mode=VAL(mode$):mode(x,y)=mode:mode_x(mode)=x:mode_y(mode)=y ELSE mode(x,y)=7
    IF x<min_x(y) THEN min_x(y)=x
    IF x>=max_x THEN max_x=x
    IF y>max_y(x) THEN max_y(x)=y
    IF y>max_y THEN max_y=y
    cell_x(i)=x:cell_y(i)=y
    READ highlight_left_x(x),highlight_right_x(x)
    NEXT

    PRINT CHR$header_fg;"Screen mode:";CHR$normal_fg;CHR$electron_space;"(hit ";:sep$="":FOR i=1 TO LEN(mode_list$):PRINT sep$;MID$(mode_list$,i,1);:sep$="/":NEXT:PRINT " to change)"
    menu_top_y=VPOS
    mode_keys_vpos=menu_top_y+max_y+2
    FOR i=0 TO n
    x=cell_x(i):y=cell_y(i)
    PRINTTAB(highlight_left_x(x)+2,menu_top_y+y);text$(x,y);
    NEXT

    x=mode_x(?screen_mode):y=mode_y(?screen_mode)
    PROChighlight(x,y,TRUE):PROCspace

    REPEAT
    old_x=x:old_y=y
    key=GET
    IF key=136 AND x>0 THEN x=x-1:IF y>max_y(x) THEN y=0
    IF key=137 AND x<max_x THEN x=x+1:IF y>max_y(x) THEN y=0
    IF key=138 AND y<max_y(x) THEN y=y+1
    IF key=139 AND y>0 THEN y=y-1
    REM We don't set y if mode 7 is selected by pressing "7" so subsequent movement
    REM with cursor keys remembers the old y position.
    key$=CHR$key:IF INSTR(mode_list$,key$)<>0 THEN x=mode_x(VALkey$):IF key$<>"7" THEN y=mode_y(VALkey$)
    IF x<>old_x OR (y<>old_y AND mode(x,y)<>7) THEN PROChighlight(old_x,old_y,FALSE):PROChighlight(x,y,TRUE)
    UNTIL FNhandle_common_key(key)
    ENDPROC

    DEF PROChighlight(x,y,on)
    IF on THEN ?screen_mode=mode(x,y)
    IF on THEN PROCshow_mode_keys
    IF electron THEN PROChighlight_internal_electron(x,y,on):ENDPROC
    IF mode(x,y)=7 THEN PROChighlight_internal(x,0,on):y=1
    PROChighlight_internal(x,y,on)
    ENDPROC
    DEF PROChighlight_internal(x,y,on)
    REM We put the "normal background" code in at the right hand side first before
    REM (maybe) putting a "coloured background" code in at the left hand side to try
    REM to reduce visual glitches.
    IF highlight_right_x(x)<39 THEN PRINTTAB(highlight_right_x(x),menu_top_y+y);CHR$normal_fg;CHR$156;
    PRINTTAB(highlight_left_x(x)-1,menu_top_y+y);
    IF on THEN PRINT CHR$highlight_bg;CHR$157;CHR$highlight_fg ELSE PRINT "  ";CHR$normal_fg
    ENDPROC
    DEF PROChighlight_internal_electron(x,y,on)
    PRINTTAB(highlight_left_x(x),menu_top_y+y);
    IF on THEN COLOUR 135:COLOUR 0 ELSE COLOUR 128:COLOUR 7
    PRINT SPC(2);text$(x,y);SPC(2);
    COLOUR 128:COLOUR 7
    ENDPROC

    DEF FNhandle_common_key(key)
    IF electron AND key=2 THEN ?bg_colour=(?bg_colour+1) MOD 8:VDU 19,0,?bg_colour,0;0
    IF electron AND key=6 THEN ?fg_colour=(?fg_colour+1) MOD 8:VDU 19,7,?fg_colour,0;0
    =key=32 OR key=13
}

REM This is not a completely general pretty-print routine, e.g. it doesn't make
REM any attempt to handle words which are longer than the screen width. It's
REM good enough for our needs.
REM SFTODO: Might be nice to speed it up a bit
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
!ifdef ACORN_TURBO_SUPPORTED {
    REM !BOOT will have run the TURBO executable, which will have set
    REM is_turbo to &FF if we're on a turbo copro or 0 otherwise. It's just
    REM possible (e.g. on a hard drive installation) our !BOOT isn't in use, so
    REM if is_turbo isn't 0 or &FF let's complain. This won't always catch the
    REM problem, but it's better than nothing.
    IF ?${is_turbo}<>0 AND ?${is_turbo}<>&FF THEN PROCdie("Invalid turbo test flag")
    turbo=0<>?${is_turbo}
} else {
    turbo=FALSE
}
IF turbo THEN tube_ram$=" (256K)" ELSE tube_ram$=" (64K)"
ENDPROC

!ifdef ACORN_SHADOW_VMEM {
REM SFTODO: Should we set shadow_extra$ to "(screen only)" or some other distinctive string if the game fits in memory but we have too little main RAM free to have a shadow cache? It might be prudent to offer some clue this is happening, because (e.g.) having ADFS+DFS vs DFS might be enough to tip us from one state to the other and performance would drop dramatically because we've suddenly lost access to the spare shadow RAM
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
REM flexible_swr_ro is the amount of sideways RAM in the first bank which can be
REM used as dynamic memory (perhaps in combination with main RAM, depending on the
REM memory model) or vmem cache. Other sideways RAM can only be used as vmem cache.
IF swr_banks>0 THEN flexible_swr_ro=&4000 ELSE flexible_swr_ro=0
IF FNpeek(${private_ram_in_use})=0 THEN PROCadd_private_ram_as_swr
IF FNpeek(${swr_type})>2 THEN swr$="("+STR$(swr_banks*16)+"K unsupported sideways RAM)":PROCupdate_swr_banks(0)
swr_size=&4000*swr_banks-swr_adjust
IF swr_banks=0 THEN ENDPROC
REM SFTODO: Maybe a bit confusing that we call it "private RAM" here but sideways RAM if we have real sideways RAM to go with it - also as per TODO above we may not actually have the full 12K, and while it's maybe confusing to say "11.5K private RAM" we also don't want the user adding up their memory and finding it doesn't come out right - arguably we *can* say 12K private RAM (at least on B+, not sure about Integra-B) because we *do* have it all, it's just we set aside the last 512 bytes for other uses, but still for Ozmoo
IF swr_size<=12*1024 THEN swr$="12K private RAM":ENDPROC
REM We use integer division here so that the 11.5K sideways RAM from the B+/
REM Integra-B private RAM doesn't cause the text to wrap at the screen right
REM margin if we have a lot of sideways RAM banks.
REM SFTODO: I'm not entirely happy with this, is there a better way?
swr$=STR$(swr_size DIV 1024)+"K sideways RAM (bank":IF swr_banks>1 THEN swr$=swr$+"s"
swr$=swr$+" &":FOR i=0 TO swr_banks-1:bank=FNpeek(${ram_bank_list}+i)
IF bank>=64 THEN bank$="P" ELSE bank$=STR$~bank
swr$=swr$+bank$:NEXT:swr$=swr$+")"
ENDPROC

DEF PROCadd_private_ram_as_swr
REM If this is a tube-only build, these *_private_ram_size constants might not be
REM defined. SFTODONOW: Still true? Might be if it's a tube only build with no host cache support.
!ifdef integra_b_private_ram_size {
    IF swr_banks<${max_ram_bank_count} AND integra_b THEN PROCpoke(swr_banks+${ram_bank_list},64):PROCupdate_swr_banks(swr_banks+1):swr_adjust=16*1024-${integra_b_private_ram_size}
}
!ifdef b_plus_private_ram_size {
    IF swr_banks<${max_ram_bank_count} AND host_os=2 THEN PROCpoke(swr_banks+${ram_bank_list},128):PROCupdate_swr_banks(swr_banks+1):swr_adjust=16*1024-${b_plus_private_ram_size}:IF swr_banks=1 THEN flexible_swr_ro=${b_plus_private_ram_size}
}
ENDPROC

DEF PROCupdate_swr_banks(i):swr_banks=i:PROCpoke(${ram_bank_count},i):ENDPROC

DEF PROCunsupported_machine(machine$):PROCdie("Sorry, this game won't run on "+machine$+".")

DEF FNmaybe_die_ram(amount1,ram_type1$,amount2,ram_type2$)
IF amount1<=0 AND amount2<=0 THEN =TRUE
IF NOT die_if_not_ok THEN =FALSE
IF amount1<=0 THEN amount1=amount2:ram_type1$=ram_type2$:amount2=0
message$="Sorry, you need at least " + STR$(amount1/1024)+"K more "+ram_type1$
IF amount2>0 THEN message$=message$+" and at least "+STR$(amount2/1024)+"K more "+ram_type2$+" as well"
PROCdie(message$+".")

DEF PROCshow_mode_keys
mode_keys_last_max_y=mode_keys_last_max_y:REM set variable to 0 if it doesn't exist
IF mode_keys_last_max_y=0 THEN PRINTTAB(0,mode_keys_vpos);CHR$header_fg;"In-game controls:" ELSE PRINTTAB(0,mode_keys_vpos+1);
REM The odd indentation on the next few lines is so a) it's easy to see all the
REM different possible output lines have the same length and will completely
REM obliterate each other b) the build script will strip off the extra
REM indentation as it's at the start of the line.
REM SFTODO: We should probably show command history up/down if USE_HISTORY is set (but we should probably regard SHIFT+cursor for split cursor editing as too obscure to mention here)
                                PRINT CHR$normal_fg;"  SHIFT:  show next page of text"
!ifdef MODE_7_STATUS {
         IF ?screen_mode=7 THEN PRINT CHR$normal_fg;"  CTRL-F: change status line colour"
}
!ifdef MODE_7_INPUT {
         IF ?screen_mode=7 THEN PRINT CHR$normal_fg;"  CTRL-I: change input colour      "
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

DEF PROCoscli($block%):X%=block%:Y%=X%DIV256:CALL&FFF7:ENDPROC

DEF FNpeek(addr):!block%=&FFFF0000 OR addr:A%=5:X%=block%:Y%=block% DIV 256:CALL &FFF1:=block%?4

DEF PROCpoke(addr,val):!block%=&FFFF0000 OR addr:block%?4=val:A%=6:X%=block%:Y%=block% DIV 256:CALL &FFF1:ENDPROC

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

DEF FNhimem_for_mode(mode):A%=&85:X%=mode:=(USR&FFF4 AND &FFFF00) DIV &100

REM SFTODO: Moving this data to the start of the program would speed up RESTORE
REM by about a centisecond; not huge but maybe worth having.

!ifdef NEED_MODE_MENU_0_TO_7 {
    10000DATA 6
    DATA 0,0,"0) 80x32",1,12
    DATA 0,1,"3) 80x25",1,12
    DATA 1,0,"4) 40x32",13,24
    DATA 1,1,"6) 40x25",13,24
    DATA 2,0,"7) 40x25",25,39
    DATA 2,1,"   teletext",25,39
}

!ifdef NEED_MODE_MENU_3_TO_7 {
    10500DATA 5
    DATA 0,0,"3) 80x25",1,12
    DATA 1,0,"4) 40x32",13,24
    DATA 1,1,"6) 40x25",13,24
    DATA 2,0,"7) 40x25",25,39
    DATA 2,1,"   teletext",25,39
}

!ifdef NEED_MODE_MENU_4_TO_7 {
    11000DATA 4
    DATA 0,0,"4) 40x32",1,12
    DATA 0,1,"6) 40x25",1,12
    DATA 1,0,"7) 40x25",13,27
    DATA 1,1,"   teletext",13,27
}

!ifdef NEED_MODE_MENU_6_TO_7 {
    11500DATA 3
    DATA 0,0,"6) 40x25",1,12
    DATA 1,0,"7) 40x25",13,27
    DATA 1,1,"   teletext",13,27
}

!ifdef NEED_MODE_MENU_0_TO_6 {
    12000DATA 4
    DATA 0,0,"0) 80x32",1,12
    DATA 0,1,"3) 80x25",1,12
    DATA 1,0,"4) 40x32",13,24
    DATA 1,1,"6) 40x25",13,24
}

!ifdef NEED_MODE_MENU_3_TO_6 {
    12500DATA 3
    DATA 0,0,"3) 80x25",1,12
    DATA 1,0,"4) 40x32",13,24
    DATA 2,0,"6) 40x25",25,36
}

!ifdef NEED_MODE_MENU_4_TO_6 {
    13000DATA 2
    DATA 0,0,"4) 40x32",1,12
    DATA 1,0,"6) 40x25",13,24
}

!ifdef NEED_MODE_MENU_0_TO_4 {
    13500DATA 3
    DATA 0,0,"0) 80x32",1,12
    DATA 0,1,"3) 80x25",1,12
    DATA 1,0,"4) 40x32",13,24
}

!ifdef NEED_MODE_MENU_0_TO_3 {
    14000DATA 2
    DATA 0,0,"0) 80x32",1,12
    DATA 1,0,"3) 80x25",13,24
}

!ifdef NEED_MODE_MENU_3_TO_4 {
    14500DATA 2
    DATA 0,0,"3) 80x25",1,12
    DATA 1,0,"4) 40x25",13,24
}

REM SFTODO: Not a likely case, but it might be nice if we cope nicely (even if
REM just an error, or perhaps reducing the mode menu selection) when we're on tube
REM with no shadow RAM and OSHWM>=&3000 on host so we can't use mode 0 etc. As an
REM extra complication, if OSHWM is high in host, we may not have room for the host
REM cache. Not saying I should massively complicate the loader for this, but giving
REM some kind of error if OSHWM is super high in host might be better than just
REM crashing.
