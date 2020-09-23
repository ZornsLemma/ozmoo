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
A%=&85:X%=135:potential_himem%=(USR&FFF4 AND &FFFF00) DIV &100
IF potential_himem%=&8000 AND HIMEM<&8000 THEN MODE 135:CHAIN "LOADER"

fg_colour=${fg_colour}
bg_colour=${bg_colour}

MODE 135:VDU 23,1,0;0;0;0;
?fg_colour=7:?bg_colour=4
A%=0:X%=1:host_os=(USR&FFF4 AND &FF00) DIV &100:electron=host_os=0
IF electron THEN VDU 19,0,?bg_colour,0;0,19,7,?fg_colour,0;0
DIM block% 256
REM SFTODO: SET UP HEADER AND FOOTER
VDU 28,0,22,39,12:REM SFTODO TEMPORARY, TO SIMULATE BANNER

shadow=potential_himem=&8000
tube=PAGE<&E00
REM SFTODO: If the build *only* supports tube with no cache, we don't need detect_swr - not sure it's worth worrying about this, but will make a note for now.
PROCdetect_swr

REM The tube build works on both the BBC and Electron, so we check that first.
!ifdef TUBE_BINARY {
IF tube THEN binary$="${TUBE_BINARY}":GOTO 2000
}
!ifdef ELECTRON_SWR_BINARY {
IF electron THEN binary$="${ELECTRON_SWR_BINARY}":max_page=${ELECTRON_SWR_PAGE}:relocatable=${ELECTRON_SWR_RELOCATABLE}:swr_needed=${ELECTRON_SWR_SWR_DYNMEM}:GOTO 1000
} else {
IF electron THEN PROCunsupported_machine("Electron")
}
REM SFTODO: Not just here - if I am able to run some games with no SWR, I should probably take SWR out of the "plain B" and "shadow+sideways RAM" build names (which would affect the new make-acorn.py script too, just as an internal naming thing)
!ifdef BBC_SHR_SWR_BINARY {
IF shadow THEN binary$="${BBC_SHR_SWR_BINARY}":max_page=${BBC_SHR_SWR_MAX_PAGE}:relocatable=${BBC_SHR_SWR_RELOCATABLE}:swr_needed=${BBC_SHR_SWR_SWR_DYNMEM}:GOTO 1000
} else {
REM BBC_SWR_BINARY only works on a model B because of the mode-7-at-&3C00 trick,
REM so if we don't have BBC_SHR_SWR_BINARY we must refuse to work on anything
REM else.
IF host_os<>1 THEN PROCunsupported_machine("BBC B+/Master")
}
!ifdef BBC_SWR_BINARY {
binary$="${BBC_SWR_BINARY}":max_page=${BBC_SWR_MAX_PAGE}:relocatable=${BBC_SWR_RELOCATABLE}:swr_needed=${BBC_SWR_SWR_DYNMEM}:GOTO 1000
} else {
PROCunsupported_machine("BBC B")
}

REM For builds which can use sideways RAM, we need to check if we have enough
REM main RAM and/or sideways RAM to run successfully.
REM SFTODO: WE SHOULD SHOW HARDWARE DETECTION EARLIER THAN THIS, SO USER CAN SEE WHAT SWR WE DETECTED BEFORE WE COMPLAIN WE DON'T HAVE ENOUGH, IN CASE OF DETECTION PROBLEMS
REM SFTODO: We shouldn't emit this block of code if we *only* support tube.
REM SFTODO THIS WON'T DO THE RIGHT THING ON ELECTRON, WHERE MAIN RAM CAN SUBSTITUTE FOR VMEM BUT NOT DYNMEM
1000IF PAGE>max_page THEN PROCdie("Sorry, you need PAGE<=&"+STR$~max_page+"; it is &"+STR$~PAGE+".")
IF relocatable THEN extra_main_ram=max_page-PAGE:?${relocate_to}=PAGE DIV 256 ELSE extra_main_ram=0
swr_needed=swr_needed-&4000*?${ram_bank_count}
REM On the BBC extra_main_ram will reduce the need for sideways RAM for dynamic
REM memory, but on the Electron it is used as swappable memory only.
IF electron AND swr_needed>0 THEN PROCdie_ram(swr_needed, "sideways RAM")
mem_needed=swr_needed+${MIN_VMEM_BYTES}-extra_main_ram
IF mem_needed>0 THEN PROCdie_ram(mem_needed, "main or sideways RAM")
REM SFTODO: If we have >=MIN_VMEM_BYTES but not >=PREFERRED_MIN_VMEM_BYTES we should maybe show a warning

2000REM SFTODO ALL THE MENU STUFF (IF NOT AUTO START)
REM SFTODO: WE MAY WANT TO NOT ALLOW RUNNING IN EG 40 COLUMN MODES, IF THE GAME IS REALLY NOT HAPPY WITH THEM SO IDEALLY MENU WILL BE MORE FLEXIBLE THAN IT WAS

REM SFTODO: SHOW "LOADING, PLEASE WAIT"
!ifdef TUBE_CACHE_BINARY {
IF tube THEN */${TUBE_CACHE_BINARY}
}
fs=FNfs
IF fs<>4 THEN path$=FNpath
REM Select user's home directory on NFS
IF fs=5 THEN *DIR
REM On non-DFS, select a SAVES directory if it exists but don't worry if it doesn't.
ON ERROR GOTO 3000
IF fs=4 THEN PROCoscli("DIR S") ELSE *DIR SAVES
3000ON ERROR PROCerror
REM On DFS this is actually a * command, not a filename, hence the leading "/" (="*RUN").
IF fs=4 THEN filename$="/"+binary$ ELSE filename$=path$+".DATA"
IF LENfilename$>=${filename_size} THEN PROCdie("Game data path too long")
REM We do this last, as it uses a lot of resident integer variable space and this reduces
REM the chances of it accidentally getting corrupted.
$${filename_data}=filename$
*FX4,0
REM SFTODO: Should test with BASIC I at some point, probably work fine but galling to do things like PROCoscli and still not work on BASIC I!
IF fs=4 THEN PROCoscli($filename_data) ELSE PROCoscli("/"+path$+"."+binary$)
END

DEF PROCerror:CLS:REPORT:PRINT" at line ";ERL:PROCfinalise

DEF PROCdie(message$)
REM SFTODO: This needs to print nicely on screen preserving any hardware detected output, word-wrapping and using the normal fg colour if we're in mode 7.
PRINT message$
REM Fall through to PROCfinalise
DEF PROCfinalise
*FX229,0
*FX4,0
END

DEF PROCdie_ram(amount,ram_type$):PROCdie("Sorry, you need at least "+STR$(amount/1024)+"K more "+ram_type$+".")

DEF PROCoscli(command$):$block%=command$:X%=block%:Y%=X%DIV256:CALL&FFF7:ENDPROC
