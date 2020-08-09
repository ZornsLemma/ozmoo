REM The loader uses some of the resident integer variables (or at least the
REM corresponding memory) to communicate with the Ozmoo executable. This
REM means it's probably least error prone to avoid using resident integer
REM variables gratuitously in this code.
REM SFTODO: It would be nice if the loader and build system could work
REM together to allow the user to *optionally* specify a high-res title
REM screen and/or a nice mode 7 banner to display at the top of the
REM "options" screen before we launch the game proper.
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
MODE 135
*FX229,1
PRINT "Powered by Ozmoo 3.4 (Acorn alpha 1)"'
:
REM The following need to be kept consistent with asm/constants.asm
relocate_target=&408
ram_bank_count=&410
ram_bank_list=&411
max_ram_bank_count=11
:
shadow%=(HIMEM>=&8000)
tube%=(PAGE<&E00)
A%=0:X%=1:host_os%=USR(&FFF4) DIV &100 AND &FF
IF NOT tube% THEN PROCdetect_swr ELSE PRINT "Second processor detected"'
IF NOT tube% AND shadow% THEN PRINT "Shadow RAM detected"'
IF NOT tube% THEN ?relocate_target=FNrelocate_to DIV 256
mode%=FNmode
REM We don't change mode if we're using mode 7; this means if we're using
REM mode 7 out of necessity not choice the user gets a chance to see the
REM output of the loader while the game is loading. (If the user is
REM prompted to choose a mode they get a chance to read whatever else is
REM on the screen at that point.)
REM SFTODONOW: Should I make the Ozmoo executable change mode when it's
REM ready to start playing the game? (Just where it currently does vdu_cls
REM and calls update_colours.) That way we'd always keep the loading screen
REM up during the initial load on all versions regardless. On the other hand,
REM we'd then end up with a "flicker" when the mode is changed needlessly
REM after a restart - but of course we could compare current with target mode
REM and only do the change if they're different.
IF mode%<>7 THEN MODE 128+mode%
VDU 23,1,0;0;0;0;
VDU 19,0,4,0,0,0
PRINT "Loading, please wait...";
*DIR S
IF tube% THEN */$.OZMOO2P
IF shadow% THEN */$.OZMOOSH
REM We must be on a BBC B with no shadow RAM.
*/$.OZMOOSW
END
:
DEF PROCdetect_swr
DIM code% 256
paged_rom_table=&2A1
binary_version_number=&8008
romsel_copy=&F4
romsel=&FE30
FOR opt%=0 TO 2 STEP 2
P%=code%
[OPT opt%
.detect_swr
LDA romsel_copy
PHA
LDX #0
LDY #15
.detect_swr_loop
\ Does bank Y have a language or service entry?
LDA paged_rom_table,Y
AND #&C0
BNE skip_bank
\ No, it doesn't. See if it's RAM.
\ SFTODO Isn't there some possibility of false positives due to values hanging
\ around on the data bus? What's the recommended way to deal with this?
STY romsel_copy
STY romsel
INC binary_version_number
LDA binary_version_number
DEC binary_version_number
CMP binary_version_number
BEQ skip_bank
\ Yes, it's RAM.
TYA
STA ram_bank_list,X
INX
CPX #max_ram_bank_count
BEQ done
.skip_bank
DEY
BPL detect_swr_loop
.done
STX ram_bank_count
PLA
STA romsel_copy
STA romsel
RTS
]
NEXT
CALL detect_swr
IF ?ram_bank_count = 0 THEN PROCdie("Sorry, no sideways RAM or second"+CHR$(13)+CHR$(10)+"processor detected.")
REM SFTODO: I'm not happy with the visual presentation here but let's get it working first.
REM SFTODONOW: If someone has >11 banks of sideways RAM it's maybe a bit confusing to not detect all of it. Not a big deal really.
PRINT ;16*?ram_bank_count;"K of free sideways RAM detected"
PRINT "(bank";
IF ?ram_bank_count > 1 THEN PRINT "s";
PRINT " ";
c$=""
FOR i%=(?ram_bank_count)-1 TO 0 STEP -1
IF i%=0 AND (?ram_bank_count > 1) THEN c$=" and "
PRINT c$;~(ram_bank_list?i%);
c$=", "
NEXT
PRINT ")"'
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
DEF FNmode
LOCAL M$
IF NOT shadow% THEN =7
PRINT "Which screen mode do you want to play"'"in, 0, 3, 4, 6 or 7? ";
*FX21
REPEAT
M$=GET$
UNTIL INSTR("03467",M$)<>0
PRINT M$'
=VAL(M$)
:
DEF PROCdie(message$)
PRINT message$
*FX229,0
END
