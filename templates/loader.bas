REM The loader uses some of the resident integer variables (or at least the
REM corresponding memory) to communicate with the Ozmoo executable. This
REM means it's probably least error prone to avoid using resident integer
REM variables gratuitously in the loader.
MODE 7
tube%=(PAGE<&E00)
ram_bank_count=&410
ram_bank_list=&411
max_ram_bank_count=11
IF NOT tube% THEN PROCdetect_swr ELSE PRINT "Second processor detected"'
PRINT "Which screen mode do you want to play"'"in, 0, 3, 4, 6 or 7? ";
*FX21
REPEAT
M$=GET$
UNTIL INSTR("03467",M$)<>0
mode%=VAL(M$)
REM SFTODO: TEMP HACK
?&400=4
?&401=4
?&402=5
?&403=6
?&404=7
MODE 128+mode%
VDU 23,1,0;0;0;0;
VDU 19,0,4,0,0,0
REM Z3 games may put a teletext control character at top left before
REM loading the game data, so put one there ourselves to avoid a
REM visual glitch. SFTODO: Perhaps tweak this.
IF mode%=7 THEN PRINT CHR$(134);
PRINT "Loading, please wait...";
*DIR S
*/$.OZMOO
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
IF ?ram_bank_count = 0 THEN PRINT "Sorry, no sideways RAM or second"'"processor detected.":END
REM SFTODO: I'm not happy with the visual presentation here but let's get it working first.
REM SFTODO: If someone has >11 banks of sideways RAM it's maybe a bit confusing to not detect all of it. Not a big deal really.
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
