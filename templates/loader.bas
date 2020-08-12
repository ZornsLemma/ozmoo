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
REM SFTODO: Note that for Z3 games, anything shown on the top line of
REM the screen will remain present occupying the not-yet-displayed
REM status line until the game starts. This means that if any disc
REM errors occur during the initial loading, the screen may scroll
REM but the top line won't. This isn't a big deal but for the nicest
REM possible appearance in this admittedly unlikely situation either
REM clear the top line before running the Ozmoo executable or make
REM sure it has something that looks OK on its own. (For example,
REM *not* the top half of some double-height text.)
PRINT "Powered by Ozmoo 3.4 (Acorn alpha 1.2)"'
:
REM The following need to be kept consistent with asm/constants.asm
relocate_target=&408
ram_bank_count=&410
ram_bank_list=&411
max_ram_bank_count=9:REM 255*0.5K for VM plus 16K for dynamic memory
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
REM SFTODO: Should I make the Ozmoo executable change mode when it's
REM ready to start playing the game? (Just where it currently does vdu_cls
REM and calls update_colours.) That way we'd always keep the loading screen
REM up during the initial load on all versions regardless. On the other hand,
REM we'd then end up with a "flicker" when the mode is changed needlessly
REM after a restart - but of course we could compare current with target mode
REM and only do the change if they're different.
IF mode%<>7 THEN MODE 128+mode%
VDU 23,1,0;0;0;0;
VDU 19,0,4,0,0,0
PRINT "Loading, please wait..."'
*DIR S
IF tube% THEN */$.OZMOO2P
IF shadow% THEN */$.OZMOOSH
REM We must be on a BBC B with no shadow RAM.
*/$.OZMOOSW
END
:
DEF PROCdetect_swr
REM This sideways RAm detection code is derived from Wouter Scholten's public
REM domain swrtype-0.7. (http://wouter.bbcmicro.net/bbc/software-whs.html)
DIM code 512
DIM data 64
swr_backup = data
swr_test = data + &10

swr_type = data + &20
swr_banks = data + &21

swr_byte_value1 = data  + &22
swr_byte_value2 = data  + &23

dummy = data  + &24
tmp = data  + &25

test_location = &8008:REM binary version number

FOR N%=0 TO 2 STEP 2
P%=code
[ OPT N%

.swr_check
SEI
\ save original contents
LDY #0
.lp STY &FE30 \ set rom -> #Y
LDA test_location:STA swr_backup,Y
INY:CPY #16:BCC lp

LDA #0: STA swr_banks
LDA #255:STA swr_type

\ now test which type:
LDY #0:LDA #0
.lp0 STA swr_test,Y:INY:CPY #16:BCC lp0

LDA #&80: STA swr_byte_value1
LDA #&E3: STA swr_byte_value2

LDY #0
.bank_lp_y
  JSR set_all
  TYA:EOR swr_byte_value1:STA tmp:STA test_location
  LDX #15
.bank_lp_x
    LDA #0:STA dummy
    STX &FE30:LDA test_location:CMP tmp:BNE cmp_next_x
    \ equality could be accidental (ROM or RAM bank had tested value
    \ already), so try a 2nd value.
    \ First restore romsel for write
    JSR set_romsel
    TYA:EOR swr_byte_value2:STA tmp: STA test_location
    \ write 0 to a dummy location. Goal is to change the databus value.
    \ Otherwise, in a fully decoded bank system, locations that do not
    \ have ROM or RAM, will not change the value on the address bus,
    \ at least for several cycles, so the old one stays...
    \ On my main BBC, this works with at least 2 NOPs
    \     LDA #value STA test_location NOP NOP CMP test_location
    \ but here we need at least 3
    \     LDA #value STA test_location NOP NOP NOP LDA test_location CMP #value
    \ Better explicity change the value to something else, as this may
    \ vary between CPUs/systems? (e.g. depending on load on the bus).
    LDA #0:STA dummy

    STX &FE30:LDA test_location:CMP tmp:BNE cmp_next_x
    INC swr_banks:INC swr_test,X
    \ we don't know which method it is yet...
    JMP cmp_next_y
.cmp_next_x
    \ restore the corrupted byte in swr (from the 2nd write!)
    JSR set_romsel
    TYA:EOR swr_byte_value1:STA tmp: STA test_location
    DEX:BPL bank_lp_x
.cmp_next_y
  INY:CPY #16:BCC bank_lp_y
LDA swr_banks:BNE continue:STA swr_type \ no SWR found
JMP end2

.found_type_1
LDA #1:STA swr_type:STA swr_banks
\ restore swr bank byte
LDA swr_backup,Y:STA test_location
JMP end2

.continue \ type 1-6
LDY #16
.find_type_lp
DEY: \BMI find_type_end \ no need, we only get here if there is RAM.
LDA swr_test,Y:BEQ find_type_lp
CMP #8:BCS found_type_1 \XXX CHANGE THIS
JSR set_only_solidisk
\ N.B. STY &FE30 should take care of the databus problem, as long as the
\ value written to RAM is not in the set {0,...,15}.
TYA:EOR swr_byte_value1:EOR #&22: STA test_location:STY &FE30:CMP test_location:BEQ found_soli
JSR set_only_ramsel
TYA:EOR swr_byte_value2:EOR #&23: STA test_location:STY &FE30:CMP test_location:BEQ found_ram_sel
JSR set_only_romsel
TYA:EOR swr_byte_value2:EOR #&34: STA test_location:STY &FE30:CMP test_location:BEQ found_rom_sel
\ which leaves the watford rom/ram method

.found_watford_romram
LDA #6:STA swr_type
JMP end

.found_rom_sel
LDA #2:STA swr_type
JMP end

.found_ram_sel
LDA #3:STA swr_type
JMP end

.found_soli LDA #4:STA swr_type
LDA swr_test,Y:CMP #1:BNE soli_3bits
INC swr_type
JMP end
.soli_3bits
\ remove factor 2 from solidisk's incomplete address decoding
LSR swr_banks
JMP end

.end
\ restore swr, using method found
LDY #0
\ note that we must write in low to high order for swr_type=4, or only

\   copy for Y=8-15 in that case.
.restore_lp
JSR set_all
LDA swr_backup,Y:STA test_location
INY:CPY #16:BCC restore_lp
.end2
LDA &F4:STA &FE30:CLI
RTS

\ Utilities

.set_only_solidisk
JSR set_all_to_wrong_bank
.set_solidisk \ for old solidisk swr
LDX #&0F:STX &FE62 \ user port -> output
STY &FE60 \ user port output = A
RTS

.set_only_romsel
JSR set_all_to_wrong_bank
.set_romsel
STY &FE30:RTS

\ RAMSEL may not exist, in which case it is equivalent to ROMSEL
\ (incomplete address decoding), therefore this code:
.set_only_ramsel
JSR set_all_to_wrong_bank
JSR set_ramsel
\ now set ROMSEL to the wrong bank, if ROMSEL = RAMSEL, RAMSEL will be deselected too.
TYA:EOR #1:TAY:JSR set_romsel:TYA:EOR #1:TAY
RTS
.set_ramsel
STY &FE32:RTS

.set_only_watford_romram
JSR set_all_to_wrong_bank
.set_watford_romram
STA &FF30,Y:RTS \ write latch set by writing anything to location (FF30+n)

.set_all
JSR set_solidisk:JSR set_romsel:JSR set_ramsel:JMP set_watford_romram

.set_all_to_wrong_bank
TYA:EOR #1:TAY \ this is fine with solidisks incomplete address decoding (bit 3 not used)
JSR set_all
TYA:EOR #1:TAY
RTS

]
NEXT
IF P%-code > 512 PRINT"Too much code":END

CALL swr_check
PRINT
IF ?swr_banks = 0 PRINT "No sideways RAM found (if there is RAM, it's write protected or uses an unknown write select method)":END

IF ?swr_banks = 1 PRINT "1 RAM bank";
IF ?swr_banks > 1 PRINT "";?swr_banks;" RAM banks";

IF ?swr_type = 1 AND ?swr_banks < 16 PRINT ", almost always selected for writing"
IF ?swr_type = 1 AND ?swr_banks = 16 PRINT ", always selected for writing"

IF ?swr_type > 1 PRINT ", write bank is selected with ";
IF ?swr_type = 2 PRINT "FE30 (ROMSEL)"
IF ?swr_type = 3 PRINT "FE32 (RAMSEL)"
IF ?swr_type = 4 PRINT "FE62/FE60 (Solidisk, user port), only 3 bits decoded (0=8)"
IF ?swr_type = 5 PRINT "FE62/FE60 (Solidisk, user port), all 4 bits properly decoded"
IF ?swr_type = 6 PRINT " a write to {FF30+bank_no} (Watford ROM/RAM board)"

FOR R%=0 TO 15
 U%=?(swr_test+R%)
 IF U%>0 PRINT"RAM: ";R%;" used ";U%;" time";
 IF U%>1 PRINT ;"s" ELSE IF U%=1 PRINT
NEXT
END






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
LDA #0
STA ram_bank_count



LDY #15
.SFTODOX1
JSR select_y_and_check_entry
BNE SFTODOX2

.SFTODOX2
DEY
BPL SFTODOX1




:
\ We do an initial search not modifying anything except binary_version_number
\ so we don't damage anything if the hardware can write to a bank which isn't
\ currently selected.
LDY #15
.initial_write_loop
JSR select_y
JSR check_entry
BNE .initial_write_skip
LDA binary_version_number
STA original_binary_version_number,Y
TYA
EOR #&F0 \ make sure there's always a change, even if Y=0
EOR binary_version_number
STA changed_binary_version_number,Y
STA binary_version_number
.initial_write_skip
DEY
BPL initial_write_loop
LDY #15
.initial_read_loop
JSR select_y
JSR check_entry
BNE .initial_read_skip
LDA binary_version_number
CMP original_binary_version_number,Y
BEQ not_ram
SFTODO
.initial_read_skip
DEY
BPL initial_read_loop









:
\ Try storing some distinct data in each bank which doesn't have a language or
\ service entry.
LDY #15
.prime_swr_loop
STY romsel_copy
STY romsel
LDA paged_rom_table,Y
AND #&C0
BNE prime_skip_bank
LDX #0
.copy_loop
TYA
EOR code%,X
STA &8100,X
DEX
BNE copy_loop
.prime_skip_bank
DEY
BPL prime_swr_loop
:
\ Now see if we can retrieve that distinct data from all those banks.
LDY #15
.detect_swr_loop
STY romsel_copy
STY romsel
LDA paged_rom_table,Y
AND #&C0
BNE detect_skip_bank
LDX #0
.check_loop
TYA
EOR code%,X
\ Note that the value we're looking for was just created in A inside the CPU,
\ not read from memory, which I hope will prevent any weird buffering effects
\ causing false positives.
CMP &8100,X
BNE not_ram
DEX
BNE check_loop
\ It's RAM.
LDX ram_bank_count
CPX #max_ram_bank_count
BEQ done
TYA
STA ram_bank_list,X
INC ram_bank_count
.not_ram
.detect_skip_bank
DEY
BPL detect_swr_loop
:
.done
PLA
STA romsel_copy
STA romsel
RTS
:
.variable_data
]
NEXT
REM Try to ensure the data we use in the RAM test varies from run to run; it
REM doesn't really matter.
!variable_data=TIME
CALL detect_swr
IF ?ram_bank_count = 0 THEN PROCdie("Sorry, no sideways RAM or second"+CHR$(13)+CHR$(10)+"processor detected.")
REM SFTODO: I'm not happy with the visual presentation here but let's get it working first.
PRINT "Will use ";16*?ram_bank_count;"K of sideways RAM (bank";
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
