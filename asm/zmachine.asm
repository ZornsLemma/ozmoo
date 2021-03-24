; z_extended_opcode 	!byte 0
; z_operand_count		!byte 0
; z_operand_type_arr  !byte 0, 0, 0, 0, 0, 0, 0, 0
; z_operand_value_high_arr  !byte 0, 0, 0, 0, 0, 0, 0, 0
; z_operand_value_low_arr   !byte 0, 0, 0, 0, 0, 0, 0, 0
; z_local_var_count	!byte 0
; z_temp				!byte 0, 0, 0, 0, 0
z_rnd_a				!byte 123
z_rnd_b				!byte 75
z_rnd_c				!byte 93
z_rnd_x				!byte 1
z_rnd_mode 			!byte 0
!ifdef Z4PLUS {
z_interrupt_return_value !byte 0,0
}
!ifdef Z5PLUS {
z_font				!byte 1, 1
}
!ifdef DEBUG {
z_test				!byte 0
z_test_mode_print = 1
z_test_mode_print_and_store = 2
}

; opcount0 = 0
; opcount1 = 16
; opcount2 = 32
; opcountvar = 64
; opcountext = 96

; Entered with A containing new z_exe_mode value; Z flag should reflect A.
set_z_exe_mode
	sta z_exe_mode
	beq .normal
	lda #<not_normal_exe_mode
	sta jmp_main_loop + 1
	lda #>not_normal_exe_mode
	bne .finish ; Always branch
.normal
	lda #<main_loop_normal_exe_mode
	sta jmp_main_loop + 1
	lda #>main_loop_normal_exe_mode
.finish
	sta jmp_main_loop + 2
	rts

; These get zeropage addresses in constants.asm:
; z_opcode 
; z_opcode_number
; z_opcode_opcount ; 0 = 0OP, 1=1OP, 2=2OP, 3=VAR

!ifdef Z5PLUS {
z_opcode_extended = 190
z_opcode_call_vn2 = 250
}
!ifdef Z4PLUS {
z_opcode_call_vs2 = 236
}

z_opcode_opcount_0op = 0
z_opcode_opcount_1op = 16
z_opcode_opcount_2op = 32
z_opcode_opcount_var = 64
z_opcode_opcount_ext = 96

z_exe_mode_normal = $0
z_exe_mode_return_from_read_interrupt = $80
!ifndef ACORN {
z_exe_mode_exit = $ff
}


!zone z_execute {

not_normal_exe_mode
!ifdef Z4PLUS {
!ifdef VMEM { ; Non-VMEM games can't be restarted, so they don't get z_exe_mode_exit and don't need this code.
!ifndef ACORN { ; ACORN builds never use z_exe_mode_exit and don't need this code.
	lda z_exe_mode
	bne .return_from_z_execute
}
}
}
	lda #z_exe_mode_normal
	jsr set_z_exe_mode ; SFTODO: COULD BE JMP AND OMIT FOLLOWING RTS, BUT PROB NOT WORTH THE CONDITIONAL ASSEMBLY FAFF
.return_from_z_execute
	rts

z_execute

!ifdef DEBUG {
; Play high-pitched beep
;	lda #1
;	sta z_operand_value_low_arr
;	jsr z_ins_sound_effect
}

!ifdef DEBUG {
!ifdef PRINTSPEED {
!ifndef ACORN {
	lda #0
	sta ti_variable
	sta ti_variable + 1
	sta ti_variable + 2
	sta object_num
	sta object_num + 1
} else {
    lda #0
    jsr kernal_readtime
    sta .printspeed_last_time
    stx .printspeed_last_time + 1
    sty .printspeed_last_time + 2
    jmp +
.printspeed_last_time
    !byte 0, 0, 0
.printspeed_this_time
    !byte 0, 0, 0
.printspeed_count
    !word 0
+
}
}
}

main_loop_normal_exe_mode

; Timing
!ifdef TIMING {
	lda ti_variable + 1
	ldx ti_variable + 2
	ldy #header_high_mem
	jsr write_header_word
}

!ifdef DEBUG {
!ifdef PRINTSPEED {
!ifndef ACORN {
	lda ti_variable + 2
	cmp #60
	bcc ++
	bne +
	lda ti_variable + 1
	bne +
	lda object_num + 1
	ldx object_num
	jsr printinteger
	jsr comma
	
+	lda #0
	sta ti_variable
	sta ti_variable + 1
	sta ti_variable + 2
	sta object_num
	sta object_num + 1

++	inc object_num
	bne +
	inc object_num + 1
+
} else {
    jsr kernal_readtime
    sta .printspeed_this_time
    stx .printspeed_this_time + 1
    sty .printspeed_this_time + 2
    sec
    sbc .printspeed_last_time ; A is now $a2 from C64 code
    php
    cmp #100 ; Acorn port has 100Hz jiffies, not 60Hz ones
    bcc ++
    bne +
    plp
    txa
    sbc .printspeed_last_time + 1 ; A is now $a1 from C64 code
    bne +++
    lda .printspeed_count + 1
    ldx .printspeed_count
    jsr printinteger
    jsr comma
    php

+   plp
+++ lda .printspeed_this_time
    sta .printspeed_last_time
    lda .printspeed_this_time + 1
    sta .printspeed_last_time + 1
    lda .printspeed_this_time + 2
    sta .printspeed_last_time + 2
    lda #0
    sta .printspeed_count
    sta .printspeed_count + 1
    php

++  plp
    inc .printspeed_count
    bne +
    inc .printspeed_count + 1
+
}
}
}

!ifdef VICE_TRACE {
	; send trace info to $DE00-$DE02, which a patched
	; version of Vice can use to trace z_pc onto stderr
	; and store on a file. To enable, edit src/c64/c64io.c
	; void c64io_de00_store(uint16_t addr, uint8_t value)
	; if(addr == 0xde01) fprintf(stderr, "%02x", value);
	; if(addr == 0xde02) { fprintf(stderr, "\n"); fflush(stderr); }
	lda z_pc
	sta $de01
	lda z_pc + 1
	sta $de01
	lda z_pc + 2
	sta $de01
	sta $de02
	; send a memory dump if at specific address (e.g. $ad30)
	lda z_pc+1
	cmp #$ad ; $ad
	bne +
	lda z_pc+2
	cmp #$30 ; $30
	bne +
	; dump dynmem
	; first find out how many lines to dump (16 bytes/line)
dumptovice
	ldy #header_static_mem
	jsr read_header_word
	stx .dyndump + 2
	sta .dyndump + 1
	ldx #4
-   lsr .dyndump + 2
	ror .dyndump + 1
	dex
	bne -
	ldy .dyndump + 1
	iny
	lda #<story_start
	sta .dyndump + 1
	lda #>story_start
	sta .dyndump + 2
-   ldx  #0
.dyndump
	lda $8000,x
	sta $de01 ; dump byte
	inx
	cpx #16
	bne .dyndump
	sta $de02 ; newline in dump
	clc
	lda .dyndump + 1
	adc #16
	sta .dyndump + 1
	lda .dyndump + 2
	adc #0
	sta .dyndump + 2
	dey
	bne -
+
}

!ifdef TRACE {
	; Store z_pc to trace page 
	ldx #0
	ldy z_trace_index
-	lda z_pc,x
	sta z_trace_page,y
	iny
	inx
	cpx #3
	bne -
	sty z_trace_index
}

!ifndef CMOS {
	lda #0
	sta z_operand_count
} else {
    stz z_operand_count
}
!ifdef Z4PLUS {	
!ifndef CMOS {
	sta z_temp + 5 ; Signal to NOT read up to four more operands
} else {
	stz z_temp + 5 ; Signal to NOT read up to four more operands
}
}
	+read_next_byte_at_z_pc
	sta z_opcode
	
!ifdef DEBUG {	
	;jsr print_following_string
	;!pet "opcode: ",0
	;ldx z_opcode
	;jsr printx
	;jsr print_following_string
	;!pet " @ ",0
	;ldx z_pc + 2
	;lda z_pc + 1
	;jsr printinteger
	;jsr newline
	;lda z_opcode
}
	bit z_opcode
	bpl .top_bits_are_0x
	bvc .top_bits_are_10

	; Top bits are 11. Form = Variable
	and #%00011111
	sta z_opcode_number
	ldy #z_opcode_opcount_2op
	lda z_opcode
	and #%00100000
	beq + ; This is a 2OP instruction, with up to 4 operands
; This is a VAR instruction
!ifdef Z4PLUS {	
	lda z_opcode
	cmp #z_opcode_call_vs2
!ifdef Z5PLUS {
	beq .get_4_extra_op_types
	cmp #z_opcode_call_vn2
}
	bne .dont_get_4_extra_op_types
.get_4_extra_op_types
	+read_next_byte_at_z_pc
	tax
	dec z_temp + 5 ; Signal to read up to four more operands, and first four operand types are in x
.dont_get_4_extra_op_types
}
	ldy #z_opcode_opcount_var
+	sty z_opcode_opcount
	jmp .get_4_op_types ; Always branch

.top_bits_are_10
!ifdef Z5PLUS {
	cmp #z_opcode_extended
	beq .extended_form
}
	; Form = Short
	and #%00001111
	sta z_opcode_number
	lda z_opcode
	and #%00110000
	cmp #%00110000
	beq .short_0op
	ldx #z_opcode_opcount_1op
	stx z_opcode_opcount
	lsr
	lsr
	lsr
	lsr
	tax
	jsr read_operand
	jmp .perform_instruction
.short_0op
	lda #z_opcode_opcount_0op 
	sta z_opcode_opcount
!ifndef ACORN_SWR { ; SFTODO: Might be worth investigating this, upstream doesn't need it, am I doing something wrong? Can I tweak code ordering to avoid needing this? Thought it only wastes a byte, it has no performance impact.
	beq .perform_instruction ; Always branch
} else {
	jmp .perform_instruction ; Always branch
}

.top_bits_are_0x
	; Form = Long
	and #%00011111
	sta z_opcode_number
	lda #z_opcode_opcount_2op 
	sta z_opcode_opcount
	lda z_opcode
	asl
	asl
	ldx #%00000010
	bcs +
	dex
+	sta z_temp + 3 ; Temporary storage while we jsr
	jsr read_operand
	lda z_temp + 3
	asl
	ldx #%00000010
	bcs +
	dex
+	jsr read_operand
	jmp .perform_instruction

!ifdef Z5PLUS {
.extended_form
	; Form = Extended
	lda #z_opcode_opcount_ext
	sta z_opcode_opcount ; Set to EXT
	+read_next_byte_at_z_pc
	sta z_extended_opcode
	sta z_opcode_number
	jmp .get_4_op_types
}

.get_4_op_types
; If z_temp + 5 = $ff, x holds first byte of arg types and we need to read one more byte and store in z_temp + 4 
	ldy #4 ; index of last possible operand + 1 (4 or 8) 
	sty z_temp
	+read_next_byte_at_z_pc
!ifdef Z4PLUS {
	ldy z_temp + 5
	beq .not_4_extra_args
	sta z_temp + 4
	txa
.not_4_extra_args
}
	
.get_next_op_type
	asl
	bcc .optype_0x
	asl
	bcs .done ; %11
	ldx #%00000010
	bne .store_optype ; Always branch
.optype_0x
	asl
	ldx #%00000000
	bcc .store_optype
	inx
.store_optype
	sta z_temp + 3 ; Temporary storage while we jsr
	jsr read_operand
	lda z_temp + 3
	ldx z_operand_count
	cpx z_temp
	bcc .get_next_op_type

.done

!ifdef Z4PLUS {
	lda z_temp + 5 ; $0 = Don't read more args, $ff = read more args
	beq .perform_instruction
	inc z_temp + 5
	lda z_temp + 4
	ldy #8
	sty z_temp
	bne .get_next_op_type ; Always branch
}
	
.perform_instruction
	lda z_opcode_opcount
	clc
	adc z_opcode_number
!ifdef TRACE {
	ldy z_trace_index
	sta z_trace_page,y
	inc z_trace_index
}
!ifndef UNSAFE {
	cmp #z_number_of_opcodes_implemented
	bcs z_not_implemented
}
	tax 
	lda z_jump_low_arr,x
	sta .jsr_perform + 1
	lda z_jump_high_arr,x
	sta .jsr_perform + 2
.jsr_perform
	jsr $8000
jmp_main_loop
	jmp main_loop_normal_exe_mode ; target patched by set_z_exe_mode_subroutine


z_not_implemented

!ifdef UNSAFE {
	rts
} else {
!ifdef DEBUG {
	jsr print_following_string
!ifndef ACORN {
	!pet "opcode: ",0
} else {
	!text "opcode: ",0
}
	ldx z_opcode
	jsr printx
	jsr print_following_string
	!pet " @ ",0
	ldx z_pc + 2
	lda z_pc + 1
	jsr printinteger
	jsr newline
}
	lda #ERROR_OPCODE_NOT_IMPLEMENTED
	jsr fatalerror
}
}

!zone read_operand {
.operand_is_small_constant
	; Operand is small constant
	tax
	lda #0
	beq .store_operand

read_operand
; Operand type in x - Zero flag must reflect if X is 0 upon entry
	bne .operand_is_not_large_constant
	+read_next_byte_at_z_pc
	sta z_temp + 2 ; Temporary storage while we read another byte
	+read_next_byte_at_z_pc
	tax
	lda z_temp + 2
	jmp .store_operand ; Always branch
.operand_is_not_large_constant
	+read_next_byte_at_z_pc
	cpx #%00000001
	beq .operand_is_small_constant

	; Variable# in a
	cmp #0
	beq .read_from_stack
!ifdef COMPLEX_MEMORY {
	tay
	jsr z_get_variable_reference_and_value
;	jmp .store_operand
} else {
	cmp #16
	bcs .read_global_var
	; Local variable
!ifndef UNSAFE {
	tay
	dey
	cpy z_local_var_count
	bcs .nonexistent_local_SFTODO_HACK
}
	asl ; This clears carry
	tay
	iny
	lda (z_local_vars_ptr),y
	tax
	dey
	lda (z_local_vars_ptr),y
} ; Not COMPLEX_MEMORY

.store_operand
	ldy z_operand_count
	sta z_operand_value_high_arr,y
!ifdef TARGET_C128 {
	txa
	sta z_operand_value_low_arr,y
} else {
	stx z_operand_value_low_arr,y
}
	inc z_operand_count
	rts
.nonexistent_local_SFTODO_HACK jmp .nonexistent_local

.read_from_stack
!ifdef SLOW {
	jsr stack_pull
} else {
	ldy stack_has_top_value
	beq +
	lda stack_top_value
	ldx stack_top_value + 1
	dec stack_has_top_value
	beq .store_operand ; Always branch
+	jsr stack_pull_no_top_value
}
	jmp .store_operand ; Always branch


; SFTODO: IT'S POSSIBLE THAT THERE'S A SAVING TO BE HAD BY RECOGNISING AT RUNTIME OR BUILD TIME THAT ON A BIGDYN BUILD EVEN WITH A SCREEN HOLE, THE GLOBAL VARS MAY WELL LIVE BELOW $8000 AND CAN BE ACCESSED WITHOUT NEEDING TO PAGE IN DYNMEM SWR BANK.
!ifndef COMPLEX_MEMORY {
.read_global_var
	; SFTODO: AS ELSEWHERE, WE COULD MOVE THE ASL FROM BOTH PATHS TO HERE AND NOT BOTHER WITH CMP #128
	cmp #128
	bcs .read_high_global_var
!ifdef SLOW {
	jsr z_get_low_global_variable_value
} else {
	; SFTODO: WOULD IT BE POSSIBLE (ALSO FOR HIGH GLOBAL VARS) FOR US TO MAKE AN ASSEMBLY-TIME DECISION WHETHER OR NOT THE GLOBAL VARS (WHICH ARE AT A KNOWN OFFSET WITHIN THE GAME - IT'S IN THE HEADER, CAN A GAME CHANGE THIS AT RUNTIME!?!?!) CAN EVER COLLIDE WITH THE SCREEN HOLE? WE MIGHT NEED TO BE CAREFUL WHEN DOING ASSEMBLY AT DIFFERENT ADDRESSES TO GENERATE RELOCATIONS, IT MIGHT BE THE BUILD SYSTEM WOULD NEED TO BE IN CHARGE OF "TRYING" THIS, BUT MAYBE WE CAN DO IT JUST HERE IN THE ASSEMBLY (AND PERHAPS THE BUILD SYSTEM COULD COPY A FLAG FROM THE HIGHEST-PAGE BUILD'S OUTPUT TO THE INPUT OF THE LOWER-PAGE INPUT TO MAKE SURE THE TWO BINARIES ARE "THE SAME", OR THE BUILD SYSTEM WOULD PASS IN "ASSUME PAGE IS XXXX (THE HIGH VALUE) FOR DECISIONS LIKE THIS", THAT WOULD PROBABLY BE SIMPLER)
	; SFTODO: OK, AT LEAST ON OZMOO (AND I SUSPECT IN GENERAL), ADDRESS OF GLOBAL VARS IN HEADER IS ONLY READ ONCE ON STARTUP, SO IT CAN'T CHANGE (AND WE THUS KNOW IT AT BUILD TIME)
	; SFTODO: IT MAY ALSO BE POSSIBLE/USEFUL TO TAKE ADVANTAGE OF THE TWO BYTE ASCENDING ACCESS TO AVOID DOING THE MEMORY HOLE CHECK ON BOTH BYTES AND JUST DO IT ONCE
	asl
	tay
	iny
	+before_dynmem_read_corrupt_a
    +lda_dynmem_ind_y z_low_global_vars_ptr
	tax
	dey
    +lda_dynmem_ind_y z_low_global_vars_ptr
	+after_dynmem_read_corrupt_y
}
!ifndef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
!ifdef ACORN_DEBUG_ASSERT {
-	bcs -
}
	bcc .store_operand ; Always branch
} else {
    jmp .store_operand ; SFTODO: carry not guaranteed to be clear, and it's probably too far for bcc anyway
}
.read_high_global_var
	; and #$7f ; Change variable# 128->0, 129->1 ... 255 -> 127 (Pointless, since ASL will remove top bit anyway)
	asl ; This sets carry
	tay
	iny
	+before_dynmem_read_corrupt_a
	+lda_dynmem_ind_y z_high_global_vars_ptr
	tax
	dey
	+lda_dynmem_ind_y z_high_global_vars_ptr
	+after_dynmem_read_corrupt_y
!ifndef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
!ifdef ACORN_DEBUG_ASSERT {
-	bcc -
}
	bcs .store_operand_SFTODO_HACK ; Always branch
} else {
    jmp .store_operand ; SFTODO: carry not guaranteed to be set, and it's (probably) too far for bcs anyway
}
.store_operand_SFTODO_HACK jmp .store_operand
} ; end COMPLEX_MEMORY
; SFTODO: WHERE ARE Z_{HIGH,LOW}_GLOBAL_VARS_PTR SET WRT MY !COMPLEX_MEMORY POSSIBLE SCREEN HOLE HACK? - THESE ARE SET UP IN OZMOO.ASM ON STARTUP AND NEVER CHANGE, SO I SHOULD JUST BE ABLE TO DO ANY ONE-OFF ADJUSTMENT THERE AND NEVER WORRY ABOUT IT AGAIN - *EXCEPT* THAT THERE'S NO REASON THE GLOBAL VARS CAN'T END UP STRADDLING THE SCREEN HOLE, SO I NEED SOME WAY TO HANDLE THAT - PROBABLY NOT THE BEST WAY, BUT POSSIBLY THE BUILD SYSTEM (ALTHOUGH I WAS HOPING TO GET AWAY FROM A FIXED PAGE REQUIREMENT FOR B-NO-SHADOW) COULD DETECT IF THIS WILL HAPPEN AND BUILD WITH (PARTIAL; WE ONLY NEED IT FOR THE GLOBALS) COMPLEX_MEMORY IN THAT CASE - DON'T REALLY LIKE THAT THOUGH - IT MIGHT BE POSSIBLE/ACCEPTABLE TO ADJUST STORY_START AT RUNTIME (PERHAPS INDIRECTLY BY FUDGING THE CHOICE OF RELOCATION TARGET) TO ENSURE THE (I THINK) 512-32 BYTE GLOBAL VAR TABLE *DOESN'T* STRADDLE SCREEN RAM. THAT *MIGHT* BE ACCEPTABLE ON A B IN MODE 7, BUT IT'S NOT VIABLE ON AN ELECTRON IN MODE 6 (OR A B IN MODE 6, IF I SUPPORT THAT, AS I MIGHT WELL DO) - IT'S NOT GREAT EVEN ON A B IN MODE 7, AS IN THE WORST CASE WE COULD LOSE ~1K FROM THIS ADJUSTMENT

!ifndef UNSAFE {
.nonexistent_local
	lda #ERROR_USED_NONEXISTENT_LOCAL_VAR
	jsr fatalerror
} ; Ifdef SLOW {} else
} ; zone read_operand

; These instructions use variable references: inc,  dec,  inc_chk,  dec_chk,  store,  pull,  load

!zone {
z_set_variable_reference_to_value
	; input: Value in a,x.
	;        (zp_temp) must point to variable, possibly using zp_temp + 2 to store bank
	; affects registers: a,x,y,p
!ifdef TARGET_C128 {
	bit zp_temp + 2
	bpl .set_in_bank_0
	ldy #zp_temp
	sty write_word_c128_zp_1
	sty write_word_c128_zp_2
	ldy #0
	jmp write_word_to_bank_1_c128
	; sty $02b9
	; stx zp_temp + 3
	; ldx #$7f
	; ldy #0
	; jsr $02af
	; lda zp_temp + 3
	; iny
	; ldx #$7f
	; jmp $02af
.set_in_bank_0
}
	; SFTODO: THIS IS A RELATIVELY HOT DYNMEM ACCESS (WRT MEM HOLE) - AND SINCE WE ARE ACCESSING TWO BYTES IN ASCENDING ORDER, WE COULD PROBABLY GET SOME BENEFIT (IF IT'S NOT TOO HARD) BY AVOIDING THE MEM HOLE CHECK AND INSERTION FOR THE SECOND WRITE
	+before_dynmem_read_corrupt_y ; SFTODO: I added this but I think it's correct/necessary
!ifndef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
	ldy #0
    +sta_dynmem_ind_y zp_temp
	iny
	txa
    +sta_dynmem_ind_y zp_temp
	+after_dynmem_read_corrupt_a ; SFTODO: I added this but I think it's correct/necessary
	rts
} else {
!zone { ; SFTODO TEMP
SFTODOQQ4
	ldy zp_temp + 1
	cpy acorn_screen_hole_start_page
	bcs .zp_y_not_ok
	ldy #0
	sta (zp_temp),y
	ldy zp_temp
	iny
	beq .zp_y_maybe_no_longer_ok
	txa
	ldy #1
	sta (zp_temp),y
	+after_dynmem_read_corrupt_a ; SFTODO: I added this but I think it's correct/necessary
	rts
.zp_y_not_ok
	; SF: I have forced this case to execute by manually fiddling around with
	; the screen hole location.
	tay
	lda zp_temp + 1
	adc acorn_screen_hole_pages_minus_one ; -1 because carry is set
	sta screen_hole_zp_ptr + 1
	lda zp_temp
	sta screen_hole_zp_ptr
	tya
	ldy #0
	sta (screen_hole_zp_ptr),y
	iny
	txa
	sta (screen_hole_zp_ptr),y
	+after_dynmem_read_corrupt_a ; SFTODO: I added this but I think it's correct/necessary
	rts
.zp_y_maybe_no_longer_ok
	; SF: I have forced this case to execute by changing the beq to this code
	; above to a jmp; this isn't ideal, but it's better than nothing.
	; SFTODO: WE COULD CHECK ZP_TEMP+1 BUT FOR NOW LET'S JUST FALL BACK ON THIS DEFINITELY-OK IF SLOWER THAN NEC CODE
	ldy #1
	txa
	+sta_dynmem_ind_y zp_temp
	; SFTODO: Make this next one slow? Seems a bit "unfair", what about the game+machine that happens to hit this case?
	+after_dynmem_read_corrupt_a ; SFTODO: I added this but I think it's correct/necessary
	rts
}
}

.find_global_var_SFTODO_HACK jmp .find_global_var
.nonexistent_local_SFTODO_HACK jmp .nonexistent_local
z_get_variable_reference_and_value
	; input: Variable in y
	; output: Address is stored in (zp_temp), bank may be stored in zp_temp + 2
	;         Value in a,x
	; affects registers: p
	cpy #0
	bne +
	; Find on stack
!ifdef TARGET_C128 {
	ldx #0
	stx zp_temp + 2
}
	jsr stack_get_ref_to_top_value
	stx zp_temp
	sta zp_temp + 1
	jmp z_get_referenced_value ; SFTODO: WRT MEM HOLE, WE KNOW IT'S ON STACK HERE - ALTHOUGH THIS HARDLY EVER GETS EXECUTED, BUT IF IT'S "FREE" TO OPTIMISE THIS, MIGHT AS WELL
+	tya
	cmp #16
	bcs .find_global_var_SFTODO_HACK
	; Local variable
!ifdef TARGET_C128 {
	ldx #0
	stx zp_temp + 2
}
	dey
!ifndef UNSAFE {
	cpy z_local_var_count
	bcs .nonexistent_local_SFTODO_HACK
}
	asl ; Also clears carry
	adc z_local_vars_ptr
	sta zp_temp
	lda z_local_vars_ptr + 1
	adc #0
	sta zp_temp + 1

!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
	; We would normally fall through into z_get_referenced_value here, but
	; that's slow when we're doing memory hole shenanigans and here we know
	; we're accessing a value on the stack, so we inline the standard version.
	; There's no value in having this separate code if we don't have a memory
	; hole in dynamic memory; it just wastes a bit of memory on redundant code.
z_get_referenced_value_simple
	ldy #1
	+before_dynmem_read_corrupt_a
	lda (zp_temp),y
	tax
	dey
	lda (zp_temp),y
	+after_dynmem_read_corrupt_y
	rts
}

z_get_referenced_value
!ifdef TARGET_C128 {
	bit zp_temp + 2
	bpl .in_bank_0
	lda #zp_temp
	ldy #0
	jmp read_word_from_bank_1_c128
	; sta $02aa
	; ldx #$7f
	; ldy #0
	; jsr $02a2
	; pha
	; iny
	; ldx #$7f
	; jsr $02a2
	; tax
	; pla
	; rts
.in_bank_0
}
!ifndef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
	ldy #1
	+before_dynmem_read_corrupt_a
	+lda_dynmem_ind_y zp_temp
	tax
	dey
	+lda_dynmem_ind_y zp_temp
	+after_dynmem_read_corrupt_y
	rts
} else {
!zone { ; SFTODO!?
	; Many calls to this code are to access stack variables, which can be
	; accessed without worrying about the memory hole. SFTODO: SEARCH AND REPLCAE "MEMORY HOLE" TO "SCREEN HOLE" EVERYWHERE
	lda zp_temp + 1
	cmp #>story_start
	bcc z_get_referenced_value_simple
	; SFTODO: IS IT STILL WORTH OPTIMISING THE REMAINING CASES? QUITE POSSIBLY IT IS...
	+before_dynmem_read_corrupt_a_slow
	; SFTODO: THIS CODE MIGHT BE USABLE (FACTORED OUT AS A MACRO) FOR GLOBAL VAR ACCESS TOO
	lda zp_temp + 1
!if 0 { ; SFTODO: ON BENCHMARK 2/3 OF CALLS TO THIS CODE ARE FOR STACK VARS
	; SFTODO START TEMP MEASUREMENT HACK
	cmp #>story_start
	bcs SFTODOTEMP
	nop
SFTODOTEMP
	; SFTODO END TEMP MEASUREMENT HACK
}
	cmp acorn_screen_hole_start_page
	bcs .zp_y_not_ok
	ldy #0
	lda (zp_temp),y
	sta screen_hole_tmp
	ldy zp_temp
	iny
	beq .zp_y_maybe_no_longer_ok
	ldy #1
	lda (zp_temp),y
	tax
	lda screen_hole_tmp
	+after_dynmem_read_corrupt_y_slow
	rts
.zp_y_not_ok
	; SF: I have forced this case to execute by manually fiddling around with
	; the screen hole location.
	adc acorn_screen_hole_pages_minus_one ; -1 as carry is set
	sta screen_hole_zp_ptr + 1
	lda zp_temp
	sta screen_hole_zp_ptr
	ldy #1
	lda (screen_hole_zp_ptr),y
	tax
	dey
	lda (screen_hole_zp_ptr),y
	+after_dynmem_read_corrupt_y_slow
	rts
.zp_y_maybe_no_longer_ok
	; SF: I have forced this case to execute by changing the beq to this code
	; above to a jmp; this isn't ideal, but it's better than nothing.
	; SFTODO: IT MIGHT BE OK IF WE CHECK HIGH BYTE, BUT LET'S JUST DO THIS FOR NOW
	ldy #1
	+lda_dynmem_ind_y zp_temp
	tax
	lda screen_hole_tmp
	+after_dynmem_read_corrupt_y_slow
	rts
}
} ; end ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE

.find_global_var
	ldx #0
	stx zp_temp + 1
!ifdef TARGET_C128 {
	dex
	stx zp_temp + 2 ; Value $ff, meaning bank = 1
}
	asl
	rol zp_temp + 1
	adc z_low_global_vars_ptr ; Carry is already clear after rol
	sta zp_temp
	lda zp_temp + 1
	adc z_low_global_vars_ptr + 1
	sta zp_temp + 1
	jmp z_get_referenced_value

!ifndef UNSAFE {
.nonexistent_local
	lda #ERROR_USED_NONEXISTENT_LOCAL_VAR
	jsr fatalerror
}

!ifdef Z3 {
	Z3_OR_SLOW = 1
} else {
	!ifdef SLOW {
		Z3_OR_SLOW = 1
	}
}
!ifdef Z3_OR_SLOW { ; SFTODO: I ADDED THIS, I THINK IT'S RIGHT, IF SO UPSTREAM IT
; z_get_variable_value
z_get_low_global_variable_value
	; Read global var 0-111
	; input: a = variable# + 16 (16-127)
	asl ; Clears carry
	tay
!ifdef TARGET_C128 {
	lda #z_low_global_vars_ptr
	jmp read_word_from_bank_1_c128
	; sta $02aa
	; ldx #$7f
	; jsr $02a2
	; pha
	; iny
	; ldx #$7f
	; jsr $02a2
	; tax
	; pla
	; rts
} else {
	; Not TARGET_C128
	iny
	+before_dynmem_read_corrupt_a_slow
	+lda_dynmem_ind_y_slow z_low_global_vars_ptr
	tax
	dey
	+lda_dynmem_ind_y_slow z_low_global_vars_ptr
	+after_dynmem_read_corrupt_y_slow
	; SFTODO: Permanent comment if true - caller *doesn't* assume carry clear if ACORN_SWR_BIG_DYNMEM, so we're OK (also need to describe MEDIUM situation in this comment)
	rts ; Note that caller may assume that carry is clear on return!
} ; End else - Not TARGET_C128
}


; Used by z_set_variable
.write_to_stack
	jmp stack_push
	
z_set_variable
	; Value in a,x
	; Variable in y
	; affects: a, x, y
	cpy #0
	beq .write_to_stack	
	sta z_temp
	stx z_temp + 1
	tya
!ifdef SLOW {
	jsr z_get_variable_reference_and_value
	lda z_temp
	ldx z_temp + 1
	jmp z_set_variable_reference_to_value
} else {
	cmp #16
	bcs .write_global_var
	; Local variable
	tay
	dey
!ifndef UNSAFE {
	cpy z_local_var_count
	bcs .nonexistent_local
}
	asl
	tay
!if 0 { ; SFTODO TEMP HACK
	lda z_local_vars_ptr + 1
	cmp #$7b
HANG	bcs HANG
}
	lda z_temp
	sta (z_local_vars_ptr),y
	iny
	lda z_temp + 1
	sta (z_local_vars_ptr),y
	rts
.write_global_var
	; SFTODO: NOT HUGE, BUT IF WE DID THE ASL NOW (WHICH OCCURS ON BOTH PATHS ANYWAY) WE COULD AVOID DOING THE CMP #128
	cmp #128
	bcs .write_high_global_var
	asl
	tay
	+before_dynmem_read_corrupt_a_slow ; SFTODO: I added this but I think it's correct/necessary
	lda z_temp
	+sta_dynmem_ind_y_slow z_low_global_vars_ptr
	iny
	lda z_temp + 1
	+sta_dynmem_ind_y_slow z_low_global_vars_ptr
	+after_dynmem_read_corrupt_a_slow ; SFTODO: I added this but I think it's correct/necessary
	rts
.write_high_global_var
;	and #$7f ; Change variable# 128->0, 129->1 ... 255 -> 127 ; Pointless, since ASL will remove top bit
	asl
	tay
	+before_dynmem_read_corrupt_a_slow ; SFTODO: I added this but I think it's correct/necessary
	lda z_temp
	+sta_dynmem_ind_y_slow z_high_global_vars_ptr
	iny
	lda z_temp + 1
	+sta_dynmem_ind_y_slow z_high_global_vars_ptr
	+after_dynmem_read_corrupt_a_slow ; SFTODO: I added this but I think it's correct/necessary
	rts
} ; Not SLOW
} ; Zone


!zone {
z_ins_not_supported
	ldy #>.not_supported_string
	lda #<.not_supported_string
	jmp printstring
.not_supported_string
!raw "[Not supported]",13,0
	rts
}

!zone z_division {
z_divide
	; input: Dividend in arg 0, divisor in arg 1, y = signed? 0 = unsigned, $ff = signed
	; output: result in division_result (low byte, high byte)
!ifndef UNSAFE {
	lda z_operand_value_high_arr + 1
	ora z_operand_value_low_arr + 1
	bne .not_div_by_0
	lda #ERROR_DIVISION_BY_ZERO
	jsr fatalerror
.not_div_by_0	
}
	cpy #0
	beq .div_unsigned
	lda z_operand_value_high_arr
	eor z_operand_value_high_arr + 1
	sta zp_temp + 2 ; Top bit: 1 = Result is negative, other bits must be ignored
	; Get 2-complement of dividend, if negative
	lda z_operand_value_low_arr
	bit z_operand_value_high_arr
	bpl +
	; It's negative!
	eor #$ff
	clc
	adc #1
	tax
	lda z_operand_value_high_arr
	eor #$ff
	adc #0
	jmp ++
.div_unsigned
	sty zp_temp + 2 ; Top bit: 1 = Result is negative, other bits must be ignored
	lda z_operand_value_low_arr
+	tax
	lda z_operand_value_high_arr
++	stx dividend
	sta dividend + 1
	; Get 2-complement of divisor, if negative
	lda z_operand_value_low_arr + 1
	cpy #0
	beq + ; Unsigned div, no sign inveversion
	bit z_operand_value_high_arr + 1
	bpl +
	; It's negative!
	eor #$ff
	clc
	adc #1
	tax
	lda z_operand_value_high_arr + 1
	eor #$ff
	adc #0
	jmp ++
+	tax
	lda z_operand_value_high_arr + 1
++	stx divisor
	sta divisor + 1
	; Perform the division
	jsr divide16
	; Inverse sign if applicable. 
	bit zp_temp + 2
	bpl +
	; Inverse sign of result
	lda division_result
	eor #$ff
	clc
	adc #1
	sta division_result
	lda division_result + 1
	eor #$ff
	adc #0
	sta division_result + 1
+	rts
}

!zone {
calc_address_in_byte_array
	; output: z_address is set
	lda z_operand_value_low_arr
	clc
	adc z_operand_value_low_arr + 1
	tax
	lda z_operand_value_high_arr
	adc z_operand_value_high_arr + 1
	jmp set_z_address
}

!zone rnd {
z_rnd_init_random
	; in: Nothing
!ifndef ACORN {
	lda $dc04
	eor #%10101010
	eor z_rnd_a
	tay
	lda $dc05
	eor #%01010101
	eor z_rnd_b
	tax
	lda $d41b
	eor $d012
	eor z_rnd_c
} else {
    lda #osword_read_clock
    ldx #<z_temp
    ldy #>z_temp
    jsr osword
    ; z_temp+[34] are unlikely to change so we don't try to include them.
    ; z_temp+3 only advances every 46.6 hours, z_temp+4 only advances every
    ; 497.1 days.
    lda z_rnd_c
    eor z_temp+0
    eor z_temp+1
    tay
    lda z_rnd_b
    eor z_temp+0
    eor z_temp+2
    tax
    lda z_rnd_a
    eor z_temp+1
    eor z_temp+2
}
z_rnd_init
	; in: a,x,y as seed
	sta z_rnd_a
	stx z_rnd_b
	sty z_rnd_c
	eor #$ff
	sta z_rnd_x
z_rnd_number
	inc z_rnd_x
	lda z_rnd_x
	eor z_rnd_c
	eor z_rnd_a
	sta z_rnd_a
	clc
	adc z_rnd_b
	sta z_rnd_b
	lsr
	eor z_rnd_a
	clc
	adc z_rnd_c
	sta z_rnd_c
	rts
}

!zone z_instructions {
; 0OP instructions
z_ins_rtrue
	lda #0
	ldx #1
	jmp stack_return_from_routine

z_ins_rfalse
	lda #0
	tax
	jmp stack_return_from_routine

; z_ins_print (moved to text.asm)

; z_ins_print_ret (moved to text.asm)

; z_ins_nop is part of 1OP z_ins_inc

; z_ins_catch (moved to stack.asm)

z_ins_quit
!ifndef ACORN {
!ifdef TARGET_MEGA65 {
	; TODO: how to reset without activating autoboot?
}
	jsr printchar_flush
	jsr show_more_prompt
	jmp kernal_reset
} else {
    +clean_up_and_quit_inline ; never returns
}

; z_ins_restart (moved to disk.asm)
	
z_ins_ret_popped
	jsr stack_pull
	jmp stack_return_from_routine
	
;z_ins_pop
;	jmp stack_pull
	
; z_ins_new_line (moved to text.asm)

; z_ins_show_status (moved to screen.asm)

; z_ins_verify has no implementation, jump table points to make_branch_true instead.

; z_ins_extended needs no implementation

; z_ins_piracy jumps directly to make_branch_true

; 1OP instructions

; z_ins_jz placed later to allow relative jumps

; z_ins_get_sibling (moved to objecttable.asm)

; z_ins_get_child (moved to objecttable.asm)

; z_ins_get_parent (moved to objecttable.asm)

; z_ins_get_prop_len (moved to objecttable.asm)

z_ins_inc
	ldy z_operand_value_low_arr
	jsr z_get_variable_reference_and_value
	inx
	bne +
	clc
	adc #1
+	jmp z_set_variable_reference_to_value
	
z_ins_dec
	ldy z_operand_value_low_arr
	jsr z_get_variable_reference_and_value
	dex
	cpx #255
	bne +
	sec
	sbc #1
+	jmp z_set_variable_reference_to_value
	
; z_ins_print_addr (moved to text.asm)
	
; z_ins_remove_obj (moved to objecttable.asm)

; z_ins_print_obj (moved to objecttable.asm)

z_ins_ret
	lda z_operand_value_high_arr
	ldx z_operand_value_low_arr
	jmp stack_return_from_routine

z_ins_jump
	lda #0
	bit z_operand_value_high_arr
	bpl +
	lda #$ff
+	sta zp_temp
	lda z_operand_value_high_arr
	sta zp_temp + 1
	lda z_operand_value_low_arr
	sta zp_temp + 2
	jmp z_jump_to_offset_in_zp_temp

; z_ins_print_paddr (moved to text.asm)

z_ins_load
	ldy z_operand_value_low_arr
	jsr z_get_variable_reference_and_value
	jmp z_store_result

z_ins_not
	lda z_operand_value_low_arr
	eor #$ff
	tax
	lda z_operand_value_high_arr
	eor #$ff
	jmp z_store_result

; z_ins_jz moved to after z_ins_jl to allow relative branching	

; 2OP instructions
	
z_ins_jl
	lda z_operand_value_low_arr
.jl_comp
	cmp z_operand_value_low_arr + 1
	lda z_operand_value_high_arr
	sbc z_operand_value_high_arr + 1
	bvc +
	eor #$80
+	bpl make_branch_false
	jmp make_branch_true

z_ins_jz
	lda z_operand_value_low_arr
	ora z_operand_value_high_arr
	bne make_branch_false
	jmp make_branch_true
	
z_ins_inc_chk
	jsr z_ins_inc
	jsr z_get_referenced_value
	sta z_operand_value_high_arr
	stx z_operand_value_low_arr
	
z_ins_jg
	lda z_operand_value_low_arr + 1
	cmp z_operand_value_low_arr
	lda z_operand_value_high_arr + 1
	sbc z_operand_value_high_arr
	bvc +
	eor #$80
!ifndef ACORN_ELECTRON_SWR {
+	bmi make_branch_true
	bpl make_branch_false ; Always branch
} else {
    ; Unfortunately the Electron's more verbose ROM paging code means
    ; make_branch_true is out of range of a branch instruction.
	; SFTODO: CHECK THIS STILL TRUE IN FINAL-ISH 5.X PORT
+   bpl make_branch_false
    jmp make_branch_true
}

z_ins_dec_chk
	jsr z_ins_dec
	jsr z_get_referenced_value
	sta z_operand_value_high_arr
	txa
	jmp .jl_comp

z_ins_je
	ldx z_operand_count
	dex
-	lda z_operand_value_low_arr
	cmp z_operand_value_low_arr,x
	bne .je_try_next
	lda z_operand_value_high_arr
	cmp z_operand_value_high_arr,x
	beq make_branch_true
.je_try_next
	dex
	bne -
make_branch_false
	+read_next_byte_at_z_pc
	sta zp_temp + 1
	bit zp_temp + 1
	bvs +
	+read_next_byte_at_z_pc
	sta zp_temp + 2
	bit zp_temp + 1
+	bpl .choose_jumptype
-	rts
make_branch_true
	+read_next_byte_at_z_pc
	sta zp_temp + 1
	bit zp_temp + 1
	bvs + ; 1 byte of branch information
	; 2 bytes of branch information
	+read_next_byte_at_z_pc
	sta zp_temp + 2
	bit zp_temp + 1
+	bpl -
.choose_jumptype
	; We have decided to jump
	bvc .two_byte_jump
	; This is a single byte jump
	lda zp_temp + 1
	and #%00111111
	cmp #2
	bcs .jump_to_single_byte_offset
	; Return value (true or false)
	tax
.return_x
	lda #0
	jmp stack_return_from_routine
.jump_to_single_byte_offset
	sbc #2 ; Carry is already set
	clc
	adc z_pc + 2
	bcc +
	tay
	lda z_pc + 1
	adc #0
	tax
	lda z_pc
	adc #0
	jmp set_z_pc
+	sta z_pc + 2 ; Within same page
	rts
.two_byte_jump
	lda zp_temp + 1
	and #%00111111
	tax
	and #%00100000
	beq +
	; Propagate minus bit
	txa
	ora #%11000000
	sta zp_temp + 1
	lda #$ff
	sta zp_temp
	bne z_jump_to_offset_in_zp_temp ; Always branch
+	stx zp_temp + 1
	lda #0
	sta zp_temp
; two_byte_check_return
	ldx zp_temp + 2
	cpx #2
	bcs z_jump_to_offset_in_zp_temp
	lda zp_temp + 1
	beq .return_x 
z_jump_to_offset_in_zp_temp
	lda z_pc + 2
	clc
	adc zp_temp + 2
	tay
	lda z_pc + 1
	adc zp_temp + 1
	tax
	lda z_pc
	adc zp_temp
	pha
	tya
	sec
	sbc #2
	tay
	bcc +
	pla
	jmp set_z_pc
+	txa
	sbc #0
	tax
	pla
	sbc #0
	jmp set_z_pc

; z_ins_jin (moved to objecttable.asm)

z_ins_test
	lda z_operand_value_low_arr
	and z_operand_value_low_arr + 1
	cmp z_operand_value_low_arr + 1
	bne .test_branch_false
	lda z_operand_value_high_arr
	and z_operand_value_high_arr + 1
	cmp z_operand_value_high_arr + 1
	bne .test_branch_false
	jmp make_branch_true
.test_branch_false
	jmp make_branch_false
	
z_ins_or
	lda z_operand_value_low_arr
	ora z_operand_value_low_arr + 1
	tax
	lda z_operand_value_high_arr
	ora z_operand_value_high_arr + 1
	jmp z_store_result

z_ins_and
	lda z_operand_value_low_arr
	and z_operand_value_low_arr + 1
	tax
	lda z_operand_value_high_arr
	and z_operand_value_high_arr + 1
	jmp z_store_result

; z_ins_test_attr (moved to objecttable.asm)

; z_ins_set_attr (moved to objecttable.asm)
	
; z_ins_clear_attr (moved to objecttable.asm)
	
z_ins_store
	ldy z_operand_value_low_arr
	jsr z_get_variable_reference_and_value
	lda z_operand_value_high_arr + 1
	ldx z_operand_value_low_arr + 1
	jmp z_set_variable_reference_to_value

; z_ins_insert_obj (moved to objecttable.asm)
	
z_ins_loadw_and_storew
	asl z_operand_value_low_arr + 1 
	rol z_operand_value_high_arr + 1
	lda z_operand_value_low_arr
	clc
	adc z_operand_value_low_arr + 1
	tax
	lda z_operand_value_high_arr
	adc z_operand_value_high_arr + 1
	jsr set_z_address
	lda z_opcode_number
	cmp #15 ; Code for loadw
	bne .storew
	jsr read_next_byte
	pha
	jsr read_next_byte ; SFTODO: moderately hot caller of read_next_byte where we know we are reading the next byte in memory not an arbitrary byte
	tax
	pla
	jmp z_store_result
.storew
	lda z_operand_value_high_arr + 2
	jsr write_next_byte
	lda z_operand_value_low_arr + 2
	jsr write_next_byte
z_ins_nop
	rts
	
z_ins_loadb
	jsr calc_address_in_byte_array
	jsr read_next_byte ; SFTODO: fairly hot caller of read_next_byte
	tax
	lda #0
	jmp z_store_result

; VAR instruction, moved here to allow relative jump to error	
z_ins_storeb
	jsr calc_address_in_byte_array
	lda z_operand_value_low_arr + 2
	jmp write_next_byte

; z_ins_get_prop (moved to objecttable.asm)
	
; z_ins_get_prop_addr (moved to objecttable.asm)

; z_ins_get_next_prop (moved to objecttable.asm)

z_ins_add
	lda z_operand_value_low_arr
	clc
	adc z_operand_value_low_arr + 1
	tax
	lda z_operand_value_high_arr
	adc z_operand_value_high_arr + 1
	jmp z_store_result

z_ins_sub
	lda z_operand_value_low_arr
	sec
	sbc z_operand_value_low_arr + 1
	tax
	lda z_operand_value_high_arr
	sbc z_operand_value_high_arr + 1
	jmp z_store_result

.mul_product = memory_buffer ; 5 bytes (4 for product + 1 for last bit)
.mul_inv_multiplicand = memory_buffer + 5 ; 2 bytes

z_ins_mul
	lda #0
	ldy #16
	sta .mul_product
	sta .mul_product + 1
	sta .mul_product + 4
	lda z_operand_value_high_arr
	sta .mul_product + 2
	lda z_operand_value_low_arr
	sta .mul_product + 3
	lda z_operand_value_low_arr + 1
	eor #$ff
	clc
	adc #1
	sta .mul_inv_multiplicand + 1
	lda z_operand_value_high_arr + 1
	eor #$ff
	adc #0
	sta .mul_inv_multiplicand
	; Perform multiplication
.mul_next_iteration
	lda .mul_product + 3
	and #1
	beq .mul_bottom_is_0
	; Bottom bit is 1
	bit .mul_product + 4
	bmi .mul_do_nothing
	; Subtract
	lda .mul_product + 1
	clc
	adc .mul_inv_multiplicand + 1
	sta .mul_product + 1
	lda .mul_product
	adc .mul_inv_multiplicand
	sta .mul_product
	jmp .mul_do_nothing
.mul_bottom_is_0
	; Bottom bit is 0
	bit .mul_product + 4
	bpl .mul_do_nothing
	; Add
	lda .mul_product + 1
	clc
	adc z_operand_value_low_arr + 1
	sta .mul_product + 1
	lda .mul_product
	adc z_operand_value_high_arr + 1
	sta .mul_product
.mul_do_nothing
	clc
	bit .mul_product
	bpl +
	sec
+	ror .mul_product
	ror .mul_product + 1
	ror .mul_product + 2
	ror .mul_product + 3
	ror .mul_product + 4
	dey
	bne .mul_next_iteration
	lda .mul_product + 2
	ldx .mul_product + 3
	jmp z_store_result

z_ins_div
	ldy #$ff
	jsr z_divide
	lda division_result + 1
	ldx division_result
	jmp z_store_result
	
z_ins_mod
	ldy #$ff
	jsr z_divide
	lda remainder
	bit z_operand_value_high_arr
	bmi +
	tax
	lda remainder  + 1
	jmp z_store_result
+	eor #$ff
	clc
	adc #1
	tax
	lda remainder + 1
	eor #$ff
	adc #0
	jmp z_store_result
	
!ifdef Z5PLUS {
z_ins_call_xn
	; If value in argument 0 is 0, set status flag Z to 1, otherwise set to 0
	lda z_operand_value_high_arr
	ora z_operand_value_low_arr
	bne +
	rts
+	ldx z_operand_count
	dex
	ldy #0 ; Don't store result
	tya ; Normal call mode
	jmp stack_call_routine
}
	
; z_ins_set_colour (moved to screenkernal.asm)

; z_ins_throw (moved to stack.asm)

	
; VAR instructions
	
z_ins_call_xs
;	jsr check_for_routine_0_and_store
	lda z_operand_value_high_arr
	ora z_operand_value_low_arr
	bne +
	lda #0
	tax
	jmp z_store_result
+	ldx z_operand_count
	dex
	ldy #1 ; Store result = 1
	lda #z_exe_mode_normal
	jmp stack_call_routine

; VAR storew is implemented in z_ins_loadw_and_storew, under 2OP	

; VAR storeb was moved to 2OP area, to allow for relative jump for error.
	
; z_ins_put_prop (moved to objecttable.asm)
	
; z_ins_read (moved to text.asm)

; z_ins_print_char (moved to text.asm)

z_ins_print_num
	lda z_operand_value_high_arr
	bpl print_num_unsigned 
	ldx z_operand_value_low_arr
	tay
	lda #$2d
	jsr streams_print_output
	txa
	eor #$ff
	clc
	adc #1
	sta z_operand_value_low_arr
	tya
	eor #$ff
	adc #0
	sta z_operand_value_high_arr
print_num_unsigned
	; Sign has been printed, if any. Now print number (0 to 32768)
	lda #10
	sta z_operand_value_low_arr + 1
	lda #0
	sta z_operand_value_high_arr + 1
	; Divide by 10 up to four times
	ldy #0
	sty z_temp
-	lda z_operand_value_low_arr
	cmp #10
	bcs +
	tax
	lda z_operand_value_high_arr
	beq .done_dividing
+	ldy #0
	jsr z_divide
	lda remainder
	ldy z_temp
	sta z_temp + 1,y
	inc z_temp
	lda division_result
	sta z_operand_value_low_arr
	lda division_result + 1
	sta z_operand_value_high_arr
	jmp -
.done_dividing
	ldy z_temp
	txa
	sta z_temp + 1,y
-	lda z_temp + 1,y
	clc
	adc #$30
	jsr streams_print_output
	dey
	bpl -
!ifdef Z5PLUS {
z_ins_set_true_colour
}
	rts

z_ins_random
	lda z_operand_value_high_arr
	beq .random_highbyte_empty
	bpl .random_wordsize
	jmp .random_seed
.random_highbyte_empty
+	lda z_operand_value_low_arr
	bne .random_wordsize
	jmp	.random_seed_0
.random_bytesize
	ldy #1
	sty zp_temp + 2 ; mask
-	lda zp_temp + 2
	cmp z_operand_value_low_arr
	bcs .random_bytesize_found_mask
	sec
	rol zp_temp + 2
	bcc - ; Branch unless the mask is now > $ff (which can't happen)
.random_bytesize_found_mask
-	jsr z_rnd_number
	and zp_temp + 2
	cmp z_operand_value_low_arr
	bcs -
	tax
	inx

!ifdef DEBUG {
	ldy z_test
	beq .rnd_store_bytesize
	stx z_temp + 1
	lda #0
	jsr printinteger
	jsr space
	ldx z_temp + 1
	ldy z_test
	cpy #z_test_mode_print
	bne .rnd_store_bytesize
	rts
.rnd_store_bytesize
}

	jmp z_store_result

.random_wordsize	
	ldy #1
	sty zp_temp + 2 ; lowbyte of mask
	dey
	sty zp_temp + 3 ; highbyte of mask
-	lda zp_temp + 2
	cmp z_operand_value_low_arr
	lda zp_temp + 3
	sbc z_operand_value_high_arr
	bcs .random_found_mask
	sec
	rol zp_temp + 2
	rol zp_temp + 3
	bcc - ; Branch unless the mask is now > $ffff (which can't happen)
.random_found_mask
-	jsr z_rnd_number
	and zp_temp + 3
	tay
	jsr z_rnd_number
	and zp_temp + 2
	tax
	cmp z_operand_value_low_arr
	tya
	sbc z_operand_value_high_arr
	bcs -
; .rnd_store_good_rnd_number
	tya
	inx
	bne +
	adc #1 ; Carry is always clear here, no need for clc
+

!ifdef DEBUG {
	ldy z_test
	beq .rnd_store
	sta z_temp
	stx z_temp + 1
	jsr printinteger
	jsr space
	lda z_temp
	ldx z_temp + 1
	ldy z_test
	cpy #z_test_mode_print
	bne .rnd_store
	rts
.rnd_store
}

	jmp z_store_result

.random_seed_0
!ifndef BENCHMARK {
!ifdef DEBUG {
	ldy z_test
	beq +
	jsr print_following_string
!ifndef ACORN {
	!pet "seed 0!",13,0
} else {
    !text "seed 0!",13,0
}
+	
}
	jsr z_rnd_init_random
	lda #0
	sta z_rnd_mode
	beq .rnd_tax_and_return ; Always branch
}
.random_seed

!ifdef DEBUG {
	ldy z_test
	beq +
	tax
	jsr print_following_string
!ifndef ACORN {
	!pet "seed -1!",13,0
} else {
	!text "seed -1!",13,0
}
	txa
+
}	

	tay
	ldx z_operand_value_low_arr
	clc
	adc #%10101010
	jsr z_rnd_init
	lda #1 ; Predictable sequence
	sta z_rnd_mode
	lda #0
.rnd_tax_and_return
	tax

!ifdef DEBUG {
	ldy z_test
	cpy #z_test_mode_print
	bne .rnd_store_seed
	rts
.rnd_store_seed
}

	jmp z_store_result

; z_ins_push moved to stack.asm
	
; z_ins_pull moved to stack.asm

; z_ins_split_window moved to screen.asm

; z_ins_set_window moved to screen.asm

; z_ins_erase_window moved to screen.asm

; z_ins_erase_line moved to screen.asm

; z_ins_set_cursor moved to screen.asm

; z_ins_get_cursor moved to screen.asm

; z_ins_buffer_mode moved to screen.asm

; z_ins_set_text_style moved to screen.asm
	
; z_ins_output_stream jumps directly to streams_output_stream.

z_ins_sound_effect
!ifndef ACORN {
	lda #$08
} else {
    lda #0 ; B1
}
	ldx z_operand_value_low_arr
	dex
	beq .sound_high_pitched_beep
	dex
	beq .sound_low_pitched_beep
	rts
!ifndef ACORN {
!ifdef HAS_SID {
.sound_high_pitched_beep
	lda #$40
.sound_low_pitched_beep
	sta $d401
	lda #$21
	sta $d404
	ldy #40
--	ldx #0
-	dex
	bne -
	dey
	bne --
	lda #$20
	sta $d404
	rts
} else {
	!ifdef TARGET_PLUS4 {
.sound_high_pitched_beep
	lda #$f2
.sound_low_pitched_beep
	sta ted_voice_2_low
	sta ted_voice_2_high
	lda #32 + 15
	sta ted_volume
	ldy #40
--	ldx #0
-	dex
	bne -
	dey
	bne --
	lda #0 + 15
	sta ted_volume
	rts
	} else {
.sound_high_pitched_beep
.sound_low_pitched_beep
	rts
	}
}
} else {
    ; MOS 6581 SID datasheet says for standard 1.0MHz clock, frequency is given
    ; by Fout = (Fn * 0.0596) Hz. The C64 code uses Fn=$4000->976 Hz for the
    ; high-pitched bleep and Fn=$0800->122 Hz for the low-pitched bleep. Looking
    ; up those frequencies in https://en.wikipedia.org/wiki/Piano_key_frequencies
    ; and correlating that with the table in the BBC User Guide gives the values
    ; used here. This is far too much effort for such a tiny sound. :-)
sound_high_pitched_beep
.sound_high_pitched_beep
    lda #144 ; B4
.sound_low_pitched_beep
    sta .sound_block + 4
    ; SF: ENHANCEMENT: The C64 code won't return until the sound has finished
    ; playing. I am really struggling to find a way to wait until the sound has
    ; played; checking the free space in the sound buffer only tells us it has
    ; started playing as far as I can see from my experiments. It probably
    ; doesn't matter that we return to the caller before the sound has finished,
    ; but I could vaguely imagine a game playing sounds repeatedly in a timed
    ; loop and with this implementation the OS buffer would cause the sounds to
    ; go on a bit longer than desired. In practice the buffer isn't huge and
    ; these sounds are very short and it's almost certainly not going to matter.
    lda #osword_sound
    ldx #<.sound_block
    ldy #>.sound_block
    jmp osword

.sound_block
    !word 1   ; channel
    !word -15 ; amplitude
    !word 0   ; pitch
    !word 1   ; duration (twentieths of a second) - C64 bleep is ~(1/20) sec
}


!ifdef Z4PLUS {
z_ins_scan_table
	lda #$82
	ldx z_operand_count
	cpx #4
	bcc +
	lda z_operand_value_low_arr + 3
+	sta zp_temp ; form (bit 7 = 1 means words, 0 means bytes)
	and #$80
	bne + ; This is word compare, so don't perform the following test
	lda z_operand_value_high_arr
	bne .scan_table_false ; A value > 255 will never be matched by a byte
+	lda zp_temp
	and #$7f
	sta zp_temp + 1 ; entry length (1-127)
	ldx z_operand_value_low_arr + 1
	stx zp_temp + 2 ; Lowbyte of table address
	lda z_operand_value_high_arr + 1
	sta zp_temp + 3 ; Highbyte of table address
.scan_next
	lda z_operand_value_high_arr + 2
	ora z_operand_value_low_arr + 2
	beq .scan_table_false
	lda zp_temp + 3
	ldx zp_temp + 2
	jsr set_z_address
	jsr read_next_byte
	ldx zp_temp
	bpl .scan_byte_compare
	cmp z_operand_value_high_arr
	bne .scan_not_a_match
	jsr read_next_byte
.scan_byte_compare
	cmp z_operand_value_low_arr
	beq .scan_is_a_match
.scan_not_a_match
	; Move to next address in table
	lda zp_temp + 2
	clc
	adc zp_temp + 1
	sta zp_temp + 2
	lda zp_temp + 3
	adc #0
	sta zp_temp + 3
	; Decrease number of entries left
	dec z_operand_value_low_arr + 2
	ldy z_operand_value_low_arr + 2
	cpy #$ff
	bne .scan_next
	dec z_operand_value_high_arr + 2
	jmp .scan_next
.scan_table_false
	lda #0
	tax
	jsr z_store_result
	jmp make_branch_false
.scan_is_a_match
	lda zp_temp + 3
	ldx zp_temp + 2
	jsr z_store_result
	jmp make_branch_true
}

; z_ins_read_char moved to text.asm	

; z_ins_tokenise_text moved to text.asm

; z_ins_encode_text moved to text.asm

!ifdef Z5PLUS {
z_ins_copy_table
	; copy_table first second size 

	lda z_operand_value_low_arr + 1
	ora z_operand_value_high_arr + 1
	bne .copy_table_not_zerofill

	; Fill with zero
	; Copy target table address to ZP vector
	lda z_operand_value_low_arr
	sta string_array
	lda z_operand_value_high_arr
!ifndef COMPLEX_MEMORY {
	clc
	adc #>story_start
}
	sta string_array + 1

	; Perform zero-fill
	ldy #0
	ldx z_operand_value_low_arr + 2
-	txa
	ora z_operand_value_high_arr + 2
	beq .copy_all_done
	lda #0
	+macro_string_array_write_byte
;	sta (zp_temp),y
	iny
	bne +
	inc string_array + 1
+	dex
	cpx #$ff
	bne -
	dec z_operand_value_high_arr + 2
	bpl - ; Always branch
.copy_all_done
	rts ; We are done

.copy_table_not_zerofill

	; Copy target table address to ZP vector
	lda z_operand_value_low_arr + 1
	sta string_array
	lda z_operand_value_high_arr + 1
!ifndef COMPLEX_MEMORY {
	clc
	adc #>story_start
}
	sta string_array + 1

	; If size is negative, we invert it and copy forwards
	ldy z_operand_value_high_arr + 2
	bmi .copy_table_forwards_invert_size
	
	; Choose direction
	lda z_operand_value_low_arr + 1
	cmp z_operand_value_low_arr
	lda z_operand_value_high_arr + 1
	sbc z_operand_value_high_arr
	bcc .copy_table_forwards

	; Copy table backwards
	; Add size - 1 to first
	lda z_operand_value_low_arr
	clc
	adc z_operand_value_low_arr + 2
	tay
	lda z_operand_value_high_arr
	adc z_operand_value_high_arr + 2
	tax
	tya
	sec
	sbc #1
	sta z_operand_value_low_arr
	txa
	sbc #0
	sta z_operand_value_high_arr
	; Add size - 1 to second
	lda string_array
	clc
	adc z_operand_value_low_arr + 2
	tay
	lda string_array + 1
	adc z_operand_value_high_arr + 2
	tax
	tya
	sec
	sbc #1
	sta string_array
	txa
	sbc #0
	sta string_array + 1
	; Store direction
	ldx #$ff
	stx zp_temp + 2
	stx zp_temp + 3
	bne .copy_table_common ; Always branch
	
.copy_table_forwards_invert_size
	lda z_operand_value_low_arr + 2
	sec
	sbc #1
	eor #$ff
	sta z_operand_value_low_arr + 2
	tya
	sbc #0
	eor #$ff
	sta z_operand_value_high_arr + 2

.copy_table_forwards
	ldx #1
	stx zp_temp + 2
	dex
	stx zp_temp + 3

.copy_table_common
	ldx z_operand_value_high_arr
	ldy z_operand_value_low_arr
-	lda z_operand_value_low_arr + 2
	ora z_operand_value_high_arr + 2
!ifdef TARGET_C128 {
	; when -P is used then the branch becomes too far away
	bne +
	jmp .copy_all_done
+
} else {
	beq .copy_all_done
}
	lda #0
	; Read next byte from first table
	jsr read_byte_at_z_address
	; Store byte in second table
	ldy #0
	+macro_string_array_write_byte
;	sta (zp_temp),y
	; Increase/decrease pointer to second
	lda string_array
	clc
	adc zp_temp + 2
	sta string_array
	lda string_array + 1
	adc zp_temp + 3
	sta string_array + 1
	; Increase/decrease pointer to first
	lda z_operand_value_low_arr
	clc
	adc zp_temp + 2
	sta z_operand_value_low_arr
	tay
	lda z_operand_value_high_arr
	adc zp_temp + 3
	sta z_operand_value_high_arr
	tax
	; Decrease # of bytes left to copy
	dec z_operand_value_low_arr + 2
	lda z_operand_value_low_arr + 2
	cmp #$ff
	bne -
	dec z_operand_value_high_arr + 2
	bpl - ; Always branch
}
; z_ins_print_table moved to screen.asm

; z_ins_check_arg_count moved to stack.asm	
	
; EXT instructions

!ifdef Z5PLUS {
z_ins_log_shift
	lda z_operand_value_high_arr + 1
	ora z_operand_value_low_arr + 1
	beq .shift_store
	bit z_operand_value_high_arr + 1
	bpl .left_shift
-	lsr z_operand_value_high_arr
	ror z_operand_value_low_arr
	inc z_operand_value_low_arr + 1
	bne -
	beq .shift_store ; Always branch
.left_shift
	asl z_operand_value_low_arr
	rol z_operand_value_high_arr
	dec z_operand_value_low_arr + 1
	bne .left_shift
.shift_store
	ldx z_operand_value_low_arr
	lda z_operand_value_high_arr
	jmp z_store_result

z_ins_art_shift
	lda z_operand_value_high_arr + 1
	ora z_operand_value_low_arr + 1
	beq .shift_store
	bit z_operand_value_high_arr + 1
	bpl .left_shift
-	clc
	bit z_operand_value_high_arr
	bpl +
	sec
+	ror z_operand_value_high_arr
	ror z_operand_value_low_arr
	inc z_operand_value_low_arr + 1
	bne -
	beq .shift_store ; Always branch

z_ins_set_font
	ldy current_window
	lda z_operand_value_low_arr
	beq .set_font_check_status
	cmp #1
	beq .set_font_do
	cmp #4
	beq .set_font_do
	; Font is unavailable
	lda #0
	tax
	jmp z_store_result
.set_font_do
	ldx z_font,y
	sta z_font,y
	lda #0
	jmp z_store_result
.set_font_check_status	
	ldx z_font,y ; a is already 0
	jmp z_store_result

z_ins_save_restore_undo
	; Return -1 to indicate that this is not supported
	ldx #$ff
	txa
	jmp z_store_result
}

; z_ins_set_true_colour placed at end of VAR z_ins_print_num
	
	
}
