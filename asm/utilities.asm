; various utility functions
; - conv2dec
; - mult16
; - divide16
; - fatalerror
; - enable_interrupts
; - disable_interrupts
; - set_memory_normal
; - set_memory_all_ram
; - set_memory_no_basic
; various debug functions

!ifndef ACORN {
; zero_processorports: ...<d000><e000><a000> on/off
!macro set_memory_all_ram {
    ; Don't forget to disable interrupts first!
    pha
    lda #%00110000 
    sta zero_processorports
    pla
}
!macro set_memory_all_ram_unsafe {
    ; Don't forget to disable interrupts first!
    lda #%00110000 
    sta zero_processorports
}

!macro set_memory_no_basic {
    pha
    lda #%00110110
    sta zero_processorports
    pla
}
!macro set_memory_no_basic_unsafe {
    lda #%00110110
    sta zero_processorports
}

!macro set_memory_normal {
    pha
    lda #%00110111
    sta zero_processorports
    pla
}

; to be expanded to disable NMI IRQs later if needed
!macro disable_interrupts {
    sei 
}

!macro enable_interrupts {
    cli
}
}

!ifdef SLOW {
!ifdef ACORN_SWR {
; SFTODO: This lot is a bit unreadable, I suspect some reordering and/of fallthrough might improve matters

; SF: This must preserve X, but it can corrupt Y; we don't need to return with Y=0.
; SFTODO: I SAY THAT IN A FEW PLACES - I am not longer so sure. I still think it
; practice it's fine but I'm a bit worried about saying this is OK.
read_next_byte_at_z_pc_sub
    jsr read_next_byte_at_z_pc_unsafe_start_sub
    ; Fall through to finish_read_next_byte_at_z_pc_unsafe_sub

; This must preserve A and X.
finish_read_next_byte_at_z_pc_unsafe_sub
!ifndef ACORN_SWR_SMALL_DYNMEM {
    ; We must keep the first bank of sideways RAM paged in by default, because
    ; dynamic memory may have overflowed into it.
    ldy ram_bank_list
    sty romsel_copy
    sty romsel
}
!if 1 { ; SFTODO
    pha
    lda z_pc_mempointer_ram_bank
    cmp romsel_copy
-   bne -
    pla
}
    rts

; SF: This must preserve A and X.
restart_read_next_byte_at_z_pc_unsafe_sub
!ifndef ACORN_SWR_SMALL_DYNMEM {
    ldy z_pc_mempointer_ram_bank
    sty romsel_copy
    sty romsel
}
!if 1 { ; SFTODO
    pha
    lda z_pc_mempointer_ram_bank
    cmp romsel_copy
-   bne -
    pla
}
    rts

; SF: This must preserve X, but it can corrupt Y; we don't need to return with Y=0.
read_next_byte_at_z_pc_unsafe_start_sub
!ifndef ACORN_SWR_SMALL_DYNMEM {
    lda z_pc_mempointer_ram_bank
    sta romsel_copy
    sta romsel
} else {
    ; z_pc_mempointer_ram_bank is kept paged in by default if we don't have to
    ; keep the first bank paged in all the time for dynamic memory.
}
    ; Fall through to read_next_byte_at_z_pc_unsafe_middle_sub
}
!ifdef ACORN_SWR {
!if 1 { ; SFTODO
    pha
    lda z_pc_mempointer_ram_bank
    cmp romsel_copy
-   bne -
    pla
}
}

; SF: This must preserve X, but it can corrupt Y; we don't need to return with Y=0.
!ifndef ACORN_SWR {
read_next_byte_at_z_pc_sub
} else {
read_next_byte_at_z_pc_unsafe_middle_sub
}
!ifdef ACORN_SWR {
!if 1 { ; SFTODO
    pha
    lda z_pc_mempointer_ram_bank
    cmp romsel_copy
-   bne -
    pla
}
}
	ldy #0
	lda (z_pc_mempointer),y
	inc z_pc_mempointer ; Also increases z_pc
	beq ++
	rts
++
!ifndef ACORN_SWR {
    jmp inc_z_pc_page
} else {
    jsr inc_z_pc_page
    ; SFTODO: ACORN_SWR_SMALL_DYNMEM could potentially boost performance quite a
    ; bit, I think. It may be worth not making the relocatable version load
    ; quite so high to maximise the chances of this coming into play.
!ifndef ACORN_SWR_SMALL_DYNMEM {
    ; SFTODO: THAT MIGHT HAVE PAGED IN THE FIRST BANK - WE NEED TO UNDO THAT -
    ; WE ONLY NEED TO DO THIS IF SWR MIGHT BE IN DYNMEM OTHERWISE IT WON'T HAVE
    ; PAGED IT IN... - CAN WE JUST SET A FLAG TO TELL IT NOT TO DO THAT? NOTE
    ; HOWEVER THIS IS A VERY RARE CASE, SO IT MAY WE SIMPLEST AND SAFEST (A
    ; FLAG MAY REMAIN ACCIDENTALLY SET) TO JUST DO THIS, WHICH IS COMPLETELY
    ; CORRECT.
    ldy z_pc_mempointer_ram_bank
    sty romsel_copy
    sty romsel
}
!if 1 { ; SFTODO
    pha
    lda z_pc_mempointer_ram_bank
    cmp romsel_copy
-   bne -
    pla
}
    rts
}

!macro read_next_byte_at_z_pc {
	jsr read_next_byte_at_z_pc_sub
}

!macro read_next_byte_at_z_pc_unsafe_start {
!ifndef ACORN_SWR {
    +read_next_byte_at_z_pc
} else {
    jsr read_next_byte_at_z_pc_unsafe_start_sub
}
}

!macro read_next_byte_at_z_pc_unsafe_middle {
!ifndef ACORN_SWR {
    +read_next_byte_at_z_pc
} else {
    jsr read_next_byte_at_z_pc_unsafe_middle_sub
}
}

!macro finish_read_next_byte_at_z_pc_unsafe {
!ifdef ACORN_SWR {
    jsr finish_read_next_byte_at_z_pc_unsafe_sub
}
}

!macro restart_read_next_byte_at_z_pc_unsafe {
!ifdef ACORN_SWR {
    jsr restart_read_next_byte_at_z_pc_unsafe_sub
}
}
	
} else { ; not SLOW SFTODO: THIS NEEDS UPDATING FOR NEW MODEL

; SF: This must preserve X, but it can corrupt Y; we don't need to return with Y=0.
!macro read_next_byte_at_z_pc {
!ifdef ACORN_SWR {
    lda z_pc_mempointer_ram_bank
    sta romsel_copy
    sta romsel
}
	ldy #0
	lda (z_pc_mempointer),y
	inc z_pc_mempointer ; Also increases z_pc
	bne ++
	jsr inc_z_pc_page
++
!ifdef ACORN_SWR {
!ifndef ACORN_SWR_SMALL_DYNMEM {
    ; We must keep the first bank of sideways RAM paged in by default, because
    ; dynamic memory may have overflowed into it.
    ldy ram_bank_list
    sty romsel_copy
    sty romsel
    ldy #76 ; SFTODO JUST TO PROVE IT'S OK ldy #0
}
}
}	

}

ERROR_UNSUPPORTED_STREAM = 1
ERROR_CONFIG = 2
ERROR_STREAM_NESTING_ERROR = 3
ERROR_FLOPPY_READ_ERROR = 4
ERROR_MEMORY_OVER_64KB = 5
ERROR_STACK_FULL = 6
ERROR_STACK_EMPTY = 7
ERROR_OPCODE_NOT_IMPLEMENTED = 8
ERROR_USED_NONEXISTENT_LOCAL_VAR = 9
ERROR_BAD_PROPERTY_LENGTH = 10
ERROR_UNSUPPORTED_STORY_VERSION = 11
ERROR_OUT_OF_MEMORY = 12
ERROR_WRITE_ABOVE_DYNMEM = 13
ERROR_READ_ABOVE_STATMEM = 14
ERROR_TOO_MANY_TERMINATORS = 15
ERROR_NO_VMEM_INDEX = 16

; SFTODO: General point - there's lots of !pet stuff in debug-only code which
; I haven't ported yet. If ACME allows it, maybe define a !native macro which
; wraps !pet or !text depending on whether ACORN is defined or not.
!ifdef DEBUG {
!ifndef ACORN {
.error_unsupported_stream !pet "unsupported stream#",0
.error_config !pet "broken config",0
.error_stream_nesting_error !pet "stream nesting error",0
.error_floppy_read_error !pet "floppy read error", 0
.error_memory_over_64kb !pet "tried to access z-machine memory over 64kb", 0
.error_stack_full !pet "stack full",0
.error_stack_empty !pet "stack empty",0
.error_opcode_not_implemented !pet "opcode not implemented!",0
.error_used_nonexistent_local_var !pet "used non-existent local var",0
.error_bad_property_length !pet "bad property length", 0
.error_unsupported_story_version !pet "unsupported story version", 0
.error_out_of_memory !pet "out of memory", 0
.error_write_above_dynmem !pet "tried to write to non-dynamic memory", 0
.error_read_above_statmem !pet "tried to read from himem", 0
.error_too_many_terminators !pet "too many terminators", 0
.error_no_vmem_index !pet "no vmem index found", 0
} else {
.error_unsupported_stream !text "unsupported stream#",0
.error_config !text "broken config",0
.error_stream_nesting_error !text "stream nesting error",0
.error_floppy_read_error !text "floppy read error", 0
.error_memory_over_64kb !text "tried to access z-machine memory over 64kb", 0
.error_stack_full !text "stack full",0
.error_stack_empty !text "stack empty",0
.error_opcode_not_implemented !text "opcode not implemented!",0
.error_used_nonexistent_local_var !text "used non-existent local var",0
.error_bad_property_length !text "bad property length", 0
.error_unsupported_story_version !text "unsupported story version", 0
.error_out_of_memory !text "out of memory", 0
.error_write_above_dynmem !text "tried to write to non-dynamic memory", 0
.error_read_above_statmem !text "tried to read from himem", 0
.error_too_many_terminators !text "too many terminators", 0
.error_no_vmem_index !text "no vmem index found", 0
}

.error_message_high_arr
    !byte >.error_unsupported_stream
    !byte >.error_config
    !byte >.error_stream_nesting_error
    !byte >.error_floppy_read_error
    !byte >.error_memory_over_64kb
    !byte >.error_stack_full
    !byte >.error_stack_empty
    !byte >.error_opcode_not_implemented
    !byte >.error_used_nonexistent_local_var
    !byte >.error_bad_property_length
    !byte >.error_unsupported_story_version
    !byte >.error_out_of_memory
    !byte >.error_write_above_dynmem
    !byte >.error_read_above_statmem
    !byte >.error_too_many_terminators
    !byte >.error_no_vmem_index

.error_message_low_arr
    !byte <.error_unsupported_stream
    !byte <.error_config
    !byte <.error_stream_nesting_error
    !byte <.error_floppy_read_error
    !byte <.error_memory_over_64kb
    !byte <.error_stack_full
    !byte <.error_stack_empty
    !byte <.error_opcode_not_implemented
    !byte <.error_used_nonexistent_local_var
    !byte <.error_bad_property_length
    !byte <.error_unsupported_story_version
    !byte <.error_out_of_memory
    !byte <.error_write_above_dynmem
    !byte <.error_read_above_statmem
    !byte <.error_too_many_terminators
    !byte <.error_no_vmem_index
}

fatalerror
    ; prints the error, then resets the computer
    ; input: a (error code)
    ; side effects: resets the computer
	sta z_temp + 11
!ifndef DEBUG {
    pha
!ifndef ACORN {
    +set_memory_normal
}
    ldy #>.fatal_error_string
	lda #<.fatal_error_string
	jsr printstring
    pla
    tax
    lda #0
    jsr printinteger
    lda #$0d
    jsr streams_print_output
    jsr printchar_flush
!ifndef ACORN {
    jsr kernal_readchar   ; read keyboard
    jmp kernal_reset      ; reset
} else {
-   jmp -
}
.fatal_error_string !pet "fatal error: ",0
} else {
    pha
    jsr print_following_string
!ifndef ACORN {
    !pet "fatal error ", 0
} else {
    !text "fatal error ", 0
}
    pla
    tax
    dex
    jsr printa
    jsr colon
    jsr space
    lda .error_message_high_arr,x
    tay
    lda .error_message_low_arr,x
    jsr printstring
    jsr newline
    jsr print_trace
    jsr printchar_flush
!ifndef ACORN {
    jsr kernal_readchar   ; read keyboard
    jmp kernal_reset      ; reset
} else {
-   jmp -
}

.saved_a !byte 0
.saved_x !byte 0
.saved_y !byte 0

space
    ; subroutine: print space
    ; input: 
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    lda #$20
    jsr streams_print_output
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

comma
    ; subroutine: print comma
    ; input: 
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    lda #44
    jsr streams_print_output
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

dollar
    ; subroutine: print dollar
    ; input: 
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    lda #36
    jsr printchar_buffered
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

colon
    ; subroutine: print colon
    ; input: 
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    lda #58
    jsr streams_print_output
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

arrow
    ; subroutine: print ->
    ; input: 
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    lda #$2d
    jsr streams_print_output
    lda #$3e
    jsr streams_print_output
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts


newline
    ; subroutine: print newline
    ; input: 
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    lda #$0d
    jsr streams_print_output
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

printx
    ; subroutine: print value stored in x register
    ; input: x
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    lda #$00
    jsr printinteger
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

printy
    ; subroutine: print value stored in y register
    ; input: y
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    tya
    tax
    lda #$00
    jsr printinteger
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

printa
    ; subroutine: print value stored in a register
    ; input: a
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    tax
    lda #$00
    jsr printinteger
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

pause
    ; subroutine: print newline
    ; input: 
    ; output:
    ; used registers:
    ; side effects:
    php
    sta .saved_a
    stx .saved_x
    sty .saved_y
    jsr print_following_string
!ifndef ACORN {
	!pet "[Intentional pause. Press ENTER.]",13,0
} else {
	!text "[Intentional pause. Press ENTER.]",13,0
}
    jsr print_trace
    jsr printchar_flush
!ifndef ACORN {
    jsr kernal_readchar   ; read keyboard
} else {
    jsr osrdch
}
    lda .saved_a
    ldx .saved_x
    ldy .saved_y
    plp
    rts

print_following_string
    ; print text (implicit argument passing)
    ; input: 
    ; output:
    ; used registers: a
    ; side effects:
!zone {
    ; usage:
    ;    jsr print_following_string
    ;    !pet "message",0
    ; uses stack pointer to find start of text, then
    ; updates the stack so that execution continues
    ; directly after the end of the text

    ; store the return address
    ; the address on stack is -1 before the first character
    pla  ; remove LO for return address
    sta .return_address + 1
    pla  ; remove HI for return address
    sta .return_address + 2

    ; print the string
-   inc .return_address + 1
    bne .return_address
    inc .return_address + 2
.return_address
    lda $0000 ; self-modifying code (aaarg! but oh, so efficent)
    beq +
    jsr streams_print_output
    jmp -

    ; put updated return address on stack
+   lda .return_address + 2
    pha 
    lda .return_address + 1
    pha
    rts
}

print_trace
!ifdef TRACE {
    jsr newline
	jsr print_following_string
!ifndef ACORN {
	!pet "last opcodes: (#, z_pc, opcode)",0
} else {
	!text "last opcodes: (#, z_pc, opcode)",0
}
    jsr newline
	lda z_trace_index
	tay
	and #%11
	cmp #%11
	bne +
	jsr print_following_string
!ifndef ACORN {
	!pet "last opcode not stored (shown as $ee)",13,0
} else {
	!text "last opcode not stored (shown as $ee)",13,0
}
	lda #$ee
	sta z_trace_page,y
	iny
+	tya
	sec
	sbc #40
	tay
	ldx #0
.print_next_op	
	jsr printx
	jsr comma
	jsr dollar
	lda z_trace_page,y
	jsr print_byte_as_hex
	iny
	lda z_trace_page,y
	jsr print_byte_as_hex
	iny
	lda z_trace_page,y
	jsr print_byte_as_hex
	iny
	jsr comma
	jsr dollar
	lda z_trace_page,y
	jsr print_byte_as_hex
	jsr newline
	iny
	inx
	cpx #10
	bcc .print_next_op
	rts
} else {
	rts ; If TRACE is not enabled, there is no trace info to print
}

print_byte_as_hex
	pha
	lda #$ff
	sta .print_bad_code_buffered
	pla

; Must be followed by print_byte_as_hex_primitive

; Must follow print_byte_as_hex
print_byte_as_hex_primitive
	stx .saved_x
	pha
	lsr
	lsr
	lsr
	lsr
	tax
	lda .hex_num,x
	jsr .print_byte_as_hex_one_char
	pla
	pha
	and #$0f
	tax
	lda .hex_num,x
	ldx .saved_x
	jsr .print_byte_as_hex_one_char
	pla
	rts

print_bad_zscii_code_buffered
	pha
	lda #$80
	bne .print_bad_zscii_code_main ; Always branch
print_bad_zscii_code
	pha
	lda #0
.print_bad_zscii_code_main
	sta .print_bad_code_buffered
	lda #$2f ; "/"
	jsr .print_byte_as_hex_one_char
	pla
	pha
	jsr print_byte_as_hex_primitive
	lda #$2f ; "/"
	jsr .print_byte_as_hex_one_char
	pla
	rts
	
.print_byte_as_hex_one_char
	bit .print_bad_code_buffered
	bmi +
	jmp s_printchar
+	bvs +
	jmp printchar_buffered	
+	jmp streams_print_output

.print_bad_code_buffered	!byte 0	; 0 = s_printchar, $80 = printchar_buffered, $ff = streams_print_output
.hex_num
!ifndef ACORN {
	!pet "0123456789abcdef"
} else {
	!text "0123456789abcdef"
}
} ; ifdef DBUG



printinteger
    ; subroutine: print 16 bit integer value
    ; input: a,x (x = low, a = high);
    ; output:
    ; used registers: a, x, y
    ; side effects:
!zone {
	pha
	ldy #1
-	lda z_operand_value_high_arr,y
	sta .temp,y
	lda z_operand_value_low_arr,y
	sta .temp + 2,y
	dey
	bpl -
	pla
	sta z_operand_value_high_arr
	stx z_operand_value_low_arr
	jsr print_num_unsigned
	ldy #1
-	lda .temp,y
	sta z_operand_value_high_arr,y
	lda .temp + 2,y
	sta z_operand_value_low_arr,y
	dey
	bpl -
	rts
.temp
	!byte 0,0,0,0
}

printstring
    ; input: a,y (lo/hi)
    ; output:
    ; used registers:
    ; side effects:
!zone {
    sta .loop+1
    sty .loop+2
    ldy #0
.loop
    lda $8000,y
    beq +
    jsr streams_print_output
    iny
    bne .loop
+   rts
}

!ifdef VMEM {
!ifndef ACORN {
conv2dec
    ; convert a to decimal in x,a
    ; for example a=#$0f -> x='1', a='5'
    ldx #$30 ; store '0' in x
-   cmp #10
    bcc +    ; a < 10
    inx
    sec
    sbc #10
    jmp -
+   adc #$30
    rts
}
}

mult16
    ;16-bit multiply with 32-bit product
    ;http://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product
    lda #$00
    sta product+2 ; clear upper bits of product
    sta product+3 
    ldx #$10 ; set binary count to 16 
shift_r
    lsr multiplier+1 ; divide multiplier by 2 
    ror multiplier
    bcc rotate_r 
    lda product+2 ; get upper half of product and add multiplicand
    clc
    adc multiplicand
    sta product+2
    lda product+3 
    adc multiplicand+1
rotate_r
    ror ; rotate partial product 
    sta product+3 
    ror product+2
    ror product+1 
    ror product 
    dex
    bne shift_r 
    rts
    ;SFTODODATA
multiplier
divisor
	!byte 0, 0
multiplicand
dividend
division_result
	!byte 0, 0
product
remainder 
	!byte 0 ,0 ,0 ,0

; divisor = $58     ;$59 used for hi-byte
; dividend = $fb	  ;$fc used for hi-byte
; remainder = $fd	  ;$fe used for hi-byte
; result = dividend ;save memory by reusing divident to store the result

!zone {
divide16	
	lda #0	        ;preset remainder to 0
	sta remainder
	sta remainder + 1
	ldx #16	        ;repeat for each bit: ...
.divloop
	asl dividend	;dividend lb & hb*2, msb -> Carry
	rol dividend + 1	
	rol remainder	;remainder lb & hb * 2 + msb from carry
	rol remainder + 1
	lda remainder
	sec
	sbc divisor	;substract divisor to see if it fits in
	tay	        ;lb result -> Y, for we may need it later
	lda remainder + 1
	sbc divisor+1
	bcc .skip	;if carry=0 then divisor didn't fit in yet

	sta remainder + 1	;else save subtraction result as new remainder,
	sty remainder
	inc division_result	;and INCrement result cause divisor fit in 1 times
.skip
	dex
	bne .divloop
	rts
}

; This macro is called at strategically chosen points where it might be
; appropriate to insert the screen hole for ACORN_NO_SHADOW builds. To minimise
; clutter it isn't protected by !ifndef everywhere it occurs; on all other
; builds (including non-Acorn builds) we just define it here as a no-op.
; it a no-op. (The ACORN_NO_SHADOW implementation is in acorn.asm.)
!ifndef ACORN_NO_SHADOW {
!macro make_acorn_screen_hole {
    ; no-op
}
}

; screen update routines
