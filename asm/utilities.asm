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
read_next_byte_at_z_pc_sub
!ifdef ACORN_SWR {
    !error "SFTODONOW"
}
	ldy #0
	lda (z_pc_mempointer),y
	inc z_pc_mempointer ; Also increases z_pc
	beq ++
	rts
++	jmp inc_z_pc_page

!macro read_next_byte_at_z_pc {
	jsr read_next_byte_at_z_pc_sub
}
	
} else {

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
    ; We must keep the first bank of sideways RAM paged in by default, because
    ; dynamic memory may have overflowed into it.
    ldy ram_bank_list
    sty romsel_copy
    sty romsel
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

; SFTODONOW: MOVE ALL THIS TO acorn.asm?
!ifdef ACORN {
!zone {
do_osbyte_rw_escape_key
    lda #osbyte_rw_escape_key
do_osbyte_y_0
    ldy #0
    jmp osbyte

error_handler
    ldy .error_handler_newlines
    beq +
-   lda #13
    jsr .error_handler_print_char
    dey
    bne -
+   ldy #1
-   lda (error_ptr), y
    beq +
    jsr .error_handler_print_char
    iny
    bne - ; Always branch
    ; The following jmp will be patched by code which wants to gain control
    ; after an error.
.error_handler_jmp
+   jmp .press_break

default_error_handler_newlines = 2
.error_handler_newlines !byte default_error_handler_newlines

.press_break
    ; We don't use print_following_string here because we don't want to assume
    ; Ozmoo's own printing mechanisms are properly initialised.
    jsr error_print_following_string
    !text " - press BREAK",0
-   jmp -

; Depending on what's happening when an error occurs, we need to output using
; different primitives. We therefore always use this subroutine and it gets
; patched at runtime.
.error_handler_print_char
    jmp s_printchar

; Like print_following_string, but using .error_handler_print_char.
error_print_following_string
    pla
    sta .error_print_following_string_lda + 1
    pla
    sta .error_print_following_string_lda + 2
-   inc .error_print_following_string_lda + 1
    bne +
    inc .error_print_following_string_lda + 2
+
.error_print_following_string_lda
    lda $ffff
    beq +
    jsr .error_handler_print_char
    jmp -
+   lda .error_print_following_string_lda + 2
    pha
    lda .error_print_following_string_lda + 1
    pha
    rts

error_print_s_printchar = 0
error_print_osasci = 1
.error_print_table_l
    !byte <s_printchar
    !byte <osasci
.error_print_table_h
    !byte >s_printchar
    !byte >osasci

; Allow trapping of errors signalled by the OS via BRKV. Used like this:
;   ldx #2 ; number of newlines to print before any error
;   ldy #n ; type of printing to use if an error occurs (0 s_printchar, 1 osasci)
;   jsr setjmp
;   beq ok
;   ; error occurred, do something
;   jsr set_default_error_handler ; before returning
;   rts
; ok
;   ; do something that might cause an error
;   jsr set_default_error_handler ; errors after this point aren't our problem
; SFTODONOW: Any problems with SWR here? On a BRK, will the OS page the current
; language back in or will it leave whatever bank we had paged in (and set at $f4)
; ourselves paged in? Remember we might be inside readblock doing a retry here
; and if the retry succeeds the vmem code which called readblock will expect to
; see the same bank paged in (and readblock to have written to that bank, not
; some random one).
setjmp
    stx .error_handler_newlines
    lda .error_print_table_l,y
    sta .error_handler_print_char + 1
    lda .error_print_table_h,y
    sta .error_handler_print_char + 2
    lda #<.setjmp_error_handler
    sta .error_handler_jmp + 1
    lda #>.setjmp_error_handler
    sta .error_handler_jmp + 2
    ; We need to save the contents of the stack, because they may be corrupted
    ; when an error message is generated. (They probably won't be, but we can't
    ; rely on it.) As a nice side effect of this, the return address for our
    ; caller is saved so .setjmp_error_handler can simply rts after restoring
    ; the stack.
    ; SFTODO: If jmp_buf is made smaller, we could probably fairly easily
    ; detect overflow - initialise y with -buffer_size, do sta jmp_buf+1+buffer_size,y
    ; and if the bne after the iny isn't taken we've overflowed. There might be
    ; an off by one error in that, I'm just sketching the idea out. This is
    ; tempting, *but* at the moment jmp_buf is going to live in $400-800 and
    ; (except for the possibility of starting code at say $600 on 2P) we have
    ; loads of free space down there, so adding a few bytes of code to the VM
    ; to detect overflow and cause a fatal error will eat into valuable memory
    ; for the sake of optimising use of a currently not-scare resource. Think
    ; about it, maybe convert this to an SF: comment.
    tsx
    stx jmp_buf
    ldy #0
-   inx
    beq +
    lda stack,x
    sta jmp_buf+1,y
    iny
    bne -
+   ; Z flag is set
    rts

.setjmp_error_handler
    ldx jmp_buf
    txs
    ldy #0
-   inx
    beq +
    lda jmp_buf+1,y
    sta stack,x
    iny
    bne -
+   lda #1 ; Z flag is clear
    rts

set_default_error_handler
    lda #default_error_handler_newlines
    sta .error_handler_newlines
    lda #<s_printchar
    sta .error_handler_print_char + 1
    lda #>s_printchar
    sta .error_handler_print_char + 2
    lda #<.press_break
    sta .error_handler_jmp + 1
    lda #>.press_break
    sta .error_handler_jmp + 2
.set_default_error_handler_rts
    rts

; Like printstring_raw, but using OSASCI.
printstring_os
    stx .printstring_os_lda + 1
    sta .printstring_os_lda + 2
-
.printstring_os_lda
   lda $ffff
   beq .set_default_error_handler_rts
   jsr osasci
   inc .printstring_os_lda + 1
   bne -
   inc .printstring_os_lda + 2
   bne - ; Always branch

; Calculate a CRC over A bytes of data at YX (A=0 => 256 bytes), returning it in
; YX.
calculate_crc
.crc = zp_temp ; 2 bytes
    sta .cpy_imm + 1
    stx .eor_abs + 1
    sty .eor_abs + 2
    lda #0
    sta .crc + 1
    sta .crc
    tay
.nbyt
    lda .crc + 1
.eor_abs
    eor $ffff,y
    sta .crc + 1
    ldx #8
.loop
    lda .crc + 1
    rol
    bcc .b7z
    lda .crc + 1
    eor #8
    sta .crc + 1
    lda .crc
    eor #$10
    sta .crc
.b7z
    rol .crc
    rol .crc + 1
    dex
    bne .loop
    iny
.cpy_imm
    cpy #$ff
    bne .nbyt
    ldx .crc
    ldy .crc + 1
    rts
}
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
