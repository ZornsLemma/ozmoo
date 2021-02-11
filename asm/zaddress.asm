; z_address !byte 0,0,0
; z_address_temp !byte 0

!zone zaddress {

set_z_address
	stx z_address + 2
	sta z_address + 1
	; SFTODO: The next couple of instructions are executed often enough it might be worth adding a CMOS version which does stz
	lda #$0
	sta z_address
	rts

+make_acorn_screen_hole
dec_z_address
	pha
	dec z_address + 2
	lda z_address + 2
	cmp #$ff
	bne +
	dec z_address + 1
	lda z_address + 1
	cmp #$ff
	bne +
	dec z_address
+   pla
	rts

+make_acorn_screen_hole
set_z_himem_address
	stx z_address + 2
	sta z_address + 1
	sty z_address
	rts

skip_bytes_z_address
	; skip <a> bytes
	clc
	adc z_address + 2
	sta z_address + 2
	bcc +
	inc z_address + 1
	bne +
	inc z_address
+   rts

!ifdef DEBUG {
print_z_address
	jsr dollar
	lda z_address + 1 ; low
	jsr print_byte_as_hex
	lda z_address + 2 ; high
	jsr print_byte_as_hex
	jmp newline
}

get_z_address
	; input: 
	; output: a,x
	; side effects: 
	; used registers: a,x
	ldx z_address + 2 ; low
	lda z_address + 1 ; high
	rts

; SFTODO: Could we rejig this slightly so it does the ldy and falls through into get_z_address? Would save a few bytes and probably no less clear, maybe arguably clearer. Check callers don't use flags-reflect-Y first!
get_z_himem_address
	ldx z_address + 2
	lda z_address + 1
	ldy z_address
	rts

read_next_byte
	; input: 
	; output: a
	; side effects: z_address
	; used registers: a,x
	sty z_address_temp
	lda z_address
	ldx z_address + 1
	ldy z_address + 2
	; SFTODO: Note that this is the main call to read_byte_at_z_address; a handful originate elsewhere, but nearly all come from here.
	jsr read_byte_at_z_address
	inc z_address + 2
	bne +
	inc z_address + 1
	bne +
	inc z_address
+   ldy z_address_temp
	rts

set_z_paddress
	; convert a/x to paddr in z_address
	; input: a,x
	; output: 
	; side effects: z_address
	; used registers: a,x
	; example: $031b -> $00, $0c, $6c (Z5)
	sta z_address + 1
	txa
	asl
	sta z_address + 2
	rol z_address + 1
	lda #$0
	rol
!ifdef Z4PLUS {
	asl z_address + 2
	rol z_address + 1
	rol
}
!ifdef Z8 {
	asl z_address + 2
	rol z_address + 1
	rol
}
	sta z_address
	rts

write_next_byte
; input: value in a 
; a,x,y are preserved
	sta z_address_temp
!ifndef UNSAFE {
!if 0 { ; SFTODO: I SHOULD PROB SUPPORT THIS ON ACORN, BUT RIGHT NOW dynmem_size IS NOT BEING INITIALISED ON ACORN AS IT'S OTHERWISE UNNEEDED. THE IDEA OF BURNING TWO BYTES ON A COPY OF THIS FROM THE HEADER FOR SUCH LITTLE USE ANNOYS ME FAR THAN IS PROBABLY RATIONAL. OF COURSE, IF I *DO* DECIDE TO DO SOMETHING DIFFERENT, I SHOULD MAKE SURE NOT TO ALLOCATE 2 BYTES TO dynmem_size ON ACORN BUILDS! - OF COURSE, WORTH NOTING WE HAVE DYNMEM SIZE AS ACORN_DYNAMIC_SIZE_BYTES AS AN ASSEMBLY-TIME CONSTANT, AND IT WOULD BE FASTER (IF NOT NECESSARILY SIGNIFICANTLY FASTER) TO DO THIS COMPARISON AGAINST IMMEDIATE CONSTANTS
	lda z_address
	bne .write_outside_dynmem
	lda z_address + 2
	cmp dynmem_size
	lda z_address + 1
	sbc dynmem_size + 1
	bcs .write_outside_dynmem
}
}

!ifdef TARGET_C128 {
	txa
	pha
	tya
	pha
	lda z_address + 2
	sta mem_temp
	lda z_address + 1
	clc
	adc #>story_start_bank_1
	sta mem_temp + 1
	ldx #mem_temp
	stx $02b9
	ldx #$7f
	ldy #0
	lda z_address_temp
	jsr $02af ; y has correct value already
	pla
	tay
	pla
	tax
	lda z_address_temp
} else {
; SFTODO: NOTE THIS DOESN'T USE THE BEFORE/AFTER DYNMEM MACROS, MAYBE I SHOULD ADD THEM OR SOME ACORN EQUIVALENT - DOES THE CALLER DO THEM? OR ARE THEY NOT NEC AS WE HAVE C128 BRANCH ABOVE?
	; not TARGET_C128
	lda z_address + 2
	sta .write_byte + 1
	lda z_address + 1
	clc
	adc #>story_start
!ifdef ACORN_SCREEN_HOLE {
	cmp #ACORN_SCREEN_HOLE_START_PAGE
	bcc +
	clc
	adc #ACORN_SCREEN_HOLE_PAGES
+
}
	sta .write_byte + 2
	lda z_address_temp
.write_byte
	sta $8000 ; This address is modified above
}

	inc z_address + 2
	bne +
	inc z_address + 1
	bne +
	inc z_address
+	rts

!ifndef UNSAFE {
.write_outside_dynmem
	lda #ERROR_WRITE_ABOVE_DYNMEM
	jsr fatalerror
}
	
	
} ; End zone zaddress
