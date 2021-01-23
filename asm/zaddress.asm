; z_address !byte 0,0,0
; z_address_temp !byte 0

!zone zaddress {

set_z_address
	stx z_address + 2
	sta z_address + 1
	lda #$0
!ifndef SFTODOXXX {
    sta SFTODOFLAG ; set to 0
}
	sta z_address
	rts

+make_acorn_screen_hole
dec_z_address
!ifndef SFTODOXXX {
    lsr SFTODOFLAG ; set to 0
}
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
!ifndef SFTODOXXX {
    lsr SFTODOFLAG ; set to 0
}
	stx z_address + 2
	sta z_address + 1
	sty z_address
	rts

skip_bytes_z_address
!ifndef SFTODOXXX {
    lsr SFTODOFLAG ; set to 0
}
	; skip <a> bytes
	clc
	adc z_address + 2
	sta z_address + 2
!ifndef SFTODOXXX {
    lda #0
    sta SFTODOFLAG ; set to 0
    adc z_address + 1
} else {
	lda z_address + 1
	adc #0
}
	sta z_address + 1
	lda z_address
	adc #0
	sta z_address
	rts

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

get_z_himem_address
	ldx z_address + 2
	lda z_address + 1
	ldy z_address
	rts

read_next_byte
!ifndef SFTODOXXX { ; SFTODO EXPERIMENTAL
	lda SFTODOFLAG ; 1 if last call to read_byte_at_z_address was us (read_next_byte) with no intervening set_z_address, otherwise 0
	bne SFTODO999
 }
	; input: 
	; output: a
	; side effects: z_address
	; used registers: a,x
	sty z_address_temp
	lda z_address
	ldx z_address + 1
	ldy z_address + 2
	jsr read_byte_at_z_address
	inc SFTODOFLAG ; set to 1
	; SFTODO: FOLLOWING CODE IS DUPLICATED, IN A TIDIER IMPL WE'D PROB JUST BNE (ALWAYS) TO THE FAST PATH COPY
	inc z_address + 2
	bne +
!ifndef SFTODOXXX {
	dec SFTODOFLAG ; set back to 0; we've wrapped to a new page
}
	inc z_address + 1
	bne +
	inc z_address
+   ldy z_address_temp
	rts
!if 1 { ; SFTODO EXPERIMENTAL
SFTODO999
	sty z_address_temp
	; SFTODO: NOT THOUGHT WHETHER WE NEED ANY ACORN SWR PAGING ETC HERE; JUST WING IT FOR NOW
!ifdef ACORN_SWR {
    +acorn_page_in_bank_using_a mempointer_ram_bank
}
	inc mempointer
-	beq - ; SFTODO TEMP FOR DEBUGGING, I BELIEVE THIS IS IMPOSSIBLE
	ldy #0
	lda (mempointer),y
!ifdef ACORN_SWR {
	+acorn_swr_page_in_default_bank_using_y
}
	inc z_address + 2
	bne +
!ifndef SFTODOXXX {
	dec SFTODOFLAG ; set back to 0; we've wrapped to a new page
}
	inc z_address + 1
	bne +
	inc z_address
+   ldy z_address_temp
	rts
SFTODO
}

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
!ifndef SFTODOXXX {
    sta SFTODOFLAG ; set to 0
}
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
	; not TARGET_C128
	lda z_address + 2
	sta .write_byte + 1
	lda z_address + 1
	clc
	adc #>story_start
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
