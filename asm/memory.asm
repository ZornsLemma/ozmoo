; Routines to handle memory
; SFTODO: ALLRAM might have been done away with - probably not a big deal, but check to see if I have any comments or code related to it and fix up if it has gone

; SFTODO: Is this relevant/needed/correct for 5.3? Just carrying it over for now without thinking...
!ifdef ACORN_SWR_BIG_DYNMEM {
inc_z_pc_page_acorn_unsafe
    jsr inc_z_pc_page
    +acorn_page_in_bank_using_y z_pc_mempointer_ram_bank
    rts
}

; SF: On Acorn non-VMEM (and, I believe, C64 non-ALLMEM) this just needs to do
; the two inc statements and rts, no need for anything else. Conditionally
; assembling this is a real faff so we just accept the small inefficiency. SFTODO: Is this still true/relevant for 5.3?
inc_z_pc_page
!zone {
	pha
	inc z_pc_mempointer + 1
	inc z_pc + 1
!ifdef VMEM {
	bne +
	inc z_pc
+	lda z_pc + 1
	and #vmem_indiv_block_mask
	beq get_page_at_z_pc_did_pha
	lda z_pc_mempointer + 1
!ifndef ACORN {
	cmp #>story_start
	bcc get_page_at_z_pc_did_pha
}
} else {
; No vmem
; SFTODO: Probably correct we don't need this for Acorn, but need to rethink as 5.3 port develops
!ifndef ACORN {
	!ifndef TARGET_PLUS4 {
		lda z_pc + 1
		cmp #(first_banked_memory_page - (>story_start))
		bcs get_page_at_z_pc_did_pha
	}
}
}
; safe
	pla
inc_z_pc_page_rts
	rts

}


set_z_pc
; Sets new value of z_pc, and makes sure z_pc_mempointer points to the right memory
; Parameters: New value of z_pc in a,x,y
!zone {
	sty z_pc + 2
!ifdef VMEM {
	cmp z_pc
	bne .unsafe_1
}
	cpx z_pc + 1
	beq .same_page 
	; Different page.
!ifdef VMEM {
	; Let's find out if it's the same vmem block.
	txa
	eor z_pc + 1
	and #(255 - vmem_indiv_block_mask)
	bne .unsafe_2
!ifndef ACORN {
	; z_pc is in same vmem_block unless it's in vmem_cache
	lda z_pc_mempointer + 1
	cmp #>story_start
	bcc .unsafe_2
}
	; z_pc is in same vmem_block, but different page.
	stx z_pc + 1
	lda z_pc_mempointer + 1
	eor #1
	sta z_pc_mempointer + 1
} else {
; No vmem 
; SFTODO: Prob correct we don't need this for any ACORN, but rethink later as 5.3 progresses
!ifndef ACORN {
!ifndef TARGET_PLUS4 {
	cpx #(first_banked_memory_page - (>story_start))
	bcs .unsafe_2
}
}
	stx z_pc + 1
	txa
	clc
	adc #>story_start
	sta z_pc_mempointer + 1
}
.same_page
	rts
.unsafe_1
	sta z_pc
.unsafe_2
	stx z_pc + 1
}

; Must follow set_z_pc
get_page_at_z_pc
!zone {
	pha
get_page_at_z_pc_did_pha
	stx mem_temp
!ifdef TARGET_C128 {
	; Special treatment if PC is in same block as end of dynmem
	lda z_pc
	bne .not_in_dynmem_block
	lda z_pc + 1
	cmp nonstored_blocks
	bcs .not_in_dynmem_block
	; This is in a dynmem block
	adc #>story_start_bank_1 ; Carry already clear
	sta mem_temp + 1
	ldx #0
-	cmp vmem_cache_page_index,x
	bne +
	lda vmem_cache_bank_index,x
	bne .found_it ; The page we're looking for belongs to bank 1, so this is a match!
	lda mem_temp + 1
+	inx
	cpx #vmem_cache_count
	bcc -
	; Block not found, will copy it to vmem_cache
	jsr get_free_vmem_buffer
	tay
	lda #1
	sta vmem_cache_bank_index,x
	lda mem_temp + 1
	sta vmem_cache_page_index,x
	ldx #1
	jsr copy_page_c128
	ldx vmem_cache_cnt
	stx mem_temp + 1
	jsr inc_vmem_cache_cnt
	ldx mem_temp + 1
.found_it
	txa
	clc
	adc #>vmem_cache_start
	sta z_pc_mempointer + 1
	ldy #0
	ldx mem_temp
	pla
	rts
.not_in_dynmem_block
}
	lda z_pc
	ldx z_pc + 1
	ldy z_pc + 2
    sty z_pc_mempointer
	jsr read_byte_at_z_address
!ifdef ACORN_SWR {
!ifdef ACORN_SWR_SMALL_DYNMEM {
; SFTODO: read_byte_at_z_address_for_z_pc NO LONGER SEEMS TO EXIST, IS THIS JUST A RENAME OR IS THIS COMMENT OUTDATED/INVALID?
    ; read_byte_at_z_address_for_z_pc will have left the then-current value of
    ; z_pc_mempointer_ram_bank paged in, so we need to explicitly page in the
    ; newly set z_pc_mempointer_ram_bank. This is mildly inefficient, but it
    ; only happens when the Z-machine PC crosses a page boundary and the
    ; contortions required to avoid it are not worth it.
    +acorn_page_in_bank_using_y mempointer_ram_bank ; leaves bank in Y
} else {
    ldy mempointer_ram_bank
}
    sty z_pc_mempointer_ram_bank
} else {
!ifdef ACORN_TURBO_SUPPORTED {
	; This isn't necessary on a normal second processor, but it's harmless and
	; it's only one cycle slower to just do it instead of checking the second
	; processor type before doing it.
	ldy mempointer_turbo_bank
	sty z_pc_mempointer_turbo_bank
}
}
	ldy mempointer + 1
	sty z_pc_mempointer + 1
    ; SFTODO: I am struggling to see why it matters that Y is 0 on exit
	ldy #201 ; SFTODO TO SEE IF IT IS NECESSARY ldy #0 ; Important: y should always be 0 when exiting this routine! SF: I have analysed the code and I don't think this is true (maybe it was at one point) - SFTODO: HMM, I'M A BIT LESS SURE NOW - FWIW THIS IS *NOT* A HOT INSTRUCTION AT LEAST IN THE BENCHMARK - SEE ALSO THE LATEST ANALYSIS IN sf-notes.txt
	ldx mem_temp
	pla
	rts
}

!zone {
; !ifdef VMEM {
; .reu_copy
	; ; a = source C64 page
	; ; y = destination C64 page
	; stx mem_temp
	; sty mem_temp + 1
	; ; Copy to REU
	; tay
	; lda #0
	; tax
	; jsr store_reu_transfer_params
	; lda #%10000000;  c64 -> REU with delayed execution
	; sta reu_command
	; sei
	; +set_memory_all_ram_unsafe
	; lda $ff00
	; sta $ff00
	; +set_memory_no_basic_unsafe
	; cli
	; ; Copy to C64
	; txa ; X is already 0, set a to 0 too
	; ldy mem_temp + 1
	; jsr store_reu_transfer_params
	; lda #%10000001;  REU -> c64 with delayed execution
	; sta reu_command
	; sei
	; +set_memory_all_ram_unsafe
	; lda $ff00
	; sta $ff00
	; +set_memory_no_basic_unsafe
	; cli
	; ldx mem_temp
	; ldy #0
	; rts
; }	
!ifdef TARGET_C128 {
copy_page_c128_src
; a = source
; y = destination
; x = bank (0 or 1)

!pseudopc copy_page_c128 {
	sta .copy + 2
	sty .copy + 5

; Skip speed decrease if we can
	lda COLS_40_80
	bne + ; In 80 col mode, there is no reason to lower speed
	bit $d011
	bmi + ; If we're at raster line > 255, stay at 2 MHz
	lda $d012
	cmp #52
	bcs + ; If at line 52-255, stay at 2 Mhz
	cmp #19
	bcc + ; If at line 0-18, stay at 2 MHz
	; Go down to 1 MHz, to avoid screen glitches
	lda #0
	sta reg_2mhz	;CPU = 1MHz

+	sei
	sta c128_mmu_load_pcrb,x
-   ldy #0
.copy
	lda $8000,y
	sta $8000,y
	iny
	bne .copy
	sta c128_mmu_load_pcra
	cli
	rts

read_word_from_bank_1_c128
; a = zp vector pointing to base address
; y = offset from address in zp vector
; Returns word in a,x (byte 1, byte 2)
; y retains its value
	sta .read_word + 1
	sta .read_word_2 + 1
	sei
	sta c128_mmu_load_pcrc
	iny
.read_word
	lda ($fb),y
	tax
	dey
.read_word_2
	lda ($fb),y
	sta c128_mmu_load_pcra
	cli
	rts

write_word_to_bank_1_c128
; zp vector pointing to base address must be stored in
;   write_word_c128_zp_1 and write_word_c128_zp_2 before call 
; a,x = value (byte 1, byte 2)
; y = offset from address in zp vector
; y is increased by 1
	sei
	sta c128_mmu_load_pcrc
.write_word
	sta ($fb),y
	txa
	iny
.write_word_2
	sta ($fb),y
	sta c128_mmu_load_pcra
	cli
	rts

write_word_c128_zp_1 = .write_word + 1
write_word_c128_zp_2 = .write_word_2 + 1


} ; pseudopc
copy_page_c128_src_end

} else { ; not TARGET_C128

!ifndef ACORN {
copy_page
; a = source
; y = destination

; !ifdef VMEM {
	; bit use_reu
	; bmi .reu_copy
; }
	sta .copy + 2
	sty .copy + 5
	sei
	+set_memory_all_ram_unsafe
	+before_dynmem_read
-   ldy #0
.copy
	lda $8000,y
	sta $8000,y
	iny
	bne .copy
	+after_dynmem_read
	+set_memory_no_basic_unsafe
	cli
	rts
}
} ; not TARGET_C128
}



read_header_word
; y contains the address in the header
; Returns: Value in a,x
; y retains its original value
!ifdef TARGET_C128 {
	lda #<story_start_bank_1
	sta mem_temp
	lda #>story_start_bank_1
	sta mem_temp + 1
	lda #mem_temp
	jmp read_word_from_bank_1_c128
} else {
	iny
	lda story_start,y
	tax
	dey
	lda story_start,y
	rts
}

write_header_word
; y contains the address in the header
; a,x contains word value
; a,x,y are destroyed
!ifdef TARGET_C128 {
	stx .tmp
	jsr setup_to_write_to_header_c128
	ldx #mem_temp
	stx write_word_c128_zp_1
	stx write_word_c128_zp_2
	ldx .tmp
	jmp write_word_to_bank_1_c128
} else {
	sta story_start,y
	iny
	txa
	sta story_start,y
	rts
}

write_header_byte
; y contains the address in the header
; a contains byte value
; a,x,y are preserved
!ifdef TARGET_C128 {
	sta .tmp
	stx .tmp + 1
	jsr setup_to_write_to_header_c128
	ldx #mem_temp
	stx $02b9
	ldx #$7f
	jsr $02af
	lda .tmp
	ldx .tmp + 1
	rts
} else {
	; SFTODO: Far from the only place, but this may want special-casing for some Acorn builds.
	; (It may not, though. We're accessing the first 256 bytes of dynmem, and those may always
	; be trivially accessible.)
	sta story_start,y
	rts
}

!ifdef TARGET_C128 {
setup_to_write_to_header_c128
	ldx #<story_start_bank_1
	stx mem_temp
	ldx #>story_start_bank_1
	stx mem_temp + 1
	rts

.tmp !byte 0, 0
}
