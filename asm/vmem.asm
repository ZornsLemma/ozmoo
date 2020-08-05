
!ifndef ACORN {
!ifdef ALLRAM {
vmem_cache_cnt !byte 0         ; current execution cache
vmem_cache_index !fill cache_pages + 1, 0
}
}

!ifndef VMEM {
; Non-virtual memory

!ifndef ALLRAM {
read_byte_at_z_address
    ; Subroutine: Read the contents of a byte address in the Z-machine
    ; x,y (high, low) contains address.
    ; Returns: value in a
    sty mempointer ; low byte unchanged
    ; same page as before?
    cpx zp_pc_l
    bne .read_new_byte
    ; same 256 byte segment, just return
-	ldy #0
	lda (mempointer),y
	rts
.read_new_byte
	txa
    sta zp_pc_l
	clc
	adc #>story_start
	sta mempointer + 1
	jmp - ; Always branch
} else {
; No vmem, but ALLRAM 
!ifdef ACORN {
    !error "ALLRAM only supported/needed with VMEM on Acorn"
}

read_byte_at_z_address
    ; Subroutine: Read the contents of a byte address in the Z-machine
    ; a,x,y (high, mid, low) contains address.
    ; Returns: value in a
    sty mempointer ; low byte unchanged
    ; same page as before?
    cpx zp_pc_l
    bne .read_new_byte
    ; same 256 byte segment, just return
.return_result
	ldy #0
	lda (mempointer),y
	rts
.read_new_byte
	txa
    sta zp_pc_l
	clc
	adc #>story_start
	sta mempointer + 1
	cmp #first_banked_memory_page
	bcc .return_result ; Always branch
; swapped memory
	; ; Carry is already clear
	; adc #>story_start
	; sta vmap_c64_offset
	; cmp #first_banked_memory_page
    ; bcc .unswappable
    ; this is swappable memory
    ; update vmem_cache if needed
	; Check if this page is in cache
    ldx #vmem_cache_count - 1
-   cmp vmem_cache_index,x
    beq .cache_updated
    dex
    bpl -
	; The requested page was not found in the cache
    ; copy vmem to vmem_cache (banking as needed)
	ldx vmem_cache_cnt
	; Protect page held in z_pc_mempointer + 1
	pha
	txa
	clc
	adc #>vmem_cache_start
	cmp z_pc_mempointer + 1
	bne +
	inx
	cpx #vmem_cache_count
	bcc ++
	ldx #0
++	stx vmem_cache_cnt

+	pla
	sta vmem_cache_index,x
	pha
    lda #>vmem_cache_start ; start of cache
    clc
    adc vmem_cache_cnt
	tay
	pla
	jsr copy_page
    ; set next cache to use when needed
	inx
	txa
	dex
	cmp #vmem_cache_count
	bcc ++
	lda #0
++	sta vmem_cache_cnt
.cache_updated
    ; x is now vmem_cache (0-4) where we want to read
    txa
    clc
    adc #>vmem_cache_start
    sta mempointer + 1
	jmp .return_result 
} ; End of block for ALLRAM=1
	
} else {
; virtual memory

; virtual memory address space
; Z1-Z3: 128 kB (0 - $1ffff)
; Z4-Z5: 256 kB (0 - $3ffff)
; Z6-Z8: 512 kB (0 - $7ffff)
;
; map structure: one entry for each block (512 bytes) of available virtual memory
; each map entry is:
; 1 byte: ZMachine offset high byte (1-3 lowest bits used for ZMachine offset, the rest used to store ticks since block was last used)
; 1 byte: ZMachine offset low byte
;
; needs 102*2=204 bytes for $3400-$FFFF
; will store in datasette_buffer
;

!ifdef SMALLBLOCK {
	vmem_blocksize = 512
} else {
	vmem_blocksize = 1024 ; This hasn't been used in a long time, and probably doesn't work anymore.
!ifdef ACORN {
    ; Given the above comment I've not made any attempt to make this work on
    ; Acorn.
    !error "VMEM only supports SMALLBLOCK on Acorn"
}
}


; SFTODO: I suspect - I haven't yet analysed the code which can access dynamic memory, especially writes - we may need to keep the RAM bank which dynamic memory (might, depending on game size) overflow into paged in by default.
vmem_blockmask = 255 - (>(vmem_blocksize - 1))
vmem_block_pagecount = vmem_blocksize / 256
; vmap_max_length  = (vmem_end-vmem_start) / vmem_blocksize
!ifndef ACORN {
vmap_max_size = 102 ; If we go past this limit we get in trouble, since we overflow the memory area we can use.
} else {
!ifndef ACORN_SWR {
; SFTODO: DOES vmap_max_size NEED TWEAKING FOR ACORN NON-SWR? NO POINT MAKING IT BIGGER THAN THE 2P CAN ACCOMMODATE
vmap_max_size = 102 ; If we go past this limit we get in trouble, since we overflow the memory area we can use.
} else {
vmap_max_size = 120 ; SFTODO SHOULD BE 255 BUT I WILL KEEP IT 7-BIT FOR NOW TO AVOID CODE ASSUMING 7-BIT WHICH I HAVE MISSED BREAKING THINGS AT FIRST, THEN WHEN THAT WORKS CAN RAMP THIS UP ; If we go past this limit we get in trouble, since we overflow the memory area we can use.
}
}
; vmap_max_entries	!byte 0 ; Moved to ZP
; vmap_used_entries	!byte 0 ; Moved to ZP
; SFTODO: Do we need vmap_blocks_preloaded on Acorn?
vmap_blocks_preloaded !byte 0
vmap_z_h = datasette_buffer_start
vmap_z_l = vmap_z_h + vmap_max_size

;SFTODODATA
vmap_clock_index !byte 0        ; index where we will attempt to load a block next time

vmap_first_ram_page		!byte 0
vmap_c64_offset !byte 0
vmap_index !byte 0              ; current vmap index matching the z pointer
vmem_offset_in_block !byte 0         ; 256 byte offset in 512 byte block (0-1)
; vmem_temp !byte 0

vmem_tick 			!byte $e0
vmem_oldest_age		!byte 0
vmem_oldest_index	!byte 0

; SFTODO: I suspect with large amounts of RAM backing the virtual memory, the
; tick resolution may not really be high enough. We'll probably mostly get
; away with it, but maybe come back to this later. It might be OK actually given
; the way vmap_clock_index moves round rather than us always starting at 0;
; anyway, I can see how this works in practice later.
!ifdef Z8 {
	vmem_tick_increment = 8
	vmem_highbyte_mask = $07
} else {
!ifdef Z3 {
	vmem_tick_increment = 2
	vmem_highbyte_mask = $01
} else {
	vmem_tick_increment = 4
	vmem_highbyte_mask = $03
}
}

; SFTODO: Might be a useful statistic for benchmarking with different builds/
; amounts of SWR (not just benchmarking, checking more RAM is reducing swaps)
!ifdef COUNT_SWAPS {
vmem_swap_count !byte 0,0
}

!ifdef DEBUG {
!ifdef PREOPT {
print_optimized_vm_map
	stx zp_temp ; Nonzero means premature exit
	jsr printchar_flush
	lda #0
	sta streams_output_selected + 2
	sta is_buffered_window
	jsr newline
	jsr dollar
	jsr dollar
	jsr dollar
	jsr print_following_string
	!pet "clock",13,0
	ldx #0
-	lda vmap_z_h,x
	jsr print_byte_as_hex
	lda vmap_z_l,x
	jsr print_byte_as_hex
	jsr colon
	inx
	cpx vmap_used_entries
	bcc -

	lda zp_temp
	bne +++
	; Print block that was just to be read
	lda zp_pc_h
	jsr print_byte_as_hex
	lda zp_pc_l
	and #vmem_blockmask
	jsr print_byte_as_hex
	jsr colon
	
+++	jsr newline
	jsr dollar
	jsr dollar
	jsr dollar
	jsr newline
    jsr kernal_readchar   ; read keyboard
    jmp kernal_reset      ; reset
}

; SFTODO: This might need tweaking to print SWR stuff correctly
!ifdef TRACE_VM {
print_vm_map
!zone {
!ifndef ACORN {
    ; print caches
    jsr space
    lda #66
    jsr streams_print_output
    jsr space
    lda vmem_cache_cnt
    jsr printa
    jsr space
    jsr dollar
    lda vmem_cache_index
    jsr print_byte_as_hex
    jsr space
    jsr dollar
    lda vmem_cache_index + 1
    jsr print_byte_as_hex
    jsr space
    jsr dollar
    lda vmem_cache_index + 2
    jsr print_byte_as_hex
    jsr space
    jsr dollar
    lda vmem_cache_index + 3
    jsr print_byte_as_hex
}
    jsr newline
    ldy #0
-	; print
    cpy #10
    bcs +
    jsr space ; alignment when <10
+   jsr printy
    jsr space
    lda vmap_z_h,y ; zmachine mem offset ($0 - 
    ; SF: I changed the masks here, I think this is correct but it is a
    ; divergence from upstream.
    and #($ff xor vmem_highbyte_mask)
    jsr print_byte_as_hex
    jsr space
    jsr dollar
    lda vmap_z_h,y ; zmachine mem offset ($0 - 
    and #vmem_highbyte_mask
    jsr printa
    lda vmap_z_l,y ; zmachine mem offset ($0 - 
    jsr print_byte_as_hex
    lda #0 ; add 00
    jsr print_byte_as_hex
    jsr space
	tya
	asl
!ifndef SMALLBLOCK {
	asl
}
	adc vmap_first_ram_page
    jsr print_byte_as_hex
    lda #$30
    jsr streams_print_output
    lda #$30
    jsr streams_print_output
    jsr newline
.next_entry
    iny 
    cpy vmap_used_entries
    bcc -
    rts
}
}
}

load_blocks_from_index
    ; vmap_index = index to load
    ; side effects: a,y,x,status destroyed
!ifdef TRACE_FLOPPY {
	jsr dollar
	jsr dollar
	lda vmap_index
	jsr print_byte_as_hex
	jsr comma
	tax
	lda vmap_z_h,x
	jsr print_byte_as_hex
	lda vmap_z_l,x
	jsr print_byte_as_hex
}

!ifndef ACORN_SWR {
	lda vmap_index
	tax
	asl
!ifndef SMALLBLOCK {
	asl
}
	; Carry is already clear
	adc vmap_first_ram_page
} else {
    ldx vmap_index
    jsr .convert_index_x_to_ram_bank_and_address
}

!ifdef TRACE_FLOPPY {
	jsr comma
	jsr print_byte_as_hex
}
	tay ; Store in y so we can use it later.
;	cmp #$e0
;	bcs +
!ifndef ACORN {
    cmp #first_banked_memory_page
    bcs load_blocks_from_index_using_cache
}
+	lda #vmem_block_pagecount ; number of blocks
	sta readblocks_numblocks
    lda #0
    sta readblocks_mempos
	sty readblocks_mempos + 1
	lda vmap_z_l,x ; start block
	sta readblocks_currentblock
	lda vmap_z_h,x ; start block
	and #vmem_highbyte_mask
	sta readblocks_currentblock + 1
	jsr readblocks
!ifdef TRACE_VM {
    jsr print_following_string
!ifndef ACORN {
    !pet "load_blocks (normal) ",0
} else {
    !text "load_blocks (normal) ",0
}
    jsr print_vm_map
}
    rts

!ifndef ACORN {
load_blocks_from_index_using_cache
    ; vmap_index = index to load
    ; vmem_cache_cnt = which 256 byte cache use as transfer buffer
	; y = first c64 memory page where it should be loaded
    ; side effects: a,y,x,status destroyed
    ; initialise block copy function (see below)

	; Protect buffer which z_pc points to
	lda vmem_cache_cnt
	tax
	clc
	adc #>vmem_cache_start
	cmp z_pc_mempointer + 1
	bne +
	inx
	cpx #vmem_cache_count
	bcc ++
	ldx #0
++	stx vmem_cache_cnt
+
    ldx vmap_index
    lda #>vmem_cache_start ; start of cache
    clc
    adc vmem_cache_cnt
	sta vmem_temp
	sty vmem_temp + 1
    ldx #0 ; Start with page 0 in this 1KB-block
    ; read next into vmem_cache
-   lda #>vmem_cache_start ; start of cache
    clc
    adc vmem_cache_cnt
    sta readblocks_mempos + 1
    txa
    pha
    ldx vmap_index
    ora vmap_z_l,x ; start block
    sta readblocks_currentblock
    lda vmap_z_h,x ; start block
    and #vmem_highbyte_mask
    sta readblocks_currentblock + 1
    jsr readblock
    ; copy vmem_cache to block (banking as needed)
	lda vmem_temp
	ldy vmem_temp + 1
	jsr copy_page
	inc vmem_temp + 1
    pla
    tax
    inx
	cpx #vmem_block_pagecount ; read 2 or 4 blocks (512 or 1024 bytes) in total
    bcc -

	ldx vmem_temp + 1
	dex
	txa
	ldx vmem_cache_cnt
    sta vmem_cache_index,x
    rts
}

read_byte_at_z_address
    ; Subroutine: Read the contents of a byte address in the Z-machine
    ; a,x,y (high, mid, low) contains address.
    ; Returns: value in a
    sty mempointer ; low byte unchanged
    ; same page as before?
    cpx zp_pc_l
    bne .read_new_byte
    cmp zp_pc_h
    bne .read_new_byte
    ; same 256 byte segment, just return
-
!ifdef ACORN_SWR {
    ; SFTODO: For now I'll assume I always need to page the bank in.
    lda mempointer_ram_bank
    sta romsel_copy
    sta romsel
}
 	ldy #0
	lda (mempointer),y
!ifdef ACORN_SWR {
    ; SFTODO: Hack to see if I'm "accidentally" accessing SWR
    pha
    lda #hack_ram_bank
    sta romsel_copy
    sta romsel
    pla
}
	rts
.read_new_byte
	cmp #0
	bne .non_dynmem
	cpx nonstored_blocks
	bcs .non_dynmem
	; Dynmem access
    ; SFTODO: This will need tweaking to allow for a hole for a non-shadow
    ; screen, but let's not worry about that right now. It may also need to page
    ; in the right RAM bank once dynmem can spill over into SWR.
	sta zp_pc_h
	txa
    sta zp_pc_l
	adc #>story_start
	sta mempointer + 1
	bne - ; Always branch
.non_dynmem
	sta zp_pc_h
	sta vmem_temp + 1
	lda #0
	sta vmap_quick_index_match
    txa
    sta zp_pc_l
    and #255 - vmem_blockmask ; keep index into kB chunk
    sta vmem_offset_in_block
	txa
	and #vmem_blockmask
	sta vmem_temp
	; Check quick index first
	ldx #vmap_quick_index_length - 1
-	ldy vmap_quick_index,x
    cmp vmap_z_l,y ; zmachine mem offset ($0 -
	beq .quick_index_candidate
--	dex
	bpl -
	bmi .no_quick_index_match ; Always branch
.quick_index_candidate
	lda vmap_z_h,y
	and #vmem_highbyte_mask
	cmp vmem_temp + 1
	beq .quick_index_match
	lda vmem_temp
	jmp --
.quick_index_match
	inc vmap_quick_index_match
	tya
	tax
	jmp .correct_vmap_index_found ; Always branch
	
.no_quick_index_match
	lda vmem_temp

    ; is there a block with this address in map?
    ldx vmap_used_entries
	dex
-   ; compare with low byte
    cmp vmap_z_l,x ; zmachine mem offset ($0 - 
    beq +
.check_next_block
	dex
!ifndef ACORN_SWR {
	bpl -
	bmi .no_such_block ; Always branch
} else {
    ; SFTODO: So note that (as I expected) we can only have 255 entries in the
    ; vmem map, numbered 0-254. Probably not worth playing games trying to have
    ; vmap_used_entries == 0 meaning 256 used.
    cpx #255
	bne -
	beq .no_such_block ; Always branch
}
	; is the highbyte correct?
+   lda vmap_z_h,x
	and #vmem_highbyte_mask
	cmp vmem_temp + 1
	beq .correct_vmap_index_found
    lda vmem_temp
    jmp .check_next_block
.correct_vmap_index_found
    ; vm index for this block found
    stx vmap_index

	ldy vmap_quick_index_match
	bne ++ ; This is already in the quick index, don't store it again
	txa
	ldx vmap_next_quick_index
	sta vmap_quick_index,x
	inx
	cpx #vmap_quick_index_length
	bcc +
	ldx #0
+	stx vmap_next_quick_index
++	jmp .index_found

; no index found, add last
.no_such_block

	; Load 512 byte block into RAM
!ifndef ACORN {
	; First, check if this is initial REU loading
	ldx use_reu
	cpx #$80
	bne +
	ldx #0
	ldy vmap_z_l ; ,x is not needed here, since x is always 0
	cpy z_pc + 1
	bne .block_chosen
	inx ; Set x to 1
	bne .block_chosen ; Always branch
}

; SFTODO: I am not sure the bcs case can ever occur on Acorn, since we always
; pre-populate vmap. If this is true we can probably ifdef this out for both SWR
; and 2P builds. (But it's only one instruction, so hardly worth it?)
+	ldx vmap_clock_index
-	cpx vmap_used_entries
	bcs .block_chosen
!ifdef DEBUG {
!ifdef PREOPT {
	ldx #0
	jmp print_optimized_vm_map
}	
}
        ; Store very recent oldest_age so the first valid index in the following
        ; loop will be picked as the first candidate.
        lda #$ff
!ifdef DEBUG {
        sta vmem_oldest_index
}
	sta vmem_oldest_age
        bne ++ ; Always branch
	
	; Check all other indexes to find something older
-	lda vmap_z_h,x
	cmp vmem_oldest_age
	bcs +
++
	; Found older
	; Skip if z_pc points here; it could be in either page of the block.
!ifndef SMALLBLOCK {
	!error "Only SMALLBLOCK supported"
}
	ldy vmap_z_l,x
	cpy z_pc + 1
	beq +++
        iny
        cpy z_pc + 1
        bne ++
+++	tay
	and #vmem_highbyte_mask
	cmp z_pc
	beq +
	tya
++	sta vmem_oldest_age
	stx vmem_oldest_index
+	inx
	cpx vmap_used_entries
	bcc +
	ldx #0
+	cpx vmap_clock_index
	bne -

	; Load chosen index
	ldx vmem_oldest_index
	
.block_chosen
!ifdef COUNT_SWAPS {
	inc vmem_swap_count + 1
	bne ++
	inc vmem_swap_count
++
}
	
    ; SFTODO: The bcc here may always occur on Acorn?
	cpx vmap_used_entries
	bcc +
	inc vmap_used_entries
+	txa
	tay
	asl
!ifndef SMALLBLOCK {
	asl
}
	; Carry is already clear
    ; SFTODO: The next couple of lines aren't "right" for SWR case, but I think
    ; they are only used by PRINT_SWAPS or the non-Acorn cache case just below.
    ; I think we could therefore !if out these couple of lines on all Acorn
    ; builds except for debug ones.
	adc vmap_first_ram_page
	sta vmap_c64_offset
	; Pick next index to use
	iny
	cpy vmap_max_entries
	bcc .not_max_index
	ldy #0
.not_max_index
	sty vmap_clock_index

!ifdef DEBUG {
        lda vmem_oldest_index
        cmp #$ff
        bne +
        lda #ERROR_NO_VMEM_INDEX
        jsr fatalerror
+
}

	; We have now decided on a map position where we will store the requested block. Position is held in x.
    ; SFTODO: Will need tweaking for SWR
!ifdef DEBUG {
!ifdef PRINT_SWAPS {
	lda streams_output_selected + 2
	beq +
	lda #20
	jsr s_printchar
	lda #64
	jsr s_printchar
	lda #20
	jsr s_printchar
	jmp ++
+	jsr space
	jsr dollar
	txa
	jsr print_byte_as_hex
	jsr colon
	lda vmap_c64_offset
	jsr dollar
	jsr print_byte_as_hex
	jsr colon
	cpx vmap_used_entries
	bcs .printswaps_part_2
    lda vmap_z_h,x
    ; SF: I altered the mask here, I think it's correct but it's a divergence
    ; from upstream.
	and #vmem_highbyte_mask
	jsr dollar
	jsr print_byte_as_hex
    lda vmap_z_l,x
	jsr print_byte_as_hex
.printswaps_part_2
	jsr arrow
	jsr dollar
	lda zp_pc_h
	jsr print_byte_as_hex
    lda zp_pc_l
	and #vmem_blockmask
	jsr print_byte_as_hex
	jsr space
++	
}
}
	
!ifndef ACORN {
	; Forget any cache pages belonging to the old block at this position.
	lda vmap_c64_offset
	cmp #first_banked_memory_page
	bcc .cant_be_in_cache
	ldy #vmem_cache_count - 1
-	lda vmem_cache_index,y
	and #vmem_blockmask
	cmp vmap_c64_offset
	bne +
	lda #0
	sta vmem_cache_index,y
+	dey
	bpl -
.cant_be_in_cache	
}

	; Update tick
	lda vmem_tick
	clc
	adc #vmem_tick_increment
	bcc +

	; Tick counter has passed max value. Decrease tick value for all pages. Set tick counter back.
	txa
	pha
	
	ldx vmap_used_entries
	dex
-	lda vmap_z_h,x
	sec
	sbc #$80
	bpl ++
	and #vmem_highbyte_mask
++	sta vmap_z_h,x
	dex
!ifndef ACORN_SWR {
	bpl -
} else {
    cpx #255
    bne -
}
	
	pla
	tax
	lda #$80
+	sta vmem_tick

	; Store address of 512 byte block to load, then load it
	lda zp_pc_h
    sta vmap_z_h,x
    lda zp_pc_l
    and #vmem_blockmask ; skip bit 0 since 512 byte blocks
    sta vmap_z_l,x
    stx vmap_index
    ; SF: Be aware that if tracing is on here, the newly loaded block will
    ; show with its pre-adjustment tick.
    jsr load_blocks_from_index
.index_found
    ; index found
	; Update tick for last access 
    ldx vmap_index
	lda vmap_z_h,x
	and #vmem_highbyte_mask
	ora vmem_tick
	sta vmap_z_h,x
!ifndef ACORN_SWR {
	txa
	
	asl
!ifndef SMALLBLOCK {
	asl
}
	; Carry is already clear
	adc vmap_first_ram_page
	sta vmap_c64_offset
!ifndef ACORN {
	cmp #first_banked_memory_page
    bcc .unswappable
    ; this is swappable memory
    ; update vmem_cache if needed
    clc
    adc vmem_offset_in_block
	; Check if this page is in cache
    ldx #vmem_cache_count - 1
-   cmp vmem_cache_index,x
    beq .cache_updated
    dex
    bpl -
	; The requested page was not found in the cache
    ; copy vmem to vmem_cache (banking as needed)
	sta vmem_temp
	ldx vmem_cache_cnt
	; Protect page held in z_pc_mempointer + 1
	pha
	txa
	clc
	adc #>vmem_cache_start
	cmp z_pc_mempointer + 1
	bne +
	inx
	cpx #vmem_cache_count
	bcc ++
	ldx #0
++	stx vmem_cache_cnt

+	pla
	sta vmem_cache_index,x
    lda #>vmem_cache_start ; start of cache
    clc
    adc vmem_cache_cnt
	tay
	lda vmem_temp
	jsr copy_page
    ; set next cache to use when needed
	inx
	txa
	dex
	cmp #vmem_cache_count
	bcc ++
	lda #0
++	sta vmem_cache_cnt
.cache_updated
    ; x is now vmem_cache (0-3) where current z_pc is
    txa
    clc
    adc #>vmem_cache_start
    sta mempointer + 1
    ldx vmap_index
    bne .return_result ; always true
.unswappable
}
    ; update memory pointer
    lda vmem_offset_in_block
    clc
    adc vmap_c64_offset
} else {
    jsr .convert_index_x_to_ram_bank_and_address
    clc
    adc vmem_offset_in_block
}
    sta mempointer + 1
.return_result
    ldy #0
    lda (mempointer),y
!ifdef ACORN_SWR {
    ; SFTODO: Hack to see if I'm "accidentally" accessing SWR
    pha
    lda #hack_ram_bank
    sta $f4
    sta $fe30
    pla
}
    rts

!ifdef ACORN_SWR {
; SFTODO: Not sure I will want this as a subroutine, but let's write it here
; like this to help me think about it. For the moment it returns page of physical
; memory in A and ram bank is selected and stored at mempointer_ram_bank.
.convert_index_x_to_ram_bank_and_address
    ; SFTODO: This code is not necessarily optimal, e.g. among other things it
    ; may calculate things we could calculate once and cache.
    ; SFTODO: Once dynmem can properly spill over into the first SWR bank,
    ; this first test may be redundant - more to the point, this naive
    ; implementation will probably fail to detect this case correctly.
    sec
    lda #>ramtop
    sbc vmap_first_ram_page
    lsr
    sta vmap_main_ram_vm_blocks ; SFTODO: think we could calc this once on startup rather than having it in a temp
    cpx vmap_main_ram_vm_blocks
    bcs .address_in_swr
    lda #hack_ram_bank ; we don't care about the RAM bank, let's force to this for now but in principle we could let it be any old value
    sta mempointer_ram_bank
    sta romsel_copy
    sta romsel
    lda vmap_c64_offset
    rts
.address_in_swr
    txa
    sec
    sbc vmap_main_ram_vm_blocks ; SFTODO: could we do this instead of the cpx above, to save "duplication"?
    ; A is now the block number within SWR. There are 32 512-byte blocks per RAM bank.
    ; SFTODO: This assumes vmem starts at beginning of first bank, which won't
    ; be true once dynmem is allowed to spill over into first bank.
    pha
    lsr
    lsr
    lsr
    lsr
    lsr
    tay
    lda ram_bank_list,y
    sta mempointer_ram_bank
    sta romsel_copy
    sta romsel
    pla
    and #31
    asl
    ; Carry is already clear
    adc #$80
    rts
}
}

; SFTODO: Hack, let's just allocate a fake datasette buffer here
; SFTODO: *If* we permanently leave this allocated "in" the binary, we could
; have the build script patch in the initial VM table instead of having to "*LOAD"
; it at startup. However, if we have the 1K of language workspace at $400 unused
; (I haven't yet decided if it can be used for something else), using that to
; hold this would work and we would need to *LOAD the data there as that's 
; outside our binary. *LOADing the initial VM data is not a huge deal, but it
; will "waste" a sector on disc and slow loading slightly if it could otherwise
; have been included straight into the binary.
; SFTODO: For now I'm going to pre-fill this as part of the build
; SFTODODATA - THIS IS INITIALISED, BUT I AM HALF THINKING WE SHOULD JUST
; POPULATE IT IN THE DISCARDABLE INIT CODE - BUT MAYBE DON'T RUSH INTO THIS AS
; SWR AND 'SUGGESTED' PAGES AND PREOPT WILL AFFECT THIS DECISION
!ifdef ACORN {
!ifdef VMEM {
datasette_buffer_start
    !FILL vmap_max_size * 2, 'V'
datasette_buffer_end
}
}
