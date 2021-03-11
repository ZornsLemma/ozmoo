
!ifndef ACORN {
dynmem_size !byte 0, 0

vmem_cache_cnt !byte 0         ; current execution cache
vmem_cache_page_index !fill cache_pages + 1, 0
!ifdef TARGET_C128 {
vmem_cache_bank_index !fill cache_pages + 1, 0
}
} else { ; ACORN
!ifdef ACORN_SHADOW_VMEM {
!error "SFTODO: This is working but it's way slower than the prototype - need to investigate. Perhaps we're halving a value when we shouldn't be and not using all available shadow RAM? Perhaps we're not using all the cache pages we have allocated? Not gone into this yet."
; SFTODONOW: THESE SHOULD PROBABLY LIVE IN PAGE 4, THOUGH THEY DO PROB NEED TO BE 0-INITED
vmem_cache_cnt !byte 0         ; current execution cache
; SFTODONOW: I SHOULD WORK TOGETHER WITH BUILD SYSTEM TO ENSURE THERE WILL NEVER BE MORE CACHE PAGES THAN CAN FIT HERE - JUST HACK IT FOR NOW
vmem_cache_page_index !fill 8, 0
}
}
; SFTODO: NOT A HUGE DEAL, BUT NOW VMAP VALUES ARE SHIFTED RIGHT BY ONE BIT TO AVOID WASTE, DO I NEED TO TWEAK ANY OF THE TRACE CODE TO UNDO THAT? I DON'T KNOW IF UPSTREAM HAS DONE THIS OR NOT, NOT CHECKED YET, BUT EVEN IF THEY DO IT CORRECTLY SOME OF MY TWEAKS MAY HAVE BROKEN IT.

!ifndef ACORN {
!ifdef TARGET_PLUS4 {
	SKIP_VMEM_BUFFERS = 1
} else {
!ifdef SKIP_BUFFER {
	SKIP_VMEM_BUFFERS = 1
}
}
} else { ; ACORN
!ifndef ACORN_SHADOW_VMEM {
	SKIP_VMEM_BUFFERS = 1
}
}

!ifndef SKIP_VMEM_BUFFERS {
get_free_vmem_buffer
	; Protect buffer which z_pc points to
	lda vmem_cache_cnt
	tax
	clc
!ifndef ACORN {
	adc #>vmem_cache_start
} else {
    adc vmem_cache_start_mem
}
	cmp z_pc_mempointer + 1
	bne +
	jsr inc_vmem_cache_cnt
	txa
	clc
!ifndef ACORN {
	adc #>vmem_cache_start ; start of cache
} else {
	adc vmem_cache_start_mem ; start of cache
}
+	cmp mempointer + 1
	bne +
	; mempointer points to this page. Store $ff in zp_pc_h so mempointer won't be used
	pha
	lda #$ff
	sta zp_pc_h
	pla
+	stx vmem_cache_cnt
	rts

inc_vmem_cache_cnt
	ldx vmem_cache_cnt
	inx
!ifndef ACORN {
	cpx #vmem_cache_count
} else {
    cpx vmem_cache_count_mem
}
	bcc +
	ldx #0
+	stx vmem_cache_cnt
	rts
}

!ifndef VMEM {
; Non-virtual memory

read_byte_at_z_address
	; Subroutine: Read the contents of a byte address in the Z-machine
	; a,x,y (high, mid, low) contains address.
	; Returns: value in a

	; same page as before?
	cpx zp_pc_l
	bne .read_new_byte
	; same 256 byte segment, just return
!ifdef SKIP_BUFFER {
	txa
	clc
	adc #>story_start
	cmp #first_banked_memory_page
	bcs .read_under_rom
}
.return_result
	+before_dynmem_read_corrupt_a
	lda (mempointer),y
	+after_dynmem_read_corrupt_y
	rts
.read_new_byte
!ifndef TARGET_PLUS4 { ; SFTODO: MAYBE WE CAN GET AWAY WITHOUT THIS ON ACORN TOO? I THINK I MADE THIS CHANGE ON COMMODORE VERSION WITHOUT CONSIDERING ACORN ASPECTS...
	sty mempointer_y
}
	txa
	sta zp_pc_l
	clc
	adc #>story_start
	sta mempointer + 1
!ifdef ACORN { ; SFTODO: IDEALLY WE MIGHT "RENAME" THE TARGET_PLUS4 BELOW (AND IN OTHER PLACES?) TO NO_BANKED_MEMORY OR SIMILAR AND USE THAT RATHER THAN NEEDING TO SPECIAL-CASE ACORN
	bne .return_result ; Always branch
} else {
!ifdef TARGET_PLUS4 {
	bne .return_result ; Always branch
} else {
	cmp #first_banked_memory_page
	bcc .return_result
; Memory under IO / ROM
!ifdef SKIP_BUFFER {
.read_under_rom
	+disable_interrupts
	+set_memory_all_ram_unsafe
	lda (mempointer),y
	+set_memory_no_basic
	+enable_interrupts
	rts
} else { 	
	; Check if this page is in cache
	ldx #vmem_cache_count - 1
-   cmp vmem_cache_page_index,x
	bne +
	txa
	clc
	adc #>vmem_cache_start
	sta mempointer + 1
	bne .return_result ; Always branch
+	dex
	bpl -
	; The requested page was not found in the cache
	; copy vmem to vmem_cache (banking as needed)
	pha
	
	jsr get_free_vmem_buffer
	sta mempointer + 1
	sta vmem_temp
	ldx vmem_cache_cnt
	pla
	sta vmem_cache_page_index,x
	pha
	ldy vmem_temp
	pla
	jsr copy_page

	; set next cache to use when needed
	jsr inc_vmem_cache_cnt
	ldy mempointer_y
	jmp .return_result 
} ; Not SKIP_VMEM_BUFFERS
} ; Not TARGET_PLUS4
}
	
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

; vmap_max_size determines the memory allocated for the vmap; vmap_max_entries
; is the actual run-time limit, which may be less than vmap_max_size but
; obviously can't be larger.
; SFTODO: For a Z3 game 255 is actually likely (not guaranteed) to be slightly
; too large. Not necessarily a problem, but think about it - will there be a
; problem? Are we wasting (a few bytes only) of RAM for no good reason?
vmem_blocksize = 512
vmem_indiv_block_mask = >(vmem_blocksize - 1)
vmem_block_pagecount = vmem_blocksize / 256
!ifndef ACORN {
vmap_max_size = (vmap_buffer_end - vmap_buffer_start) / 2
; If we go past this limit we get in trouble, since we overflow the memory area we can use.
} else { ; ACORN
!ifndef ACORN_SWR {
    !ifndef ACORN_TUBE_CACHE {
        !ifdef ACORN_TURBO_SUPPORTED {
            ; A turbo second processor has enough RAM to hold 255 512-byte blocks.
            max_vmap_max_size = 255
        } else { ; !ACORN_TURBO_SUPPORTED
            max_vmap_max_size = (flat_ramtop - story_start) / 512
        }
    } else { ; ACORN_TUBE_CACHE
        ; The host cache is initialised using "extra" entries in the vmap.
        max_vmap_max_size = 255
        !ifndef ACORN_TURBO_SUPPORTED {
            ; During execution (after the initial preload of the host cache),
            ; vmap_max_entries only covers the second processor's own 64K, so we
            ; don't need large vmap support.
            ACORN_SMALL_RUNTIME_VMAP = 1
        }
    }
} else { ; ACORN_SWR
    ; We might have enough main+sideways RAM to hold 255 512-byte blocks.
    max_vmap_max_size = 255
}

; Set vmap_max_size = min(max_vmap_max_size, ACORN_VMEM_BLOCKS) - there's no
; point allocating space for more vmem blocks than the game can ever use.
!if max_vmap_max_size < ACORN_VMEM_BLOCKS {
    vmap_max_size = max_vmap_max_size
} else {
    vmap_max_size = ACORN_VMEM_BLOCKS
}

!if vmap_max_size < 128 {
    !ifndef ACORN_SMALL_RUNTIME_VMAP {
        ACORN_SMALL_RUNTIME_VMAP = 1
    }
}
} ; end of ACORN
; vmap_max_entries	!byte 0 ; Moved to ZP
; vmap_used_entries	!byte 0 ; Moved to ZP
!ifndef ACORN {
vmap_blocks_preloaded !byte 0
vmap_z_l = vmap_buffer_start
vmap_z_h = vmap_z_l + vmap_max_size
vmap_first_ram_page		!byte 0
} else {
; SFTODODATA 8-ish
!ifndef ACORN_SWR {
; SFTODO: I believe this is constant on Acorn and except for conditional compilation
; pain we could just replace its uses with an immediate constant. Not sure if it's
; worth it for performance, but using a macro to encapsulate the Acorn/Commodore
; difference would be not too bad.
vmap_first_ram_page		!byte ACORN_INITIAL_NONSTORED_BLOCKS + >story_start
}
}
vmap_index !byte 0              ; current vmap index matching the z pointer
vmem_offset_in_block !byte 0         ; 256 byte offset in 512 byte block (0-1)
; vmem_temp !byte 0

vmap_temp			!byte 0,0,0

!ifndef ACORN_SWR {
vmap_c64_offset !byte 0
}

!ifdef TARGET_C128 {
vmap_c64_offset_bank !byte 0
first_vmap_entry_in_bank_1 !byte 0
vmap_first_ram_page_in_bank_1 !byte 0
vmem_bank_temp !byte 0
}

vmem_tick 			!byte $e0
vmem_oldest_age		!byte 0
vmem_oldest_index	!byte 0

; SFTODO: IT LOOKS LIKE THE HIGH/MID BYTES IN THE VMAP ARE NOW STORED SHIFTED RIGHT ONE BIT IN 5.3, WHICH MEANS WE NO LONGER "WASTE" A BIT ON THE ALWAYS-ZERO LOW BIT, ALLOWING AN EXTRA BIT FOR THE TICK. SHOULD UPDATE THE DIAGRAM I DREW AND THE TEXT IN THE TECH MANUAL ACCORDINGLY AND SUBMIT A PULL REQUEST UPSTREAM FOR THIS (CHECK I HAVE THE RIGHT IDEA FIRST).
!ifdef Z8 {
	vmem_tick_increment = 4
	vmem_highbyte_mask = $03
} else {
!ifdef Z3 {
	vmem_tick_increment = 1
	vmem_highbyte_mask = $00
} else {
	vmem_tick_increment = 2
	vmem_highbyte_mask = $01
}
}

!ifdef COUNT_SWAPS {
vmem_swap_count !byte 0,0
}

!ifdef ACORN_TURBO_SUPPORTED {
!macro acorn_adc_vmap_first_ram_page_or_set_mempointer_turbo_bank_from_c {
    ; If we're running on a normal second processor, carry will be clear and we
    ; need to do "adc vmap_first_ram_page". Note that we can't treat this as a
    ; no-op on a turbo second processor simply by setting vmap_first_ram_page to
    ; 0, because on a turbo second processor carry may be set.
    ;
    ; If we're running on a turbo second processor, set mempointer_turbo_bank to
    ; select virtual memory cache bank C, i.e. bank 1+C (bank 0 is used for code
    ; and dynamic memory). This must preserve A, X and Y.
    bit is_turbo
    bpl .is_normal_second_processor
    stz mempointer_turbo_bank
    rol mempointer_turbo_bank
    inc mempointer_turbo_bank
    bra .done
.is_normal_second_processor
	; Carry is already clear
	adc vmap_first_ram_page
.done
}
}

!ifdef DEBUG {
!ifdef PREOPT {
print_optimized_vm_map
	stx zp_temp ; Nonzero means premature exit
!ifdef ACORN {
.handle = zp_temp + 1
    lda #0
    sta .handle
    ldx #2
    ldy #error_print_s_printchar
    jsr setjmp
    beq .print_optimized_vm_map_no_error
    ldy .handle
    beq .not_open
    lda #osfind_close ; 0
    sta .handle
    jsr osfind
.not_open
    jsr error_print_following_string
    !text 13, "Press SPACE to retry...", 0
    jsr wait_for_space
.print_optimized_vm_map_no_error
    lda #osfind_open_output
    ldx #<.preopt_filename
    ldy #>.preopt_filename
    jsr osfind
    sta .handle
}
	jsr printchar_flush
	ldx #$ff
	jsr erase_window
	lda #0
	sta streams_output_selected + 2
	sta is_buffered_window
	jsr print_following_string
!ifndef ACORN {
	!pet 13,"$po$:",0
} else {
	!text 13,"$po$:",0
}

	ldx #0
-	lda vmap_z_h,x
!ifdef ACORN {
    ldy .handle
    jsr osbput
}
	jsr print_byte_as_hex
	lda vmap_z_l,x
!ifdef ACORN {
    ldy .handle
    jsr osbput
}
	jsr print_byte_as_hex
	jsr colon
	inx
	cpx vmap_used_entries
	bcc -

	lda zp_temp
	bne +++
	; Print block that was just to be read
    ; SF: Entries in vmap are shifted right one bit (to avoid wasting a bit which is always
    ; zero because blocks are aligned on 512-byte boundaries); for consistency, we do the
    ; same to zp_pc as we output it.
	lda zp_pc_h
!ifdef ACORN {
    lsr
    php
    ldy .handle
    jsr osbput
}
	jsr print_byte_as_hex
	lda zp_pc_l
!ifdef ACORN {
    plp
    ror
    ldy .handle
    jsr osbput
}
	jsr print_byte_as_hex
	jsr colon
	
+++	
	jsr print_following_string
	!pet "$$$$",0
!ifndef ACORN {
	jsr kernal_readchar   ; read keyboard
	jmp kernal_reset      ; reset
} else {
    lda #osfind_close
    ldy .handle
    jsr osfind
	jsr print_following_string
    !text "Saved PREOPT - press BREAK", 0
-   jmp -

.preopt_filename
    !text "PREOPT", 13
}
}

!ifdef TRACE_VM {
print_vm_map
!zone {
	; print caches
	jsr space
	lda #66
	jsr streams_print_output
	jsr space
	lda vmem_cache_cnt
	jsr printa
	jsr space
	jsr dollar
	lda vmem_cache_page_index
	jsr print_byte_as_hex
	jsr space
	jsr dollar
	lda vmem_cache_page_index + 1
	jsr print_byte_as_hex
	jsr space
	jsr dollar
	lda vmem_cache_page_index + 2
	jsr print_byte_as_hex
	jsr space
	jsr dollar
	lda vmem_cache_page_index + 3
	jsr print_byte_as_hex
	jsr newline
	ldy #0
-	; print
!ifdef ACORN_SWR {
    ; SFTODO: THIS IS PROB RIGHT, BUT DO WE NEED ANYTHING LIKE THIS FOR THE TURBO CASE AS WELL??
    cpy #100
    bcs +
    jsr space ; alignment when <100
}
	cpy #10
	bcs +
	jsr space ; alignment when <10
+   jsr printy
	jsr space
	lda vmap_z_h,y ; zmachine mem offset ($0 -
    ; SF: I changed the masks here, I think this is correct but it is a
    ; divergence from upstream. SFTODO: IS THAT STILL TRUE? IF SO MAYBE SUGGEST THIS CHANGE TO UPSTREAM
    and #($ff xor vmem_highbyte_mask)
;	and #%11100000 ; SFTODO: OLD
	jsr print_byte_as_hex
	jsr space
	jsr dollar
	lda vmap_z_h,y ; zmachine mem offset ($0 -
    and #vmem_highbyte_mask
	;and #%00011111 ; SFTODO: OLD
	jsr printa
	lda vmap_z_l,y ; zmachine mem offset ($0 - 
	jsr print_byte_as_hex
	lda #0 ; add 00
	jsr print_byte_as_hex
    ; SF: For ACORN_SWR we don't try to calculate the physical address of the
    ; VM block as it's moderately involved.
!ifndef ACORN_SWR {
	jsr space
	tya
	asl
!ifdef ACORN_TURBO_SUPPORTED {
    bit is_turbo
    bpl +
    pha
    lda #1
    adc #0
    jsr print_byte_as_hex
    pla
    bra .skip_adc_vmap_first_ram_page
+
}
	adc vmap_first_ram_page
.skip_adc_vmap_first_ram_page
	jsr print_byte_as_hex
	lda #$30
	jsr streams_print_output
	lda #$30
	jsr streams_print_output
}
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
!ifdef TARGET_C128 {
	ldy #0
	sty vmem_bank_temp
	cmp first_vmap_entry_in_bank_1
	bcc .in_bank_0
	sbc first_vmap_entry_in_bank_1 ; Carry is already set
	asl
	adc vmap_first_ram_page_in_bank_1 ; Carry is already clear
	tay ; This value need to be in y when we jump to load_blocks_from_index_using_cache 
	inc vmem_bank_temp
	bne load_blocks_from_index_using_cache ; Always branch
.in_bank_0
}	
	asl
!ifndef ACORN_TURBO_SUPPORTED {
	; Carry is already clear
	adc vmap_first_ram_page
} else {
    +acorn_adc_vmap_first_ram_page_or_set_mempointer_turbo_bank_from_c
}

; SFTODO: Maybe add some tracing for this
!ifdef ACORN_TUBE_CACHE {
    ; Offer the cache on the host a chance to save the block we're about to
    ; evict from our cache, and ask it if it has the block we want before we
    ; go to disk for it.
    sta osword_cache_data_ptr + 1
!ifdef ACORN_TURBO_SUPPORTED {
    ; If we're on a normal second processor this is redundant but harmless, and
    ; it's only one cycle slower to just do it rather than check if we need to
    ; do it first.
    lda mempointer_turbo_bank
    sta osword_cache_data_ptr + 2
}
    ; Other bytes at osword_cache_data_ptr always stay 0 and don't need setting.
    lda vmap_z_l,x
    sta osword_cache_index_requested
!if vmem_highbyte_mask > 0 {
    lda vmap_z_h,x
    and #vmem_highbyte_mask
    sta osword_cache_index_requested + 1
} else {
    ; osword_cache_index_requested + 1 is initialised to 0 and will never change.
}
    lda #osword_cache_op
    ldx #<osword_cache_block
    ldy #>osword_cache_block
    jsr osword
    lda osword_cache_result
    beq load_blocks_from_index_done ; Host cache has provided the block
    ; The host cache didn't have the block we want. Restore A and X and fall
    ; through to the following code to get it from disk.
    lda osword_cache_data_ptr + 1
    ldx vmap_index
}
} else { ; ACORN_SWR
    ldx vmap_index
    jsr convert_index_x_to_ram_bank_and_address
!ifdef ACORN_SHADOW_VMEM {
    bvc .block_not_in_cache
    sta vmem_temp
    ; Forget any cache pages belonging to the old block at this position.
    ldy vmem_cache_count_mem
    dey
-   lda vmem_cache_page_index,y
    and #(255 - vmem_indiv_block_mask)
    cmp vmem_temp
    bne +
    lda #0
    sta vmem_cache_page_index,y
+   dey
    bpl -
    ; We need to load into scratch_double_page; we'll copy into shadow RAM later.
    ; SFTODO: On some machines we could probably load directly into shadow RAM, but
    ; I am not sure it's worth the extra complication. (On a Master, for example, the
    ; OS is likely to adjust ACCCON during this process; would it leave it as it found
    ; it, given we'd have played around with it directly to page in shadow RAM?)
    lda #>scratch_double_page
.block_not_in_cache
}
}

!ifdef TRACE_FLOPPY {
	jsr comma
	jsr print_byte_as_hex
}
    ; SFTODO: OLD CODE (NOT JUST FOR ACORN) USED TO STORE 0 TO readblocks_mempos; I SUSPECT THAT'S REDUNDANT (READS ARE ALWAYS 512-BYTE ALIGNED) BUT PUTTING THIS NOTE HERE IN CASE IT TURNS OUT THIS BREAKS SOMETHING ACORN-Y
	sta readblocks_mempos + 1
!ifndef ACORN {
!ifndef TARGET_PLUS4 {
	tay ; This value need to be in y if we jump to load_blocks_from_index_using_cache
	cmp #first_banked_memory_page
	bcs load_blocks_from_index_using_cache
}
}
!ifdef ACORN_TURBO_SUPPORTED {
    ; If we're on a normal second processor this is redundant but harmless, and
    ; it's only one cycle slower to just do it rather than check if we need to
    ; do it first.
    lda mempointer_turbo_bank
    sta readblocks_mempos + 2
}
	lda #vmem_block_pagecount ; number of blocks
	sta readblocks_numblocks
	lda vmap_z_l,x ; start block
	asl
	sta readblocks_currentblock
!if vmem_highbyte_mask > 0 {
	lda vmap_z_h,x ; start block
	and #vmem_highbyte_mask
} else {
	lda #0
}
	rol
	sta readblocks_currentblock + 1
	jsr readblocks
!ifdef ACORN_SHADOW_VMEM {
    ; Did we just load into scratch_double_page? If so, we need to copy this
    ; block into shadow RAM.
    lda readblocks_mempos + 1
    cmp #(>scratch_double_page) + vmem_block_pagecount ; readblocks advanced this
    bne .not_shadow_load
    lda #>scratch_double_page
    ldy vmem_temp
    jsr shadow_ram_copy ; SFTODO: RENAME THIS shadow_ram_copy_a_to_y?
    lda #(>scratch_double_page) + 1
    ldy vmem_temp
    iny
    jsr shadow_ram_copy
.not_shadow_load
}
load_blocks_from_index_done ; except for any tracing
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

!ifdef ACORN_TUBE_CACHE {
osword_cache_block
    !byte 12 ; send block length
    !byte 12 ; receive block length
osword_cache_data_ptr
    !word 0  ; data address low
    !word 0  ; data address high
osword_cache_index_offered
    !word 0  ; block index offered
osword_cache_index_offered_timestamp_hint
    !byte 0  ; block offered timestamp hint
osword_cache_index_requested
    !word 0  ; block index requested
osword_cache_result
    !byte 0  ; result
}

!ifndef ACORN {
!ifndef TARGET_PLUS4 {
load_blocks_from_index_using_cache
	; vmap_index = index to load
	; vmem_cache_cnt = which 256 byte cache use as transfer buffer
	; y = first c64 memory page where it should be loaded
	; For C128: vmem_bank_temp = RAM bank in which page y resides
	; side effects: a,y,x,status destroyed
	; initialise block copy function (see below)

	jsr get_free_vmem_buffer
	sta vmem_temp
	
	sty vmem_temp + 1
	ldx #0 ; Start with page 0 in this 512-byte block
	; read next into vmem_cache
-   lda vmem_temp ; start of cache
	sta readblocks_mempos + 1
	txa
	pha
	sta vmap_temp
	ldx vmap_index
	lda vmap_z_l,x ; start block
	asl
	ora vmap_temp
	sta readblocks_currentblock
!if vmem_highbyte_mask > 0 {
	lda vmap_z_h,x ; start block
	and #vmem_highbyte_mask
} else {
	lda #0
}
	rol
	sta readblocks_currentblock + 1
	jsr readblock
	; copy vmem_cache to block (banking as needed)
	lda vmem_temp
	ldy vmem_temp + 1
!ifdef TARGET_C128 {
	ldx vmem_bank_temp
	jsr copy_page_c128
} else {
	jsr copy_page
}
	inc vmem_temp + 1
	pla
	tax
	inx
	cpx #vmem_block_pagecount ; read 2 blocks (512 bytes) in total
	bcc -

	ldx vmem_temp + 1
	dex
	txa
	ldx vmem_cache_cnt
	sta vmem_cache_page_index,x
!ifdef TARGET_C128 {
	lda vmem_bank_temp
	sta vmem_cache_bank_index,x
}
	rts
}
}

; SF: Note that this is allowed to corrupt X and Y. SFTODO PROBABLY STILL TRUE IN 5.3 BUT MAYBE CHECK
read_byte_at_z_address
	; Subroutine: Read the contents of a byte address in the Z-machine
	; a,x,y (high, mid, low) contains address.
	; Returns: value in a

!ifdef TARGET_C128 {
	; TODO: For C128, we do the dynmem check both here and 40 lines down. Make it better!
	cmp #0
	bne .not_dynmem
	cpx nonstored_blocks
	bcs .not_dynmem

	; This is in dynmem, so we always read from bank 1
	txa
	clc
	adc #>story_start_bank_1
	sta vmem_temp + 1
	lda #0
	sta vmem_temp
	lda #vmem_temp
	sta $02aa
	ldx #$7f
	jmp $02a2
	
.not_dynmem	
}


	; same page as before?
	cpx zp_pc_l
	bne .read_new_byte
	cmp zp_pc_h
	bne .read_new_byte
	; same 256 byte segment, just return
!ifdef ACORN_SWR {
    ; In either the big or small dynamic memory model, at this point we could be
    ; accessing dynamic or read-only memory and needing to page in the relevant
    ; SWR bank is a possibility. However, if we're accessing main RAM, we don't
    ; need to page in mempointer_ram_bank to do the read or revert the paging
    ; afterwards. This is obviously a saving when it happens, but it comes at
    ; the cost of checking the high bit of mempointer. Playing around with the
    ; benchmark and HHGTTG, >70% of executions of this code are accessing main
    ; RAM, so it's a win to pay 5 cycles for the test in order to save 20 cycles
    ; paging. (0.7*5+(1-0.7)*(20+5)=11<20.) SFTODO: COMMENT IS OUTDATED NOW WE HAVE MEDIUM MODEL - THE POINT IS POSSIBLY STILL VALID, BUT WOULD BE GOOD TO PROFILE IT
    lda mempointer + 1
    bmi .not_main_ram
    lda (mempointer),y
!ifdef ACORN_DEBUG_ASSERT {
    ; Let's just prove it's OK to be corrupting X and Y.
    ldx #42
    ldy #86
}
    rts
.not_main_ram
    +acorn_page_in_bank_using_a mempointer_ram_bank
}
.read_and_return_value
!ifndef ACORN_SWR {
    ; SF: On ACORN_SWR builds, we're *not* necessarily reading dynamic memory
    ; here - we may be, but we may be reading from a read-only VM page. We
    ; therefore control the sideways RAM paging explicitly in this code;
    ; before_dynmem_read_corrupt_a might incorrectly page in the bank containing
    ; dynamic memory.
	+before_dynmem_read_corrupt_a
}
	lda (mempointer),y
!ifndef ACORN_SWR {
	+after_dynmem_read_corrupt_y
} else {
    +acorn_swr_page_in_default_bank_using_y
}
!ifdef ACORN_DEBUG_ASSERT {
    ; Let's just prove it's OK to be corrupting X and Y.
    ldx #42
    ldy #86
}

	rts
    ; SFTODO: TAKE A LOOK AT ZP USE FOR MEMPOINTER_RAM_BANK AND DEFAULT_BANK_USING_Y
.read_new_byte
	sta zp_pc_h
	stx zp_pc_l
!ifndef TARGET_C128 {
	cmp #0
	bne .non_dynmem
	cpx nonstored_blocks
	bcs .non_dynmem
	; Dynmem access
    ; SFTODO: I think this is a relatively rare case; it certainly is in the
    ; benchmark, so it may not be worth being too tricksy with optimisation in
    ; this code path.
	txa
	adc #>story_start
!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
    cmp acorn_screen_hole_start_page
    bcc +
    adc acorn_screen_hole_pages_minus_one ; -1 because carry is set
+   sta mempointer + 1
} else {
	sta mempointer + 1
}
!ifdef ACORN_TURBO_SUPPORTED {
    ; We need to ensure bank 0 is accessed for dynamic memory on a turbo second
    ; processor. This isn't necessary on an ordinary second processor, but it's
    ; harmless and it's faster to just do it rather than check if it's
    ; necessary. SFTODO: AS IN MANY PLACES, CAN/SHOULD I REWORK HOW THIS IS HANDLED IN THE 5.3 PORT NOW UPSTREAM HAS SOME CONCEPTS OF SPECIAL HANDLING/MACROS/ETC?
    stz mempointer_turbo_bank
    bra .read_and_return_value
} else {
!ifndef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; SF: On an ACORN_SWR_SMALL_DYNMEM build, all dynamic memory is in main
    ; RAM so it doesn't matter what the value of mempointer_ram_bank is or which
    ; bank is currently paged in.
    ; SFTODO: I think we could just do the lda+rts here; since we haven't paged out the
    ; current bank there's no need to page it back in afterwards, as read_and_return_value will.
	bne .read_and_return_value ; Always branch
} else { ; ACORN_SWR_MEDIUM_OR_BIG_DYNMEM
    ; SFTODO: WE COULD POSSIBLY DO "bpl read_and_return_value" or "bmi +:lda:rts:+" HERE, SINCE IF THE ADDRESS ISN'T IN SWR WE DON'T NEED TO PAGE ANYTHING IN OR OUT TO GET TO THE DATA
    ; We have to page in the first SWR bank now, and we have to set
    ; mempointer_ram_bank correctly so subsequent calls to
    ; read_byte_at_z_address don't page in the wrong bank.
    +acorn_page_in_bank_using_a dynmem_ram_bank
    sta mempointer_ram_bank
    bpl .read_and_return_value ; Always branch SFTODO THIS WON'T WORK IF WE START SUPPORT 12K PRIVATE RAM ON B+
}
}
}
.non_dynmem
SFTODOLL8
	sty mempointer_y
	lsr
	sta vmem_temp + 1
	lda #0
	sta vmap_quick_index_match
	txa
	and #vmem_indiv_block_mask ; keep index into kB chunk SFTODO: (UPSTREAM) COMMENT IS OUTDATED, SHOULD SAY 512-BYTE CHUNK (BLOCK)
	sta vmem_offset_in_block
	txa
	ror
	sta vmem_temp
	; Check quick index first
    ; SFTODO: Would it be worth asserting/ensuring the following loop doesn't incur a page-crossing penalty?
	ldx #vmap_quick_index_length - 1
-	ldy vmap_quick_index,x
	cmp vmap_z_l,y ; zmachine mem offset ($0 -
	beq .quick_index_candidate
--	dex
	bpl -
	bmi .no_quick_index_match ; Always branch
.quick_index_candidate
!if vmem_highbyte_mask > 0 {
	lda vmap_z_h,y
	and #vmem_highbyte_mask
	cmp vmem_temp + 1
	beq .quick_index_match
	lda vmem_temp
	jmp --
}
.quick_index_match
	inc vmap_quick_index_match
	sty vmap_index
	jmp .index_found
	
.no_quick_index_match
	lda vmem_temp

	; is there a block with this address in map?
	ldx vmap_used_entries
-   ; compare with low byte
	cmp vmap_z_l - 1,x ; zmachine mem offset ($0 -
	beq +
.check_next_block
	dex
	bne - ; SFTODO: Just might be worth asserting this branch doesn't suffer page-crossing penalty (or perhaps just silently inserting some padding after the "jmp .index_found" above if necessary to ensure it's not - but do some timings first to see how much difference it really makes)
	beq .no_such_block ; Always branch
	; is the highbyte correct?
+
!if vmem_highbyte_mask > 0 {
	lda vmap_z_h - 1,x
	and #vmem_highbyte_mask
	cmp vmem_temp + 1
	beq .correct_vmap_index_found
	lda vmem_temp
	jmp .check_next_block
}
.correct_vmap_index_found
	; vm index for this block found
        dex
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

.timestamp_equal
    ; SF: If vmap_z_h,x == vmem_oldest_age, consider this index if we haven't
    ; yet found one. (This is mostly unnecessary, but VMEM_STRESS can easily
    ; trigger this, and I believe it could in principle happen with more vmap
    ; entries, although of course the more you have the less likely it becomes.
    ; SFTODO: It might be worth mentioning this to upstream if it works out OK
    ; here, although the Acorn port is more at risk, because there are
    ; situations like a small game running on a B-no-SWR where we might end up
    ; with a very small number of vmap entries.)
    ; SFTODO: I don't believe this will significantly hit performance, but it
    ; might be worth profiling it at some point. (We can just remove the branch
    ; to this code, and it's very unlikely to break things.)
    ldy vmem_oldest_index
    iny
    bne .try_next_index ; branch if vmem_oldest_index != $ff
    beq .found_older ; always branch

; no index found, add last
.no_such_block

	; Load 512 byte block into RAM
!if SUPPORT_REU = 1 {
	; First, check if this is initial REU loading
	ldx use_reu
	cpx #$80
	bne .not_initial_reu_loading
	ldx #0
	lda vmap_z_l ; ,x is not needed here, since x is always 0
	asl
	cmp z_pc + 1
	bne .block_chosen
	inx ; Set x to 1
	bne .block_chosen ; Always branch
}

; SFTODO: Not sure right now, but it may be this little block of code is not needed on Acorn, depending on how vmap_used_entries is initialised. - I believe on Acorn the bcc can only occur when we're in PREOPT mode
.not_initial_reu_loading
	ldx vmap_used_entries
	cpx vmap_max_entries
	bcc .block_chosen

!ifdef DEBUG {
!ifdef PREOPT {
	ldx #0
	jmp print_optimized_vm_map
}	
}	
	; Find the best block to replace

	; Create a copy of the block z_pc points to, shifted one step to the right, 
	; to be comparable to vmap entries
	lda z_pc
	lsr
	sta vmap_temp + 1
	lda z_pc + 1
	ror
	sta vmap_temp + 2

	; Store very recent oldest_age so the first valid index in the following
	; loop will be picked as the first candidate. SFTODO: COMMENT IS OUT OF DATE NOW
	lda #$ff
	sta vmem_oldest_index
	sta vmem_oldest_age
	
	; Check all indexes to find something older
	ldx vmap_used_entries
	dex
-
 	lda vmap_z_h,x
	cmp vmem_oldest_age
    beq .timestamp_equal
	bcs .try_next_index
.found_older
	; Found older
	; Skip if z_pc points here; it could be in either page of the block.
	ldy vmap_z_l,x
	cpy vmap_temp + 2
!if vmem_highbyte_mask > 0 {
	bne ++
	tay
	and #vmem_highbyte_mask
	cmp vmap_temp + 1
	beq .try_next_index
	tya
} else {
	beq .try_next_index
}
++	sta vmem_oldest_age
	stx vmem_oldest_index
.try_next_index
	dex
	cpx #$ff
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
	
	cpx vmap_used_entries
	bcc +
	; This block was unoccupied
	inc vmap_used_entries
+
	txa
	
!ifdef TARGET_C128 {
	; TODO: C128: Check if x is >= vmap_first_ram_page_in_bank_1
	cmp first_vmap_entry_in_bank_1
	bcc + ; Not in bank 1
	ldy #1
	sty vmap_c64_offset_bank
	sbc first_vmap_entry_in_bank_1 ; Carry already set
	clc
	asl ; Multiply by 2 to count in 256-byte pages rather than 512-byte vmem blocks
	adc vmap_first_ram_page_in_bank_1
	bne ++ ; Always branch
+
	ldy #0
	sty vmap_c64_offset_bank
}
!ifndef ACORN_SWR {
	asl

!ifndef ACORN_TURBO_SUPPORTED {
	; Carry is already clear
	adc vmap_first_ram_page
} else {
    +acorn_adc_vmap_first_ram_page_or_set_mempointer_turbo_bank_from_c
}
++	sta vmap_c64_offset
}



!ifdef DEBUG {
	lda vmem_oldest_index
	cmp #$ff
	bne +
	lda #ERROR_NO_VMEM_INDEX
	jsr fatalerror
+
}

	; We have now decided on a map position where we will store the requested block. Position is held in x.
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
    ; SF: For ACORN_SWR we don't try to calculate the physical address of the
    ; VM block as it's moderately involved.
!ifndef ACORN_SWR {
	jsr dollar
!ifdef ACORN_TURBO_SUPPORTED {
    bit is_turbo
    bpl +
    lda mempointer_turbo_bank
    jsr print_byte_as_hex
+
}
	lda vmap_c64_offset
	jsr print_byte_as_hex
	jsr colon
}
	cpx vmap_used_entries
	bcs .printswaps_part_2
	lda vmap_z_h,x
    ; SFTODO: I altered the mask here, I think it's correct but it's a divergence
    ; from upstream. - it was "and #$7" - if this is correct, maybe suggest change to upstream?
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
	jsr print_byte_as_hex
	jsr space
++	
}
}

!ifndef ACORN {
!ifndef TARGET_PLUS4 {
	; Forget any cache pages belonging to the old block at this position.
	lda vmap_c64_offset
	cmp #first_banked_memory_page
	bcc .cant_be_in_cache
	ldy #vmem_cache_count - 1
-	lda vmem_cache_page_index,y
	and #(255 - vmem_indiv_block_mask)
	cmp vmap_c64_offset
	bne +
!ifdef TARGET_C128 {
	lda vmem_cache_bank_index,y
	cmp vmap_c64_offset_bank
	bne +
}
	lda #0
	sta vmem_cache_page_index,y
+	dey
	bpl -
.cant_be_in_cache
} ; not TARGET_PLUS4
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
!ifdef ACORN_SMALL_RUNTIME_VMAP {
	bpl -
} else {
    cpx #255
    bne -
}
	
	pla
	tax
	lda #$80
+	sta vmem_tick

!ifdef ACORN_TUBE_CACHE {
    ; Save the Z-address of the block we're about to evict before we overwrite it.
    lda vmap_z_l,x
    sta osword_cache_index_offered
!if vmem_highbyte_mask > 0 {
    lda vmap_z_h,x
    and #vmem_highbyte_mask
} else {
    ; SFTODO: It *may* be that osword_cache_index_offered + 1 would simply always be 0 and we could move the sta into the > 0 block and do nothin in this else. However, the way the initial load modifies index_offered+1 a lot makes me a bit nervous about this - of course, it could simply make sure it explicitly zeroes this at the end if highbyte_mask is 0. But for now let's just go with this.
    lda #0
}
    sta osword_cache_index_offered + 1
}

	; Store address of 512 byte block to load, then load it
	lda zp_pc_h
	lsr
	sta vmap_z_h,x
	lda zp_pc_l
	ror
	sta vmap_z_l,x
	stx vmap_index
    ; SF: Be aware that if tracing is on here, the newly loaded block will
    ; show with its pre-adjustment tick. SFTODO PROB STILL TRUE IN 5.3 BUT CHECK
	jsr load_blocks_from_index
.index_found
	; index found
	; Update tick for last access 
	ldx vmap_index
!if vmem_highbyte_mask > 0 {
	lda vmap_z_h,x
	and #vmem_highbyte_mask
	ora vmem_tick
} else {
	lda vmem_tick
}
	sta vmap_z_h,x
!ifndef ACORN_SWR {
	txa

!ifdef TARGET_C128 {
	cmp first_vmap_entry_in_bank_1
	bcc .not_in_bank_1
	ldy #1
	sty vmap_c64_offset_bank
	sbc first_vmap_entry_in_bank_1 ; Carry already set
	asl ; Multiply by 2 to count in 256-byte pages rather than 512-byte vmem blocks
	adc vmap_first_ram_page_in_bank_1 ; Carry already clear
	bne .store_offset ; Always branch
.not_in_bank_1	
	ldy #0
	sty vmap_c64_offset_bank
}	

	asl
!ifndef ACORN_TURBO_SUPPORTED {
	; Carry is already clear
	adc vmap_first_ram_page
} else {
    +acorn_adc_vmap_first_ram_page_or_set_mempointer_turbo_bank_from_c
}
.store_offset
	sta vmap_c64_offset

!ifndef ACORN {
!ifndef TARGET_PLUS4 {
!ifdef TARGET_C128 {
	; Bank is in y at this point
	cpy #0
	bne .swappable_memory
}
	cmp #first_banked_memory_page
	bcc .unswappable
.swappable_memory
	; this is swappable memory
	; update vmem_cache if needed
	clc
	adc vmem_offset_in_block
	; Check if this page is in cache
	ldx #vmem_cache_count - 1
	tay
-	tya
	cmp vmem_cache_page_index,x
!ifdef TARGET_C128 {
	bne .not_a_match
	lda vmap_c64_offset_bank
	cmp vmem_cache_bank_index,x
	bne .not_a_match
	beq.cache_updated
.not_a_match
} else {
	beq .cache_updated
}
	dex
	bpl -
	; The requested page was not found in the cache
	; copy vmem to vmem_cache (banking as needed)
	sty vmem_temp
	jsr get_free_vmem_buffer
	tay
!ifdef TARGET_C128 {
	lda vmap_c64_offset_bank
	sta vmem_cache_bank_index,x
}
	lda vmem_temp
	sta vmem_cache_page_index,x
!ifdef TARGET_C128 {
	stx vmem_temp + 1
	ldx vmap_c64_offset_bank
	jsr copy_page_c128
	ldx vmem_temp + 1
} else {
	jsr copy_page
}
	lda vmem_cache_cnt
	jsr inc_vmem_cache_cnt
	tax
.cache_updated
	; x is now vmem_cache (0-3) where current z_pc is
	txa
	clc
	adc #>vmem_cache_start
	sta mempointer + 1
	ldx vmap_index
	bne .return_result ; always true
.unswappable
} ; not TARGET_PLUS4
}

	; update memory pointer
	lda vmem_offset_in_block
	clc
	adc vmap_c64_offset
} else {
    jsr convert_index_x_to_ram_bank_and_address
!ifdef ACORN_SHADOW_VMEM {
    bvc .block_directly_accessible
    ; This block is held in shadow RAM. Is it already in the cache?
    clc
    adc vmem_offset_in_block
    ldx vmem_cache_count_mem
    dex
-   cmp vmem_cache_page_index,x
    beq .cache_updated
    dex
    bpl -
    ; No, it's not in the cache. Copy it in.
    sta vmem_temp
    jsr get_free_vmem_buffer
    tay
    lda vmem_temp
    jsr shadow_ram_copy
    lda vmem_cache_cnt
    jsr inc_vmem_cache_cnt
    tax
.cache_updated
    ; X now identifies the page in vmem_cache containing this block.
    txa
    clc
    adc vmem_cache_start_mem
    sta mempointer + 1
    ldx vmap_index
    bne .return_result ; always true SFTODONOW: IS IT? WHAT IF WE HAVE *NO* SIDEWAYS RAM AND *NO* VMEM-IN-MAIN RAM? VMAP_INDEX 0 COULD THEN BE A SHADOW RAM BLOCK COULDN'T IT?
-   jmp - ; SFTODO: TO CATCH FAILURE AS PER ABOVE SFTODONOW
.block_directly_accessible
}
    clc
    adc vmem_offset_in_block
}
	sta mempointer + 1
.return_result
	ldy mempointer_y
!ifndef ACORN_SWR {
    ; See comments on analogous code at .read_and_return_value.
	+before_dynmem_read_corrupt_a
}
	lda (mempointer),y
!ifndef ACORN_SWR {
	+after_dynmem_read_corrupt_y
} else {
    +acorn_swr_page_in_default_bank_using_y
}
	rts

!ifdef ACORN_SWR {
; SFTODO: Not sure I will want this as a subroutine, but let's write it here
; like this to help me think about it. For the moment it returns page of physical
; memory in A and ram bank is selected and stored at mempointer_ram_bank.
; adjust_dynamic_memory_inline also relies on the RAM bank index being returned
; in Y. On ACORN_SHADOW_VMEM build this also returns with V set iff A is an
; address in shadow RAM.
convert_index_x_to_ram_bank_and_address
    ; 0<=X<=254 is the index of the 512-byte virtual memory block we want to
    ; access. Index 0 may be in main RAM or sideways RAM, depending on the size
    ; of dynamic memory.
    txa
    sec
    sbc vmem_blocks_in_main_ram
    bcc .in_main_ram
!ifdef ACORN_SHADOW_VMEM {
    cmp vmem_blocks_in_sideways_ram
    bcs .in_shadow_ram
    ; clc - carry is already clear
} else {
    clc
}
!ifndef ACORN_SWR_SMALL_DYNMEM {
    adc vmem_blocks_stolen_in_first_bank ; always 0 for small dynmem model
}
    ; CA is now the 9-bit block offset of the required data from the start of
    ; our first sideways RAM bank. Each 16K bank has 32 512-byte blocks, so
    ; we need to divide by 32=2^5 to get the bank index.
    pha
    ror
    lsr
    lsr
    lsr
    lsr
    tay
    +acorn_page_in_bank_using_a_comma_y ram_bank_list ; leaves bank in A
    sta mempointer_ram_bank
!if 0 {
    ; SFTODONOWHACK
    cmp #0
-   beq -
}
    ; Now get the low 5 bits of the block offset, multiply by two to convert to
    ; 256 byte pages and that gives us the page offset within the bank.
    pla
    and #31
    asl
    ; Carry is already clear
    adc #$80
!ifdef ACORN_SHADOW_VMEM {
    clv
}
    rts
.in_main_ram
    ; A contains a negative block offset from the top of main RAM (BBC sideways
    ; RAM version) or the bottom of screen RAM (Electron sideways RAM version).
    ; Multiply by two to get a page offset and add it to the base to get the
    ; actual start.
    asl
    ; Carry is set
    adc #(>flat_ramtop)-1
!ifdef ACORN_SCREEN_HOLE {
    sec ; SFTODO: can we in fact rely on carry having a known state here and avoid this?
    sbc acorn_screen_hole_pages ; SFTODO: MAYBE DO CLC AND USE MINUS 1, IF IT AVOIDS HAVING TO *AHVE* THE NON-MINUS-1 VERSION
}
!ifdef ACORN_SHADOW_VMEM {
    clv
}
    rts
!ifdef ACORN_SHADOW_VMEM {
.in_shadow_ram
    ; sec - carry is already set
    sbc vmem_blocks_in_sideways_ram
    asl ; convert to 256-byte blocks
    ; We have at most 19K of spare shadow RAM, so 0 <= A < 19*4 < 128.
    ; clc - carry is already clear
-   bcs - ; SFTODO TOTAL PARANOIA, DELETE LATER
    adc #$30 ; SFTODO: MAGIC CONSTANT IN A COUPLE OF PLACES, USE SOMETHING LIKE shadow_start = $3000 IN CONSTANTS FOR ACORN_SHADOW_VMEM BUILD
    bit .in_shadow_ram_rts ; set V
.in_shadow_ram_rts
    rts
}
}
}

; SFTODO: For now I'm going to pre-fill this as part of the build
; SFTODODATA - THIS IS INITIALISED, BUT I AM HALF THINKING WE SHOULD JUST
; POPULATE IT IN THE DISCARDABLE INIT CODE - BUT MAYBE DON'T RUSH INTO THIS AS
; SWR AND 'SUGGESTED' PAGES AND PREOPT WILL AFFECT THIS DECISION
!ifdef ACORN {
!ifdef VMEM {
; vmap_z_l lives in low workspace so we never incur page-crossing penalties when
; accessing it in hot loops. vmap_z_h isn't so sensitive.
vmap_z_h
    !FILL vmap_max_size, 'V'
}
}

; SFTODO: WHEN I COME TO TRY TO SUPPORT THE UP-TO-19K FREE IN THE SHADOW BANK, IT MAY BE POSSIBLE TO SUPPORT THIS ON THE B+ BY USING OSRDSC. (We could in principle put copy code in the special RAM at $Axxx or wherever, but since it needs to be able to copy to/from arbitrary SWR banks I don't think that's very practical. We could potentially copy like that via a bounce buffer in main RAM, but it's an extra copy - it would depend how slow OSWRSC/OSRDSC are/if they can do what we need.)

; SFTODO: Could/should we identify any free memory in the "private 12K" on a Master and use it as vmem cache? I suspect we'd need to treat it a bit like shadow RAM and copy it to/from main memory, but maybe not. This might be a bit too hacky anyway, but worth a thought. Definitely something to do only after adding use of spare shadow RAM.
