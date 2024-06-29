; Initialization subroutines which will be placed inside the Z-machine stack.

; As the code in this file gets overwritten when we start to run the game, we
; put it all in its own zone and make an effort to use local labels for almost
; everything to reduce the risk of accidentally calling a subroutine in here
; after it has been overwritten.
!zone deletable_init {

!ifdef ACORN_TUBE_CACHE {
host_cache_size_vmem_blocks !fill 1
}

!ifdef VMEM {
vmap_meaningful_entries !fill 1
.inflated_vmap_max_entries !fill 1
.from_index !fill 1
}

.blocks_to_read !fill 1 ; SFTODO: RENAME pages_to_read? But maybe not, because this is e.g. used with the upstream-named readblocks function etc.


screenkernal_init
    +screenkernal_init_inline
.screenkernal_init_rts
    rts


half_block_graphic = 181
full_block_graphic = 255
progress_indicator_one_block = 1 << progress_indicator_fractional_bits

progress_indicator_blocks_per_step !fill 2
progress_indicator_blocks_until_next_step !fill 2
progress_indicator_graphic !byte half_block_graphic

update_progress_indicator
    ; {{{
    ; progress_indicator_blocks_until_next_step -= 1 (but fixed point)
    sec
    lda progress_indicator_blocks_until_next_step
    sbc #<progress_indicator_one_block
    sta progress_indicator_blocks_until_next_step
    lda progress_indicator_blocks_until_next_step + 1
    sbc #>progress_indicator_one_block
    sta progress_indicator_blocks_until_next_step + 1
    jmp .while_test
.while_loop
    ; while progress_indicator_blocks_until_next_step <= 0:
    ;     extend the progress bar by one step (half a block graphic)
    ;     progress_indicator_blocks_until_next_step += progress_indicator_blocks_per_step
    ;
    ; We need a while loop since on machines with very little memory, reading a
    ; single 512-byte block might account for multiple steps of the progress
    ; bar.
    ;
    ; Alternate between outputting a half block graphic and a backspace+full
    ; block graphic.
    lda progress_indicator_graphic
    cmp #half_block_graphic
    beq +
    lda #vdu_back
    jsr oswrch
    lda #full_block_graphic
+   jsr oswrch
    eor #half_block_graphic xor full_block_graphic
    sta progress_indicator_graphic
    clc
    lda progress_indicator_blocks_until_next_step
    adc progress_indicator_blocks_per_step
    sta progress_indicator_blocks_until_next_step
    lda progress_indicator_blocks_until_next_step + 1
    adc progress_indicator_blocks_per_step + 1
    sta progress_indicator_blocks_until_next_step + 1
.while_test
    bmi .while_loop
    ora progress_indicator_blocks_until_next_step
    beq .while_loop
    rts
    ; }}}


!ifdef ACORN_TUBE_CACHE {
; Set A=min(>(flat_ramtop - story_start), ACORN_GAME_PAGES), i.e. the number of
; pages of RAM we actually have on a normal second processor without counting
; host cache, capped at the actual size of the game.
; SFTODO: Only one caller now - inline? Arguably clearer as a subroutine, think about it.
calculate_normal_tube_own_ram_pages ; SFTODO: RENAME??
    lda #>(flat_ramtop - story_start)
!if (>ACORN_GAME_PAGES) = 0 {
    cmp #<ACORN_GAME_PAGES
    bcc +
    lda #<ACORN_GAME_PAGES
}
+   rts
}


!ifndef ACORN {
	!error "Non-Acorn code for deletable_init has been removed"
}

; Initialization performed shortly after startup, just after
; acorn_deletable_init_start. This code actually loads the game data from disc,
; which is why it has to live in the Z-machine stack so it doesn't overwrite
; itself.
deletable_init
    ; {{{ Load the dynamic memory, or everything if !VMEM.
    ; Load the nonstored blocks (dynamic memory), or all the blocks if we're not
    ; using virtual memory. We don't need to worry about reading past the end of
    ; the game data here, because at worst we will read a final 512-byte block
    ; when we don't have a full block and that's fine.

    ; If we got tight on space in the Z-machine stack, the following code up to
    ; but not including .dynmem_load_loop could be moved into
    ; prepare_for_initial_load.

    ; Because this is initialisation code, we know the following variables are
    ; already set to predictable values. This optimisation was useful at one
    ; point but it's not really important right now; it isn't too confusing so
    ; let's keep it anyway now it's been tested.
!if 0 {
    lda #2
    sta readblocks_numblocks
    lda #0
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
    sta readblocks_mempos ; story_start is page-aligned
!ifdef ACORN_TURBO_SUPPORTED {
    ; On a turbo second processor readblocks_mempos + 2 is significant and will
    ; vary; it's probably still zero at this point but play it safe. On a normal
    ; second processor readblocks_mempos + 2 will always be 0 so this is
    ; redundant but harmless.
    sta readblocks_mempos + 2
}
} else {
    ; If we're on DFS the earlier read of the catalogue will have bumped
    ; readblocks_currentblock and readblocks_mempos, so they have to be set
    ; explicitly.
    lda #0
    sta readblocks_currentblock
}
    lda #>story_start
    sta readblocks_mempos + 1
!ifdef VMEM {
    lda nonstored_pages
} else {
    lda #ACORN_GAME_PAGES
}
    sta .blocks_to_read
!ifdef ACORN_DEBUG_ASSERT {
    ; .blocks_to_read should always be even, but let's be paranoid and check
    ; here. Note that .dynmem_load_loop loops until .blocks_to_read is zero, so
    ; there's a risk of it failing to terminate if this isn't true.
    lsr
    +assert_carry_clear
}

!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; Page in the first bank as dynamic memory may overflow into it.
    +acorn_page_in_bank_using_a dynmem_ram_bank
}

.dynmem_load_loop
!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
    lda readblocks_mempos + 1
    cmp acorn_screen_hole_start_page
    bne +
    clc
    adc acorn_screen_hole_pages
    sta readblocks_mempos + 1
+
}
    jsr update_progress_indicator
    jsr readblocks
    lda .blocks_to_read
    sec
    sbc readblocks_numblocks
    sta .blocks_to_read
    bne .dynmem_load_loop
    ; }}}

!ifdef VMEM {
!ifndef PREOPT {
    ; {{{ Debugging code - use this in conjunction with --trace-vm.
!ifdef TRACE_VM {
!if 0 {
    jsr streams_init
    !ifndef DEBUG {
        !error "This code won't work reliably unless DEBUG is defined"
    }
    jsr newline
    lda #'X'
    jsr s_printchar
    lda #>story_start
    jsr print_byte_as_hex
    lda nonstored_pages
    jsr print_byte_as_hex
    jsr newline
    jsr osrdch
}
}
    ; }}}

    ; Now load the read-only pages specified by the vmap. (This roughly
    ; corresponds to the C64 load_suggested_pages subroutine.)

!ifdef ACORN_TUBE_CACHE {
!ifdef ACORN_TURBO_SUPPORTED {
    bit is_turbo
    bpl .host_cache_aware_vmem_load
} else {
    jmp .host_cache_aware_vmem_load
}
}
   ; {{{ Do simple vmem load.
   ; On a tube build with cache support and no turbo support, this block of code
   ; is redundant, but it's fiddly to exclude it and this is discardable init
   ; code.
!ifdef ACORN_TUBE_CACHE {
    lda #$ff
    sta osword_cache_index_offered
    sta osword_cache_index_offered + 1
    lda #osword_cache_no_timestamp_hint
    sta osword_cache_index_offered_timestamp_hint
}
    lda #0
    sta vmap_index
-   jsr update_progress_indicator
    jsr load_blocks_from_index
    inc vmap_index
    lda vmap_index
    cmp vmap_meaningful_entries
    bne -
    ; }}}

!ifdef ACORN_TUBE_CACHE {
    jmp .all_loading_done

.host_cache_aware_vmem_load
    ; {{{ Do host cache-aware vmem load. SFTODONOW: THIS HASN'T BEEN REVIEWED IN DETAIL YET

; SFTODONOW: I think it's OK to use vmap_max_entries here not vmap_meaningful_entries as we *don't* do dynmem adjust on a normal non-tube 2P, but this feels a bit hacky.
    ; vmap_max_entries was deliberately artificially high up to this point so
    ; we'd retain and sort more of the initial vmap; set it to the correct value
    ; reflecting the available memory now we're about to actually load the
    ; blocks.
    lda vmap_max_entries
    sta .inflated_vmap_max_entries
    jsr calculate_normal_tube_own_ram_pages
    sec
    sbc nonstored_pages
    lsr
    sta vmap_max_entries
!ifdef HAVE_VMAP_USED_ENTRIES {
    sta vmap_used_entries
}
!if 0 {
    ; (.)check_vmap_max_entries is currently in asm/acorn-init-preload.asm and
    ; is no longer available to us here. We could move it so it is available,
    ; but we are not playing games with nonstored_pages in this case and it
    ; doesn't add a huge amount of value. SFTODO: OK?
    jsr check_vmap_max_entries
}
    ; Adjust host_cache_size_vmem_blocks so the following load loop won't try to
    ; put "too much" into the host cache; if this happens we might not have
    ; enough blocks to load into the local virtual memory cache and so some vmap
    ; entries would be present but not have actually been loaded. (This can
    ; happen because the cutover timestamp isn't that precise due to limited
    ; timestamp resolution, especially for Z4+ games.) Note that this doesn't
    ; actually stop us using more of the host cache; we will offer it blocks
    ; willy-nilly during play and if it has space it will hold onto them.
    lda .inflated_vmap_max_entries
    sec
    sbc vmap_max_entries
    sta host_cache_size_vmem_blocks

    ; We now need to load the .inflated_vmap_max_entries blocks in the vmap from
    ; disk; vmap_max_entries blocks will go into our local memory as normal, the
    ; remainder need to be handed over to the host cache. We want the newer
    ; (higher timestamp) blocks in local memory, but remember vmap is sorted by
    ; block address to avoid the disk head jumping around. We use knowledge of
    ; how the build system generates the timestamps on vmap to identify a
    ; timestamp cutover point which will (except for the fact that the same
    ; timestamp can occur on multiple entries) separate the vmap into two chunks
    ; of the required sizes. (If the cutover timestamp is calculated
    ; incorrectly, we will still load everything we should, but newer blocks
    ; will tend to be in the host cache when we'd prefer them to be in local
    ; memory.)
.vmem_blocks = ((>(flat_ramtop - story_start)) - ACORN_INITIAL_NONSTORED_PAGES) / vmem_block_pagecount
.cutover_timestamp = int(ACORN_TUBE_CACHE_MAX_TIMESTAMP + ((float(.vmem_blocks) / vmap_max_size) * (ACORN_TUBE_CACHE_MIN_TIMESTAMP - ACORN_TUBE_CACHE_MAX_TIMESTAMP))) and ($ff xor vmem_highbyte_mask)

    ; Work through the blocks in vmap, loading each in turn and offering it to
    ; the host cache if it's old and there's room, and keeping it loaded into
    ; local memory otherwise. We keep doing this until we've loaded
    ; vmap_max_entries blocks into local memory (blocks offered to the host
    ; cache don't count) or until we've loaded all the blocks in vmap.
    lda #0
    sta .from_index
    sta vmap_index ; "to_index"
    lda #$ff
    sta osword_cache_index_offered
    sta osword_cache_index_offered + 1
.first_load_loop
    ldx .from_index
    ldy vmap_index
    lda vmap_z_l,x
    sta vmap_z_l,y
    lda vmap_z_h,x
    sta vmap_z_h,y
    jsr update_progress_indicator
    jsr load_blocks_from_index ; loads block vmap_index
    ldy vmap_index
    lda vmap_z_h,y
    and #$ff xor vmem_highbyte_mask
    cmp #.cutover_timestamp + 1
    bcs .dont_put_in_cache
    ldx host_cache_size_vmem_blocks
    beq .dont_put_in_cache
    dec host_cache_size_vmem_blocks
    sta osword_cache_index_offered_timestamp_hint
    ; load_blocks_from_index will have set osword_cache_index_requested
    lda osword_cache_index_requested
    sta osword_cache_index_offered
    lda osword_cache_index_requested + 1
    sta osword_cache_index_offered + 1
    jmp .continue
.dont_put_in_cache
    lda #$ff
    sta osword_cache_index_offered
    sta osword_cache_index_offered + 1
    inc vmap_index
.continue
    inc .from_index
    lda vmap_index
    cmp vmap_max_entries
    bne .first_load_loop

    ; We may have already loaded all the blocks, but if we haven't we now need
    ; to start re-using vmap[vmap_max_entries - 1] to load the rest. (We can
    ; only load into local memory and we've used it all.) We no longer have the
    ; luxury of (easily) putting the blocks with older timestamps into the host
    ; cache and keeping the younger ones in local memory, but if we chose the
    ; cutover timestamp correctly we should end up with at most one misplaced
    ; block in each cache. SFTODO: I'M NOT SURE IT'S AS PRECISE AS ONE MISPLACED BLOCK, DUE TO LACK OF TIMESTAMP RESOLUTION - IT MAY BE ONE MISPLACED *TIMESTAMP*. IT ISN'T THAT BIG A DEAL, BUT THINK ABOUT THIS AND UPDATE THIS COMMENT. The misplaced block in the host cache will be the
    ; newest block in the host cache so it will spend a long time in there
    ; before being discarded, which should give plenty of opportunity for it to
    ; be requested during gameplay and moved into local memory before it's lost.
    dec vmap_index ; set vmap_index = vmap_max_entries - 1
.second_load_loop
    ldx .from_index
    cpx .inflated_vmap_max_entries
    beq .second_load_loop_done
    ldy vmap_index
    lda vmap_z_l,y
    sta osword_cache_index_offered
    lda vmap_z_h,y
    and #vmem_highbyte_mask
    sta osword_cache_index_offered + 1
    lda vmap_z_h,y
    and #$ff xor vmem_highbyte_mask
    sta osword_cache_index_offered_timestamp_hint
    lda vmap_z_l,x
    sta vmap_z_l,y
    lda vmap_z_h,x
    sta vmap_z_h,y
    jsr update_progress_indicator
    jsr load_blocks_from_index
    inc .from_index
    jmp .second_load_loop
.second_load_loop_done
    ; Now we've finished the initial load, specify no timestamp hint for cache
    ; operations; this setting will remain untouched for the rest of the game.
    lda #osword_cache_no_timestamp_hint
    sta osword_cache_index_offered_timestamp_hint
    ; }}}
}

.all_loading_done
!ifdef HAVE_VMAP_USED_ENTRIES {
    lda vmap_max_entries
    sta vmap_used_entries
}

} ; End of !ifndef PREOPT
} ; End of !ifdef VMEM

    ; Calculate CRC of page 0 of the game before it gets modified, so we can use
    ; it later to identify the game disc after a save or restore.
!ifdef ACORN_SWR_MEDIUM_DYNMEM {
    +acorn_page_in_bank_using_a dynmem_ram_bank
}
    lda #0
    ldx #<story_start
    ldy #>story_start
    jsr calculate_crc
    stx game_disc_crc
    sty game_disc_crc + 1

!ifdef ACORN_SWR {
    ; We should page in the appropriate bank of sideways RAM when we first start
    ; to execute Z-machine code. Page in the current language so we'll crash
    ; consistently if that doesn't happen; sice this is discardable init code,
    ; we don't wrap this in ACORN_DEBUG_INTRUSIVE.
    +acorn_page_in_bank_using_a current_language
}

; parse_header section

!ifdef VMEM {
!ifdef VMEM_STRESS {
	lda #min_vmem_blocks ; one block for PC, one block for data
!ifdef HAVE_VMAP_USED_ENTRIES {
	sta vmap_used_entries
}
	sta vmap_max_entries
}

	jsr prepare_static_high_memory

} ; End of !ifdef VMEM

!ifdef CHECK_ERRORS {
	; check z machine version
	ldy #header_version
	jsr read_header_word
	cmp #ZMACHINEVERSION
	beq .supported_version
	lda #ERROR_UNSUPPORTED_STORY_VERSION
	jsr fatalerror
.supported_version
}

	rts
; End of deletable_init


; This initialization happens quite late in the initialization process - in
; particular it happens after the lengthy loading process in
; deletable_init.
deletable_screen_init_2
!ifndef ACORN {
	!error "Non-Acorn code has been removed from deletable_screen_init_2"
}
    +stack_init_body

    ; Set the desired mode. If we're already in the right mode we don't reselect
    ; it, to avoid the screen flashing briefly to black. This is deletable init
    ; code so we can afford minor luxuries like this.
    lda #osbyte_read_screen_mode
    jsr osbyte
    cpy screen_mode
    beq .already_in_right_mode
    lda #vdu_set_mode
    jsr oswrch
    lda screen_mode
    ora #shadow_mode_bit
    jsr oswrch
    jmp .mode_set
.already_in_right_mode
    ; Clear the screen; this is mostly unnecessary, but for Z3 games which are
    ; loading from the loader in mode 7 it clears any leftover text on the top
    ; line of the screen.
    lda #vdu_cls
    jsr oswrch
.mode_set
    ; Setting the mode will have turned the cursor back on, so fix that.
    jsr init_cursor_control
    ; We must re-initialise screenkernal to pick up the details of the new mode.
    jsr screenkernal_init
    ; We must also reset the window sizes; we do this by re-executing
    ; deletable_screen_init_1.
    jsr deletable_screen_init_1

!ifdef ACORN_HW_SCROLL_FAST_OR_SLOW {
    ldx #1
    ldy screen_mode
    cpy #7
    bne +
    dex
+   stx user_prefers_hw_scroll
}
!ifdef MODE_7_INPUT {
    ; We're not currently in the middle of using coloured input, so stop update_colours
    ; trying to adjust things. SFTODO: We could do this right after startup, which would
    ; save a bit of code in the Z-machine stack.
    lda #0
    sta input_colour_code_or_0
}
    jsr update_colours

	; clear and unsplit screen, start text output from bottom of the screen (top of screen if z5)
	ldy #1
	sty is_buffered_window
	ldx #$ff
    ; SF: Note that for Z1-3 this clears from line 1 down, not line 0. This is not a big deal and
    ; I am not going to try to change it, but this is why we need to zero-initialise top_line_buffer
    ; for when we are using slow hardware scrolling, as the buffer will not be set to all spaces
    ; by this erase operation.
	jsr erase_window
	jmp start_buffering
; End of deletable_screen_init_2

!ifdef Z5PLUS {
parse_terminating_characters
    +parse_terminating_characters_subroutine
}
}
