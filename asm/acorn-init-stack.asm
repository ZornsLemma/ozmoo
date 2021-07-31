; Initialization subroutines which will be placed inside the Z-machine stack.

screenkernal_init
    +screenkernal_init_inline
.screenkernal_init_rts
    rts

update_progress_indicator
progress_indicator_one_block = 1 << progress_indicator_fractional_bits
half_block_graphic = 181
full_block_graphic = 255
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
.lda_imm_block_graphic
    lda #half_block_graphic ; patched at run time
    cmp #half_block_graphic
    beq +
    lda #vdu_back
    jsr oswrch
    lda #full_block_graphic
+   jsr oswrch
    lda .lda_imm_block_graphic + 1
    eor #half_block_graphic xor full_block_graphic
    sta .lda_imm_block_graphic + 1
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

!ifdef ACORN_TUBE_CACHE {
; Set A=min(>(flat_ramtop - story_start), ACORN_GAME_BLOCKS), i.e. the number of
; pages of RAM we actually have on a normal second processor without counting
; host cache, capped at the actual size of the game.
calculate_normal_tube_own_ram_blocks ; SFTODO: RENAME??
    lda #>(flat_ramtop - story_start)
!if (>ACORN_GAME_BLOCKS) == 0 {
    cmp #<ACORN_GAME_BLOCKS
    bcc +
    lda #<ACORN_GAME_BLOCKS
}
+   rts
}

; This initialization happens quite late in the initialization process - in
; particular it happens after the lengthy loading process in
; acorn_deletable_init_inline. SFTODONOW: Which no longer exists, tweak comment
deletable_screen_init_2
!ifndef ACORN {
	!error "Non-Acorn code has been removed from deletable_screen_init_2"
}

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
    ora #128 ; force shadow mode on
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

!ifdef ACORN_HW_SCROLL {
    ldx #1
    ldy screen_mode
    cpy #7
    bne +
    dex
+   stx use_hw_scroll
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
	jsr erase_window
	jmp start_buffering
; End of deletable_screen_init_2


!zone deletable_init {

!ifndef ACORN {
	!error "Non-Acorn code for deletable_init has been removed"
}

; Initialization performed shortly after startup, just after
; acorn_deletable_init_start.
deletable_init
!ifdef ACORN_SHOW_PROGRAM_START {
    jsr streams_init
    lda #13
    jsr s_printchar
    jsr print_following_string
    !text "program_start=$", 0
    lda #>program_start
    jsr print_byte_as_hex
    lda #<program_start
    jsr print_byte_as_hex
    jsr osrdch
}

    jsr prepare_for_initial_load
    ; SFTODO: If we got tight on space in the Z-machine stack, the following
    ; code up to but not including .dynmem_load_loop could be moved into
    ; prepare_for_initial_load.

    ; Load the nonstored blocks, or all the blocks if we're not using virtual
    ; memory. We don't need to worry about reading past the end of the game data
    ; here, because at worst we will read a final 512-byte block when we don't
    ; have a full block and that's fine.
.blocks_to_read = zp_temp + 4 ; 1 byte
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
    lda #ACORN_GAME_BLOCKS
}
    sta .blocks_to_read

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
SFTODOXA9

!ifdef VMEM {
    ; vmem_highbyte_mask might be 0 and that enables some small optimisations, but
    ; in this one-off discardable init code we favour simplicity and don't bother.

!ifndef PREOPT {
    ; Sort vmap into ascending order, preserving the timestamps but using just the
    ; addresses as keys. This avoids the drive head jumping around during the
    ; initial load. The build system can't do this sort, because we're sorting
    ; the truncated list with just vmap_sort_entries not the full list of
    ; vmap_max_size entries.
    ;
    ; This only happens once and it's not a huge list so while we don't want it
    ; to be really slow, compactness and simplicity of code are also important.
    ; This is an insertion sort, implemented based on the pseudocode from
    ; https://en.m.wikipedia.org/wiki/Insertion_sort, which I've relabelled here
    ; to match the register use in the following code:
    ;
    ;     x = 1
    ;     while x < length(vmap_z)
    ;         temp = vmap_z[x]
    ;         y = x - 1
    ;         while y >= 0 and vmap_z[y] > temp
    ;             vmap_z[y+1] = vmap_z[y]
    ;             y = y - 1
    ;         end while
    ;         vmap_z[y+1] = temp
    ;         x = x + 1
    ;     end while
    ;
    ; Invariants:
    ;       1 <= x <= length(vmap_z) <= vmap_max_size <= 255
    ;      -1 <= y < x, so -1 <= y <= 254
    ;
    ; This takes about 0.42 seconds to sort 255 shuffled entries at 2MHz; that's
    ; not great but it's not terrible. It takes about 0.1 seconds to sort 122
    ; shuffled entries, which is probably a more typical case. Given a sorted
    ; list - as will happen if the preopt mode has not been used - it takes
    ; about 0.02 seconds to "sort" 255 entries, so there's no significant
    ; performance penalty when this is not doing anything useful.
    ; (We could simply not include this code if we don't have any preopt data,
    ; but it's discardable init code so it's not really harmful and it seems
    ; best for support purposes to keep the code identical whether or not preopt
    ; data is supplied or not.)
    ;
    ; SFTODO: Couldn't we move this code so it's in the game data area not the
    ; Z-machine stack? This doesn't matter unless/until we run out of space in the
    ; Z-machine stack, of course. SFTODONOW?
    ldx #1
.outer_loop
    lda vmap_z_l,x
    sta zp_temp
    lda vmap_z_h,x
    sta zp_temp + 1
    and #vmem_highbyte_mask
    sta zp_temp + 4
    txa
    tay
.inner_loop
    dey
    lda vmap_z_h,y
    and #vmem_highbyte_mask
    cmp zp_temp + 4
    bne +
    lda vmap_z_l,y
    cmp zp_temp
    beq .exit_inner_loop
+   bcc .exit_inner_loop
    lda vmap_z_l,y
    sta vmap_z_l + 1,y
    lda vmap_z_h,y
    sta vmap_z_h + 1,y
    tya
    bne .inner_loop
    dey
.exit_inner_loop
    ; We can't omit this iny and use vmap_z_[lh] + 1,y below to compensate
    ; because Y may be -1 (255) and so they're not equivalent.
    iny
    lda zp_temp
    sta vmap_z_l,y
    lda zp_temp + 1
    sta vmap_z_h,y
    inx
    cpx vmap_sort_entries
    bne .outer_loop

!ifndef ACORN_NO_DYNMEM_ADJUST {
    ; The initial vmap created by the build system assumes nonstored_pages ==
    ; ACORN_INITIAL_NONSTORED_PAGES, so if we changed nonstored_pages earlier
    ; we need to adjust the vmap to compensate. If we didn't adjust it, this
    ; code is a no-op. As the vmap is now sorted by address we just need to find
    ; the first entry which doesn't correspond to dynamic memory and move
    ; everything down so that entry becomes the first entry in the vmap. The
    ; space freed up at the end of the vmap by this move is filled with dummy
    ; entries so those entries will be used first when the game needs to load
    ; more blocks from disc.
    ; SFTODO: Couldn't we do this in the game-data code not the Z-machine stack code? SFTODONOW?
SFTODOLABEL2
    ldx #255
.find_first_non_promoted_entry_loop
    ; We need to shift the 16-bit vmap entry left one bit before comparing it
    ; against nonstored_pages.
    inx
    lda vmap_z_l,x
    asl
    lda vmap_z_h,x
    and #vmem_highbyte_mask
    rol
    bne .found_first_non_promoted_entry
    lda vmap_z_l,x
    asl
    cmp nonstored_pages
    bcc .find_first_non_promoted_entry_loop
.found_first_non_promoted_entry
    txa
    beq .no_dynmem_promotion
    ldy #0
.vmap_move_down_loop
    cpx #vmap_max_size
    beq .use_dummy_entry
    lda vmap_z_h,x
    sta vmap_z_h,y
    lda vmap_z_l,x
    inx
    bne + ; Always branch
    +assert_discardable_unreached
.use_dummy_entry
    ; We use $0000 as a dummy entry; this has the oldest possible timestamp so
    ; the entry will be re-used ASAP and because $0000xx is always dynamic
    ; memory the virtual memory code will never match against the dummy entry.
    lda #0
    sta vmap_z_h,y
+   sta vmap_z_l,y
    iny
    cpy vmap_max_entries
    bne .vmap_move_down_loop
.no_dynmem_promotion
}

    ; Debugging code - use this in conjunction with --print-vm.
!if 0 {
    jsr streams_init
    ; vmap_used_entries is set later in normal use, but set it early here so
    ; print_vm shows the entire vmap. SFTODO: This is probably broken by
    ; "removal" of vmap_used_entries (except in PREOPT builds) - needs to be
    ; tweaked.
    lda vmap_max_entries
    sta vmap_used_entries
    lda #'X'
    jsr s_printchar
    lda #>story_start
    jsr print_byte_as_hex
    lda nonstored_pages
    jsr print_byte_as_hex
    jsr newline
    jsr osrdch
}

    ; Now we've got vmap how we want it, load the corresponding blocks into
    ; memory and initialise vmap_used_entries. (This roughly corresponds to the
    ; C64 load_suggested_pages subroutine.)

!ifdef ACORN_TURBO_SUPPORTED {
    bit is_turbo
    bpl .normal_tube_load
    ; On a turbo second processor we don't do any preloading of the host cache
    ; so we just use a straightforward load loop like the non-tube-cache case
    ; below.
    stz vmap_index
!ifdef ACORN_TUBE_CACHE {
    lda #$ff
    sta osword_cache_index_offered
    sta osword_cache_index_offered + 1
}
.turbo_load_loop
    jsr update_progress_indicator
    jsr load_blocks_from_index
    inc vmap_index
    lda vmap_index
    cmp vmap_max_entries
    bne .turbo_load_loop
!ifdef HAVE_VMAP_USED_ENTRIES {
    sta vmap_used_entries
}
    jmp .all_loading_done

.normal_tube_load
}

!ifdef ACORN_TUBE_CACHE {
; SFTODO: These labels should probably start with a "."
inflated_vmap_max_entries = zp_temp
from_index = zp_temp + 1
to_index = vmap_index
load_scratch_space = flat_ramtop - vmem_blocksize
SFTODOLABELX2

    ; vmap_max_entries was deliberately artificially high up to this point so
    ; we'd retain and sort more of the initial vmap; set it to the correct value
    ; reflecting the size of the vmap Ozmoo's virtual memory has to work with.
    ; SFTODO: Worth noting that here - and might be useful in some other code
    ; too - we are calculating using known-at-build-time values. I could
    ; potentially simplify/shorten the code in a few places by not treating this
    ; dynamically, e.g. we wouldn't need the code to populate game_blocks in
    ; the first place. (on SWR builds the dynmem growth optimisation means
    ; nonstored_pages is not precisely known at build time, but that's not an
    ; issue for a tube build) This might also simplify some corner cases in the
    ; "grow nonstored_pages" logic, because the game size is no longer a
    ; runtime variable. I just worry a little bit about this breaking
    ; already-not-supposed-to-work-but-sort-of-does-just-about things where a
    ; game developer wants to switch in an updated data file without going
    ; through the Ozmoo build process.
    lda vmap_max_entries
    sta inflated_vmap_max_entries
    jsr calculate_normal_tube_own_ram_blocks
    sec
    sbc nonstored_pages
    lsr
    sta vmap_max_entries
!ifdef HAVE_VMAP_USED_ENTRIES {
    sta vmap_used_entries
}
!error "SFTODO: LNEXT LINE IS BROKEN, check_vmap_max_entries CURRENTLY LIVES IN NON-STACK GAME DATA REG AND HAS BEEN OVERWRITTEN"
    jsr check_vmap_max_entries
    ; Adjust host_cache_size so the following load loop won't try to put "too
    ; much" into the host cache; if this happens we might not have enough blocks
    ; to load into the local virtual memory cache and so some vmap entries would
    ; be present but not have actually been loaded. (This can happen because the
    ; cutover timestamp isn't that precise due to limited timestamp resolution,
    ; especially for Z4+ games.) Note that this doesn't actually stop us using
    ; more of the host cache; we will offer it blocks willy-nilly during play
    ; and if it has space it will hold onto them.
SFTODOLABELX3
    lda inflated_vmap_max_entries
    sec
    sbc vmap_max_entries
    sta host_cache_size

    ; We now need to load the inflated_vmap_max_entries blocks in the vmap from
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
    sta from_index
    sta to_index
    lda #$ff
    sta osword_cache_index_offered
    sta osword_cache_index_offered + 1
.first_load_loop
    ldx from_index
    ldy to_index
    lda vmap_z_l,x
    sta vmap_z_l,y
    lda vmap_z_h,x
    sta vmap_z_h,y
    jsr update_progress_indicator
    jsr load_blocks_from_index
    ldy to_index
    lda vmap_z_h,y
    and #$ff xor vmem_highbyte_mask
    cmp #.cutover_timestamp + 1
    bcs .dont_put_in_cache
    ldx host_cache_size
    beq .dont_put_in_cache
    dec host_cache_size
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
    inc to_index
.continue
    inc from_index
    lda to_index
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
    ; SFTODO: vmap_index is an alias for to_index, maybe use to_index in the follow loop to match the previous loop? This (double check) is just a labelling change, the code would be identical.
    dec vmap_index ; set vmap_index = vmap_max_entries - 1
.second_load_loop
    ldx from_index
    cpx inflated_vmap_max_entries
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
    inc from_index
    jmp .second_load_loop
.second_load_loop_done
    ; Now we've finished the initial load, specify no timestamp hint for cache
    ; operations; this setting will remain untouched for the rest of the game.
    lda #osword_cache_no_timestamp_hint
    sta osword_cache_index_offered_timestamp_hint
} else { ; not ACORN_TUBE_CACHE
    ; Load the blocks in vmap.
    lda #0
    sta vmap_index
-   jsr update_progress_indicator
    jsr load_blocks_from_index
    inc vmap_index
    lda vmap_index
    cmp vmap_max_entries
    bne -
SFTODOLABEL4
!ifdef HAVE_VMAP_USED_ENTRIES {
    sta vmap_used_entries
}
}
.all_loading_done

} ; End of !ifndef PREOPT
} ; End of !ifdef VMEM

    ; Calculate CRC of block 0 before it gets modified, so we can use it later
    ; to identify the game disc after a save or restore.
!ifdef ACORN_SWR_MEDIUM_DYNMEM {
    +acorn_page_in_bank_using_a dynmem_ram_bank
}
    lda #0
    ldx #<story_start
    ldy #>story_start
    jsr calculate_crc ; corrupts some zp_temp locations
    stx game_disc_crc
    sty game_disc_crc + 1

!ifdef ACORN_SWR {
    ; The load loop or the above CRC may have left a non-default bank of sideways
    ; RAM paged in; we need to page the default bank back in. SFTODO: I am not
    ; sure this is necessary, as we should page in the right bank when we first
    ; try to get the page containing the initial Z-machine PC, but it doesn't
    ; really hurt to do this anyway.
    +acorn_swr_page_in_default_bank_using_y ; SFTODO: Should prob remove _swr_ from this macro for consistency
}

; parse_header section


!ifdef VMEM {
!ifdef VMEM_STRESS {
	lda #2 ; one block for PC, one block for data
	sta vmap_used_entries
	sta vmap_max_entries
}

	jsr prepare_static_high_memory

} ; End of !ifdef VMEM

!ifndef UNSAFE {
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
}
