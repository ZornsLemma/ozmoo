; Initialization code which may be placed after the end of the Z-machine stack;
; this means it might be overwritten as soon as anything is loaded at
; story_start.

; Initialization performed very early during startup.
deletable_init_start
!ifdef ACORN_TURBO_SUPPORTED {
    ; The TURBO executable will have set zp_temp_turbo_flag; stash the value before we clear zero page and copy it into is_turbo afterwards.
    ; SFTODONOW: This is *not* going to work across a "restart" command. But let's just hack it like this for the moment.
    lda zp_temp_turbo_flag
    pha
}

    ; Clear all our zero page; this is probably a good idea for consistency
    ; anyway but is important when some storage allocated via +allocate in
    ; acorn-ozmoo-constants.asm ends up in zero page instead of low memory.
    lda #0
    tax
-   sta $00,x
    inx
    cpx #zp_end
    bne -

!ifdef ACORN_TURBO_SUPPORTED {
    pla
    sta is_turbo
}

!if zero_end > zero_start {
    ; Clear memory betwen zero_start and zero_end; this is language workspace
    ; which holds miscellaneous variables and because it's not part of the
    ; executable will not automatically be zero-initialised. Some of these
    ; variables must be zero-initialised and for the others it's still helpful
    ; for consistency.
    ; SFTODO: This code feels a bit clumsy, can I improve it? Just for satisfaction.
    lda #0
.sta_zero_start
-   sta zero_start ; patched by following code
    inc .sta_zero_start + 1
    bne +
    inc .sta_zero_start + 2
+   ldx .sta_zero_start + 1
    cpx #<zero_end
    bne -
    ldx .sta_zero_start + 2
    cpx #>zero_end
    bne -
}

    ; maxwords and wordoffset are handled specially and won't always be automatically
    ; cleared by the previous loop, so do them explicitly here.
    ; SFTODONOW: SHOULD WE DO $400-low_fixed_gap_start AS WELL?
    lda #0
    sta maxwords
    sta wordoffset

    ; Initialise non-0 variables.
    lda #1
    sta streams_buffering
    sta streams_buffering + 1
!ifdef USE_HISTORY {
    sta history_disabled
}
    lda #2
    sta readblocks_numblocks

!ifdef TRACE_SETJMP {
    lda #$ff
    sta setjmp_min_s
}

    ldx #1
    jsr do_osbyte_rw_escape_key

    jsr screenkernal_init

    ; Now screenkernal_init has been executed, it's safe to call s_printchar, so
    ; install our own error handler which will use s_printchar by default. No
    ; error should be able to occur before this point. If an error occurs during
    ; a restart, which will re-run the executable, there's not much we can do
    ; but it's probably OK because the load will just replace the code with an
    ; identical copy.
    lda #<error_handler
    sta brkv
    lda #>error_handler
    sta brkv + 1

    ; SFTODO: Maybe we should get rid of ACORN_CURSOR_PASS_THROUGH? Not sure,
    ; but it's always set by make-acorn.py.
!ifdef ACORN_CURSOR_PASS_THROUGH {
    ldx #1
    jsr do_osbyte_set_cursor_editing
}

!ifdef ACORN_FUNCTION_KEY_PASS_THROUGH {
    ; We're going to generate ZSCII codes for the unshifted function keys. We
    ; choose a base of 133 (=ZSCII f1) for f0 because if we set a base of 132 so
    ; Acorn f1=ZSCII f1, Acorn f0 would act like cursor right. If we want Acorn
    ; f1=ZSCII f1 we'll fix that up in the translation table. SFTODO: I HAVEN'T ADDED THIS OPTION TO THE TRANSLATION TABLE YET
    lda #osbyte_rw_function_key_status
    ldx #133
    jsr do_osbyte_y_0

    ; In order to allow the use of *KEY expansions, we'll make the shifted
    ; function keys generate those.
    lda #osbyte_rw_shift_function_key_status
    ldx #1 ; expand as normal soft key
    jsr do_osbyte_y_0
}

    ldx #0
    stx mempointer
!ifndef ACORN_SWR {
    ; Patch re_enter_language to enter the current language; reading it here
    ; saves a few bytes of non-deletable code.
    lda #osbyte_read_language
    ; X is already 0
    ldy #$ff
    jsr osbyte
    stx re_enter_language_ldx_imm + 1

    ; On a second processor, a soft break will transfer control back to our
    ; execution address. We will have thrown away our initialization code by
    ; that point and can't restart properly. In order to avoid random behaviour
    ; and probably a crash, we patch the jmp at the start of the executable to
    ; transfer control to re_enter_language. This means that although the
    ; language name gets printed twice, a soft break otherwise gives a clean
    ; result similar to that on a non-second processor. This code is harmless
    ; if we're not running on a second processor, but it's not necessary either.
    lda #<re_enter_language
    sta initial_jmp + 1
    lda #>re_enter_language
    sta initial_jmp + 2
}

!ifdef ACORN_TURBO_SUPPORTED {
    ; This executable doesn't have a ROM header indicating we want the turbo
    ; mode enabled, so it will have been disabled when were were executed. Turn
    ; it back on if we do want it.
;    !error "SFTODONOW - I DON'T KNOW IF THIS IS WHAT'S BREAKING THINGS, BUT NOTE THAT WE ARE NOT KEEPING is_turbo FIXED WHERE IT NEEDS TO BE TO MATCH THE TURBO-DETECTION CODE"
    bit is_turbo
    bpl .dont_enable_turbo
    ; Set all the turbo banks to zero before enabling turbo mode; we might crash
    ; if we don't.
    lda #0
    ldy #254
-   sta turbo_bank_base,y
    dey
    cpy #255
    bne -
    lda #$80
    sta turbo_control
.dont_enable_turbo
}

!ifdef VMEM {
    ; Copy the low bytes of the vmap at initial_vmap_z_l to vmap_z_l.
    ldx #vmap_max_size
-   lda initial_vmap_z_l - 1,x
    sta vmap_z_l - 1,x
    dex
    bne -
}

!ifdef USE_HISTORY {
    ; Zero the history buffer; if this isn't done it is sometimes possible to
    ; navigate to invalid history entries.
    ldx #history_size - 1
    lda #0
-   sta history_start,x
    dex
    cpx #$ff
    bne -
}

!ifdef ACORN_SCREEN_HOLE {
    lda screen_mode
    ora #128 ; force shadow mode on SFTODO  MAGIC CONSTANT IN A FEW PLACES NOW?
    tax
    lda #osbyte_read_screen_address_for_mode
    jsr osbyte
    cpy #$80
    bcc .not_shadow_mode
    ; If we're in a shadow mode, the screen hole isn't needed; by setting the
    ; start page as high as possible, code which implements the screen hole will
    ; realise ASAP that it's not necessary. (If we didn't do this, we'd end up
    ; with a zero-size screen hole at $8000, which would work but be slower.)
    ldy #$ff
.not_shadow_mode
    sty acorn_screen_hole_start_page
    dey
    sty acorn_screen_hole_start_page_minus_one
    sec
    lda #>flat_ramtop
    sbc acorn_screen_hole_start_page
    bcs +
    lda #0 ; shadow RAM case (>flat_ramtop - $ff caused a borrow)
+   sta acorn_screen_hole_pages
    tax
    dex
    stx acorn_screen_hole_pages_minus_one
}

!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; This is used enough it's worth - if only for the code size saving -
    ; copying it into zero page. (Well, it saves 10 bytes at the time of
    ; writing; not huge, but not too bad. SFTODO: Maybe reconsider this later.)
    lda ram_bank_list
    sta dynmem_ram_bank
}

!ifdef ACORN_SHADOW_VMEM {
    ; Set vmem_cache_count_mem to the number of 256-byte cache entries we have
    ; for holding data copied out of shadow RAM. If we set this to 0, it will
    ; effectively disable the use of shadow RAM as virtual memory cache. The
    ; loader will do this if it doesn't have a shadow RAM driver for this
    ; machine; the fact that we get relocated up an additional page if PAGE has
    ; the wrong 512-byte alignment won't cause a problem because we can't use
    ; just one page of shadow cache anyway (we need a minimum of two, one for
    ; the Z-machine PC and another one) so we'll set this to 0 below if that
    ; happens.
    ;
    ; If we're in mode 0, there's no spare shadow RAM anyway. The loader won't have
    ; allocated any space, but we might have one page available if we happened to
    ; load at PAGE+256, and we mustn't let that mislead us. (I don't believe this
    ; is strictly necessary - if we didn't special-case mode 0, we would discover
    ; that we have 0 or 1 pages of cache and set vmem_cache_count_mem to 0 anyway.
    ; But we might as well be explicit, since this is discardable init code.)
    lda #0
    sta vmem_cache_count_mem
    lda screen_mode
    beq .mode_0
    ; We have as many pages of cache as there are between PAGE and
    ; program_start. In practice this is whatever the loader deliberately set
    ; aside for us plus maybe an extra page if we had to relocate down to
    ; PAGE+256 to keep the right alignment. (That extra page must be below
    ; $3000; the loader enforces this in general, but we have to do it here as
    ; the loader doesn't know about the extra page.)
    lda #osbyte_read_oshwm
    jsr osbyte
    sty vmem_cache_start_mem
    lda #>program_start
    cmp #>shadow_start
    bcc +
    lda #>shadow_start
+   sec
    sbc vmem_cache_start_mem
    ; We mustn't have just one page; the loader won't deliberately bring this
    ; about but it can happen if we get relocated one page higher because PAGE
    ; has the wrong 512-byte alignment on this machine.
    cmp #2
    bcs +
    lda #0
+   sta vmem_cache_count_mem
.mode_0

    ; SF: We don't need to zero vmem_cache_cnt and vmem_cache_page_index
    ; explicitly because we cleared all our zero page and low memory on startup.
!if 0 {
    ; Zero vmem_cache_cnt and vmem_cache_page_index.
    !if vmem_cache_cnt + 1 != vmem_cache_page_index {
        !error "vmem_cache_cnt is not just before vmem_cache_page_index"
    }
    lda #0
    ldx #vmem_cache_page_index_end - vmem_cache_cnt - 1
-   sta vmem_cache_cnt,x
    dex
    bpl -
}
}

    +prepare_static_high_memory_inline
    +init_readtime_inline
    jmp init_cursor_control
; End of deletable_init_start


; Do as much as we can to prepare for the initial load of game data from disk,
; without actually loading anything (which would overwrite this code).
prepare_for_initial_load
!ifdef TRACE_FLOPPY {
    ; Call streams_init so the tracing is able to show the readblocks calls
    ; performed here.
	jsr streams_init
}

!ifndef ACORN_ADFS {
    ; Examine the disc catalogue and determine the first sector occupied by the
    ; DATA file containing the game.

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
    sta readblocks_mempos ; catalogue is page-aligned
!ifdef ACORN_TURBO_SUPPORTED {
    ; In reality this is redundant but let's play it safe.
    sta readblocks_mempos + 2
}
}
    lda #>catalogue
    sta dir_ptr + 1
    sta readblocks_mempos + 1
    lda #0
    sta readblocks_base
!ifndef ACORN_DSD {
    sta readblocks_base + 1
}
    ; Note that because we're reading the first few sectors, this works
    ; correctly whether this is an ACORN_DSD build or not.
    jsr readblocks
    lda #8
    sta dir_ptr
.find_file_loop
    ldy #0
    ldx #(-8 & $ff)
.name_compare_loop
    lda (dir_ptr),y
    ; The directory name will have the top bit set iff the file is locked; we
    ; don't care about that here, so we need to strip it off. It's harmless to
    ; just do this for all characters.
    and #$7f
    cmp .data_filename-(-8 & $ff),x
    bne .file_not_found
    iny
    inx
    beq .file_found
    bne .name_compare_loop
.data_filename
    !text "DATA   $"
.file_not_found
    clc
    lda dir_ptr
    adc #8
    sta dir_ptr
    bne .find_file_loop
    brk
    !byte 0
    !text "DATA not found"
    !byte 0
.file_found
    ; We found the file's name using sector 0, we now want to look at the
    ; corresponding part of sector 1. Adjust dir_ptr for convenience.
    inc dir_ptr + 1
    ; Determine the start sector of the DATA file and set readblocks_base.
    ldy #6
    lda (dir_ptr),y
    and #$3
!ifndef ACORN_DSD {
    sta readblocks_base + 1
    iny
    lda (dir_ptr),y
    sta readblocks_base
} else {
    sta dividend + 1
    iny
    lda (dir_ptr),y
    sta dividend
    lda #0
    sta divisor + 1
    lda #10
    sta divisor
    jsr divide16
    lda division_result
    sta readblocks_base
}
}

; SFTODONOW: UP TO HERE WITH REVIEW
SFTODOXX89
!ifdef VMEM {
    ; How much RAM do we have available for game data?

!ifndef ACORN_SWR {
    lda #0
    sta ram_blocks
    sta ram_blocks + 1
} else {
    ; We have 64 (2^6) 256-byte blocks per sideways RAM bank, if we have any.
    lda #0
    sta ram_blocks + 1
    lda ram_bank_count
    ldx #6
-   asl
    rol ram_blocks + 1
    dex
    bne -
    sta ram_blocks

!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
    ; SFTODONOW: I'm finding it hard to convince myself this is a safe "dummy" value - what if we had 9x16K SWR, couldn't this sometimes cause us to start skipping the last block or two in the last bank?
    lda #$ff
    sta sideways_ram_hole_start ; SFTODO: RENAME THIS acorn_sideway_ram_hole_block_index OR SOMETHING?

    ; The last RAM bank might be the B+ or Integra-B private RAM, which isn't
    ; the full 16K.
    ldx ram_bank_count
    ldy ram_bank_list - 1,x
    bmi .b_plus_private_ram
    cpy #64 ; SFTODO MAGIC NUMBER
    bcc .not_private_ram
    ; This is the Integra-B private 12K.
    ; We need to skip 1024 bytes of IBOS workspace in the private RAM at $8000.
    ; Set sideways_ram_hole_start
    ; = (RAM banks including private 12K - 1) * 32
    ; = (RAM banks including private 12K * 32) - 32
    ; = (ram_blocks >> 1) - 32
    ; If this doesn't fit in a single byte, we will never try to access the
    ; private RAM so just leave the default value in sideways_ram_hole_start.
    ; (Note that convert_index_x_to_ram_bank_and_address has already added back
    ; vmem_blocks_stolen_in_first_blank before using this value, so we're just
    ; calculating the vmem block index *from the start of sideways RAM* to skip.)
SFTODOKOO
    lda ram_blocks + 1
    lsr
    bne .not_private_ram
    lda ram_blocks
    ror
    sec
    sbc #32
    sta sideways_ram_hole_start
    ; SFTODO: MAGIC CONSTANTS
    ; Set up RAMSEL so we can access the private 12K by setting b6 (PRVEN) of ROMSEL,
    ; much as we can access it by setting b7 on the B+.
    lda $37f
    ora #%00110000 ; set PRVS4 and PRVS8 to make all 12K visible
    sta $37f
    sta $fe34
    lda ram_blocks
    sec
    sbc #(16 * 1024 - integra_b_private_ram_size) / 256
    jmp .subtract_private_ram_high_byte
.b_plus_private_ram
    lda ram_blocks
    sec
    sbc #(16 * 1024 - b_plus_private_ram_size) / 256
.subtract_private_ram_high_byte
    sta ram_blocks
    bcs +
    dec ram_blocks + 1
+
.not_private_ram
}
}

!ifdef ACORN_SHADOW_VMEM {
    ; Save a copy of ram_blocks for later when we're calculating
    ; vmem_blocks_in_sideways_ram.
    ; SFTODO: Rename scratch_ram_blocks to swr_ram_blocks or similar?
    lda ram_blocks
    sta scratch_ram_blocks
    lda ram_blocks + 1
    sta scratch_ram_blocks + 1

    ; We may have some additional RAM blocks in shadow RAM not being used for the
    ; screen display.
SFTODOLM2
    lda vmem_cache_count_mem
    beq .no_spare_shadow
    ; On an Electron with a Master RAM Board in shadow mode, the shadow RAM
    ; can't be turned off under software control and
    ; osbyte_read_screen_address_for_mode will always return $8000. This makes
    ; sense, but it's not much use to us here. We use our own table of start
    ; addresses instead; we might as well do this on all platforms for
    ; consistency.
    ldx screen_mode
    lda screen_start_page_by_mode,x
    sec
    sbc #>shadow_start
    clc
    adc ram_blocks
    sta ram_blocks
    bcc +
    inc ram_blocks + 1
+
.no_spare_shadow
}

!ifdef ACORN_TUBE_CACHE {
    ; We have some blocks of cache in the host, which aren't directly accessible
    ; but are almost as good as our own RAM and which will benefit from
    ; preloading if we're on a normal second processor. We count them for now so
    ; we process more of the initial vmap and fix up the inflated value of
    ; vmap_max_entries later.
    lda screen_mode
    ora #128 ; force shadow mode on
    tax
    lda #osbyte_initialise_cache
    jsr osbyte
    stx host_cache_size
!ifdef ACORN_TURBO_SUPPORTED {
    ; A turbo second processor has enough RAM to preload everything in vmap
    ; without touching the host cache. The host cache will still work, but we
    ; don't have anything to preload into it, so having initialised it there's
    ; nothing else to do.
    bit is_turbo
    bmi .host_cache_initialised
}
    ; Note that ram_blocks is 0 at this point.
    ; X is cache size in 512-byte blocks, but we want to count 256-byte blocks here.
    txa
    asl
    rol ram_blocks + 1
    sta ram_blocks
.host_cache_initialised
SFTODOLABELX1
}

!ifdef ACORN_TURBO_SUPPORTED {
    ; On a turbo second processor, we will use all 128K in banks 1 and 2 as
    ; virtual memory cache.
    bit is_turbo
    bpl .dont_count_turbo_ram
    inc ram_blocks + 1
    inc ram_blocks + 1
.dont_count_turbo_ram
}

    ; We also have some blocks between flat_ramtop and data_start. We're doing a
    ; constant subtraction in code here, but a) this is deletable init code so
    ; it doesn't really cost anything b) if we don't, the relocation code fails
    ; because we have a variation which doesn't follow the simple fixed
    ; relationship we expect.
    lda #>flat_ramtop
    sec
    sbc #>data_start
!ifdef ACORN_SCREEN_HOLE {
    sec
    sbc acorn_screen_hole_pages
}
    clc
    adc ram_blocks
    sta ram_blocks
    bcc +
    inc ram_blocks + 1
+

!ifdef ACORN_SWR { ; SFTODO: MERGE THIS WITH ANOTHER ACORN_SWR BLOCK? FEELS A BIT ISOLATED STUCK OUT HERE ON ITS OWN.
    ; This value might be changed below.
    lda #0
    sta vmem_blocks_in_main_ram
}

    ; ram_blocks now contains the number of 256-byte blocks of RAM we have
    ; available, including RAM which will be used for dynamic memory. The build
    ; system and the loader will have worked together to guarantee that:
    ; - ram_blocks >= ACORN_INITIAL_NONSTORED_PAGES + 2*vmem_block_pagecount,
    ;   i.e. that we have enough RAM for the game's dynamic memory and two
    ;   512-byte blocks of virtual memory cache.
    ; - the game always has at least one block of non-dynamic memory.

    ; In order to avoid accessing nonexistent game data in an attempt to use all
    ; that RAM, set ram_blocks = min(ram_blocks, game_blocks).
SFTODOEE2
    ldx #>ACORN_GAME_BLOCKS
    lda #<ACORN_GAME_BLOCKS
    cpx ram_blocks + 1
    bne +
    cmp ram_blocks
+   bcs +
    stx ram_blocks + 1
    sta ram_blocks
+

    ; Set nonstored_pages to the number of 256-byte blocks of RAM we are going
    ; to treat as dynamic memory. This is normally the game's actual dynamic
    ; memory rounded up to a 512-byte boundary, i.e.
    ; ACORN_INITIAL_NONSTORED_PAGES.
    lda #ACORN_INITIAL_NONSTORED_PAGES
    sta nonstored_pages ; SFTODONOW: Do we need nonstored_pages on non-VMEM builds?
!ifdef VMEM {
!ifndef ACORN_NO_DYNMEM_ADJUST {
    ; The build system and loader have worked together to ensure that we can run
    ; the game with nonstored_pages == ACORN_INITIAL_NONSTORED_PAGES. We must
    ; not have nonstored_pages lower than that, since we'd not be respecting the
    ; game's real dynamic memory size.
    ;
    ; nonstored_pages must not be so large that we'd have less than
    ; min_vmem_blocks * vmem_block_pagecount pages of memory left over for vmem
    ; cache, as doing so would risk a deadlock when paging. SFTODO: INTRODUCE max_nonstored_pages AND DEFINE IT HERE??
    ;
    ; So we can vary nonstored_pages between those two hard limits. In general
    ; setting nonstored_pages > ACORN_INITIAL_NONSTORED_PAGES isn't a good idea;
    ; raising it will permanently lock more of the game into memory, reducing
    ; the amount of memory we have to use flexibly as vmem cache and likely
    ; increasing the amount of disc access we need to perform.
    ;
    ; The following code detects situations where it *is* useful to set
    ; nonstored_pages > ACORN_INITIAL_NONSTORED_PAGES.

!ifdef ACORN_TURBO_SUPPORTED {
    ; On a turbo second processor banks 1 and 2 provide as much virtual memory
    ; cache as we can use, so we can promote some additional data into dynamic
    ; memory to use all of bank 0. This may allow us to address more memory in
    ; total, but even if the game is small and would have been entirely
    ; addressable in RAM anyway, we will still get a performance benefit from
    ; accessing as much of the game as possible via the simpler dynamic memory
    ; code path.
    bit is_turbo
    bpl .dynmem_adjust_done
    lda #>(flat_ramtop - story_start)
    sta nonstored_pages
.dynmem_adjust_done
}

SFTODOLABEL2X
!ifdef ACORN_SWR {
    ; It may be useful to increase nonstored_pages to promote some additional
    ; data into dynamic memory, either for performance or to make use of more
    ; sideways RAM. We must not make it too large for the memory model we're
    ; using. (This optimisation has relatively limited scope in the small or
    ; medium model, but it's trivial to support it anyway. It's just conceivable
    ; some games and/or machines might benefit from --force-big-dynmem to give
    ; this optimisation more headroom, but of course the big model has its own
    ; performance drawbacks so it's probably best not using it unless we're
    ; forced to.)
.max_dynmem = zp_temp + 4 ; 1 byte SFTODONOW RENAME GET RID OF ???
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; It's not all that likely, but it's not impossible we don't actually have
    ; any sideways RAM. (Perhaps we had to use the big model to fit on some
    ; machines but on this one we have PAGE=&E00 and dynamic memory fits in
    ; main RAM, for example.) We might also have a short sideways RAM bank;
    ; if it's the Integra-B private 12K we mustn't promote dynmem into it as
    ; it's not contiguous, but we can do so with the B+ private 12k. Note that
    ; because we don't allow the Integra-B private 12K to be used as the only
    ; sideways RAM bank in the medium model, we can't end up setting .max_dynmem
    ; to 0. SFTODONOW: PROB OK BUT REVIEW LATER
    lda #>flat_ramtop
    ldy ram_bank_count
    beq .upper_bound_in_a
    ldy ram_bank_list
    bmi .b_plus_private_12k
    cpy #64 ; SFTODO: MAGIC NUMBER
    bcs .upper_bound_in_a ; Integra-B private 12K
    lda #>swr_ramtop
    jmp .upper_bound_in_a
.b_plus_private_12k
    lda #>(flat_ramtop + b_plus_private_ram_size)
.upper_bound_in_a
} else {
    lda #>flat_ramtop
}
!ifndef ACORN_SWR_MEDIUM_DYNMEM {
!ifdef ACORN_SCREEN_HOLE {
    sec
    sbc acorn_screen_hole_pages
}
}
    sec
    sbc #>story_start
    sta .max_dynmem

    ; If game_blocks == ram_blocks, we want to set nonstored_pages to
    ; .max_dynmem; there's no downside as we have enough RAM for the entire game
    ; and this will allow as much of the game as possible to be accessed via the
    ; faster dynamic memory code path.
    ldy ram_blocks + 1
    lda ram_blocks
    cpy #>ACORN_GAME_BLOCKS
    bne game_blocks_ne_ram_blocks
    cmp #<ACORN_GAME_BLOCKS
    bne game_blocks_ne_ram_blocks
.use_max_dynmem
    lda .max_dynmem
    sta nonstored_pages
    jmp .dynmem_adjust_done

game_blocks_ne_ram_blocks
    ; Note that we can't have game_blocks < ram_blocks because we reduced
    ; ram_blocks to match earlier, so game_blocks > ram_blocks. We don't want to
    ; reduce flexibility by locking parts of the game into RAM instead of
    ; allowing the virtual memory system to choose what lives in RAM. It's only
    ; a clear win to increase nonstored_pages if it brings otherwise unusable
    ; RAM into play.
    ;
    ; only_dynmem_addressable_blocks = ram_blocks - (vmap_max_size *
    ; vmem_block_pagecount) is the number of 256-byte pages we can't address via
    ; the virtual memory code, and can therefore only address as dynamic memory.
    ; If this is negative, there is no memory we can't address, so leave things
    ; alone.
.min_lhs_sub = vmap_max_size * vmem_block_pagecount ; SFTODO RENAME NOW
    sec
    sbc #<.min_lhs_sub
    tax
    tya
    sbc #>.min_lhs_sub
    bcc .dynmem_adjust_done ; branch if only_dynmem_addressable_blocks negative
    ; Set nonstored_pages = min(only_dynmem_addressable_blocks, .max_dynmem); we can't use more dynamic
    ; memory than we have memory to support, of course.
    bne .use_max_dynmem ; branch if only_dymem_addressable_blocks is > 8 bit
    ; We now have X=only_dynmem_addressable_blocks, an 8-bit quantity.
    cpx .max_dynmem
    bcs .use_max_dynmem
    stx nonstored_pages
.dynmem_adjust_done ; SFTODO RENAME LABEL
}

    ; The above adjustments deliberately ignored some general constraints on
    ; nonstored_pages to simplify the code; apply those constraints now.
    jsr constrain_nonstored_pages ; SFTODO: INLINE IF ONLY ONE CALLER? MAYBE NOT.
}

    ; Set ram_blocks -= nonstored_pages, i.e. set ram_blocks to the number of
    ; RAM blocks we have available as virtual memory cache.
    lda ram_blocks
    sec
    sbc nonstored_pages
    sta ram_blocks
    bcs +
    dec ram_blocks + 1
+
}
; SFTODONOW: WE SHOULD RUNTIME ASSERT WHATEVER WE CAN IN ALL CASES

!ifndef ACORN_SWR {
    ; vmap_first_ram_page is a constant set to suit a normal second processor
    ; and it's not used on a turbo second processor, so we don't need any code
    ; to initialise it.
}

    ; Now set vmap_max_entries = min(ram_blocks / vmem_block_pagecount,
    ; vmap_max_size), i.e. the number of vmap entries we have RAM to support.
    ; (If we're in the ACORN_TUBE_CACHE case on a normal second processor, we
    ; have that much RAM in total but the number of vmap entries we can support
    ; is lower. It's convenient to work with this larger value while we do the
    ; initial load, then vmap_max_entries is fixed up later.)
    ldx #vmap_max_size
    lda ram_blocks
    lsr ram_blocks + 1
    bne .cap_at_vmap_max_size
    ror
    cmp #vmap_max_size
    bcs .cap_at_vmap_max_size
    tax
.cap_at_vmap_max_size
    stx vmap_max_entries
    jsr check_vmap_max_entries
+

SFTODOLABEL5
!ifndef ACORN_NO_DYNMEM_ADJUST {
; SFTODONOW: This is probably true, but I probably also need to expand on this comment - it's not currently clear to me (dimly at back of mind, maybe) *why* we need to sort more - OK, rethink this later, but I suspect this is right - the vmap contains entries for pages which are going to be dynamic memory if we use ACORN_INITIAL_NONSTORED_PAGES. If we've grown dynmem, some of the entries in the vmap may be discarded (since they've been promoted to dynmem - this isn't guaranteed, if we used PREOPT) and therefore in order to be left with vmap_max_entries valid entries we need to sort the entire table
    ; If we've adjusted nonstored_pages, we may need to sort more than
    ; vmap_max_entries elements of vmap and it's definitely safe to sort all
    ; vmap_max_size entries, because we either have enough RAM for vmap_max_size
    ; blocks of virtual memory cache or we have enough RAM for the entire game.
    ; (Note that we're only talking about *sorting* here; it's important we
    ; don't sort "useful" things into parts of the vmap we don't have the memory
    ; to load. We're not talking about *how much of the vmap contains useful
    ; entries* - that is specified by vmap_max_entries.)
    ; SFTODO: We *will* sort the dummy 0 entries put at the end of vmap by the
    ; build script into the initial positions if the game didn't use the entire
    ; vmap. This is "wrong" but they will be pruned away when we discard vmap
    ; entries for memory promoted to dynmem (since 0 entries represent the first
    ; page of dynmem) and start using only the first vmap_max_entries; since
    ; this is a sort (we're just reordering things) they haven't actually
    ; displaced anything useful in the meantime. All the same, it might be
    ; neater to make the build script use $ffff for the dummy entries. SFTODONOW: Not intending to rework this now, but maybe just review this
    lda nonstored_pages
    cmp #ACORN_INITIAL_NONSTORED_PAGES
    beq +
    ldx #vmap_max_size
+
}
    stx vmap_sort_entries

    ; SFTODONOW: Not just here - I wonder if I should aggresively factor some of this into
    ; subroutines and/or actually indent nested !if blocks, yes that isn't the general Ozmoo
    ; style, but this code is !ifdef-tastic. (But do note acme warns noisily if labels are
    ; not flush left, which somewhat spoils this idea.)
!ifdef ACORN_SWR {
    ; Calculate vmem_blocks_in_main_ram and vmem_blocks_stolen_in_first_bank.
!ifndef ACORN_SWR_MEDIUM_DYNMEM {
    lda #0
    sta vmem_blocks_stolen_in_first_bank
    ; Set A = (>story_start + nonstored_pages) - (>flat_ramtop - acorn_screen_hole_pages)
    lda #>story_start
    clc
    adc nonstored_pages
!ifdef ACORN_SCREEN_HOLE {
    clc
    adc acorn_screen_hole_pages
}
    sec
    sbc #>flat_ramtop
    bcc .some_vmem_in_main_ram
    lsr
    sta vmem_blocks_stolen_in_first_bank
    bpl + ; Always branch
    +assert_discardable_unreached
.some_vmem_in_main_ram
    ; Carry is clear; negate A
    eor #$ff
    adc #1
    lsr
    sta vmem_blocks_in_main_ram
+
} else {
    lda nonstored_pages
    lsr
    sta vmem_blocks_stolen_in_first_bank
    lda #>flat_ramtop
    sec
    sbc #>vmem_start
!ifdef ACORN_SCREEN_HOLE {
    sec
    sbc acorn_screen_hole_pages
}
    lsr
    sta vmem_blocks_in_main_ram
}

!ifdef ACORN_SHADOW_VMEM {
SFTODOTPP
    ; Calculate vmem_blocks_in_sideways_ram. This is used in
    ; convert_index_x_to_ram_bank_and_address to decide when a vmem block is in
    ; shadow RAM and it doesn't matter if we actually use fewer blocks than
    ; this. This value is just used to ensure that if a vmem block index *would*
    ; access past the end of sideways RAM, it's handled via shadow RAM.
    ; scratch_ram_blocks is in 256-byte blocks, so convert it to 512-byte blocks.
    lsr scratch_ram_blocks + 1
    lda scratch_ram_blocks
    ror
    sec
    sbc vmem_blocks_stolen_in_first_bank
    sta vmem_blocks_in_sideways_ram
    lda scratch_ram_blocks + 1
    sbc #0
    beq +
    ; We have a result which won't fit in a single byte, but since we know the
    ; maximum vmap index is 254, we can just set vmem_blocks_in_sideways_ram to
    ; 255.
    lda #255
    sta vmem_blocks_in_sideways_ram
+
}
}

SFTODOXY7
    ; Now we know how much data we are going to load, we can calculate how many
    ; blocks correspond to each progress indicator position.
    ; SFTODO: Since below we convert back to 512-byte blocks, it might make more
    ; sense to stick with 512-byte blocks here and scale *nonstored_page* instead
    ; of vmap_max_entries.
    lda #0
    sta scratch_blocks_to_load + 1
    lda vmap_max_entries
    asl ; convert to 256-byte blocks
    rol scratch_blocks_to_load + 1
    clc
    adc nonstored_pages ; already in 256-byte blocks
    sta scratch_blocks_to_load
    bcc +
    inc scratch_blocks_to_load + 1
+
} else { ; Not VMEM
    lda #<ACORN_GAME_BLOCKS
    sta scratch_blocks_to_load
    lda #>ACORN_GAME_BLOCKS
    sta scratch_blocks_to_load + 1
}
    ; fall through to init_progress_indicator
; End of prepare_for_initial_load


; We use 16-bit fixed point arithmetic to represent the number of blocks per
; progress bar step, in order to get avoid the bar under-filling or over-filling
; the screen width. scratch_blocks_to_load can't be more than 64K dynamic memory plus
; 128K virtual memory cache (and that's not going to happen in practice), so it
; is effectively a 9-bit value in 512-byte blocks. We can therefore afford 7
; bits for the fractional component.
progress_indicator_fractional_bits=7

; Initialise progress_indicator_blocks_until_next_step and
; progress_indicator_blocks_per_step. This is only called once in any given
; build so we could make it a macro and inline it, but since this code overlaps
; the game data we're not under that much memory pressure and it's more readable
; to just use a subroutine.
init_progress_indicator
    ; If we're not on the bottom line of the screen, set divisor = 2 *
    ; (screen_width - cursor_x), otherwise set divisor = 2 * ((screen_width - 1)
    ; - cursor_x). This way we don't have to worry about causing a mildly ugly
    ; scroll if we print in the bottom right position on the screen. The
    ; multiplication by 2 allows for the use of half-character blocks.
    ;
    ; (We haven't called screenkernal_init yet; that would be wrong because we
    ; might be in mode 6/7 from the loader and not yet have changed into the
    ; final mode for running the game. So we can't use s_screen_width here.)
    lda #osbyte_read_cursor_position
    jsr osbyte ; set X=cursor X, Y=cursor Y
    cpx #0
    bne .cursor_not_in_first_column
    ; The cursor's in the first column, so this is a restart; the loader prints
    ; the "Loading: " prefix for us but a restart doesn't, so do it now. By
    ; doing this here instead of in the restart code, this can be discardable
    ; init code.
    ldx #<.loading_string
    lda #>.loading_string
    jsr printstring_os
    lda #osbyte_read_screen_mode
    jsr osbyte ; set Y=current screen mode
    lda #' '
    cpy #7
    bne .not_mode_7
    ; Text on the lower part of the screen is always white in mode 7, so we
    ; follow suit with the graphics for the progress bar.
    lda #mode_7_graphics_colour_base + 7
.not_mode_7
    jsr oswrch
    lda #osbyte_read_cursor_position
    jsr osbyte ; set X=cursor X, Y=cursor Y
.cursor_not_in_first_column
    stx divisor
    sty divisor + 1
    lda #osbyte_read_vdu_variable
    ldx #vdu_variable_text_window_bottom
    jsr osbyte ; set X=screen_height - 1, Y=screen width - 1
    cpx divisor + 1
    beq .cursor_on_bottom_line
    iny
.cursor_on_bottom_line
    tya
    sec
    sbc divisor
    asl
    sta divisor
    lda #0
    rol
    sta divisor + 1

    ; scratch_blocks_to_load is expressed in 256-byte blocks, but loading is done in
    ; 512-byte blocks, so we want to divide by two to convert this. We want to
    ; shift right by 1 for the division by two, then left by
    ; progress_indicator_fractional_bits bits.
    ldx #progress_indicator_fractional_bits - 1
    ; Set dividend = scratch_blocks_to_load << X.
    lda scratch_blocks_to_load + 1
    sta dividend + 1
    lda scratch_blocks_to_load
-   asl
    rol dividend + 1
    dex
    bne -
    sta dividend
SFTODOOOL
    jsr divide16
    lda division_result
    sta progress_indicator_blocks_per_step
    sta progress_indicator_blocks_until_next_step
    lda division_result + 1
    sta progress_indicator_blocks_per_step + 1
    sta progress_indicator_blocks_until_next_step + 1
.rts
    rts

.loading_string
    !text "Loading:", 0


; SFTODO: Don't forget more code can go here if it can be executed before we
; start to put data at story_start.

!ifdef VMEM {
check_vmap_max_entries
    ; Assert vmap_max_entries >= 1. We need this as the vmem code assumes it
    ; implicitly (e.g. when looping over the vmap).
    lda vmap_max_entries
    bne .rts
    brk
    !byte 0
    !text "vmap_max_entries == 0", 0

constrain_nonstored_pages
    ; We must have nonstored_pages >= ACORN_INITIAL_NONSTORED_PAGES, otherwise
    ; we're not respecting the game's real dynamic memory size.
    lda nonstored_pages
    cmp #ACORN_INITIAL_NONSTORED_PAGES
    bcs +
    lda #ACORN_INITIAL_NONSTORED_PAGES
    sta nonstored_pages
    rts
+
    ; We must have nonstored_pages <= max_nonstored_pages, where
    ; max_nonstored_pages satisfies:
    ;     (ram_blocks - max_nonstored_pages) / vmem_block_pagecount == min_vmem_blocks
    ; So max_nonstored_pages = ram_blocks - min_vmem_blocks * vmem_block_pagecount.
    ;
    ; This ensures that we always have min_vmem_blocks 512-byte blocks of vmem
    ; cache and won't deadlock in the vmem code if the Z-machine PC and data
    ; pointers both need to address different blocks of read-only memory.
    ;
    ; There is a superficial contradiction with this check (note min_vmem_blocks
    ; == 2) and check_vmap_max_entries (which checks vmap_max_entries >= 1). If
    ; vmap_max_entries == 1, there is only one 512-byte block of non-dynamic
    ; memory in the whole game, and therefore we couldn't deadlock in the vmem
    ; code even with just one block of vmem cache. In this case, it's
    ; unnecessarily strict to insist on at least vmem_block_pagecount 512-byte
    ; blocks of vmem, but it's easier just to insist on meeting this condition
    ; all the time. SFTODONOW: I THINK THIS IS CORRECT BUT REVIEW LATER
    ;
    ; Note that although the above talks about ram_blocks, on a normal second
    ; processor with the host cache enabled, ram_blocks is artifically inflated
    ; to include the host cache, and we need to use a value reflecting only the
    ; second processor's own RAM here.

    ; Set transient_zp = ram_blocks; on a normal second processor with the host
    ; cache enabled, we need to count only the second processor's own RAM.
    ; SFTODO: THIS CODE PROBABLY NEVER ACTUALLY EXECUTES ON A NORMAL SECOND PROCESSOR,
    ; BUT I SUPPOSE IT *MIGHT* - WE SHOULD MAYBE CALL IT ALWAYS. OTOH, IF WE *DON'T*
    ; ALWAYS CALL IT, IS IT WORTH THE COMPLEXITY HERE?
    lda ram_blocks
    sta transient_zp
    lda ram_blocks + 1
    sta transient_zp + 1
!ifdef ACORN_TUBE_CACHE {
!ifdef ACORN_TURBO_SUPPORTED {
    bit is_turbo
    bmi +
}
    jsr calculate_normal_tube_own_ram_blocks
    sta transient_zp
    lda #0
    sta transient_zp + 1
+
}

    ; Set transient_zp = max_nonstored_pages =
    ; ram_blocks - min_vmem_blocks *  vmem_block_pagecount.
    sec
    lda transient_zp
    sbc #<(min_vmem_blocks * vmem_block_pagecount)
    sta transient_zp
    lda transient_zp + 1
    sbc #>(min_vmem_blocks * vmem_block_pagecount) ; 0
    bne + ; max_nonstored_pages >= 256, so nonstored_pages < max_nonstored_pages
    lda transient_zp
    cmp nonstored_pages
    bcs +
    sta nonstored_pages
+
    ; Note that as we've already capped ram_blocks at game_blocks, we don't have to
    ; explicitly check for nonstored_blocks being so large it's larger than the game.
    rts

initial_vmap_z_l
    !fill vmap_max_size, 'V'
}

!ifdef ACORN_SHADOW_VMEM {
screen_start_page_by_mode
    !byte $30 ; mode 0
    !byte $30 ; mode 1
    !byte $30 ; mode 2
    !byte $40 ; mode 3
    !byte $58 ; mode 4
    !byte $58 ; mode 5
    !byte $60 ; mode 6
    !byte $7c ; mode 7
}
