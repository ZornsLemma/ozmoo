; Initialization code which may be placed after the end of the Z-machine stack;
; this means it might be overwritten as soon as anything is loaded at
; data_start/story_start.

; SFTODO: I am experimenting with the use of fold markers "{{{" and "}}}" to
; identify pseudo-subroutines in this file in an attempt to improve readability.
; Since this is discardable init code, I could spare the space to actually use
; subroutines; the main reason I haven't done this yet is that it is sometimes
; helpful to be able to search through the code for a variable name and know
; that the order of the code in the file reflects the order in which the
; variable is written to and read from. I suppose if I were strict about
; ordering the subroutines this property would still be preserved.

; As the code in this file gets overwritten when we start to load the game data,
; we put it all in its own zone and make an effort to use local labels for
; almost everything to reduce the risk of accidentally calling a subroutine in
; here after it has been overwritten.
!zone {

; {{{ Allocate space for local variables.

.dir_ptr = zp_temp ; 2 bytes

!ifndef ACORN_SWR_MEDIUM_DYNMEM {
.catalogue = scratch_overlapping_game_start
} else {
; story_start will be in sideways RAM; we could make this work, but we'd need to
; make sure the right bank was paged in and it's simpler just to use
; scratch_double_page. We can't simply always use that, because it doesn't exist
; on second processor builds.
.catalogue = scratch_double_page
}

!ifdef ACORN_SHADOW_VMEM {
.swr_ram_blocks !fill 2
}

.blocks_to_load !fill 2

!ifdef VMEM {
.ram_blocks !fill 2 ; SFTODO: rename to ram_pages???
}

; SFTODO: Probably not, but can the existence of .vmap_sort_entries help simplify the normal tube+cache loading code?
.vmap_sort_entries !fill 1

; }}}

; Initialization performed very early during startup.
deletable_init_start
    ; {{{ Clear and initialise memory.

    ; Clear all our zero page; this is probably a good idea for consistency
    ; anyway but is important when some storage allocated via +allocate in
    ; acorn-ozmoo-constants.asm ends up in zero page instead of low memory.
    lda #0
    tax
-
!ifdef ACORN_TURBO_SUPPORTED {
    ; We must avoid clearing is_turbo, which is set by the turbo test executable
    ; during boot and never touched afterwards.
    cpx #is_turbo
    beq +
}
    sta $00,x
+   inx
    cpx #zp_end
    bne -

    ; Clear memory between low_start (inclusive) and low_fixed_gap_start
    ; (exclusive); this probably isn't necessary, but since we're clearing
    ; memory in general and this is discardable init code it seems best to do as
    ; much as we can.
    lda #0
    tax
-   sta low_start,x
    inx
    cpx #low_fixed_gap_start - low_start
    bne -

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
    lda #0
    sta maxwords
    sta wordoffset

!ifdef USE_HISTORY {
    ; Zero the history buffer; if this isn't done it is sometimes possible to
    ; navigate to invalid history entries. (We don't fold this into the
    ; zero_start to zero_end loop above, because the history might not be in low
    ; memory.) SFTODO: This might be a bit redundant; *if* history is in high
    ; memory it will be zero from loading executable, if it's in low memory we
    ; could extend zero_end to cover it and get rid of this code.
    ldx #history_size - 1
    lda #0
-   sta history_start,x
    dex
    cpx #$ff
    bne -
}

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

!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; This is used enough it's worth - if only for the code size saving -
    ; copying it into zero page. (Well, it saves 10 bytes at the time of
    ; writing; not huge, but not too bad. SFTODO: Maybe reconsider this later.)
    lda ram_bank_list
    sta dynmem_ram_bank
}
    ; }}}

    ldx #1
    jsr do_osbyte_rw_escape_key

    jsr screenkernal_init

    ; Now screenkernal_init has been executed, it's safe to call s_printchar, so
    ; install our own error handler which will use s_printchar by default. No
    ; error should be able to occur before this point. If an error occurs during
    ; a restart, which will re-run the executable, there's not much we can do
    ; but it's probably OK because the load will just replace the code with an
    ; identical copy. (SFTODO: Actually we'll probably bomb out horribly because
    ; the executable is compressed. Can/should I do anything about this? I
    ; haven't tested it. If the error handler is low enough in memory it may
    ; well not be trampled on during the reading-from-disc stage, and there
    ; shouldn't be any errors during decompression itself. So it may be worth
    ; ensuring that we put the error handler as low in memory as possible, and
    ; commenting why we do that.)
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
    ; {{{ Configure OS handling of function keys.

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

    ; }}}
}

    ldx #0
    stx mempointer
!ifndef ACORN_SWR {
    ; {{{ Arrange to re-enter BASIC on exit or soft-break.

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
    ; }}}
}

!ifdef ACORN_TURBO_SUPPORTED {
    ; {{{ Enable turbo mode if necessary.
    ; This executable doesn't have a ROM header indicating we want the turbo
    ; mode enabled (because it has to run on a normal 6502 second processor as
    ; well), so turbo mode will be disabled when we are executed. Turn it back
    ; on if we do want it.
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
    ; }}}
}

!ifdef VMEM {
    ; {{{ Copy initial_vmap_z_l to vmap_z_l.
    ldx #vmap_max_size
-   lda initial_vmap_z_l - 1,x
    sta vmap_z_l - 1,x
    dex
    bne -
    ; }}}
}

!ifdef ACORN_SCREEN_HOLE {
    ; {{{ Configure screen hole settings.
    lda screen_mode
    ora #shadow_mode_bit
    tax
    lda #osbyte_read_screen_address_for_mode
    jsr osbyte
    cpy #>flat_ramtop
    bcc .not_shadow_mode
    ; If we're in a shadow mode, the screen hole isn't needed; by setting the
    ; start page as high as possible, code which implements the screen hole will
    ; realise ASAP that it has nothing to do. (If we didn't do this, we'd end up
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
    ; }}}
}

!ifdef ACORN_SHADOW_VMEM {
    ; {{{ Configure cache pages for spare shadow RAM.

    ; Set vmem_cache_count_mem to the number of 256-byte cache entries we have
    ; for holding data copied out of shadow RAM, i.e. the number of pages
    ; between PAGE/OSHWM and program_start. If we set this to 0, it will
    ; effectively disable the use of shadow RAM as virtual memory cache.
    ;
    ; The loader effectively allocates this cache by setting
    ; ozmoo_relocate_target to a value higher than PAGE; it won't allocate any
    ; memory if it knows we don't have any spare shadow RAM which needs to be
    ; cached. Because relocation works with double pages, we may in fact end up
    ; with one more page than the loader allocated. We need to check we haven't
    ; got fewer than two pages because we need at least that many to avoid
    ; deadlocks (one for the Z-machine PC and another one for data); the loader
    ; won't deliberately allocate a single useless page, but the relocation
    ; rounding to a double page boundary may do this if we started with no pages
    ; allocated.
    ;
    ; If we're in mode 0, there's no spare shadow RAM anyway. The loader won't
    ; have allocated any cache, but the double page rounding might have created
    ; one page, so we explicitly force vmem_cache_count_mem to 0 in mode 0. (I
    ; don't believe this is strictly necessary - if we didn't special-case mode
    ; 0, we would discover that we have 0 or 1 pages of cache and set
    ; vmem_cache_count_mem to 0 anyway. But we might as well be explicit, since
    ; this is discardable init code.)
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
    ; }}}
}

    +prepare_static_high_memory_inline
    +init_readtime_inline
    jsr init_cursor_control

!ifdef ACORN_SHOW_RUNTIME_INFO {
    ; {{{ Show some debug information.
    ; Call deletable_screen_init_1 here so we can output succesfully; this is a
    ; little bit hacky but not a huge problem (and this is debug-only code).
    ; SFTODO: I am not sure this is always working...
    jsr deletable_screen_init_1
    jsr streams_init
    jsr print_following_string
    !text 13, "program_start=$", 0
    lda #>program_start
    jsr print_byte_as_hex
    lda #<program_start
    jsr print_byte_as_hex
    ; }}}
}

    ; SFTODO: just fall through to prepare_for_initial_load?
    jsr prepare_for_initial_load
    rts
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
    ; {{{ Find the first sector occupied by the DATA file by examining the disc
    ; catalogue.

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
    sta readblocks_mempos ; .catalogue is page-aligned
!ifdef ACORN_TURBO_SUPPORTED {
    ; In reality this is redundant but let's play it safe.
    sta readblocks_mempos + 2
}
}
    lda #>.catalogue
    sta .dir_ptr + 1
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
    sta .dir_ptr
.find_file_loop
    ldy #0
    ldx #(-8 & $ff)
.name_compare_loop
    lda (.dir_ptr),y
    ; The directory name will have the top bit set iff the file is locked; we
    ; don't care about that here, so we need to strip it off. It's harmless to
    ; just do this for all characters.
    and #$7f
    cmp .data_filename-(-8 & $ff),x
    bne .not_this_entry
    iny
    inx
    beq .file_found
    bne .name_compare_loop
.data_filename
    !text "DATA   $"
.not_this_entry
    clc
    lda .dir_ptr
    adc #8
    sta .dir_ptr
    bne .find_file_loop
    +os_error 0, "DATA not found"
.file_found
    ; We found the file's name using sector 0, we now want to look at the
    ; corresponding part of sector 1. Adjust .dir_ptr for convenience.
    inc .dir_ptr + 1
    ; Determine the start sector of the DATA file and set readblocks_base.
    ldy #6
    lda (.dir_ptr),y
    and #$3
!ifndef ACORN_DSD {
    sta readblocks_base + 1
    iny
    lda (.dir_ptr),y
    sta readblocks_base
} else {
    sta dividend + 1
    iny
    lda (.dir_ptr),y
    sta dividend
    lda #0
    sta divisor + 1
    lda #10
    sta divisor
    jsr divide16
    lda division_result
    sta readblocks_base
}
    ; }}}
}

SFTODOXX89
!ifdef VMEM {
    ; How much RAM do we have available for game data?

!ifndef ACORN_SWR {
    lda #0
    sta .ram_blocks
    sta .ram_blocks + 1
} else {
    ; General observation: Even thought this is a sideways RAM build, it's
    ; possible we have no sideways RAM at all. Examples might be a small game
    ; where dynamic memory plus a couple of 512-byte blocks of vmem cache fit in
    ; the base 32K, or a game built with the big dynamic memory model to support
    ; high-ish values of PAGE but where we're running on a machine with
    ; PAGE=&E00 but no sideways RAM and the game happens to fit. In the medium
    ; model we will always have at least one bank, of course. But in general, we
    ; can't assume ram_bank_count>0.

    ; {{{ Set .ram_blocks to reflect available sideways RAM.
    ; We have 64 (2^6) 256-byte blocks per sideways RAM bank, if we have any.
    lda #0
    sta .ram_blocks + 1
    lda ram_bank_count
    ldx #6
-   asl
    rol .ram_blocks + 1
    dex
    bne -
    sta .ram_blocks
    ; }}}

!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
    ; {{{ Adjust .ram_blocks if we have a private 12K RAM bank.

    ; Start with the sideways RAM hole disabled; this is nearly always right.
    lda #sideways_ram_hole_start_none
    sta sideways_ram_hole_start ; SFTODO: RENAME THIS acorn_sideway_ram_hole_block_index OR SOMETHING?

    ; The last RAM bank might be the B+ or Integra-B private RAM, which isn't
    ; the full 16K. (For the Integra-B we also need to set
    ; sideways_ram_hole_start later on, once we know
    ; vmem_blocks_stolen_in_first_bank.)
    ldx ram_bank_count
    beq .no_private_ram
    lda ram_bank_list - 1,x
    bmi .b_plus_private_ram
    cmp #64 ; SFTODO MAGIC
    bcc .no_private_ram

    ; This is the Integra-B private 12K.
    ; Set up RAMSEL so we can access the private 12K by setting b6 (PRVEN) of ROMSEL,
    ; much as we can access it by setting b7 on the B+.
    lda $37f
    ora #%00110000 ; set PRVS4 and PRVS8 to make all 12K visible
    sta $37f
    sta $fe34
    ; Set sideways_ram_hole_start to 0 as a temporary indication that we need to
    ; calculate this later; this saves repeating the Integra-B private RAM
    ; detection code.
    lda #0
    sta sideways_ram_hole_start
    ; Now adjust .ram_blocks.
    lda .ram_blocks
    sec
    sbc #(16 * 1024 - integra_b_private_ram_size) / 256
    jmp .subtract_private_ram_high_byte

.b_plus_private_ram
    lda .ram_blocks
    sec
    sbc #(16 * 1024 - b_plus_private_ram_size) / 256
.subtract_private_ram_high_byte
    sta .ram_blocks
    bcs +
    dec .ram_blocks + 1
+
.no_private_ram
    ; }}}
}

!ifdef ACORN_SHADOW_VMEM {
    ; {{{ Add any spare shadow RAM to .ram_blocks

    ; Save a copy of .ram_blocks for later when we're calculating
    ; vmem_blocks_in_sideways_ram.
    lda .ram_blocks
    sta .swr_ram_blocks ; SFTODO: rename? We spell out "sideways" most of the time... (also "PIN Number" syndrome...)
    lda .ram_blocks + 1
    sta .swr_ram_blocks + 1

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
    lda .screen_start_page_by_mode,x
    sec
    sbc #>shadow_start
    clc
    adc .ram_blocks
    sta .ram_blocks
    bcc +
    inc .ram_blocks + 1
+
.no_spare_shadow
; }}}
}
}

; SFTODO: These next two blocks could move into the !ifndef ACORN_SWR block
; above; this might or might not be clearer.

!ifdef ACORN_TUBE_CACHE {
    ; {{{ Initialise host cache and add its size to .ram_blocks.

    ; We have some blocks of cache in the host, which aren't directly accessible
    ; but are almost as good as our own RAM and which will benefit from
    ; preloading if we're on a normal second processor. We count them for now so
    ; we process more of the initial vmap and fix up the inflated value of
    ; vmap_max_entries later.
    lda screen_mode
    ora #shadow_mode_bit
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
    ; Note that .ram_blocks is 0 at this point.
    ; X is cache size in 512-byte blocks, but we want to count 256-byte blocks here.
    txa
    asl
    rol .ram_blocks + 1
    sta .ram_blocks
.host_cache_initialised
SFTODOLABELX1
    ; }}}
}

; SFTODONOW: UP TO HERE IN CURRENT REVIEW
!ifdef ACORN_TURBO_SUPPORTED {
    ; {{{ Add turbo RAM in banks 1 and 2 to .ram_blocks.
    ; On a turbo second processor, we will use all 128K in banks 1 and 2 as
    ; virtual memory cache.
    bit is_turbo
    bpl .dont_count_turbo_ram
    inc .ram_blocks + 1
    inc .ram_blocks + 1
.dont_count_turbo_ram
    ; }}}
}

    ; {{{ Add spare main RAM to .ram_blocks.
    ; We also have some blocks between data_start and flat_ramtop. We're doing a
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
    adc .ram_blocks
    sta .ram_blocks
    bcc +
    inc .ram_blocks + 1
+
    ; }}}

    ; .ram_blocks now contains the number of 256-byte blocks of RAM we have
    ; available, including RAM which will be used for dynamic memory. The build
    ; system and the loader will have worked together to guarantee that:
    ; - .ram_blocks >= ACORN_INITIAL_NONSTORED_PAGES + (min_vmem_blocks *
    ;   vmem_block_pagecount), i.e. that we have enough RAM for the game's
    ;   dynamic memory and two 512-byte blocks of virtual memory cache.
    ; - the game always has at least one block of non-dynamic memory.

    ; {{{ Set .ram_blocks = min(.ram_blocks, game_blocks). We do this in order to
    ; avoid accessing nonexistent game data as we try to use all available RAM.
SFTODOEE2
    ldx #>ACORN_GAME_BLOCKS
    lda #<ACORN_GAME_BLOCKS
    cpx .ram_blocks + 1
    bne +
    cmp .ram_blocks
+   bcs +
    stx .ram_blocks + 1
    sta .ram_blocks
+
    ; }}}

    ; {{{ Set nonstored_pages to the effective dynamic RAM size.
    ; Set nonstored_pages to the number of 256-byte blocks of RAM we are going
    ; to treat as dynamic memory. This is normally the game's actual dynamic
    ; memory rounded up to a 512-byte boundary, i.e.
    ; ACORN_INITIAL_NONSTORED_PAGES.
    lda #ACORN_INITIAL_NONSTORED_PAGES
    sta nonstored_pages

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
    ; SFTODONOW: Don't risk re-use of zp_temp for this?
.max_dynmem = zp_temp + 4 ; 1 byte SFTODONOW RENAME GET RID OF ???
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; We might have no sideways RAM or just the private 12K on a B+ or
    ; Integra-B. We mustn't use the Integra-B 12K for dynamic memory because of
    ; the IBOS workspace, which we only support skipping in the read-only vmem
    ; path, but we can use the B+ private 12K as dynamic memory. Note that
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

    ; If game_blocks == .ram_blocks, we want to set nonstored_pages to
    ; .max_dynmem; there's no downside as we have enough RAM for the entire game
    ; and this will allow as much of the game as possible to be accessed via the
    ; faster dynamic memory code path.
    ldy .ram_blocks + 1
    lda .ram_blocks
    cpy #>ACORN_GAME_BLOCKS
    bne .game_blocks_ne_ram_blocks
    cmp #<ACORN_GAME_BLOCKS
    bne .game_blocks_ne_ram_blocks
.use_max_dynmem
    lda .max_dynmem
    sta nonstored_pages
    jmp .dynmem_adjust_done

.game_blocks_ne_ram_blocks
    ; Note that we can't have game_blocks < .ram_blocks because we reduced
    ; .ram_blocks to match earlier, so game_blocks > .ram_blocks. We don't want to
    ; reduce flexibility by locking parts of the game into RAM instead of
    ; allowing the virtual memory system to choose what lives in RAM. It's only
    ; a clear win to increase nonstored_pages if it brings otherwise unusable
    ; RAM into play.
    ;
    ; only_dynmem_addressable_blocks = .ram_blocks - (vmap_max_size *
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
    jsr .constrain_nonstored_pages ; SFTODO: INLINE IF ONLY ONE CALLER? MAYBE NOT.
}
    ; }}}

    ; {{{ Set .ram_blocks -= nonstored_pages, i.e. set .ram_blocks to the number
    ; of RAM blocks we have available as virtual memory cache.
    lda .ram_blocks
    sec
    sbc nonstored_pages
    sta .ram_blocks
    bcs +
    dec .ram_blocks + 1
+
    ; }}}
; SFTODONOW: WE SHOULD RUNTIME ASSERT WHATEVER WE CAN IN ALL CASES

    ; SFTODONOW: Not just here - I wonder if I should aggresively factor some of this into
    ; subroutines and/or actually indent nested !if blocks, yes that isn't the general Ozmoo
    ; style, but this code is !ifdef-tastic. (But do note acme warns noisily if labels are
    ; not flush left, which somewhat spoils this idea.)
!ifdef ACORN_SWR {
    ; {{{ Calculate vmem_blocks_in_main_ram and vmem_blocks_stolen_in_first_bank.
    lda #0
    sta vmem_blocks_in_main_ram
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
    ; }}}

!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
    ; {{{ Calculate sideways_ram_hole_start.
    lda sideways_ram_hole_start
    bne .no_sideways_ram_hole

    ; We're on an Integra-B and are using the private RAM, so we need to set up
    ; sideways_ram_hole_start to skip the 1K of IBOS workspace at $8000.
    ; Set sideways_ram_hole_start
    ; = (RAM banks including private 12K - 1) * 32 - vmem_blocks_stolen_in_first_bank
    ; = (RAM banks including private 12K * 32) - 32 - vmem_blocks_stolen_in_first_blank
    ; = (.ram_blocks >> 1) - 32 - vmem_blocks_stolen_in_first_bank
    ; If this doesn't fit in a single byte, we will never try to access the
    ; private RAM anyway so we have no hole.
    lda #sideways_ram_hole_start_none
    sta sideways_ram_hole_start
    lda .ram_blocks + 1
    lsr
    bne .no_sideways_ram_hole
    lda .ram_blocks
    ror
    sec
    sbc #32
    +assert_carry_set
    sec
    sbc vmem_blocks_stolen_in_first_bank
    +assert_carry_set
    sta sideways_ram_hole_start
.no_sideways_ram_hole
    ; }}}
}
}

    ; {{{ Calculate vmap_max_entries.

    ; Now set vmap_max_entries = min(.ram_blocks / vmem_block_pagecount,
    ; vmap_max_size), i.e. the number of vmap entries we have RAM to support.
    ; (If we're in the ACORN_TUBE_CACHE case on a normal second processor, we
    ; have that much RAM in total but the number of vmap entries we can support
    ; is lower. It's convenient to work with this larger value while we do the
    ; initial load, then vmap_max_entries is fixed up later.)
    ldx #vmap_max_size
    lda .ram_blocks
    lsr .ram_blocks + 1
    bne .cap_at_vmap_max_size
    ror
    cmp #vmap_max_size
    bcs .cap_at_vmap_max_size
    tax
.cap_at_vmap_max_size
    stx vmap_max_entries
+

!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
    lda sideways_ram_hole_start
    cmp #sideways_ram_hole_start_none
    beq +
    ; We have a sideways RAM hole, so in order to avoid a carry in
    ; convert_index_x_to_ram_bank_and_address (see the comment there), we cap
    ; vmap_max_entries at sideways_ram_hole_vmap_max_size.
    lda #sideways_ram_hole_vmap_max_size
    cmp vmap_max_entries
    bcs +
    sta vmap_max_entries
+
}

    jsr .check_vmap_max_entries
    ; }}}

SFTODOLABEL5
    ; {{{ Calculate .vmap_sort_entries.
    ldx vmap_max_entries
!ifndef ACORN_NO_DYNMEM_ADJUST {
; SFTODONOW: This is probably true, but I probably also need to expand on this comment - it's not currently clear to me (dimly at back of mind, maybe) *why* we need to sort more - OK, rethink this later, but I suspect this is right - the vmap contains entries for pages which are going to be dynamic memory if we use ACORN_INITIAL_NONSTORED_PAGES. If we've grown dynmem, some of the entries in the vmap may be discarded (since they've been promoted to dynmem - this isn't guaranteed, if we used PREOPT) and therefore in order to be left with vmap_max_entries valid entries we need to sort the entire table
; SFTODONOW: Instead could/should we "filter" the vmap to remove pages promoted to dynem, *then* just sort vmap_max_entries entries??? Don't rush into this, I will have had *some* kind of reason for doing it the way I currently am.
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
    ; neater to make the build script use $ffff for the dummy entries. SFTODONOW: Not intending to rework this now, but maybe just review this - TBH maybe I will rework it, but let's see
    lda nonstored_pages
    cmp #ACORN_INITIAL_NONSTORED_PAGES
    beq +
    ldx #vmap_max_size ; SFTODONOW IT'S PROB FINE BUT ANY ISSUES HERE WITH THE CASE WHERE WE'RE ON IB PRIVATE RAM AND HAVE BEEN USING A SLIGHTLY SMALLER MAX SIZE FOR VMAP_MAX_ENTRIES? - I BELIEVE IT'S TECHNICALLY INVALID, STILL THINKING ABOUT THIS
+
}
    stx .vmap_sort_entries
    ; }}}

!ifdef ACORN_SHADOW_VMEM {
    ; {{{ Calculate vmem_blocks_in_sideways_ram.
SFTODOTPP
    ; SFTODO: Rename 'vmem_blocks_in_sideways_ram' to 'sideways_ram_size_in_vmem_blocks'? It is the number of 512-byte blocks of SWR we have, which is *not* the same (because we have vmem_blocks_stolen_in_first_bank possibly > 0) as the number of *blocks of actual vmem* in sideways RAM. - NO, I GOT THAT WRONG - IT *IS* THE NUMBER OF *VMEM* BLOCKS IN SWR, SINCE WE HAVE SUBTRACTED OFF STOLEN
    ; Calculate vmem_blocks_in_sideways_ram. This is used in
    ; convert_index_x_to_ram_bank_and_address to decide when a vmem block is in
    ; shadow RAM and it doesn't matter if we actually use fewer blocks than
    ; this. This value is just used to ensure that if a vmem block index *would*
    ; access past the end of sideways RAM, it's handled via shadow RAM.
    ; .swr_ram_blocks is in 256-byte blocks, so convert it to 512-byte blocks.
    ; SFTODO: I *think* this code is correct and 8-bit-overflow free, but - probably once I've rethought the Integra-B SWR hole support - it may be cleaner to convert this to work with a vmem index as well.
    lsr .swr_ram_blocks + 1
    lda .swr_ram_blocks
    ror
    sec
    sbc vmem_blocks_stolen_in_first_bank
    sta vmem_blocks_in_sideways_ram
    lda .swr_ram_blocks + 1
    sbc #0
    beq +
    ; We have a result which won't fit in a single byte, but since we know the
    ; maximum vmap index is 254, we can just set vmem_blocks_in_sideways_ram to
    ; 255.
    lda #255
    sta vmem_blocks_in_sideways_ram
+
    ; }}}
}

SFTODOXY7
    ; {{{ Calculate .blocks_to_load for the progress indicator.
    ; Now we know how much data we are going to load, we can calculate how many
    ; blocks correspond to each progress indicator position.
    ; SFTODO: Since below we convert back to 512-byte blocks, it might make more
    ; sense to stick with 512-byte blocks here and scale *nonstored_page* instead
    ; of vmap_max_entries.
    ; SFTODO: Move this down to after the VMEM sort and debug-show-info code, to keep progress indicator stuff together?
    lda #0
    sta .blocks_to_load + 1
    lda vmap_max_entries
    asl ; convert to 256-byte blocks
    rol .blocks_to_load + 1
    clc
    adc nonstored_pages ; already in 256-byte blocks
    sta .blocks_to_load
    bcc +
    inc .blocks_to_load + 1
+
} else { ; Not VMEM
    lda #<ACORN_GAME_BLOCKS
    sta .blocks_to_load
    lda #>ACORN_GAME_BLOCKS
    sta .blocks_to_load + 1
}
    ; }}}

!ifdef VMEM { ; SFTODO: MERGE WITH PREV BLOCK!?
!ifndef PREOPT {
    ; {{{ Sort vmap to avoid drive head skipping during loading.

    ; vmem_highbyte_mask might be 0 and that enables some small optimisations, but
    ; in this one-off discardable init code we favour simplicity and don't bother.

    ; Sort vmap into ascending order, preserving the timestamps but using just the
    ; addresses as keys. This avoids the drive head jumping around during the
    ; initial load. The build system can't do this sort, because we're sorting
    ; the truncated list with just .vmap_sort_entries not the full list of
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
    ldx #1
    ; SFTODONOW: Don't risk re-use of zp_temp for this code? But it is mildly performance critical so use of zp may be appropriate, and if nothing *else* is using zp_temp this is probably OK.
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
    cpx .vmap_sort_entries
    bne .outer_loop
    ; }}}

!ifndef ACORN_NO_DYNMEM_ADJUST {
    ; {{{ Fix up vmap if we increased nonstored_pages from default.
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
    ; }}}
}
}
}

!ifdef ACORN_SHOW_RUNTIME_INFO {
    jsr print_following_string
    !text 13, "nonstored_pages=$", 0
    lda nonstored_pages
    jsr print_byte_as_hex
    jsr newline
    jsr osrdch
}

    ; fall through to .init_progress_indicator
; End of prepare_for_initial_load

; We use 16-bit fixed point arithmetic to represent the number of blocks per
; progress bar step, in order to get avoid the bar under-filling or over-filling
; the screen width. .blocks_to_load can't be more than 64K dynamic memory plus
; 128K virtual memory cache (and that's not going to happen in practice), so it
; is effectively a 9-bit value in 512-byte blocks. We can therefore afford 7
; bits for the fractional component.
progress_indicator_fractional_bits=7

; Initialise progress_indicator_blocks_until_next_step and
; progress_indicator_blocks_per_step. This is only called once in any given
; build so we could make it a macro and inline it, but since this code overlaps
; the game data we're not under that much memory pressure and it's more readable
; to just use a subroutine.
.init_progress_indicator
    ; {{{
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

    ; .blocks_to_load is expressed in 256-byte blocks, but loading is done in
    ; 512-byte blocks, so we want to divide by two to convert this. We want to
    ; shift right by 1 for the division by two, then left by
    ; progress_indicator_fractional_bits bits.
    ldx #progress_indicator_fractional_bits - 1
    ; Set dividend = .blocks_to_load << X.
    lda .blocks_to_load + 1
    sta dividend + 1
    lda .blocks_to_load
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
    ; }}}


; SFTODO: Don't forget more code can go here if it can be executed before we
; start to put data at story_start.

!ifdef VMEM {
.check_vmap_max_entries
    ; Assert vmap_max_entries >= 1. We need this as the vmem code assumes it
    ; implicitly (e.g. when looping over the vmap).
    lda vmap_max_entries
    bne .rts
    +os_error 0, "vmap_max_entries == 0"

; Force nonstored_pages to satisfy the necessary constraints after we possibly
; adjusted it.
.constrain_nonstored_pages
    ; {{{
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
    ;     (.ram_blocks - max_nonstored_pages) / vmem_block_pagecount == min_vmem_blocks
    ; So max_nonstored_pages = .ram_blocks - min_vmem_blocks * vmem_block_pagecount.
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
    ; Note that although the above talks about .ram_blocks, on a normal second
    ; processor with the host cache enabled, .ram_blocks is artifically inflated
    ; to include the host cache, and we need to use a value reflecting only the
    ; second processor's own RAM here.

    ; Set transient_zp = .ram_blocks; on a normal second processor with the host
    ; cache enabled, we need to count only the second processor's own RAM.
    ; SFTODO: I don't believe this code will ever actually execute on a normal
    ; second processor, but it doesn't really hurt to support this case here.
    lda .ram_blocks
    sta transient_zp
    lda .ram_blocks + 1
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
    ; .ram_blocks - min_vmem_blocks *  vmem_block_pagecount.
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
    ; Note that as we've already capped .ram_blocks at game_blocks, we don't have to
    ; explicitly check for nonstored_blocks being so large it's larger than the game.
    rts
    ; }}}

initial_vmap_z_l
    !fill vmap_max_size, 'V'
}

!ifdef ACORN_SHADOW_VMEM {
.screen_start_page_by_mode
    !byte $30 ; mode 0
    !byte $30 ; mode 1
    !byte $30 ; mode 2
    !byte $40 ; mode 3
    !byte $58 ; mode 4
    !byte $58 ; mode 5
    !byte $60 ; mode 6
    !byte $7c ; mode 7
}

}

; SFTODONOW: Sure I wrote a note about this somewhere else - should I (not touching upstream variable names) rename things so I consistently talk about "pages" (which are always 256 bytes) and "dpages" (which are always 512 bytes) and avoid the term "block" (except where upstream forces it on me)?
