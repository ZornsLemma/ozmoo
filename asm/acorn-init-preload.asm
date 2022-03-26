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

!ifndef ACORN_ADFS {
!ifndef ACORN_SWR_MEDIUM_DYNMEM {
.catalogue = scratch_overlapping_game_start
} else {
; story_start will be in sideways RAM; we could make this work, but we'd need to
; make sure the right bank was paged in and it's simpler just to use
; scratch_double_page. We can't simply always use that, because it doesn't exist
; on second processor builds.
.catalogue = scratch_double_page
}
}

!ifdef ACORN_SHADOW_VMEM {
.sideways_ram_pages !fill 2
}

.dpages_to_load !fill 2

!ifdef VMEM {
.ram_pages !fill 2
}

!ifndef ACORN_NO_DYNMEM_ADJUST {
!ifdef ACORN_SWR {
.max_dynmem_pages !fill 1
}
}

!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
.sideways_ram_hole_start_high !fill 1
}

.cursor_x !fill 1
.cursor_y !fill 1

; }}}

; Initialization performed very early during startup.
deletable_init_start
    ; {{{ Clear and initialise memory.

    ; Clear all our zero page; this is probably a good idea for consistency
    ; anyway but is important when some storage allocated via +allocate in
    ; acorn-ozmoo-constants.asm ends up in zero page instead of low memory.
    lda #0
    tax
-   sta $00,x
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

    ; Clear memory betwen bulk_clear_start and bulk_clear_end; some of the
    ; variables which may live here need to be zero initialised and it's good
    ; for consistency to clear everything.
    +assert bulk_clear_start < bulk_clear_end
    lda #0
.sta_bulk_clear_start
-   sta bulk_clear_start ; patched by following code
    inc .sta_bulk_clear_start + 1
    bne +
    inc .sta_bulk_clear_start + 2
+   ldx .sta_bulk_clear_start + 1
    cpx #<bulk_clear_end
    bne -
    ldx .sta_bulk_clear_start + 2
    cpx #>bulk_clear_end
    bne -

    ; Allocation of maxwords and wordoffset is handled specially and they won't
    ; always be automatically cleared by the previous loop, so do them
    ; explicitly here.
    lda #0
    sta maxwords
    sta wordoffset

!ifdef USE_HISTORY {
    ; The history buffer must be zero-initialised, but it's either in low memory
    ; - in which case the bulk clear loop above initialised it - or it's in high
    ; memory and it's zero-initialised in the executable.
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

    ; Call screenkernal_init and deletable_screen_init_1 so we can output text
    ; via s_printchar.
    jsr screenkernal_init
    jsr deletable_screen_init_1

    ; Install our own error handler, which will use s_printchar by default. No
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
    ; SFTODONOW: This is likely to change as part of addressing SFTODONOW elsewhere
    ; about COPY acting like one of the function keys in Beyond Zork.
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

    ; The low byte of mempointer is permanently 0. (SF: This is technically
    ; redundant as we already cleared all our zero page, but let's be explicit
    ; as this is discardable init code.)
    lda #0
    sta mempointer

!ifndef ACORN_SWR {
    ; {{{ Arrange to re-enter BASIC on exit or soft-break.

    ; Patch re_enter_language to enter the current language; reading it here
    ; saves a few bytes of non-deletable code.
    lda #osbyte_read_language
    ldx #0
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
    ; SF: It would probably be possible to patch this to enter the "restart"
    ; code, but it's not clear to me that's desirable.
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

    +prepare_static_high_memory_inline
    +init_readtime_inline
    jsr init_cursor_control

!ifdef ACORN_SHOW_RUNTIME_INFO {
    ; {{{ Enable or disable runtime debug information.

    ; Set .show_runtime_info to 0 unless CTRL-TAB is pressed, in which case set
    ; it to non-0 to trigger display of runtime debug information.
    ; SFTODONOW: TEMP HACKED TO JUST CHECK CTRL AS TAB gets into the keyboard
    ; buffer and seems to (maybe? hard to be sure) cause problems. If nothing
    ; else it gets picked up by the osrdch used to pause after showing this
    ; info. May just stick with CTRL on its own but come back to this later.
    lda #osbyte_read_key
    ldx #inkey_ctrl
    ldy #$ff
    jsr osbyte
    jmp + ; SFTODONOW TEMP HACK
    cpy #0
    beq +
    ldx #inkey_tab
    ldy #$ff
    jsr osbyte
+   sty .show_runtime_info
    lda .show_runtime_info
    beq .no_runtime_info

    ; Call streams_init here so we can output succesfully; this is a little bit
    ; hacky but not a huge problem (and this is debug-only code).
    jsr streams_init
    jsr .prepare_for_runtime_info_output

    ; Output some basic information we know already without further logic.
    jsr .print_indented_following_string
    !text 13, "memory model="
!ifndef ACORN_SWR {
    !text "tube"
!ifdef ACORN_TUBE_CACHE {
    !text "+host cache"
}
} else {
!ifdef ACORN_SWR_MEDIUM_DYNMEM {
    !text "medium"
} else {
!ifdef ACORN_SWR_BIG_DYNMEM {
    !text "big"
} else {
    !text "small"
}
}
}
    !text 13, "program_start=$", 0
    lda #>program_start
    jsr print_byte_as_hex
    lda #<program_start
    jsr print_byte_as_hex
    jsr .print_indented_following_string
    !text 13, "data_start=$", 0
    lda #>data_start
    jsr print_byte_as_hex
    lda #<data_start
    jsr print_byte_as_hex

.no_runtime_info
    ; }}}
}

!ifdef ACORN_SCREEN_HOLE {
    ; {{{ Configure screen hole settings.
    ; Note that just because we support a screen hole, it doesn't mean there is
    ; going to be one. On a BBC this currently won't happen without manual
    ; intervention, because we have separate executables for machines with and
    ; without shadow RAM, but the Electron has a single executable which handles
    ; both cases.
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

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, "acorn_screen_hole_start_page=$", 0
    lda acorn_screen_hole_start_page
    jsr print_byte_as_hex
    jsr .print_indented_following_string
    !text 13, "acorn_screen_hole_pages=$", 0
    lda acorn_screen_hole_pages
    jsr print_byte_as_hex
+
}
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
    ; min(program_start, shadow_start). We can't use memory above shadow_start
    ; to cache shadow RAM, of course; this is unlikely in practice (it would
    ; imply a very high PAGE or a very large shadow cache) and the loader will
    ; also try to enforce this restriction, but the double page alignment just
    ; might cause it to occur without the loader knowing so we have to check
    ; here anyway.
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
    ; SFTODO: We could show vmem_cache_count_mem if ACORN_SHOW_RUNTIME_INFO.

    ; SF: We don't need to zero vmem_cache_cnt and vmem_cache_page_index
    ; explicitly because we cleared all our zero page and low memory on startup.
    ; (Careful if doing this; we haven't called streams_init yet, that might
    ; need to be reordered or we'd postpone the output for a bit.)
    ; }}}
}

    ; fall through to .prepare_for_initial_load
; End of deletable_init_start


; Do as much as we can to prepare for the initial load of game data from disk,
; without actually loading anything (which would overwrite this code).
.prepare_for_initial_load
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
    ; SFTODO: Should we also be case-insensitive, just to be nice?
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
.data_misaligned
    +os_error 0, "DATA misaligned"
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
    ; In this case the start sector must be even so a 512-byte read never
    ; straddles a track boundary. make-acorn.py takes care of this when building
    ; the disc image but let's double check this hasn't been lost since, as this
    ; is discardable init code.
    lsr
    bcs .data_misaligned
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
    ; In this case the start sector must be the first sector on a track as
    ; readblocks assumes this to simplify its calculations. make-acorn.py takes
    ; care of this when building the disc image but let's double check this
    ; hasn't been lost since, as this is discardable init code.
    ; SFTODO: This won't catch errors if the DATA file on the second surface has
    ; been misaligned. We could check the catalogue on that surface, but it's
    ; not trivial to do.
    lda remainder
    bne .data_misaligned
}
    ; }}}
}

!ifdef VMEM {
    ; How much RAM do we have available for game data?

!ifndef ACORN_SWR {
    lda #0
    sta .ram_pages
    sta .ram_pages + 1

!ifdef ACORN_TUBE_CACHE {
    ; {{{ Initialise host cache and add its size to .ram_pages.

    ; We have some cache in the host, which isn't directly accessible but is
    ; almost as good as our own RAM and which will benefit from preloading if
    ; we're on a normal second processor. We count cache pages for now so we
    ; process more of the initial vmap and fix up the inflated value of
    ; vmap_max_entries later.
    lda screen_mode
    ora #shadow_mode_bit
    tax
    lda #osbyte_initialise_cache
    jsr osbyte
    stx host_cache_size_vmem_blocks
!ifdef ACORN_TURBO_SUPPORTED {
    ; A turbo second processor has enough RAM to preload everything in vmap
    ; without touching the host cache. The host cache will still work, but we
    ; don't have anything to preload into it, so having initialised it there's
    ; nothing else to do.
    bit is_turbo
    bmi .host_cache_initialised
}
    ; Note that .ram_pages is 0 at this point.
    ; X is cache size in 512-byte vmem blocks, but we want to count pages here.
    txa
    asl
    rol .ram_pages + 1
    sta .ram_pages
.host_cache_initialised
!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, "host_cache_size_vmem_blocks=$", 0
    lda host_cache_size_vmem_blocks
    jsr print_byte_as_hex
+
}
    ; }}}
}

!ifdef ACORN_TURBO_SUPPORTED {
    ; {{{ Add turbo RAM in banks 1 and 2 to .ram_pages.
    ; On a turbo second processor, we will use all 128K in banks 1 and 2 as
    ; virtual memory cache.
    bit is_turbo
    bpl .dont_count_turbo_ram
    inc .ram_pages + 1
    inc .ram_pages + 1
.dont_count_turbo_ram
    ; }}}
}
} else { ; ACORN_SWR
    ; General observation: Even though this is a sideways RAM build, it's
    ; possible we have no sideways RAM at all. Examples might be a small game
    ; where dynamic memory plus a couple of 512-byte blocks of vmem cache fit in
    ; a 32K machine, or a game built with the big dynamic memory model to support
    ; high-ish values of PAGE but where we're running on a machine with
    ; PAGE=&E00 but no sideways RAM and the game happens to fit. In the medium
    ; model we will always have at least one bank, of course. But in general, we
    ; can't assume ram_bank_count>0.

    ; {{{ Set .ram_pages to reflect available sideways RAM.
    ; We have 64 (2^6) pages per sideways RAM bank, if we have any.
    lda #0
    sta .ram_pages + 1
    lda ram_bank_count
    ldx #6
-   asl
    rol .ram_pages + 1
    dex
    bne -
    sta .ram_pages
    ; }}}

!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
    ; {{{ Adjust .ram_pages if we have a private 12K RAM bank.

    ; Start with the sideways RAM hole disabled; this is nearly always right.
    lda #sideways_ram_hole_start_none
    sta sideways_ram_hole_start

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
    +assert sideways_ram_hole_start_none != 0
    lda #0
    sta sideways_ram_hole_start
    ; Now adjust .ram_pages.
    lda .ram_pages
    sec
    sbc #(16 * 1024 - integra_b_private_ram_size) / 256
    jmp .subtract_private_ram_high_byte

.b_plus_private_ram
    lda .ram_pages
    sec
    sbc #(16 * 1024 - b_plus_private_ram_size) / 256
.subtract_private_ram_high_byte
    sta .ram_pages
    bcs +
    dec .ram_pages + 1
+
.no_private_ram
    ; }}}
}

!ifdef ACORN_SHADOW_VMEM {
    ; {{{ Add any spare shadow RAM to .ram_pages

    ; Save a copy of .ram_pages with just the sideways RAM included for later
    ; when we're calculating vmem_blocks_in_sideways_ram.
    lda .ram_pages
    sta .sideways_ram_pages
    lda .ram_pages + 1
    sta .sideways_ram_pages + 1

    ; We may have some additional RAM pages in shadow RAM not being used for the
    ; screen display.
    lda vmem_cache_count_mem
    beq .no_shadow_cache
    ; On an Electron with a Master RAM Board in shadow mode, the shadow RAM
    ; can't be turned off under software control and
    ; osbyte_read_screen_address_for_mode will always return $8000 even for
    ; modes without shadow_mode_bit set. This makes sense, but it's not much use
    ; to us here. We use our own table of start addresses instead; we might as
    ; well do this on all platforms for consistency.
    ldx screen_mode
    lda .screen_start_page_by_mode,x
    sec
    sbc #>shadow_start
    clc
    adc .ram_pages
    sta .ram_pages
    bcc +
    inc .ram_pages + 1
+
.no_shadow_cache
; }}}
}
} ; end ACORN_SWR

    ; {{{ Add spare main RAM to .ram_pages.
    ; We also have some pages between data_start and flat_ramtop. We're doing a
    ; constant subtraction in code here, but a) this is deletable init code so
    ; it doesn't really cost anything b) if we don't, the relocation code fails
    ; because we have a variation which doesn't follow the simple fixed
    ; relationship it expects.
    lda #>flat_ramtop
    sec
    sbc #>data_start
!ifdef ACORN_SCREEN_HOLE {
    sec
    sbc acorn_screen_hole_pages
}
    clc
    adc .ram_pages
    sta .ram_pages
    bcc +
    inc .ram_pages + 1
+
    ; }}}

    ; .ram_pages now contains the number of pages of RAM we have available,
    ; including RAM which will be used for dynamic memory. Assert that the build
    ; system and the loader have worked together to guarantee that:
    ; - .ram_pages >= ACORN_INITIAL_NONSTORED_PAGES + (min_vmem_blocks *
    ;   vmem_block_pagecount), i.e. that we have enough RAM for the game's
    ;   dynamic memory and two 512-byte blocks of virtual memory cache.
.initial_nonstored_plus_min_vmem_pages = ACORN_INITIAL_NONSTORED_PAGES + (min_vmem_blocks * vmem_block_pagecount)
    sec
    lda .ram_pages
    sbc #<.initial_nonstored_plus_min_vmem_pages
    lda .ram_pages + 1
    sbc #>.initial_nonstored_plus_min_vmem_pages
    bcs +
    +os_error 0, "Not enough RAM"
+
    ; - the game always has at least one 512-byte block of non-dynamic memory.
    +assert ACORN_GAME_PAGES >= ACORN_INITIAL_NONSTORED_PAGES + vmem_block_pagecount

    ; {{{ Set .ram_pages = min(.ram_pages, game_pages). We do this in order to
    ; avoid accessing nonexistent game data as we try to use all available RAM.

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, ".ram_pages (uncapped)=$", 0
    lda .ram_pages + 1
    jsr print_byte_as_hex
    lda .ram_pages
    jsr print_byte_as_hex
+
}

    ldx #>ACORN_GAME_PAGES
    lda #<ACORN_GAME_PAGES
    cpx .ram_pages + 1
    bne +
    cmp .ram_pages
+   bcs +
    stx .ram_pages + 1
    sta .ram_pages
+
    ; }}}

    ; {{{ Set nonstored_pages to the effective dynamic RAM size.
    ; Set nonstored_pages to the number of pages of RAM we are going to treat as
    ; dynamic memory. This is normally the game's actual dynamic memory rounded
    ; up to a 512-byte boundary, i.e. ACORN_INITIAL_NONSTORED_PAGES.
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
    ; cache, as doing so would risk a deadlock when paging.
    ;
    ; So we can vary nonstored_pages between those two hard limits, although
    ; that doesn't mean we *should*. In general this isn't a good idea; raising
    ; nonstored_pages will permanently lock more of the game into memory,
    ; reducing the amount of memory we have to use flexibly as vmem cache and
    ; likely increasing the amount of disc access we need to perform.
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
    bpl .initial_dynmem_adjust_done
    lda #>(flat_ramtop - story_start)
    sta nonstored_pages
}

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
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; We might have no sideways RAM or just the private 12K on a B+ or
    ; Integra-B. We mustn't use the Integra-B 12K for dynamic memory because of
    ; the IBOS workspace, which we only support skipping in the read-only vmem
    ; path, but we can use the B+ private 12K as dynamic memory. Note that
    ; because we don't allow the Integra-B private 12K to be used as the only
    ; sideways RAM bank in the medium model, we can't end up setting .max_dynmem_pages
    ; to 0.
    lda #>flat_ramtop
    ldy ram_bank_count
    beq .upper_bound_in_a
    bit ram_bank_list
    bmi .b_plus_private_12k
    bvs .upper_bound_in_a ; Integra-B private 12K
    lda #>swr_ramtop
    jmp .upper_bound_in_a
.b_plus_private_12k
    lda #>(flat_ramtop + b_plus_private_ram_size)
.upper_bound_in_a
} else { ; not ACORN_SWR_MEDIUM_OR_BIG_DYNMEM, i.e. ACORN_SWR_SMALL_DYNMEM
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
    sta .max_dynmem_pages
    cmp nonstored_pages
    bcs + ; Always branch; .max_dynmem_pages >= nonstored_pages == ACORN_INITIAL_NONSTORED_PAGES
    +assert_discardable_unreached
+

    ; Note that we can't have game_pages < .ram_pages because we capped
    ; .ram_pages earlier, so game_pages >= .ram_pages. If game_pages ==
    ; .ram_pages, we want to set nonstored_pages to .max_dynmem_pages; there's
    ; no downside as we have enough RAM for the entire game and this will allow
    ; as much of the game as possible to be accessed via the faster dynamic
    ; memory code path.
    ldy .ram_pages + 1
    lda .ram_pages
    cpy #>ACORN_GAME_PAGES
    bne .game_pages_gt_ram_pages
    cmp #<ACORN_GAME_PAGES
    bne .game_pages_gt_ram_pages
.use_max_dynmem_pages
    lda .max_dynmem_pages
    sta nonstored_pages
    jmp .initial_dynmem_adjust_done

.game_pages_gt_ram_pages
    ; We have game_pages > .ram_pages. We don't want to reduce flexibility by
    ; locking parts of the game into RAM instead of allowing the virtual memory
    ; system to choose what lives in RAM. It's only a clear win to increase
    ; nonstored_pages if it brings otherwise unusable RAM into play.
    ;
    ; only_dynmem_addressable_pages = .ram_pages - (vmap_max_size *
    ; vmem_block_pagecount) is the number of 256-byte pages we can't address via
    ; the virtual memory code, and can therefore only address as dynamic memory.
    ; If this is negative, there is no memory we can't address, so leave things
    ; alone.
.vmap_max_size_pages = vmap_max_size * vmem_block_pagecount
    ; YA contains .ram_pages.
    sec
    sbc #<.vmap_max_size_pages
    tax
    tya
    sbc #>.vmap_max_size_pages
    bcc .initial_dynmem_adjust_done ; branch if only_dynmem_addressable_pages negative
    ; Set nonstored_pages = min(only_dynmem_addressable_pages,
    ; .max_dynmem_pages); we can't use more dynamic memory than
    ; .max_dynmem_pages, of course.
    bne .use_max_dynmem_pages ; branch if only_dymem_addressable_blocks is > 8 bit
    ; We now have X=only_dynmem_addressable_pages, an 8-bit quantity.
    cpx .max_dynmem_pages
    bcs .use_max_dynmem_pages
    stx nonstored_pages
}
.initial_dynmem_adjust_done
    ; The above adjustments deliberately ignored some general constraints on
    ; nonstored_pages to simplify the code; apply those constraints now.
    jsr .constrain_nonstored_pages ; SFTODO: INLINE IF ONLY ONE CALLER? MAYBE NOT.
} ; end not ACORN_NO_DYNMEM_ADJUST
    ; }}}

    ; {{{ Set .ram_pages -= nonstored_pages, i.e. set .ram_pages to the number
    ; of RAM pages we have available as virtual memory cache.
    lda .ram_pages
    sec
    sbc nonstored_pages
    sta .ram_pages
    bcs +
    dec .ram_pages + 1
+
    ; }}}

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
    jmp +
.some_vmem_in_main_ram
    ; Negate A.
    clc
    eor #$ff
    adc #1
    lsr
    sta vmem_blocks_in_main_ram
+
} else { ; ACORN_SWR_MEDIUM_DYNMEM
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

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, "vmem_blocks_in_main_ram=$", 0
    lda vmem_blocks_in_main_ram
    jsr print_byte_as_hex
    jsr .print_indented_following_string
    !text 13, "vmem_blocks_stolen_in_first_bank=$", 0
    lda vmem_blocks_stolen_in_first_bank
    jsr print_byte_as_hex
+
}
    ; }}}

!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
    ; {{{ Calculate sideways_ram_hole_start for the Integra-B.
    lda sideways_ram_hole_start
    bne .no_sideways_ram_hole

    ; We're on an Integra-B and are using the private RAM, so we need to set up
    ; sideways_ram_hole_start to skip the 1K of IBOS workspace at $8000.
    ; Set sideways_ram_hole_start to:
    ;   (RAM banks including private 12K - 1) * 32 - vmem_blocks_stolen_in_first_bank
    ; unless the result won't fit in 8 bits, in which case we have:
    ;   sideways_ram_hole_start > 255 >= vmap_max_size
    ; and therefore we effectively don't have a hole, so we set it to
    ; sideways_ram_hole_start_none.
    ;
    ; We subtract off vmem_blocks_stolen_in_first_bank because
    ; sideways_ram_hole_start is a logical block number within sideways RAM and
    ; (in convert_index_x_to_ram_bank_and_address) we compare it against the
    ; required such block number, *before* adding vmem_blocks_stolen_in_first_bank
    ; back in as we start to convert to physical block numbers. Conceptually we
    ; could do this the other way around:
    ; - make sideways_ram_hole_start be a physical sideways RAM block number
    ; - therefore don't subtract off vmem_blocks_stolen_in_first_bank when
    ;   calculating it
    ; - compare the physical block number against it *after* adding
    ;   vmem_blocks_stolen_in_first_bank in convert_index_x_to_ram_bank_and_address.
    ; The reason we don't do this is simply that it would require treating
    ; sideways_ram_hole_start as a 16-bit value, slowing things down slightly.
    ; (There are never more than 255 logical blocks of vmem cache, but non-vmem
    ; uses of sideways RAM mean there can be up to 288 physical blocks of
    ; addressable sideways RAM.)
    lda #sideways_ram_hole_start_none
    sta sideways_ram_hole_start
    lda #0
    sta .sideways_ram_hole_start_high
    lda ram_bank_count
    sec
    sbc #1
    ldx #5 ; 32 == 1 << 5
-   asl
    rol .sideways_ram_hole_start_high
    dex
    bne -
    sec
    sbc vmem_blocks_stolen_in_first_bank
    tax
    lda .sideways_ram_hole_start_high
    sbc #0 ; high byte of vmem_blocks_stolen_in_first_bank
    bne .no_sideways_ram_hole
    stx sideways_ram_hole_start
.no_sideways_ram_hole

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, "sideways_ram_hole_start=$", 0
    lda sideways_ram_hole_start
    jsr print_byte_as_hex
+
}
    ; }}}
} ; end ACORN_PRIVATE_RAM_SUPPORTED
} ; end ACORN_SWR

    ; {{{ Calculate vmap_max_entries.

    ; Now set vmap_max_entries = min(.ram_pages / vmem_block_pagecount,
    ; vmap_max_size), i.e. the number of vmap entries which exist and which we
    ; have RAM for. (If we're in the ACORN_TUBE_CACHE case on a normal second
    ; processor, we have that much RAM in total but the number of vmap entries
    ; we can support is lower. It's convenient to work with this larger value
    ; while we do the initial load, then vmap_max_entries is fixed up later.)
    ldx #vmap_max_size
    lda .ram_pages + 1
    lsr
    bne .cap_at_vmap_max_size
    lda .ram_pages
    ror
    cmp #vmap_max_size
    bcs .cap_at_vmap_max_size
    tax
.cap_at_vmap_max_size
    stx vmap_max_entries

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

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, "vmap_max_entries=$", 0
    lda vmap_max_entries
    jsr print_byte_as_hex
+
}

    jsr .check_vmap_max_entries ; SFTODO: inline this?!
    ; }}}

!ifndef ACORN_NO_DYNMEM_ADJUST {
    ; {{{ Remove promoted dynmem from vmap, set vmap_meaningful_entries

    ; The vmap created by the loader assumes nonstored_pages ==
    ; ACORN_INITIAL_NONSTORED_PAGES. If we've increased nonstored_pages, some
    ; vmap entries may refer to memory which is now dynamic memory.
    ;
    ; We iterate over the vmap, removing any entries which now refer to dynamic
    ; memory and shuffling everything down. Any space freed up at the end of the
    ; vmap is populated by dummy entries, leaving us with
    ; vmap_meaningful_entries non-dummy entries. vmap_meaningful_entries <=
    ; vmap_max_entries; if nonstored_pages hasn't been increased, the loop has
    ; no net effect and vmap_meaningful_entries == vmap_max_entries.
    ;
    ; If vmap_meaningful_entries < vmap_max_entries, the dummy entries do have
    ; RAM backing them and can be used for vmem caching during gameplay; we just
    ; don't have anything to load into them to start with. If the Acorn port
    ; hadn't mostly got rid of vmap_used_entries as a distinct value, we might
    ; use to describe this situation, but the dummy entries do the job. They are
    ; $0000, so:
    ; - They have the oldest possible timestamp and the random junk they contain
    ;   will be evicted in preference to anything else when/if we need to load
    ;   more blocks from disc during play.
    ; - They are associated with the initial 512-byte block of the Z-machine
    ;   address space, which is always dynamic memory and therefore they can't
    ;   ever be accessed by the vmem code when the game requests access to a
    ;   read-only block of data.
    ;
    ; In principle we could create valid entries for parts of the game which
    ; aren't already present in the vmap instead of dummy entries, which would
    ; cause us to load a little more data during the initial load. This isn't
    ; entirely straightforward - remember the use of PREOPT can mean the vmap
    ; contains an arbitrarily ordered subset of the game's 512-byte blocks; it
    ; isn't guaranteed the vmap contains the 512-byte blocks starting at
    ; ACORN_INITIAL_NONSTORED_PAGES in ascending order - and it isn't likely to
    ; offer a huge benefit most of the time so we don't try.
    ;
    ; Remove entries in the vmap for dynamic memory, copying from index X to Y
    ; as we go.
    ldx #0
    ldy #0
.vmap_dynmem_removal_loop
    ; We need to shift the 16-bit vmap entry left one bit before comparing it
    ; against nonstored_pages.
    lda vmap_z_l,x
    asl
    lda vmap_z_h,x
    and #vmem_highbyte_mask
    rol
    bne .not_dynmem_entry
    lda vmap_z_l,x
    asl
    cmp nonstored_pages
    beq .not_dynmem_entry ; SFTODO: technically redundant? but harmless even if so.
    bcc .is_dynmem_entry
.not_dynmem_entry
    ; Entry X isn't dynamic memory, so copy it to entry Y and bump Y.
    lda vmap_z_l,x
    sta vmap_z_l,y
    lda vmap_z_h,x
    sta vmap_z_h,y
    iny
    cpy vmap_max_entries
    beq .vmap_dynmem_removal_done
.is_dynmem_entry
    inx
    +assert vmap_max_size != 0 ; loop assumes we go round at least once
    cpx #vmap_max_size
    bne .vmap_dynmem_removal_loop
.vmap_dynmem_removal_done
    sty vmap_meaningful_entries
    ; If vmap_meaningful_entries < vmap_max_entries, fill the vmap from
    ; vmap_meaningful_entries up with dummy entries.
    lda #0
.vmap_add_dummy_entry_loop
    ; SF: This loop could stop at Y == vmap_max_entries, but it doesn't take
    ; significantly longer to create dummy entries for the entire vmap and it
    ; seems safer to do that.
    cpy #vmap_max_size
    beq .vmap_add_dummy_entries_done
    sta vmap_z_l,y
    sta vmap_z_h,y
    iny
    jmp .vmap_add_dummy_entry_loop
.vmap_add_dummy_entries_done

    ; Assert that if we haven't increased nonstored_pages, vmap_max_entries ==
    ; vmap_meaningful_entries.
    lda nonstored_pages
    cmp #ACORN_INITIAL_NONSTORED_PAGES
    bne +
    lda vmap_max_entries
    cmp vmap_meaningful_entries
    beq +
    +os_error 0, "Unexpected vmap adjust"
+
    ; }}}
} else {
    lda vmap_max_entries
    sta vmap_meaningful_entries
}

    ; Assert vmap_meaningful_entries >= 1; the vmap sort assumes this.
    lda vmap_meaningful_entries
    bne +
    +os_error 0, "vmap_meaningful_entries == 0"
+

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, "vmap_meaningful_entries=$", 0
    lda vmap_meaningful_entries
    jsr print_byte_as_hex
+
}

!ifdef ACORN_SHADOW_VMEM {
    ; {{{ Calculate vmem_blocks_in_sideways_ram.

    ; Calculate vmem_blocks_in_sideways_ram. This is used in
    ; convert_index_x_to_ram_bank_and_address to decide when a vmem block is in
    ; shadow RAM. (If we have a lot of sideways RAM, it might be impossible for
    ; a vmem block index to be high enough to touch shadow RAM, but that doesn't
    ; matter here.)
    lda .sideways_ram_pages + 1
    lsr ; convert from pages to 512-byte vmem blocks
    pha
    lda .sideways_ram_pages
    ror
    sec
    sbc vmem_blocks_stolen_in_first_bank
    sta vmem_blocks_in_sideways_ram
    pla
    sbc #0
    beq +
    ; We have a result which won't fit in a single byte, but since we know the
    ; maximum vmap index is 254, we can just set vmem_blocks_in_sideways_ram to
    ; 255 to effectively disable the check for vmem in shadow RAM.
    lda #255
    sta vmem_blocks_in_sideways_ram
+

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, "vmem_blocks_in_sideways_ram=$", 0
    lda vmem_blocks_in_sideways_ram
    jsr print_byte_as_hex
+
}
    ; }}}
}

!ifndef PREOPT {
    ; {{{ Sort vmap to avoid drive head skipping during loading.

    ; vmem_highbyte_mask might be 0 and that enables some small optimisations, but
    ; in this one-off discardable init code we favour simplicity and don't bother.

    ; Sort vmap into ascending order, preserving the timestamps but using just
    ; the addresses as keys. This avoids the drive head jumping around during
    ; the initial load. The build system can't do this sort, because we're
    ; sorting the truncated list with just vmap_meaningful_entries (which will
    ; vary with the RAM available at runtime) not the full list of vmap_max_size
    ; entries.
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
    ; So x and y can be 8-bit unsigned values without any problems, provided we
    ; take care to recognise y=255 as meaning -1.
    ;
    ; This takes about 0.42 seconds to sort 255 shuffled entries at 2MHz; that's
    ; not great but it's not terrible. It takes about 0.1 seconds to sort 122
    ; shuffled entries, which is probably a more typical case. Given a sorted
    ; list - as will happen if the preopt mode has not been used - it takes
    ; about 0.02 seconds to "sort" 255 entries, so there's no significant
    ; performance penalty when this is not doing anything useful.
    ;
    ; (We could simply not include this code if we don't have any preopt data,
    ; but it's discardable init code so it's not really harmful and it seems
    ; best for support purposes to keep the code identical whether or not preopt
    ; data is supplied.)
.temp_l = zp_temp
.temp_h_with_timestamp = zp_temp + 1
.temp_h_without_timestamp = zp_temp + 2
    ldx #1
.outer_loop
    lda vmap_z_l,x
    sta .temp_l
    lda vmap_z_h,x
    sta .temp_h_with_timestamp
    and #vmem_highbyte_mask
    sta .temp_h_without_timestamp
    txa
    tay
.inner_loop
    dey
    lda vmap_z_h,y
    and #vmem_highbyte_mask
    cmp .temp_h_without_timestamp
    bne +
    lda vmap_z_l,y
    cmp .temp_l
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
    lda .temp_l
    sta vmap_z_l,y
    lda .temp_h_with_timestamp
    sta vmap_z_h,y
    inx
    cpx vmap_meaningful_entries
    bne .outer_loop
    ; }}}
}

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    jsr .print_indented_following_string
    !text 13, "nonstored_pages=$", 0
    lda nonstored_pages
    jsr print_byte_as_hex
+
}
} ; VMEM

!ifdef ACORN_SHOW_RUNTIME_INFO {
    lda .show_runtime_info
    beq +
    ; I'll indulge myself with a brief but technically incorrect "any key" (not
    ; Shift, not Ctrl, not Break...) message here.
    ; SFTODONOW: PERHAPS ADD A DOUBLE NEWLINE AT FRONT AND TWEAK MESSAGE NOW
    jsr newline
    jsr .print_indented_following_string
    !text "[press any key]", 0
    jsr osrdch
    ; Start a new line and force the OS cursor to the right position; this will cause
    ; .init_progress_indicator below to set up the progress bar correctly (using the
    ; "we are restarting and have to print Loading: ourselves" code path).
    jsr newline
    jsr newline
    jsr s_cursor_to_screenrowcolumn
+
}

    ; SFTODO: Rename .dpages_to_load to .progress_indicator_full_steps or similar? The progress indicator is block-size agnostic - what we're saying is "we will call update_progress_indicator n times and we want it to go from empty to start with to full after n calls".
    ; {{{ Set .dpages_to_load for the progress indicator.
!ifdef VMEM {
    ; Set .dpages_to_load = (nonstored_pages / vmem_block_pagecount) +
    ; vmap_meaningful_entries.
    lda #0
    sta .dpages_to_load + 1
    lda nonstored_pages
    lsr
    ror .dpages_to_load + 1
    clc
    adc vmap_meaningful_entries
    sta .dpages_to_load
    bcc +
    inc .dpages_to_load + 1
+
} else { ; Not VMEM
    ; SFTODO: vmem_block_pagecount isn't defined for non-vmem builds; this feels
    ; a bit hacky.
.vmem_block_pagecount = 2
    ; Note that because the loader rounds the game up to a multiple of 512 bytes
    ; we don't need to worry about rounding down (which would cause an over-fill
    ; of the progress bar) here.
    lda #<(ACORN_GAME_PAGES / .vmem_block_pagecount)
    sta .dpages_to_load
    lda #>(ACORN_GAME_PAGES / .vmem_block_pagecount)
    sta .dpages_to_load + 1
}
    ; }}}

    ; fall through to .init_progress_indicator
; End of prepare_for_initial_load

; We use 16-bit fixed point arithmetic to represent the number of blocks per
; progress bar step, in order to avoid the bar under-filling or over-filling the
; screen width. .dpages_to_load can't be more than 64K dynamic memory plus 128K
; virtual memory cache (and that's not going to happen in practice), so it is
; effectively a 9-bit value in 512-byte blocks. We can therefore afford 7 bits
; for the fractional component.
progress_indicator_fractional_bits = 7

; Set up the progress indicator so that .dpages_to_load calls to
; update_progress_indicator will completely fill the bar.
.init_progress_indicator
    ; {{{
    ; The loader prints the "Loading: " prefix for us, but when this executable
    ; re-executes itself to implement a restart it doesn't print a prefix. If
    ; the cursor is in the first column of the screen, print the prefix
    ; ourselves. This way we make the "Loading: " prefix printing code part of
    ; the discardable init code instead of the restart code, at the cost of not
    ; showing anything until the re-executed executable has been loaded from
    ; disc.
    lda #osbyte_read_cursor_position
    jsr osbyte ; set X=cursor X, Y=cursor Y
    cpx #0
    bne .cursor_not_in_first_column
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
    stx .cursor_x
    sty .cursor_y

    ; If we're not on the bottom line of the screen, set divisor = 2 *
    ; (screen_width - cursor_x), otherwise set divisor = 2 * ((screen_width - 1)
    ; - cursor_x). This way we don't have to worry about causing a mildly ugly
    ; scroll if we print in the bottom right position on the screen. The
    ; multiplication by 2 allows for the use of half-character steps.
    ;
    ; (We haven't called screenkernal_init yet; that would be wrong because we
    ; might be in mode 6/7 from the loader and not yet have changed into the
    ; final mode for running the game. So we can't use s_screen_width here.)
    lda #osbyte_read_vdu_variable
    ldx #vdu_variable_text_window_bottom
    jsr osbyte ; set X=screen_height - 1, Y=screen width - 1
    cpx .cursor_y
    beq .cursor_on_bottom_line
    iny
.cursor_on_bottom_line
    tya
    sec
    sbc .cursor_x
    asl
    sta divisor
    lda #0
    rol
    sta divisor + 1

    ; Set dividend = .dpages_to_load << progress_indicator_fractional_bits.
    ldx #progress_indicator_fractional_bits
    lda .dpages_to_load + 1
    sta dividend + 1
    lda .dpages_to_load
-   asl
    rol dividend + 1
    dex
    bne -
    sta dividend

    ; Divide and save the result.
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
    beq +
    bcs ++
+   lda #ACORN_INITIAL_NONSTORED_PAGES
    sta nonstored_pages
    rts
++

    ; At this point we know nonstored_pages has been modified. We therefore
    ; can't be running on a non-turbo second processor and can ignore the
    ; complication that .ram_blocks may be inflated by the host cache, which
    ; isn't helpful when checking nonstored_pages. (!ACORN_SWR &&
    ; !ACORN_TURBO_SUPPORTED builds could omit all the code after
    ; +assert_discardable_unreached but it's not worth the fuss; this is
    ; discardable init code anyway.)
!ifndef ACORN_SWR {
!ifdef ACORN_TURBO_SUPPORTED {
    bit is_turbo
    bmi +
}
    +assert_discardable_unreached
+
}

    ; We must have nonstored_pages <= max_nonstored_pages, where
    ; max_nonstored_pages satisfies:
    ;     (.ram_pages - max_nonstored_pages) / vmem_block_pagecount == min_vmem_blocks
    ; So max_nonstored_pages = .ram_pages - min_vmem_blocks * vmem_block_pagecount.
    ;
    ; This ensures that we always have min_vmem_blocks 512-byte blocks of vmem
    ; cache and won't deadlock in the vmem code if the Z-machine PC and data
    ; pointers both need to address different blocks of read-only memory.
    ;
    ; There is a superficial contradiction with this check (note min_vmem_blocks
    ; == 2) and .check_vmap_max_entries (which checks vmap_max_entries >= 1). If
    ; vmap_max_entries == 1, there is only one 512-byte block of non-dynamic
    ; memory in the whole game, and therefore we couldn't deadlock in the vmem
    ; code even with just one block of vmem cache. In this case, it's
    ; unnecessarily strict to insist on at least min_vmem_blocks 512-byte blocks
    ; of vmem, but it's easier just to insist on meeting this condition all the
    ; time.
    ;
    ; Calculate max_nonstored_pages and cap nonstored_pages if required.
    sec
    lda .ram_pages
    sbc #<(min_vmem_blocks * vmem_block_pagecount)
    tax
    lda .ram_pages + 1
    sbc #>(min_vmem_blocks * vmem_block_pagecount) ; 0
    ; AX is now the 16-bit value of max_nonstored_pages
    bne + ; max_nonstored_pages >= 256, so nonstored_pages < max_nonstored_pages
    cpx nonstored_pages
    bcs +
    stx nonstored_pages
+
    ; Note that as we've already capped .ram_pages at game_pages, we don't have
    ; to worry about nonstored_pages being so large it's larger than the game.
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

!ifdef ACORN_SHOW_RUNTIME_INFO {
.show_runtime_info !fill 1
.runtime_info_start_row !fill 1
.column !fill 1 ; SFTODO: rename?

.prepare_for_runtime_info_done
    rts

; Position the cursor and (if appropriate) clear part of the screen so the
; runtime information appears relatively neatly.
.prepare_for_runtime_info_output
    ; We try to leave the "Hardware detected:" part of the loader screen intact
    ; and output the runtime info below that. This might not be present at all,
    ; if we're being re-executed as part of a restart - we can detect this as
    ; .init_progress_indicator does by checking if the cursor is in the leftmost
    ; column. In this case we just leave the cursor where it is so the info
    ; appears at the bottom left of the screen.
    lda #osbyte_read_cursor_position
    jsr osbyte ; set X=cursor X, Y=cursor Y
    cpx #0
    beq .prepare_for_runtime_info_done

    ; We're probably being run for the first time with the loader screen visible.
    ; We don't want to make any definite assumptions about what it looks like, and
    ; we also don't want to allocate a memory location for the loader to convey the
    ; row after the end of the hardware detected list to this code. We therefore
    ; try to find a blank row starting at row 4 or lower and use that as our starting
    ; point. We know we're in a 40x25 mode (6 or 7) here.
    lda #4
    sta .runtime_info_start_row
.row_loop
    lda #0
    sta .column
.column_loop
    ldx .column
    ldy .runtime_info_start_row
    jsr do_oswrch_vdu_goto_xy
    lda #osbyte_read_screen_mode ; also returns character at cursor in X
    jsr osbyte
    txa
    and #$7f ; SFTODO: comment?!
    cmp #' '+1
    bcs .not_blank
    inc .column
    lda .column
    cmp #40
    bne .column_loop
    beq .blank_row_found
.not_blank
    inc .runtime_info_start_row
    lda .runtime_info_start_row
    cmp #25
    bne .row_loop
    ; We couldn't find a blank row. Just start at row 10.
    ; SFTODONOW TEST THIS CASE
    lda #10
    sta .runtime_info_start_row
.blank_row_found
    ; We found a blank row. Clear from here to the bottom of the screen.
    lda #vdu_define_text_window
    jsr oswrch
    lda #0 ; left X
    jsr oswrch
    lda #24 ; bottom Y
    jsr oswrch
    lda #39 ; right X
    jsr oswrch
    lda .runtime_info_start_row ; top left Y
    jsr oswrch
    lda #vdu_cls
    jsr oswrch
    lda #vdu_reset_text_window
    jsr oswrch

    ; SFTODONOW COMMENT
    ; SFTODONOW NEED TO DETECT MODE 6
    ldx #0
    ldy .runtime_info_start_row
    iny
    jsr do_oswrch_vdu_goto_xy
    lda #osbyte_read_screen_mode
    jsr osbyte
    cpy #7
    bne .runtime_info_not_mode_7
.runtime_info_colour = 3 ; SFTODONOW!? MOVE ANYWAY
    lda #mode_7_text_colour_base + .runtime_info_colour
    jsr oswrch
    ; Patch a couple of instructions to allow for mode 7 control codes.
    inc .ldy_imm_column + 1
    inc .print_indented_following_string_ldx_imm_indent + 1
.runtime_info_not_mode_7
    ldx .runtime_info_start_row
    inx
.ldy_imm_column
    ldy #0 ; column - patched to 1 above if mode 7 (to skip colour code)
    jsr set_cursor
    jsr .print_indented_following_string
    !text "Technical details:", 0 ; SFTODONOW: TWEAK STRING
    rts

    ; SFTODONOW: Tweak name of this?
    ; Since this is discardable init code and we used to include a copy of print_following_string in the
    ; "!ifndef DEBUG {" block below anyway, we're indulgent and have a tweaked copy here which includes an
    ; indent after a newline to make the runtime info look nicer.
.print_indented_following_string
	; print text (implicit argument passing)
	; input:
	; output:
	; used registers: a
	; side effects:
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
    cmp #13 ; SFTODO: MAGIC CONSTANT
    beq .do_indent
    jsr streams_print_output
	jmp -
.do_indent
    ; Print 2 (in mode 6) or 3 (in mode 7) spaces after the newline.
    jsr streams_print_output
.print_indented_following_string_ldx_imm_indent
    ldx #2 ; patched at runtime if SFTODO
--  lda #' '
    jsr streams_print_output
    dex
    bne --
    beq -

	; put updated return address on stack
+   lda .return_address + 2
	pha
	lda .return_address + 1
	pha
	rts

; ACORN_SHOW_RUNTIME_INFO needs these utility subroutines which are normally only included
; in debug builds. We don't want to bloat the non-discardable code with them if they're not
; otherwise needed, so we duplicate the code here. (This isn't ideal, but this isn't critical
; code, nor is it likely to change much.)
!ifndef DEBUG {
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

.print_byte_as_hex_one_char
	bit .print_bad_code_buffered
	bmi +
	jmp s_printchar
+	bvs +
	jmp printchar_buffered
+	jmp streams_print_output

.print_bad_code_buffered	!byte 0	; 0 = s_printchar, $80 = printchar_buffered, $ff = streams_print_output
.hex_num
	!text "0123456789abcdef"

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

.saved_a !byte 0
.saved_x !byte 0
.saved_y !byte 0
}
}

}
