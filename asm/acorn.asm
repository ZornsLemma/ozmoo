; Acorn-specific code factored out into its own file for readability.

; Note that the code macros defined in here have the suffix "_inline" if control
; flows straight through them or "_subroutine" if they end by executing rts (or
; equivalent).

; A note on Acorn memory models - this affects code in many places, but I have
; to write this somewhere.
;
; The second processor build (ifndef ACORN_SWR) has a simple flat memory model
; with user RAM from $0400-$f7ff inclusive. It's rather like the C64 but without
; even the complication of paging the kernal ROM in and out, so it doesn't need
; the cache which the C64 code uses when ALLMEM is defined.
;
; The sideways RAM build (ifdef ACORN_SWR) is a bit more involved. The hardware
; situation here is that we have main RAM (not paged) from $0000-$7fff
; inclusive, with up to 16 different 16K banks of "sideways" RAM paged in at
; $8000-$bfff inclusive by writing to romsel_copy and romsel (in that order).
; The OS is not paged and lives permanently at $c000-$ffff inclusive. The loader
; will have located any available sideways RAM banks, verified there's at least
; one and put the count and a list of bank numbers at ram_bank_{count,list} for
; us. SFTODO: Acorn Electron paging is a bit different, may want to tweak this comment
;
; Acorn Ozmoo uses two slightly different sideways RAM models. Both of them
; allow static/high memory to be spread over approximately 9 sideways RAM banks
; (indexed in 512-byte chunks with indexes from 0-254, with chunk 0 starting
; at story_start+nonstored_blocks). The standard Ozmoo mempointer (data) and 
; z_pc_mempointer (Z-machine PC) pointers are extended to each have an associated
; RAM bank (mempointer_ram_bank and z_pc_mempointer_ram_bank respectively). (If
; the relevant byte of Z-machine memory lives in main RAM, the bank number is
; irrelevant as main RAM is not affected by paging.)
;
; The "big dynamic RAM" model (ifndef ACORN_SWR_SMALL_DYNMEM) allows the game's
; dynamic memory (which starts in main RAM at story_start, as on any Ozmoo build)
; to be larger than main RAM and overflow into the first 16K sideways RAM bank.
; The first 16K sideways RAM bank therefore has to be paged in by default, so
; that miscellaneous Ozmoo code which might try to access dynamic memory can do
; so without any trouble. In this model, accesses to memory via read_next_byte
; and read_next_byte_at_z_pc temporarily page in the relevant bank to read the
; byte and then page the first 16K sideways RAM bank back in. (As an
; optimisation, read_next_byte_at_z_pc_unsafe* and friends are used during
; instruction decoding to avoid flipping back and forth excessively while
; reading a multi-byte instruction. This is possible because only a very limited
; set of cases can cause accesses to dynamic memory during instruction decoding.)
;
; The "small dynamic RAM" model (ifdef ACORN_SWR_SMALL_DYNMEM) requires the game's
; dynamic memory to fit in main RAM. Since dynamic memory can then be accessed
; regardless of the currently paged in bank, Ozmoo instead keeps the bank
; containing the Z-machine's PC paged in by default, temporarily paging it out
; only when reading a data byte.
;
; To improve readability of code and avoid double-nesting so we can test for
; ACORN_SWR and !ACORN_SWR_SMALL_DYNMEM in a single !ifdef, we define
; ACORN_SWR_BIG_DYNMEM internally - the build script should never set this.
!ifdef ACORN_SWR {
!ifndef ACORN_SWR_SMALL_DYNMEM {
ACORN_SWR_BIG_DYNMEM = 1
}
}

!zone {

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sideways RAM paging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!ifdef ACORN_SWR {

; These macros must leave the selected bank number in A or Y as appropriate.
; SFTODO: It would probably be possible to use AQR cartridge for all sideways RAM
; on a Master or Electron; it has its own special paging register though and
; I don't know if we can or should auto-detect it (there's also some unlock
; stuff) or if we'd have to ask the user for permission.

!ifndef ACORN_ELECTRON {
bbc_romsel = $fe30

!macro acorn_page_in_bank_using_a .operand {
    lda .operand
    sta romsel_copy
    sta bbc_romsel
}

!macro acorn_page_in_bank_using_a_comma_x .operand {
    lda .operand,x
    sta romsel_copy
    sta bbc_romsel
}

!macro acorn_page_in_bank_using_a_comma_y .operand {
    lda .operand,y
    sta romsel_copy
    sta bbc_romsel
}

!macro acorn_page_in_bank_using_y .operand {
    ldy .operand
    sty romsel_copy
    sty bbc_romsel
}
} else { ; ACORN_ELECTRON
electron_romsel = $fe05

!macro acorn_page_in_bank_using_a .operand {
    lda #12
    sta romsel_copy
    sta electron_romsel
    lda .operand
    sta romsel_copy
    sta electron_romsel
}

!macro acorn_page_in_bank_using_a_comma_x .operand {
    lda #12
    sta romsel_copy
    sta electron_romsel
    lda .operand,x
    sta romsel_copy
    sta electron_romsel
}

!macro acorn_page_in_bank_using_a_comma_y .operand {
    lda #12
    sta romsel_copy
    sta electron_romsel
    lda .operand,y
    sta romsel_copy
    sta electron_romsel
}

!macro acorn_page_in_bank_using_y .operand {
    ldy #12
    sty romsel_copy
    sty electron_romsel
    ldy .operand
    sty romsel_copy
    sty electron_romsel
}
}
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialization and finalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialization performed ASAP on startup.
!macro acorn_deletable_init_start_subroutine {
    ldx #1
    jsr do_osbyte_rw_escape_key

!ifdef ACORN_NO_SHADOW {
    +set_up_mode_7_3c00_inline
}

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

!ifdef ACORN_CURSOR_PASS_THROUGH {
    ldx #1
    jsr do_osbyte_set_cursor_editing
}

    +init_readtime_inline
    jmp init_cursor_control

screenkernal_init
    +screenkernal_init_inline
    rts
} ; End of acorn_deletable_init_start

; Initialization performed shortly after startup, just after
; acorn_deletable_init_start. (The distinction is not that important on Acorn
; as the Ozmoo executable itself doesn't generate a splash screen.)
!macro acorn_deletable_init_inline {
!ifdef TRACE_FLOPPY {
    ; Call streams_init so the tracing is able to show the readblocks calls
    ; performed here.
	jsr streams_init
}

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
    ; if we're not running on a second processor. SFTODO: But we could possibly
    ; conditionally compile it out anyway, even if this is deletable init code
    ; we might need the space for something else deletable.
    lda #<re_enter_language
    sta initial_jmp + 1
    lda #>re_enter_language
    sta initial_jmp + 2

.dir_ptr = zp_temp ; 2 bytes
.length_blocks = zp_temp + 2 ; 2 bytes
; We can't always use story_start to store the catalogue sectors because on an
; ACORN_ELECTRON build that's in sideways RAM, and we can't always use
; scratch_double_page because second processor builds don't have it.
!ifdef scratch_double_page {
.catalogue = scratch_double_page
} else {
.catalogue = story_start
}
!ifndef ACORN_ADFS {
    ; Examine the disc catalogue and determine the first sector occupied by the
    ; DATA file containing the game.
    lda #2
    sta readblocks_numblocks
    lda #0
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
    sta readblocks_mempos ; .catalogue is page-aligned
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
    bne .file_not_found
    iny
    inx
    beq .file_found
    bne .name_compare_loop
.data_filename
    !text "DATA   $"
.file_not_found
    clc
    lda .dir_ptr
    adc #8
    sta .dir_ptr
    bne .find_file_loop
    brk
    !byte 0
    !text "DATA not found"
    !byte 0
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
    ; Determine the length of the DATA file in blocks.
    ldy #6
    lda (.dir_ptr),y
    and #%110000
    lsr
    lsr
    lsr
    lsr
    sta .length_blocks + 1 ; high byte of length in blocks
    dey
    lda (.dir_ptr),y
    sta .length_blocks ; low byte of length in blocks
    dey
    lda (.dir_ptr),y ; low byte of length in bytes
    beq +
    inc .length_blocks
    bne +
    inc .length_blocks + 1
+
!ifdef ACORN_DSD {
    ; If this is a double-sided game, there will be *approximately* (definitely
    ; no more, possibly a track's worth of data less) the same amount of data
    ; on the second side. We don't look up :2.$.DATA and determine its length,
    ; we just double .length_blocks. The absolute worst case here is we read a
    ; track's worth of junk which won't be accessed because it's past the end
    ; of the game.
    asl .length_blocks
    rol .length_blocks + 1
}
} else { ; ACORN_ADFS
    lda #<game_data_filename
    sta scratch_page
    lda #>game_data_filename
    sta scratch_page + 1
    lda #osfile_read_catalogue_information
    ldx #<scratch_page
    ldy #>scratch_page
    jsr osfile
    bne +
    ; The wording of this error is technically incorrect - we're trying to read
    ; information about the file, not open it - but I don't think it's a big
    ; deal. It shouldn't be happening at all, of course.
    jmp cant_open_data_error
+   lda scratch_page + $a
    beq +
    inc scratch_page + $b
    bne +
    inc scratch_page + $c
+   lda scratch_page + $b
    sta .length_blocks
    lda scratch_page + $c
    sta .length_blocks + 1
}

    ; If .length_blocks is odd, increment it by one so the game data is always
    ; considered to be a multiple of 512 bytes. This avoids having to worry
    ; about some corner cases and doesn't cause any problems; on DFS we're doing
    ; raw sector reads and the extra sector will always exist, on ADFS we may try
    ; to do a 512-byte read when only 256 bytes are available but that's fine.
    ; SFTODO: RENAME .length_blocks TO .game_blocks?
    lda .length_blocks
    and #1
    beq +
    inc .length_blocks
    bne +
    inc .length_blocks + 1
+

!ifdef VMEM {
.ram_blocks = .dir_ptr ; 2 bytes

    ; How much RAM do we have available for game data?
    ; We have 64 (2^6) 256-byte blocks per sideways RAM bank, if we have any.
    lda #0
    sta .ram_blocks + 1
!ifdef ACORN_SWR {
    lda ram_bank_count
    ldx #6
-   asl
    rol .ram_blocks + 1
    dex
    bne -
    sta .ram_blocks
} else {
    sta .ram_blocks
}

    ; We also have some blocks between flat_ramtop and story_start.
    ; SF: We're doing a constant subtraction in code here, but a) this is
    ; deletable init code so it doesn't really cost anything b) if we don't,
    ; the relocation code fails because we have a variation which doesn't follow
    ; the simple fixed relationship we expect.
    lda #>flat_ramtop
    sec
    sbc #>story_start
    clc
    adc .ram_blocks
    sta .ram_blocks
    bcc +
    inc .ram_blocks + 1
+

    ; SFTODO: Rename ACORN_ELECTRON to ACORN_ELECTRON_SWR? The tube build is
    ; also for the Electron, it just doesn't need any special case code.
!ifdef ACORN_ELECTRON {
    ; We also have some blocks free between extra_vmem_start and the screen RAM.
    lda #osbyte_read_screen_address
    jsr osbyte
    sec
    tya
    dey
    sty screen_ram_start_minus_1
    sbc #>extra_vmem_start
    tax
    clc
    adc .ram_blocks
    sta .ram_blocks
    bcc +
    inc .ram_blocks + 1
+   txa
    lsr
    sta vmem_blocks_in_main_ram
} else {
!ifdef ACORN_SWR {
    ; This value might be changed below.
    lda #0
    sta vmem_blocks_in_main_ram
}
}

    ; .ram_blocks now contains the number of 256-byte blocks of RAM we have
    ; available, including RAM which will be used for dynamic memory. If the
    ; game is smaller than this, shrink .ram_blocks now so we won't feel the
    ; temptation to (e.g.) access "dummy" vmap entries for nonexistent game data
    ; and read past the end of the data file.
    ; SFTODO: It might be nice to tell the user (how exactly? does the loader
    ; leave us positioned correctly to output a string, and then we say "press
    ; SPACE to start" or something?) if the game has loaded entirely into RAM
    ; and they can remove the disc, and then we'd also want to remove the
    ; check for the game disc being in the drive after a save/restore. (But
    ; they would still need the disc in the drive to do a RESTART, so this is
    ; maybe not a good idea.)
    ldy .length_blocks
    lda .length_blocks + 1
    cmp .ram_blocks + 1
    bne +
    cpy .ram_blocks
+   bcs .game_larger_than_ram
    sty .ram_blocks
    sta .ram_blocks + 1
.game_larger_than_ram
}

    ; SFTODO: I suspect this would be a natural point to populate nonstored_blocks
    ; (not where it's currently done) and to increase it if desired to bring more
    ; SWR into play on systems with very large amounts.
    ; SFTODO: I suspect once the "enlarge dynmem if it helps use more SWR" change
    ; gets implemented, ACORN_NONSTORED_BLOCKS should be renamed something like
    ; ACORN_INITIAL_NONSTORED_BLOCKS to avoid confusion, because it won't
    ; necessarily be the *actual* value of "nonstored_blocks".
    lda #ACORN_NONSTORED_BLOCKS
    sta nonstored_blocks

!ifdef VMEM {
!ifndef ACORN_SWR {
    clc
    lda nonstored_blocks ; SFTODO REDUNDANT BUT LET'S NOT OPTIMISE NOW
    adc #>story_start
    sta vmap_first_ram_page
}

    sec
    lda .ram_blocks
    sbc nonstored_blocks
    sta .ram_blocks
    bcs +
    dec .ram_blocks + 1
+

!ifndef ACORN_ELECTRON { ; SFTODO: RIGHT NOW I HAVE NO IDEA IF THIS IS COMPATIBLE WITH ELECTRON SWR, LET'S JUST RRULE IT OUT WHILE I GET SOMETHING WORKING THEN I CAN THINK ABOUT IT LATER
!ifndef ACORN_NO_DYNMEM_ADJUST {
    ; SFTODO: In theory this optimisation is perfectly valid for the small
    ; dynamic memory model, except that we can't grow nonstored_blocks past
    ; flat_ramtop instead of swr_ramtop. It would be near trivial to support this,
    ; but since it's an extra case to test (or not test and perhaps be broken)
    ; and I suspect it offers little benefit (a big game is quite likely to be
    ; using the big dynamic memory model anyway), I won't do so unless/until a
    ; game which would benefit turns up.
!ifdef ACORN_SWR_BIG_DYNMEM {
!ifdef Z4PLUS {
    ; If we have more RAM for virtual memory than we can support with the
    ; vmap_max_size entries at our disposal, we may be able to use more RAM by
    ; bumping up nonstored_blocks. This will lock some non-dynamic parts of the
    ; game into memory but free up some vmap entries to represent RAM we'd
    ; otherwise be unable to use, so we're never worse off as a result and in
    ; practice we are likely to be better off.
    ;
    ; This is only useful for Z4+ games; a Z3 game is limited to 128K and the 255
    ; 512-byte vmap entries already allow us to access 127.5K; given any realistic
    ; game is going to have at least 512 bytes of dynamic memory, we are not
    ; constrained at all by the 255 entry limit. For similar reasons, only a
    ; machine with a lot of sideways RAM is ever going to need this optimisation.
    ; To take an extreme example, if a game had no dynamic memory at all and we
    ; had 16K of main RAM free, we could use 255*0.5-16=111.5K of sideways RAM
    ; even without this optimisation; in practice we can probably use ~120K+ of
    ; sideways RAM without needing this optimisation.

    ; Do we have any wasted RAM in the first place?
.wasted_ram_blocks = zp_temp + 4 ; 1 byte
    sec
    lda .ram_blocks
    sbc #<(vmap_max_size * vmem_block_pagecount)
    sta .wasted_ram_blocks
    lda .ram_blocks + 1
    sbc #>(vmap_max_size * vmem_block_pagecount)
    bmi .no_wasted_ram
    ; A is zero; we can't have vastly more RAM than vmap_max_size. The loader
    ; (for not unrelated reasons) maxes out at detecting nine banks of sideways
    ; RAM, say we have 16K of main RAM too, that means .ram_blocks is at most
    ; 4*(16+9*16)=640 and vmap_max_size*vmem_block_pagecount will in practice
    ; be 510, so the difference is at most 130, which will fit in a single byte.
    ; (There'd be no point with the current code detecting way more sideways
    ; RAM even if fitted; given vmap_max_size=255, we can't possibly use more
    ; than (just under) eight 16K banks of sideways RAM for virtual memory, and
    ; one more bank for dynamic memory.)
-   bne - ; SFTODO: Temporary code to verify that
    ; We could profitably increment nonstored_blocks by .wasted_ram_blocks.
.candidate_nonstored_blocks = .wasted_ram_blocks
    clc
    lda .wasted_ram_blocks
    adc nonstored_blocks
    sta .candidate_nonstored_blocks
    ; However, we must not make dynamic RAM overflow the first sideways RAM bank. SFTODO: OR MAIN RAM, IF/WHEN WE EXTEND THIS TO HANDLE SMALL DYNMEM MODEL.
    ; We're doing a constant subtraction here; this is OK/necessary as noted in
    ; a different case above.
    sec
    lda #>swr_ramtop ; this would be flat_ramtop for ACORN_SWR_SMALL_DYNMEM
    sbc #>story_start
    ; A now contains the maximum nonstored_blocks we can have without breaking
    ; the memory model. So we want nonstored_blocks=min(A,
    ; .candidate_nonstored_blocks).
    cmp .candidate_nonstored_blocks
    bcc +
    lda .candidate_nonstored_blocks
+   ; A contains the new value for nonstored_blocks. We need to adjust
    ; .ram_blocks (which already had the old value of nonstored_blocks
    ; subtracted from it) before updating nonstored_blocks.
    tax
    clc
    lda .ram_blocks
    adc nonstored_blocks
    sta .ram_blocks
    bcc +
    inc .ram_blocks + 1
+   stx nonstored_blocks
    sec
    lda .ram_blocks
    sbc nonstored_blocks
    sta .ram_blocks
    bcs +
    dec .ram_blocks + 1
+
    ; SFTODONOW: Not necessarily right here - but probably (why not? keep it all in one place) - it would be good to
    ; set the age of vmap blocks which are <=nonstored_blocks to very old, so 
    ; that those now-useless blocks will be the first to be used when loading
    ; new blocks from disc. Their initial ages may well be quite young, since
    ; they are potentially desirable in the early game - it's just that our
    ; dynmem growth has meant we've pulled them into memory permanently anyway.
    ; Actually it would probably not be hard to just set these "dead" blocks to
    ; have "odd" addresses and very old ages, and tweak load_suggested to skip
    ; blocks with "odd" addresses. We wouldn't need the Electron extra twist
    ; where the VM code doesn't consider odd addresses - we *want* these to be
    ; reused, the odd address is just there to stop them matching when we're
    ; looking for data. Doing this would avoid wasting time loading data into
    ; them during the initial load and make them immediately available for use
    ; the first time we need to load anything during play.
.no_wasted_ram
}
}
}
}

    ; At this point, .ram_blocks is the number of RAM blocks we have to use as
    ; backing RAM for the virtual memory subsystem, because we've subtracted
    ; off nonstored_blocks.
    ldx #vmap_max_size
    lda .ram_blocks
    lsr .ram_blocks + 1
    bne .cap_at_vmap_max_size
    ror
    tax
.cap_at_vmap_max_size
    stx vmap_max_entries
}

    ; Load the nonstored blocks. We don't need to worry about reading past the
    ; end of the game data here, because at worst we will read a final 512-byte
    ; block when we don't have a full block and that's fine. In reality there
    ; will always be a big chunk of non-dynamic memory following the nonstored
    ; blocks anyway.
.blocks_to_read = zp_temp + 4 ; 1 byte
    lda #2
    sta readblocks_numblocks
    lda #0
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
    sta readblocks_mempos ; story_start is page-aligned
    lda #>story_start
    sta readblocks_mempos + 1
    lda nonstored_blocks
    sta .blocks_to_read

!ifdef ACORN_SWR_BIG_DYNMEM {
    ; Page in the first bank.
    +acorn_page_in_bank_using_a ram_bank_list
}

.dynmem_load_loop
    jsr readblocks
    lda .blocks_to_read
    sec
    sbc readblocks_numblocks
    sta .blocks_to_read
    bne .dynmem_load_loop

    ; Calculate CRC of block 0 before it gets modified, so we can use it later
    ; to identify the game disc after a save or restore.
    lda #0
    ldx #<story_start
    ldy #>story_start
    jsr calculate_crc
    stx game_disc_crc
    sty game_disc_crc + 1

!ifdef ACORN_SWR {
    ; Calculate vmem_blocks_in_main_ram and vmem_blocks_stolen_in_first_bank.
    lda nonstored_blocks
    clc
    adc #>story_start
    ldx #0
    stx vmem_blocks_stolen_in_first_bank
    sec
    sbc #>flat_ramtop
    bcc .some_vmem_in_main_ram
    lsr
    sta vmem_blocks_stolen_in_first_bank
    bpl + ; Always branch
.some_vmem_in_main_ram
    ; Carry is clear; negate A
    eor #$ff
    adc #1
    lsr
    sta vmem_blocks_in_main_ram
+
}
} ; End of acorn_deletable_init_inline

!ifdef ACORN_SWR {
!macro acorn_swr_page_in_default_bank_using_y {
!ifdef ACORN_SWR_BIG_DYNMEM {
    +acorn_page_in_bank_using_y ram_bank_list
} else {
    +acorn_page_in_bank_using_y z_pc_mempointer_ram_bank
}
}
}

; Acorn-specific initialization to carry out in deletable_screen_init_2. This is
; quite late in the initialization process - in particular it happens after the
; lengthy loading process in acorn_deletable_init_inline.
!macro acorn_deletable_screen_init_2_inline {
!ifdef ACORN_NO_SHADOW {
    ; It's not safe to do vdu_cls without having a text window in effect.
    ; Normally the one set up when we first entered this version of mode 7 is
    ; in effect, but if there's been any output (e.g. due to disc errors) via
    ; s_printchar between then and now we may no longer have one in effect.
    ; s_printchar normally takes care of creating one as needed, but since we're
    ; going to do vdu_cls directly we need to take care of this.
    +define_mode_7_3c00_text_window_inline
    lda #vdu_cls
    jsr oswrch
} else {
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
}

!ifdef ACORN_HW_SCROLL {
    ldx #1
    ldy screen_mode
    cpy #7
    bne +
    dex
+   stx use_hw_scroll
}
    jsr update_colours
} ; End of acorn_deletable_screen_init_2_inline

!macro clean_up_and_quit_inline {
!ifdef ACORN_NO_SHADOW {
    jsr undo_mode_7_3c00
}
    jsr set_os_normal_video
    jsr turn_on_cursor
    ldx #0
    jsr do_osbyte_rw_escape_key
!ifdef ACORN_CURSOR_PASS_THROUGH {
    jsr do_osbyte_set_cursor_editing_x_0
}
    ; Re-enter the current language.
re_enter_language
    lda #osbyte_enter_language
re_enter_language_ldx_imm
    ldx #$ff
    jsr osbyte
    ; never returns
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OS error handling and associated routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

error_handler
    ldy .error_handler_newlines
    beq +
-   lda #13
    jsr .error_handler_print_char
    dey
    bne -
+   ldy #1
-   lda (error_ptr), y
    beq +
    jsr .error_handler_print_char
    iny
    bne - ; Always branch
    ; The following jmp will be patched by code which wants to gain control
    ; after an error.
.error_handler_jmp
+   jmp .press_break

default_error_handler_newlines = 2
.error_handler_newlines !byte default_error_handler_newlines

.press_break
    ; We don't use print_following_string here because we don't want to assume
    ; Ozmoo's own printing mechanisms are properly initialized.
    jsr error_print_following_string
    !text " - press BREAK",0
-   jmp -

; Depending on what's happening when an error occurs, we need to output using
; different primitives. We therefore always use this subroutine and it gets
; patched at runtime.
.error_handler_print_char
    jmp s_printchar

; Like print_following_string, but using .error_handler_print_char.
error_print_following_string
    pla
    sta .error_print_following_string_lda + 1
    pla
    sta .error_print_following_string_lda + 2
-   inc .error_print_following_string_lda + 1
    bne +
    inc .error_print_following_string_lda + 2
+
.error_print_following_string_lda
    lda $ffff
    beq +
    jsr .error_handler_print_char
    jmp -
+   lda .error_print_following_string_lda + 2
    pha
    lda .error_print_following_string_lda + 1
    pha
    rts

error_print_s_printchar = 0
error_print_osasci = 1
.error_print_table_l
    !byte <s_printchar
    !byte <osasci
.error_print_table_h
    !byte >s_printchar
    !byte >osasci

; Allow trapping of errors signalled by the OS via BRKV. Used like this:
;   ldx #2 ; number of newlines to print before any error
;   ldy #n ; type of printing to use if an error occurs (0 s_printchar, 1 osasci)
;   jsr setjmp
;   beq ok
;   ; error occurred, do something
;   jsr set_default_error_handler ; before returning
;   rts
; ok
;   ; do something that might cause an error
;   jsr set_default_error_handler ; errors after this point aren't our problem
setjmp
    stx .error_handler_newlines
    lda .error_print_table_l,y
    sta .error_handler_print_char + 1
    lda .error_print_table_h,y
    sta .error_handler_print_char + 2
    lda #<.setjmp_error_handler
    sta .error_handler_jmp + 1
    lda #>.setjmp_error_handler
    sta .error_handler_jmp + 2
!ifdef ACORN_SWR {
    ; The OS will page the current language back in on BRK, so we need to save
    ; the current bank and restore it in .setjmp_error_handler.
    lda romsel_copy
    sta jmp_buf_ram_bank
}
    ; We need to save the contents of the stack, because they may be corrupted
    ; when an error message is generated. (They probably won't be, but we can't
    ; rely on it.) As a nice side effect of this, the return address for our
    ; caller is saved so .setjmp_error_handler can simply rts after restoring
    ; the stack.
    ; SFTODO: If jmp_buf is made smaller, we could probably fairly easily
    ; detect overflow - initialize y with -buffer_size, do sta jmp_buf+1+buffer_size,y
    ; and if the bne after the iny isn't taken we've overflowed. There might be
    ; an off by one error in that, I'm just sketching the idea out. This is
    ; tempting, *but* at the moment jmp_buf is going to live in $400-800 and
    ; (except for the possibility of starting code at say $600 on 2P) we have
    ; loads of free space down there, so adding a few bytes of code to the VM
    ; to detect overflow and cause a fatal error will eat into valuable memory
    ; for the sake of optimising use of a currently not-scare resource. Think
    ; about it, maybe convert this to an SF: comment.
    tsx
    stx jmp_buf
    ldy #0
-   inx
    beq +
    lda stack,x
    sta jmp_buf+1,y
    iny
    bne -
+   ; Z flag is set
    rts

.setjmp_error_handler
    ldx jmp_buf
    txs
    ldy #0
-   inx
    beq +
    lda jmp_buf+1,y
    sta stack,x
    iny
    bne -
+   
!ifdef ACORN_SWR {
    +acorn_page_in_bank_using_a jmp_buf_ram_bank
}
    lda #1 ; Z flag is clear
    rts

set_default_error_handler
    lda #default_error_handler_newlines
    sta .error_handler_newlines
    lda #<s_printchar
    sta .error_handler_print_char + 1
    lda #>s_printchar
    sta .error_handler_print_char + 2
    lda #<.press_break
    sta .error_handler_jmp + 1
    lda #>.press_break
    sta .error_handler_jmp + 2
.set_default_error_handler_rts
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C64 kernal_readtime emulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; The Acorn OS time counter is a 5 byte value, whereas (ignoring the
    ; difference in resolution) the Commodore one is 3 bytes. Because the Acorn
    ; OS time counter may have an arbitrarily large value (perhaps some kind of
    ; soft RTC solution) when we start up and we don't want to zero it, we read
    ; the initial value and subtract that from any subsequent reads.
!macro init_readtime_inline {
    lda #osword_read_clock
    ldx #<initial_clock
    ldy #>initial_clock
    jsr osword
}

kernal_readtime
.current_clock = scratch_page
    lda #osword_read_clock
    ldx #<.current_clock
    ldy #>.current_clock
    jsr osword
    ldx #(256-5)
    sec
-   lda .current_clock-(256-5),x
    sbc initial_clock-(256-5),x
    sta .current_clock-(256-5),x
    inx
    bne -
    ; The Acorn clock is in centiseconds; a PAL C64 would use fiftieths of a
    ; second, so halve the value before we return it.
    ldx #4
    clc
-   ror .current_clock,x
    dex
    bpl -
    ; All the above means we're at no more or less risk than a C64 of having the
    ; time roll over during a game. It would take >3.8 days for this to happen.
    ldy .current_clock+2
    ldx .current_clock+1
    lda .current_clock+0
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ACORN_NO_SHADOW support (mode 7 screen at $3c00 instead of $7c00)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; If we have no shadow RAM, we need to relocate the mode 7 screen to $3c00 to
; create a contiguous area of RAM from $4000-$c000. See
; https://stardot.org.uk/forums/viewtopic.php?f=54&t=20149 for more discussion
; on this.
!ifdef ACORN_NO_SHADOW {

!ifndef ACORN_SWR {
    !error "ACORN_NO_SHADOW only makes sense with ACORN_SWR"
}

!ifdef ACORN_HW_SCROLL {
    ; The OS can't hardware scroll the mode 7 screen at this non-standard
    ; location.
    !error "ACORN_HW_SCROLL is not compatible with ACORN_NO_SHADOW"
}

; SFTODO: !set just might be useful in making this only do anything once -
; this actually probably isn't necessary, but let me make this note for reference
!macro make_acorn_screen_hole {
.tolerance = 256
    !if * <= $3c00 {
        !if ($3c00 - *) <= .tolerance {
acorn_screen_hole_start = *
            !fill $4000 - *, 'X'
acorn_screen_hole_end
        }
    }
}

; SFTODO: This may be useful in places I currently just use make_acorn_screen_hole
!macro make_acorn_screen_hole_jmp {
.jmp_size = 3
.tolerance = 256
    !if * <= ($3c00 - .jmp_size) {
        !if ($3c00 - *) <= .tolerance {
            jmp acorn_screen_hole_end
acorn_screen_hole_start = *
            !fill $4000 - *, 'X'
acorn_screen_hole_end
        }
    }
}

!macro check_acorn_screen_hole {
    ; This check is important to ensure the no shadow RAM build doesn't crash,
    ; but when the check fails, we need to be able to disable it in order to
    ; allow assembly to complete so we can look at the acme report output and
    ; decide where to add a +make_acorn_screen_hole invocation.
    !ifndef ACORN_DISABLE_SCREEN_HOLE_CHECK {
        !ifndef acorn_screen_hole_start {
            !error "Acorn screen hole has not been added"
        } else {
            !if acorn_screen_hole_start > $3c00 {
                !error "Acorn screen hole starts too late"
            }
            !if acorn_screen_hole_end < $4000 {
                !error "Acorn screen hole ends too soon"
            }
        }
    }
}

; This macro is like an initialization subroutine, but by using a macro we
; can place it in the discardable init code while still having it in this file
; where it logically belongs.
!macro set_up_mode_7_3c00_inline {
    ; In reality we don't expect to be called with our handlers already
    ; installed, but be paranoid - this is deletable init code so it's mostly
    ; free.
    lda wrchv
    cmp #<our_wrchv
    bne +
    lda wrchv + 1
    cmp #>our_wrchv
    beq .set_up_mode_7_3c00_done
+

    ; Copy the contents of the current screen for neatness.
    ldx #$7c
    ldy #$3c
    jsr screen_copy

    ; Reprogram the CRTC and poke the OS variables to mostly compensate.
    lda #crtc_screen_start_high
    sta crtc_register
    lda #$20
    sta crtc_data
    lda #$3c
    sta bottom_of_screen_memory_high
    sta display_start_address + 1

    ; Define a text window and reposition the cursor; doing this forces the OS
    ; to notice the changes we just made. The text window also prevents hardware
    ; scrolling; all the output from the main Ozmoo code will be protected using
    ; text windows anyway since ACORN_HW_SCROLL is not defined, but having one
    ; in place straight away is useful insurance.
    lda #osbyte_read_cursor_position
    jsr osbyte
    +define_mode_7_3c00_text_window_inline
    jsr do_oswrch_vdu_goto_xy

    ; Install our handlers to fix some problems with the OS's handling of this
    ; unofficial mode.
    lda wrchv
    sta call_old_wrchv + 1
    lda wrchv + 1
    sta call_old_wrchv + 2
    lda #<our_wrchv
    sta wrchv
    lda #>our_wrchv
    sta wrchv + 1
    lda keyv
    sta call_old_keyv + 1
    lda keyv + 1
    sta call_old_keyv + 2
    lda #<our_keyv
    sta keyv
    lda #>our_keyv
    sta keyv + 1

.set_up_mode_7_3c00_done
}

!macro define_mode_7_3c00_text_window_inline {
    lda #vdu_define_text_window
    jsr oswrch
    lda #0
    jsr oswrch
    lda #24
    jsr oswrch
    lda #39
    jsr oswrch
    lda #0
    jsr oswrch
}

!macro adjust_cursor_inline {
    lda #crtc_cursor_start_high
    sta crtc_register
    lda text_cursor_address + 1
    sec
    sbc #$1c
    sta crtc_data
}

our_wrchv
call_old_wrchv
    jsr $ffff ; patched during initialization
    pha
    +adjust_cursor_inline
    pla
    rts

our_keyv
    bcc call_old_keyv
    bvc call_old_keyv
    ; keyboard timer interrupt entry
    jsr call_old_keyv
    php
    pha
    bit vdu_status
    bvc .not_cursor_editing
    +adjust_cursor_inline
.not_cursor_editing
    pla
    plp
    rts
call_old_keyv
    jmp $ffff ; patched during initialization

undo_mode_7_3c00
    ; Reset the vectors we fiddled with.
    lda call_old_wrchv + 1
    sta wrchv
    lda call_old_wrchv + 2
    sta wrchv + 1
    lda call_old_keyv + 1
    sta keyv
    lda call_old_keyv + 2
    sta keyv + 1

    ; Switch to normal mode 7. This clears the screen, so we copy the contents
    ; back and restore the cursor position. This is maybe overkill, but it's
    ; important any final message shown when the game quits can be seen by the
    ; user.
    lda #osbyte_read_cursor_position
    jsr osbyte
    lda #vdu_set_mode
    jsr oswrch
    lda #7
    jsr oswrch
    jsr do_oswrch_vdu_goto_xy

    ldx #$3c
    ldy #$7c
    ; fall through to screen_copy

screen_copy
    stx .screen_copy_lda_abs_y + 2
    sty .screen_copy_sta_abs_y + 2
    ldx #4
.screen_copy_loop
    ldy #0
.screen_copy_loop2
.screen_copy_lda_abs_y
    lda $ff00,y ; patched
.screen_copy_sta_abs_y
    sta $ff00,y ; patched
    iny
    bne .screen_copy_loop2
    inc .screen_copy_lda_abs_y + 2
    inc .screen_copy_sta_abs_y + 2
    dex
    bne .screen_copy_loop
    rts
} ; End of !ifdef ACORN_NO_SHADOW

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Miscellaneous utility routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Like printstring_raw, but using OSASCI.
printstring_os
    stx .printstring_os_lda + 1
    sta .printstring_os_lda + 2
-
.printstring_os_lda
   lda $ffff
   beq .calculate_crc_rts
   jsr osasci
   inc .printstring_os_lda + 1
   bne -
   inc .printstring_os_lda + 2
   bne - ; Always branch

; Calculate a CRC over A bytes of data at YX (A=0 => 256 bytes), returning it in
; YX.
calculate_crc
.crc = zp_temp ; 2 bytes
    sta .cpy_imm + 1
    stx .eor_abs + 1
    sty .eor_abs + 2
    lda #0
    sta .crc + 1
    sta .crc
    tay
.nbyt
    lda .crc + 1
.eor_abs
    eor $ffff,y
    sta .crc + 1
    ldx #8
.loop
    lda .crc + 1
    rol
    bcc .b7z
    lda .crc + 1
    eor #8
    sta .crc + 1
    lda .crc
    eor #$10
    sta .crc
.b7z
    rol .crc
    rol .crc + 1
    dex
    bne .loop
    iny
.cpy_imm
    cpy #$ff
    bne .nbyt
    ldx .crc
    ldy .crc + 1
.calculate_crc_rts
    rts

; Two wrappers for calling osbyte_set_cursor_editing to reduce code size; we do
; this is in several places.
do_osbyte_set_cursor_editing_x_0
    ldx #0
do_osbyte_set_cursor_editing
    lda #osbyte_set_cursor_editing
    bne do_osbyte_y_0 ; Always branch

; Two wrappers for calling osbyte_rw_escape_key to reduce code size; we do this
; in several places.
do_osbyte_rw_escape_key
    lda #osbyte_rw_escape_key
do_osbyte_y_0
    ldy #0
    jmp osbyte

; Move the OS cursor to (X, Y).
do_oswrch_vdu_goto_xy
    lda #vdu_goto_xy
    jsr oswrch
    txa
    jsr oswrch
    tya
    jmp oswrch

; SF: ENHANCEMENT: It would potentially be possible to support bold and
; underlined text in modes other than 7, although we'd either need to do it via
; OSWORD 10 and UDG redefinition (which is probably quite slow) or install
; some kind of custom driver around OSWRCH and have that run on the host even if
; we're using a second processor. As a "cheap" version, it would be possible to
; use colours in mode 1 for bold, but I'm not particularly keen to support this
; just for one mode which probably no one would use.
;
; SF: ENHANCEMENT: It would also with a bit more effort probably be possible
; to use a proportionally spaced font in modes other than 7, though this would
; be a more intrusive code change.

}
