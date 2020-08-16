; Acorn-specific code factored out into its own file for readability.

; Note that the code macros defined in here have the suffix "_inline" if control
; flows straight through them or "_subroutine" if they end by executing rts (or
; equivalent).

!zone {

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
    lda #osbyte_set_cursor_editing
    ldx #1
    jsr do_osbyte_y_0
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

    ; Examine the disc catalogue and determine the first sector occupied by the
    ; DATA file containing the game.
.dir_ptr = zp_temp ; 2 bytes
.length_blocks = zp_temp + 2 ; 2 bytes
    lda #2
    sta readblocks_numblocks
    lda #0
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
    ; SF: It's never going to be a problem, but note that this assumes we have
    ; at least two pages of memory at story_start.
    sta readblocks_mempos ; story_start is page-aligned
    lda #>story_start
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
    ; of the game. SFTODO: This is probably OK, it feels a little hacky but I
    ; I think it is OK, see how I feel about it later.
    asl .length_blocks
    rol .length_blocks + 1
}

    ; Preload as much of the game as possible into memory. This will always fill
    ; whole RAM banks (provided there's game data left) even if we can't access
    ; that game data, but at worst we're making the user wait while we read
    ; about 16K too much, and adjust_dynamic_memory_inline will probably mean
    ; most of the "excess" read isn't wasted after all.
    ; SFTODO: If we're restarting, we only need to reload the dynamic memory.
    ; If we set an "only load X sectors" flag in the persistent storage in page
    ; 4, we could minimise restart time. (On an ACORN_NO_SHADOW build we would
    ; need to reload enough to restore what we lost through temporarily switching
    ; to real mode 7 with the screen at $7c00.)
    ; SFTODO: It might be nice to tell the user (how exactly? does the loader
    ; leave us positioned correctly to output a string, and then we say "press
    ; SPACE to start" or something?) if the game has loaded entirely into RAM
    ; and they can remove the disc, and then we'd also want to remove the
    ; check for the game disc being in the drive after a save/restore. (But
    ; they would still need the disc in the drive to do a RESTART, so this is
    ; maybe not a good idea.)
.blocks_to_read = .dir_ptr ; 2 bytes
.current_ram_bank_index = zp_temp + 4 ; 1 byte
    lda #2
    sta readblocks_numblocks
    lda #0
    sta .blocks_to_read + 1
!ifndef ACORN_SWR {
    sta .blocks_to_read
}
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
    sta readblocks_mempos ; story_start is page-aligned
    lda #>story_start
    sta readblocks_mempos + 1
    ; We read 64 256-byte blocks per sideways RAM bank, if we have any.
!ifdef ACORN_SWR {
    lda #0
    lda ram_bank_count
    ldx #6
-   asl
    rol .blocks_to_read + 1
    dex
    bne -
    sta .blocks_to_read
}
    ; We read an additional number of 256-byte blocks between story_start and
    ; flat_ramtop.
    ; SF: We're doing a constant subtraction in code here, but a) this is
    ; deletable init code so it doesn't really cost anything b) if we don't,
    ; the relocation code fails because we have a variation which doesn't follow
    ; the simple fixed relationship we expect.
    lda #>flat_ramtop
    sec
    sbc #>story_start
    clc
    adc .blocks_to_read
    sta .blocks_to_read
    bcc +
    inc .blocks_to_read + 1
+

    ; But of course we don't want to read more blocks than there are in the game.
    lda .length_blocks + 1
    ldy .length_blocks
    cmp .blocks_to_read + 1
    bne +
    cpy .blocks_to_read
+   bcs +
    sta .blocks_to_read + 1
    sty .blocks_to_read
+

!ifdef ACORN_SWR {
    ; Stash a copy of .blocks_to_read so we can use it later to help initialize
    ; vmap_max_entries.
    lda .blocks_to_read + 1
    pha
    lda .blocks_to_read
    pha

    ; Page in the first bank.
    lda #0
    sta .current_ram_bank_index
    lda ram_bank_list
    sta romsel_copy
    sta romsel
}

.preload_loop
    ; At the end of the file, we might need to shrink readblocks_numblocks to
    ; avoid reading past the end.
    lda .blocks_to_read + 1
    bne +
    lda .blocks_to_read
    cmp readblocks_numblocks
    bcs +
    sta readblocks_numblocks
+   

!ifdef ACORN_SWR {
    ; Switch to the next bank if necessary
    lda readblocks_mempos + 1
    cmp #>swr_ramtop
    bcc +
    inc .current_ram_bank_index
    ldx .current_ram_bank_index
    lda ram_bank_list,x
    sta romsel_copy
    sta romsel
    lda #>flat_ramtop
    sta readblocks_mempos + 1
+
}

    ; Actually do the read
    jsr readblocks

    ; Decrement .blocks_to_read and loop round if it's not zero.
    lda .blocks_to_read
    sec
    sbc readblocks_numblocks
    sta .blocks_to_read
    bcs +
    dec .blocks_to_read + 1
+   ora .blocks_to_read + 1
    bne .preload_loop

!ifdef ACORN_SWR {
; SFTODO: WE SHOULD PERHAPS HAVE A MACRO FOR THE FOLLOWING IFNDEF+SET
    ; SFTODO: If we're paging in the Z-machine PC bank by default, this may SFTODO: WILL?
    ; actually page in an arbitrary bank, because the Z-machine hasn't started
    ; executing yet, but it won't hurt. We do this here because if we're keeping
    ; the dynamic memory bank paged in by default it just might be that no other
    ; code will page it in before it's needed.
    +acorn_swr_page_in_default_bank_corrupt_a
}

    ; Calculate CRC of block 0 before it gets modified, so we can use it later
    ; to identify the game disc after a save or restore.
    lda #0
    ldx #<story_start
    ldy #>story_start
    jsr calculate_crc
    stx game_disc_crc
    sty game_disc_crc + 1
} ; End of acorn_deletable_init_inline

!ifdef ACORN_SWR {
; SFTODO COMMENT?
!macro acorn_swr_page_in_default_bank_corrupt_a {
; SFTODO: POSSIBLY RENAME ACORN_NO_SWR_DYNMEM, THE DOUBLE NEGATIVES ARE SCREWING WITH MY HEAD - MAYBE KEEP THE *SENSE* (I WOULD LIKE THE UNDEFINED STATE TO BE "SAFE BUT SLOW") BUT CHANGE THE NAME
!ifndef ACORN_NO_SWR_DYNMEM {
    lda ram_bank_list
} else {
    lda z_pc_mempointer_ram_bank
}
    sta romsel_copy
    sta romsel
}

; SFTODO COMMENT
!macro acorn_swr_calculate_vmap_max_entries_inline {
    pla ; number of 256 byte blocks we read from disc earlier, low byte
    sec
    sbc nonstored_blocks
    tax
    pla ; high byte
    sbc #0
    ; Convert from 256 byte blocks to 512 byte VM bloocks.
    lsr
    bne .cap_at_vmap_max_size
    txa
    ror
    bcc +
+   ; We loaded a half VM block at the end of the game. Bump A up by 1, unless
    ; it would wrap round in which case we stick at 255.
    adc #0
    bne +
    lda #255
+   cmp #vmap_max_size
    bcc +
.cap_at_vmap_max_size
    lda #vmap_max_size
+   sta vmap_max_entries

nonstored_blocks_adjusted
    lda nonstored_blocks
    clc
    adc #>story_start
    ldx #0
    stx vmem_blocks_in_main_ram
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

!ifndef ACORN_NO_DYNMEM_ADJUST {
; SFTODO: Obviously if we're not supporting dynamic memory in sideways RAM, we
; can't use +adjust_dynamic_memory_inline, which will forcibly grow dynamic
; memory into sideways RAM. For now we simply don't try if ACORN_NO_SWR_DYNMEM
; is defined, but this may not be optimal - if we have a large game with a small
; dynamic memory requirement make-acorn.py may define ACORN_NO_SWR_DYNMEM as a
; result but the performance might be worse on machines with very large amounts
; of sideways RAM as they might have to access the disc more than they otherwise
; would, in return for a modest performance improvement because they're not
; constantly switching the first sideways RAM bank back in so dynamic memory can
; live there. Maybe make-acorn.py should take an argument telling it not to
; use ACORN_NO_SWR_DYNMEM even if the game's dynamic memory is small enough for
; main RAM.
!ifndef ACORN_NO_SWR_DYNMEM {
    +adjust_dynamic_memory_inline
}
}
} ; End of acorn_swr_calculate_vmap_max_entries_inline

; The amount of sideways RAM we can access is limited by having only
; vmap_max_size (=255) entries in vmap_z_[hl]. We independently access up to 16K
; of sideways RAM where dynamic memory spills over into sideways RAM. We can
; therefore make use of a bit more sideways RAM by bumping up nonstored_blocks
; (in multiples of vmap_block_pagecount (=2), of course) until either
; story_start+nonstored_blocks hits the top of the first 16K RAM bank ($c000)
; or the last vmap entry hits the top of the last RAM bank.
;
; This is only useful for Z4+ games; a Z3 game is limited to 128K and the 255
; 512-byte vmap entries already allow us to access 127.5K; given any realistic
; game is going to have at least 512 bytes of dynamic memory, we are not
; constrained at all by the 255 entry limit.
;
; This will also only help on machines with very large amounts of sideways
; RAM. story_start is going to be $4000 at best, let's say (fairly
; conservatively) a game needs 12K of actual dynamic memory so the first VM
; page is going to be at $7000. That means 1K of virtual memory cache fits in
; main RAM, and with our 255 entries we can have 127.5K of virtual memory cache,
; so we could use 126.5K or approximately 7.9 banks of sideways RAM without
; any difficulty. adjust_dynamic_memory_inline would allow an extra 20K of
; sideways RAM to be used in this case, but unless the machine has >128K of
; sideways RAM this isn't helpful. (With 128K of sideways RAM the adjustment
; avoids wasting the last 1.5K of sideways RAM; worth having but not a big win.)
!macro adjust_dynamic_memory_inline {
!ifndef Z3 {
    ldx vmap_max_entries
    cpx #vmap_max_size
    bcc .no_wasted_swr
    dex
    jsr convert_index_x_to_ram_bank_and_address
    iny
    cpy ram_bank_count
    php
    sta zp_temp
    lda #(>swr_ramtop) - vmem_block_pagecount
    sec
    sbc zp_temp
    plp
    bcs .last_bank_partly_used
    ; We have at least one bank completely unused
    ; Carry is clear
    adc #>(swr_ramtop - flat_ramtop) ; SFTODO: ASSUMES 16K BANK
.last_bank_partly_used
    tax
    beq .no_wasted_swr
    ; A now contains the number of 256-byte blocks wasted at the end of the
    ; final bank(s); they contain game data but can't be accessed. We therefore
    ; bump nonstored_blocks up by up to this amount; this locks some of the low
    ; could-have-been-swappable static memory into place, but brings into play
    ; some more swappable static memory in the otherwise wasted sideways RAM.
    ; It's "up to" this amount because we can't have nonstored_blocks overrunning
    ; the first bank of sideways RAM.
    ; (If the game isn't long enough, all this is harmless; we'll just have some
    ; vmap entries for too-high Z addresses pointing to uninitialised data, and
    ; those will never be used.)
    !if vmem_block_pagecount <> 2 {
        !error "Only SMALLBLOCK supported"
    }
    sta zp_temp
    lda #>story_start
    clc
    adc nonstored_blocks
    sta zp_temp + 1
    adc zp_temp
    cmp #>swr_ramtop
    bcc +
    lda #>swr_ramtop
+   sec
    sbc zp_temp + 1
    beq .no_wasted_swr
    sta zp_temp
.wasted_pages_reclaimable = zp_temp
    clc
    adc nonstored_blocks
    sta nonstored_blocks
    ; We now need to adjust vmap_[lh] to take account of the modified value of
    ; nonstored_blocks, since the build script pre-populated it based on the
    ; pre-modification value.
    ldx vmap_max_entries
    dex
.vmap_fixup_loop
    lda vmap_z_l,x
    clc
    adc .wasted_pages_reclaimable
    sta vmap_z_l,x
    bcc .no_carry
    ; There's a carry into vmap_z_h,x. This can't cause a problem, because this
    ; is a Z4+ game and so the address space is large enough that we can
    ; never overflow the vmem_highbyte_mask bits. SFTODO: If we supported
    ; PRELOAD and didn't have a linear sequence of low addresses in the initial
    ; vmap that might not be true.
    lda vmap_z_h,x
    and #vmem_highbyte_mask
    adc #0
    sta zp_temp + 1
    lda vmap_z_h,x
    and #($ff xor vmem_highbyte_mask)
    ora zp_temp + 1
    sta vmap_z_h,x
.no_carry
    dex
    cpx #255
    bne .vmap_fixup_loop
    ; Now loop back round to fix up vmem_blocks_stolen_in_first_bank and
    ; vmem_blocks_in_main_ram.
    jmp nonstored_blocks_adjusted

.no_wasted_swr
    ; convert_index_x_to_ram_bank_and_address will have left the last bank
    ; paged in, and we need the first bank paged in by default.
    lda ram_bank_list
    sta romsel_copy
    sta romsel
}
} ; End of adjust_dynamic_memory_inline
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
    ; SFTODO: Maybe (this is deletable code) if we're already in the right mode
    ; we should not set it again, to avoid screen flashing temporarily to black
    ; on RESTART.
    lda #vdu_set_mode
    jsr oswrch
    lda screen_mode
    ora #128 ; force shadow mode on
    jsr oswrch
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
    lda #osbyte_set_cursor_editing
    ldx #0
    jsr do_osbyte_y_0
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
    lda jmp_buf_ram_bank
    sta romsel_copy
    sta romsel
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

; SFTODO: I should perhaps have a variant on this or allow it to take an
; argument which will cause it to emit a jmp around the hole. This would allow
; me to minimise wasted space.
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
    lda #vdu_goto_xy
    jsr oswrch
    txa
    jsr oswrch
    tya
    jsr oswrch

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
    ; SFTODO: Do we do vdu_goto_xy a lot? Can we factor out the code to save
    ; space?
    lda #vdu_goto_xy
    jsr oswrch
    txa
    jsr oswrch
    tya
    jsr oswrch

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

; Two wrappers for calling osbyte_rw_escape_key to reduce code size; we do this
; in several places.
do_osbyte_rw_escape_key
    lda #osbyte_rw_escape_key
do_osbyte_y_0
    ldy #0
    jmp osbyte

}
