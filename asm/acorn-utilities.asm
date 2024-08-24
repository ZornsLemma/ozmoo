!zone {

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

default_error_handler_newlines = 4
.error_handler_newlines !byte default_error_handler_newlines

.press_break
    ; We don't use print_following_string here because we don't want to assume
    ; Ozmoo's own printing mechanisms are properly initialized.
    jsr error_print_following_string
    ; The following string is re-used by clean_up_and_quit_inline; this means a
    ; slight compromise on the punctuation of the output here, but it's not a
    ; big deal and normally this message should never be visible.
press_break_string
    !text cr
    !text "[Press BREAK]", cr, 0
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
    lda $ffff ; patched by code above
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
;   ; no error occurred
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
    tsx
!ifdef TRACE_SETJMP {
    cpx setjmp_min_s
    bcs +
    stx setjmp_min_s
+
}
    stx jmp_buf
.initial_y = (-jmp_buf_size) & $ff
    ldy #.initial_y
-   inx
    beq +
    lda stack,x
    sta jmp_buf+1-.initial_y,y
    iny
    bne - ; branch if we have't overflowed jmp_buf, which shouldn't happen in practice
    ; SFTODO: CHECK_ERRORS is not necessarily the best way to control this. Especially
    ; (do another code review) if Ozmoo doesn't use arbitrary amounts of stack
    ; depending on the Z-code being executed, this isn't something that should
    ; be triggered by buggy Z-code; it's internal to the interpreter.
!ifdef CHECK_ERRORS {
    ; Y starts at -jmp_buf_size and is incremented every time we store a byte,
    ; so if it reaches 0 will have written jmp_buf_size bytes starting at
    ; jmp_buf+1 and have therefore overflowed the buffer (remember jmp_buf holds
    ; a copy of S). This shouldn't happen in practice, as jmp_buf_size is chosen
    ; to accommodate the largest stack size we will have when setjmp is called.
    lda #ERROR_JMP_BUF_OVERFLOW
    jsr fatalerror
}
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
    ; SFTODO: This could use a "slow" version of the paging macro, but it would be the
    ; only such user with this ram bank so probably no point?
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

; The Acorn OS time counter is a 5 byte value, whereas (ignoring the difference
; in resolution) the Commodore one is 3 bytes. Because the Acorn OS time counter
; may have an arbitrarily large value (perhaps some kind of soft RTC solution)
; when we start up and we don't want to zero it, we read the initial value and
; subtract that from any subsequent reads. SFTODO: We could get away without
; this and save a few bytes of runtime code, although it would mean you'd have
; to subtract the initial benchmark time from the final one, and you'd not be
; able to use the initial benchmark time as a measure of startup time.
!macro init_readtime_inline {
    lda #osword_read_clock
    ldx #<initial_clock
    ldy #>initial_clock
    jsr osword
}

; The Acorn kernal_readtime uses 100Hz ticks; a C64 uses 60Hz ticks. Since we
; subtract off the initial value of the OS time it would take 1.94 days from
; game startup for the value returned by kernal_readtime to wrap round, slightly
; quicker than a C64 at 3.24 days (from reset), but still not a concern. I think
; the Ozmoo timer code would handle a wraparound transparently anyway.
kernal_readtime
.current_clock = scratch_page
    lda #osword_read_clock
    ldx #<.current_clock
    ldy #>.current_clock
    jsr osword
    +assert .current_clock >= $100 ; guaranteed, but be explicit
    ldx #(256-5)
    sec
-   lda .current_clock-(256-5),x
    ; abs,X addressing doesn't wrap around at the top of the 64K address space
    ; on a 65816 (as used in some co-pros). zp,X addressing does wrap at the end
    ; of zero page, so if initial_clock is in zero page we make sure to use zp
    ; addressing.
!if initial_clock >= $100 {
    sbc initial_clock-(256-5),x
} else {
    sbc+1 (initial_clock-(256-5)) and $ff,x ; force zp addressing just to be safe
}
    sta .current_clock-(256-5),x
    inx
    bne -
    ldy .current_clock+2
    ldx .current_clock+1
    lda .current_clock+0
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Miscellaneous utility routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!macro clean_up_and_quit_inline {
!ifndef ACORN_ON_QUIT_COMMAND {
    lda #<press_break_string
    ldy #>press_break_string
    jsr printstring
-   jmp -
} else {
    ; SFTODO: I should build up a list of things the code called via the option
    ; might want to deal with and leave a permanent comment here or document
    ; this elsewhere. The following is a start, but isn't complete:
    ; - reset host WRCHV and EVNTV in case the fast hardware scroll code is in
    ; - use
    ; - disable VSYNC event in case the fast hardware scroll code was using it
    ; - reset host INSV in case the our INSV handler is in use
    !ifdef ACORN_ON_QUIT_COMMAND_SILENT {
        lda #vdu_disable
        jsr oswrch
    }
    ldx #<.on_quit_command
    ldy #>.on_quit_command
    jsr oscli
    ldx #<.basic_command
    ldy #>.basic_command
    jmp oscli ; never returns
.on_quit_command
    !source "../temp/on-quit-command.asm"
.basic_command
    !text "BASIC", 13
}
}

!ifndef ACORN_SWR {
    ; Re-enter the current language.
re_enter_language
    lda #osbyte_enter_language
re_enter_language_ldx_imm
    ldx #$ff ; patched by initialisation code
    jsr osbyte ; never returns
}

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

; Calculate a CRC over 256 bytes of data at YX, returning it in YX.
calculate_crc_256
; This must not use the same location as .result in acorn-disk.asm as otherwise
; when .get_game_disc_back calls calculate_crc it will be overwritten.
.crc = transient_zp ; 2 bytes
    stx .eor_abs + 1
    sty .eor_abs + 2
    lda #0
    sta .crc + 1
    sta .crc
    tay
.nbyt
    lda .crc + 1
.eor_abs
    eor $ffff,y ; patched by code above
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
    bne .nbyt
    ldx .crc
    ldy .crc + 1
.calculate_crc_rts
    rts

; Two wrappers for calling osbyte_set_cursor_editing to reduce code size; we do
; this in several places.
; SFTODO: Take "osbyte" out of these names, now they may set this indirectly on
; USE_HISTORY builds?
do_osbyte_set_cursor_editing_x_0
    ldx #0
do_osbyte_set_cursor_editing
!ifndef USE_HISTORY {
    lda #osbyte_set_cursor_editing
    bne do_osbyte_y_0 ; Always branch
} else {
    ; We write X to nominal_cursor_key_status and let our INSV handler take care
    ; of the actual *FX4 state.
!ifdef ACORN_SWR {
    stx nominal_cursor_key_status
    rts
} else {
    stx .osword_write_host_block_data
    lda #osword_write_host
    ldx #<.osword_write_host_block
    ldy #>.osword_write_host_block
    jmp osword
.osword_write_host_block
    !word nominal_cursor_key_status ; low order word of address
    !word $ffff                     ; high order word of address
.osword_write_host_block_data
    !byte 0                         ; data to write, patched at runtime
}
}

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
do_oswrch_xy
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
;
; SFTODO: Once the upstream dynamic memory access rework is completed and merged, it *may* be possible to do things like (on a turbo copro) use bank 0 free memory plus the whole of bank 1 for dynamic memory, and have banks 2 and 3 for virtual memory cache, or to promote the whole of a game <= ~210K to be pure dynamic memory. Or if we have more main+SWR than the game size, to promote the whole game to dynamic memory and do a simple absolute Z-machine to bank address conversion. I suspect it won't allow >64K dynamic memory, but just making this note so I can consider it when I see how upstream change works.

; SFTODO: If the bigdyn model where Z-machine PC bank is paged in by default works out, it may be a worthwhile saving for global variable accesses to avoid paging in the dynmem bank if we know the global variables live below $8000. ("know" might ultimately mean an initialisation-time check and dynamic code patching, but before that it might mean a cheap check in the code doing the global var access to avoid the paging. although the paging is maybe - do the cycle counting - cheap enough that by the time we do even a cheap check, we're not going to come out ahead.)

}
