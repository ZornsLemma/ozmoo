; Acorn-specific code factored out into its own file for readability.

; Note that the code macros defined in here have the suffix "_inline" if control
; flows straight through them or "_subroutine" if they end by executing rts (or
; equivalent).

!zone {

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialization performed ASAP on startup. This should end with an rts or
; equivalent.
!macro acorn_deletable_init_start_subroutine {
    ldx #1
    jsr do_osbyte_rw_escape_key

!ifdef ACORN_NO_SHADOW {
    +set_up_mode_7_3c00_inline
}

    +screenkernal_init_inline

    ; Now screenkernal_init_inline has been executed, it's safe to call s_printchar, so
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
    ; SFTODO: ACORN_CURSOR_PASS_THROUGH is completely untested; I need to find
    ; a game which uses cursor keys.
    ; SFTODO: Arguably we should always use ACORN_CURSOR_PASS_THROUGH mode *but*
    ; re-enable cursor editing temporarily when we're reading a line of text
    ; instead of a single character, then you'd always have cursor editing for
    ; commands but games could still read cursor keys individually.
    ; SFTODONOW: I THINK THIS MIGHT REDUCE THE MINOR UGLINESS OF SUCCUMBING TO
    ; CURSOR KEY TEMPTATION ON THHTG Z5 HINT SCREEN
    lda #osbyte_set_cursor_editing
    ldx #1
    jsr do_osbyte_y_0
}

    +init_readtime_inline
    jmp init_cursor_control
} ; End of acorn_deletable_init_start

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
    ; Ozmoo's own printing mechanisms are properly initialised.
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
; SFTODONOW: Any problems with SWR here? On a BRK, will the OS page the current
; language back in or will it leave whatever bank we had paged in (and set at $f4)
; ourselves paged in? Remember we might be inside readblock doing a retry here
; and if the retry succeeds the vmem code which called readblock will expect to
; see the same bank paged in (and readblock to have written to that bank, not
; some random one).
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
    ; We need to save the contents of the stack, because they may be corrupted
    ; when an error message is generated. (They probably won't be, but we can't
    ; rely on it.) As a nice side effect of this, the return address for our
    ; caller is saved so .setjmp_error_handler can simply rts after restoring
    ; the stack.
    ; SFTODO: If jmp_buf is made smaller, we could probably fairly easily
    ; detect overflow - initialise y with -buffer_size, do sta jmp_buf+1+buffer_size,y
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
+   lda #1 ; Z flag is clear
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

; This macro is like an initialisation subroutine, but by using a macro we
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
    ldx #4
.screen_copy_loop
    ldy #0
.screen_copy_loop2
.screen_copy_lda_abs_y
    lda $7c00,y
.screen_copy_sta_abs_y
    sta $3c00,y
    iny
    bne .screen_copy_loop2
    inc .screen_copy_lda_abs_y + 2
    inc .screen_copy_sta_abs_y + 2
    dex
    bne .screen_copy_loop

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
   beq .set_default_error_handler_rts
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
    rts

; Two wrappers for calling osbyte_rw_escape_key to reduce code size; we do this
; in several places.
do_osbyte_rw_escape_key
    lda #osbyte_rw_escape_key
do_osbyte_y_0
    ldy #0
    jmp osbyte

}
