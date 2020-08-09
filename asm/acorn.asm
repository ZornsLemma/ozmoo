; SFTODO: Headers for ozmoo style

!zone {

; Initialization performed ASAP on startup. This should end with an rts or
; equivalent.
!macro acorn_deletable_init_start {
    ldx #1
    jsr do_osbyte_rw_escape_key

!ifdef ACORN_NO_SHADOW {
    +set_up_mode_7_3c00
}

	jsr init_screen_colours

    ; Now init_screen_colours has been called, it's safe to call s_printchar, so
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

    ; We keep the hardware cursor off most of the time; this way the user can't
    ; see it flitting round the screen doing various updates. (The C64 doesn't
    ; have this issue, as it uses direct screen writes and in fact its cursor
    ; is a software creation.) We position it appropriately and turn it on only
    ; when we're expecting user input. (As far as I can see the Z-machine has
    ; no way for the running program to turn the cursor on and off.)
    jsr init_cursor_control

    jsr init_readtime

    ; Now Ozmoo's screen output code is (about to be) initialised via
    ; init_screen_colours, errors can be reported using s_printchar.
    jmp set_default_error_handler
} ; End of acorn_deletable_init_start



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
!macro set_up_mode_7_3c00 {
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

!macro adjust_cursor {
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
    +adjust_cursor
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
    +adjust_cursor
.not_cursor_editing
    pla
    plp
    rts
call_old_keyv
    jmp $ffff ; patched during initialization

} ; End of !ifdef ACORN_NO_SHADOW

}
