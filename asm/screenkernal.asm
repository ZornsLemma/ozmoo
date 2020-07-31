; replacement for these C64 kernal routines and their variables:
; printchar $ffd2
; plot      $fff0
; zp_cursorswitch $cc
; zp_screenline $d1-$d2
; zp_screencolumn $d3
; zp_screenrow $d6
; zp_colourline $f3-$f4
;
; needed to be able to customize the text scrolling to
; not include status lines, especially big ones used in
; Border Zone, and Nord and Bert.
;
; usage: first call s_init, then replace
; $ffd2 with s_printchar and so on.
; s_scrollstart is set to the number of top lines to keep when scrolling
;
; Uncomment TESTSCREEN and call testscreen for a demo.

;TESTSCREEN = 1

; SF: s_printchar should be used for virtually all text output. This way it can
; update zp_screen{column,row} to match what it has sent to OSWRCH and they will
; agree with the OS text cursor position. If zp_screen{column,row} are modified
; without moving the OS text cursor or vice versa, setting
; s_cursors_inconsistent to a non-0 value will cause the OS cursor to be moved
; the next time it matters (when we try to print something via OSWRCH, or when
; the OS cursor becomes visible to the user).

!zone screenkernal {

; SF: I think s_init could be part of the discardable init code; the only
; problem is that it logically belongs in this file. Maybe turn it into a macro?!
s_init
    ; init cursor
    lda #$ff
!ifndef ACORN {
    sta s_current_screenpos_row ; force recalculation first time
} else {
    sta s_cursors_inconsistent

    ; SFTODO: We should query these from the OS, but since there's lots of hardcoded
    ; 40x25 assumptions at the moment we just go with that for consistency.
    ldx #40
    stx screen_width
    dex
    stx screen_width_minus_1
    ldx #25
    stx screen_height
    dex
    stx screen_height_minus_1
}
    lda #0
    sta zp_screencolumn
    sta zp_screenrow
!ifdef ACORN {
    sta s_os_reverse
}
	; Set to 0: s_ignore_next_linebreak, s_reverse
    ldx #3
-	sta s_ignore_next_linebreak,x
	dex
	bpl -
    rts

s_plot
    ; y=column (0-39)
    ; x=row (0-24)
    bcc .set_cursor_pos
    ; get_cursor
    ldx zp_screenrow
    ldy zp_screencolumn
    rts
.set_cursor_pos
+	cpx #25 ; SFTODO: Implicit screen height assumption?
	bcc +
	ldx #24 ; SFTODO: Implicit screen height assumption?
+	stx zp_screenrow
	sty zp_screencolumn
!ifndef ACORN {
	jmp .update_screenpos
} else {
    lda #1
    sta s_cursors_inconsistent
.return
    rts
}

!ifndef ACORN {
s_set_text_colour
	sta s_colour
	rts
}

!ifndef ACORN {
s_delete_cursor
	lda #$20 ; blank space
	ldy zp_screencolumn
	sta (zp_screenline),y
	rts
}

!ifdef ACORN {
s_cursor_to_screenrowcolumn
    lda s_cursors_inconsistent
!ifndef DEBUG_CURSOR {
    beq .return
} else {
    beq .check_cursor
}
    lda #0
    sta s_cursors_inconsistent
    lda #vdu_goto_xy
    jsr oswrch
    lda zp_screencolumn
    jsr oswrch
    lda zp_screenrow
    jmp oswrch
!ifdef DEBUG_CURSOR {
.check_cursor
    txa
    pha
    tya
    pha
    lda #osbyte_read_cursor_position
    jsr osbyte
    cpx zp_screencolumn
    beq +
-   jmp -
+   cpy zp_screenrow
    beq +
-   jmp -
+   pla
    tay
    pla
    tax
    rts
}

s_screenrowcolumn_from_cursor
    lda #osbyte_read_vdu_variable
    sta s_cursors_inconsistent
    ldx #vdu_variable_text_window_top
    jsr osbyte
    stx zp_screenrow
    lda #osbyte_read_cursor_position
    jsr osbyte
    stx zp_screencolumn
    tya
    clc
    adc zp_screenrow
    sta zp_screenrow
.rts
    rts

s_reverse_to_os_reverse
    lda s_reverse
    bne set_os_reverse_video
    ; fall through to set_os_normal_video

; SFTODO: This assumes non-mode 7. We can probably support reverse video at least
; for the status line in mode 7, although we'd lose a few characters to control
; codes. (Lurkio on stardot suggests just using cyan-on-black for the status
; bar in mode 7; that would only burn one space on a control code instead of
; three and seems a good idea.)
set_os_normal_video
    lda s_os_reverse
    beq .rts
    lda #0
    sta s_os_reverse
    lda #7
    jsr .set_tcol
    lda #128
    jmp .set_tcol

set_os_reverse_video
    lda s_os_reverse
    bne .rts
    lda #$80
    sta s_os_reverse
    lda #0
    jsr .set_tcol
    lda #135
    ; jmp .set_tcol - just fall through

.set_tcol
    pha
    lda #vdu_set_text_colour
    jsr oswrch
    pla
    jmp oswrch
}

s_printchar
    ; replacement for CHROUT ($ffd2)
    ; input: A = byte to write (PETASCII) [SF: ASCII not PETASCII on Acorn]
    ; output: -
    ; used registers: -
    stx s_stored_x
    sty s_stored_y

	; Fastlane for the most common characters
	cmp #$20
	bcc +
!ifndef ACORN {
	cmp #$80
} else {
    cmp #$7f
}
	bcc .normal_char
	cmp #$a0
	bcs .normal_char
+	
	cmp #$0d
    bne +
	; newline
!ifndef ACORN {
	; but first, check if the current character is the cursor so that we may delete it
	lda cursor_character
	ldy zp_screencolumn
	cmp (zp_screenline),y
	bne +++
	jsr s_delete_cursor
}
+++	jmp .perform_newline
+   
    ; SF: Note that .readkey will call s_printchar with the native delete
    ; character.
!ifndef ACORN {
    cmp #20
} else {
    cmp #127
}
    bne +
    ; delete
!ifndef ACORN {
    jsr s_delete_cursor
    dec zp_screencolumn ; move back
    bpl ++
	inc zp_screencolumn ; Go back to 0 if < 0
	lda zp_screenrow
	ldy current_window
	cmp window_start_row + 1,y
	bcc ++
	dec zp_screenrow
	lda #39 ; SFTODO: Implicit screen width assumption?
	sta zp_screencolumn
++  jsr .update_screenpos
    lda #$20
    ldy zp_screencolumn
    sta (zp_screenline),y
    lda s_colour
    sta (zp_colourline),y
} else {
    jsr s_cursor_to_screenrowcolumn
    jsr s_reverse_to_os_reverse
    lda #127
    jsr oswrch
    dec zp_screencolumn ; move back
    bpl ++
	inc zp_screencolumn ; Go back to 0 if < 0
	lda zp_screenrow
	ldy current_window
	cmp window_start_row + 1,y
	bcc ++
	dec zp_screenrow
	lda #39 ; SFTODO: Implicit screen width assumption?
	sta zp_screencolumn
++
}
    jmp .printchar_end
+
    ; SF: Reverse video isn't handled by sending control codes through
    ; s_printchar on the Acorn. (It could be, but it seems simplest just to
    ; update s_reverse directly. This is a bit gratuitously incompatible with
    ; the Commodore though.)
!ifndef ACORN {
    cmp #$93 
    bne +
    ; clr (clear screen)
    lda #0
    sta zp_screencolumn
    sta zp_screenrow
    jsr s_erase_window
    jmp .printchar_end
+   cmp #$12 ; 18
    bne +
    ; reverse on
    ldx #$80
    stx s_reverse
    jmp .printchar_end
+   cmp #$92 ; 146
    bne .printchar_end
    ; reverse off
    ldx #0
    stx s_reverse
    beq .printchar_end ; Always jump
}
; +
	; ; check if colour code
	; ldx #15
; -	cmp colours,x
	; beq ++
	; dex
	; bpl -
	; bmi .printchar_end ; Always branch
; ++	; colour <x> found
	; stx s_colour
	; beq .printchar_end ; Always jump
	
.normal_char
	ldx zp_screencolumn
	bpl +
	; Negative column. Increase column but don't print anything.
	inc zp_screencolumn
	jmp .printchar_end
+	; Skip if column > 39
	cpx #40 ; SFTODO: Implicit screen width assumption
	bcs .printchar_end
	; Reset ignore next linebreak setting
	ldx current_window
	ldy s_ignore_next_linebreak,x
	bpl +
	inc s_ignore_next_linebreak,x
	; Check if statusline is overflowing TODO: Do we really need to check any more?
+	pha
	lda zp_screenrow
	ldy current_window 
	cmp window_start_row,y
	pla ; Doesn't affect C
	bcs .outside_current_window
.resume_printing_normal_char	
!ifndef ACORN {
   ; convert from pet ascii to screen code
	cmp #$40
	bcc ++    ; no change if numbers or special chars
	cmp #$60
	bcs +
	and #%00111111
	bcc ++ ; always jump
+   cmp #$80
    bcs +
	and #%11011111
    bcc ++ ; always jump
+	cmp #$c0
	bcs +
	eor #%11000000
+	and #%01111111
++  ; print the char
    clc
    adc s_reverse
    pha
    jsr .update_screenpos
    pla
    ldy zp_screencolumn
    sta (zp_screenline),y
    lda s_colour
    sta (zp_colourline),y
    iny
    sty zp_screencolumn
	ldx current_window
	bne .printchar_end ; For upper window and statusline (in z3), don't advance to next line.
    cpy #40 ; SFTODO: Implicit screen width assumption?
    bcc .printchar_end
	dec s_ignore_next_linebreak,x ; Goes from 0 to $ff
    lda #0
    sta zp_screencolumn
    inc zp_screenrow
	lda zp_screenrow
	cmp #25 ; SFTODO: Implicit screen height assumption?
	bcs +
	jsr .update_screenpos
	jmp .printchar_end
+	jsr .s_scroll
} else {
    ; OSWRCH may cause the screen to scroll if we're at the bottom right
    ; character. We therefore don't use .s_scroll to do the scroll, we just
    ; define a text window to tell the OS what to scroll.

    pha
    jsr s_cursor_to_screenrowcolumn
    jsr s_reverse_to_os_reverse
    ldy zp_screencolumn
    iny
    sty zp_screencolumn
	ldx current_window
	bne .printchar_nowrap ; For upper window and statusline (in z3), don't advance to next line.
    cpy #40 ; SFTODO: Implicit screen width assumption?
    bcc .printchar_nowrap
	dec s_ignore_next_linebreak,x ; Goes from 0 to $ff
    lda #0
    sta zp_screencolumn
    inc zp_screenrow
	lda zp_screenrow
	cmp #25 ; SFTODO: Implicit screen height assumption?
	bcc .printchar_nowrap
    ; SF: ENHANCEMENT: I don't know if the VM allows it (do we have enough
    ; information to reliably do the redraw without the game's cooperation?),
    ; and it *might* look ugly, but we might gain a performance boost
    ; (particularly in high resolution modes) by doing a hardware scroll of the
    ; entire screen then redrawing the status line/window 1 afterwards.
    ; C is already set
    jsr s_pre_scroll
    pla
    jsr oswrch
    lda #vdu_reset_text_window
    sta s_cursors_inconsistent ; vdu_reset_text_window moves cursor to home
    lda s_reverse
    beq .not_reverse
    ; Reverse video is on and the character we just output has caused the text
    ; window to scroll, so the OS will have added a blank line in reverse video.
    ; The Z-machine spec requires the blank line to be in normal video, so we
    ; need to fix this up.
    jsr s_erase_line_from_cursor
.not_reverse
    pha
.printchar_nowrap
    pla
    ; This OSWRCH call is the one which actually prints most of the text on the
    ; screen.
    jsr oswrch
    ; Force the OS text cursor to move if necessary; normally it would be
    ; invisible at this point, but TerpEtude's "Timed full-line input" test can
    ; leave the OS text cursor visible at the top left if you type a character
    ; in the rightmost column and then wait for the timed mwessage to appear.
    jsr s_cursor_to_screenrowcolumn
}
.printchar_end
    ldx s_stored_x
    ldy s_stored_y
    rts

.outside_current_window
!ifdef Z4 {
	jmp .printchar_end
} else {
	cpy #1
	bne .printchar_end
	; This is window 1. Expand it if possible.
	ldy zp_screenrow
	cpy window_start_row ; Compare to end of screen (window 0)
	bcs .printchar_end
	iny
	sty window_start_row + 1
	; Move lower window cursor if it gets hidden by upper window
	cpy cursor_row
	bcc .resume_printing_normal_char
	sty cursor_row
	bcs .resume_printing_normal_char ; Always branch
}

.perform_newline
    ; newline/enter/return
	; Check ignore next linebreak setting
	ldx current_window
	ldy s_ignore_next_linebreak,x
	bpl +
	inc s_ignore_next_linebreak,x
	jmp .printchar_end
+   lda #0
    sta zp_screencolumn
    inc zp_screenrow
    jsr .s_scroll
!ifndef ACORN {
    jsr .update_screenpos
} else {
    lda #1
    sta s_cursors_inconsistent
}
    jmp .printchar_end

!ifndef ACORN {
s_erase_window
    lda #0
    sta zp_screenrow
-   jsr s_erase_line
    inc zp_screenrow
    lda zp_screenrow
    cmp #25
    bne -
    lda #0
    sta zp_screenrow
    sta zp_screencolumn
    rts
}

!ifndef ACORN {
.update_screenpos
    ; set screenpos (current line) using row
    ldx zp_screenrow
    cpx s_current_screenpos_row
    beq +
    ; need to recalculate zp_screenline
    stx s_current_screenpos_row
    ; use the fact that zp_screenrow * 40 = zp_screenrow * (32+8)
    lda #0
    sta zp_screenline + 1
	txa
    asl; *2 no need to rol zp_screenline + 1 since 0 < zp_screenrow < 24
    asl; *4
    asl; *8
    sta zp_colourline ; store *8 for later
    asl; *16
    rol zp_screenline + 1
    asl; *32
    rol zp_screenline + 1  ; *32
    clc
    adc zp_colourline ; add *8
    sta zp_screenline
    sta zp_colourline
    lda zp_screenline + 1
    adc #$04 ; add screen start ($0400)
    sta zp_screenline +1
    adc #$d4 ; add colour start ($d800)
    sta zp_colourline + 1
+   rts
}

.s_scroll
    lda zp_screenrow
    cmp #25 ; SFTODO: Implicit screen height assumption?
    bpl +
    rts
+
!ifndef ACORN {
    ldx window_start_row + 1 ; how many top lines to protect
    stx zp_screenrow
-   jsr .update_screenpos
    lda zp_screenline
    pha
    lda zp_screenline + 1
    pha
    inc zp_screenrow
    jsr .update_screenpos
    pla
    sta zp_colourline + 1
    pla
    sta zp_colourline
    ; move characters
    ldy #39
--  lda (zp_screenline),y ; zp_screenrow
    sta (zp_colourline),y ; zp_screenrow - 1
    dey
    bpl --
    ; move colour info
    lda zp_screenline + 1
    pha
    clc
    adc #$d4
    sta zp_screenline + 1
    lda zp_colourline + 1
    clc
    adc #$d4
    sta zp_colourline + 1
    ldy #39
--  lda (zp_screenline),y ; zp_screenrow
    sta (zp_colourline),y ; zp_screenrow - 1
    dey
    bpl --
    pla
    sta zp_screenline + 1
    lda zp_screenrow
    cmp #24
    bne -
    lda #$ff
    sta s_current_screenpos_row ; force recalculation
s_erase_line
	; registers: a,x,y
	lda #0
	sta zp_screencolumn
	jsr .update_screenpos
	ldy #0
.erase_line_from_any_col	
	lda #$20
-	cpy #40
	bcs .done_erasing
	sta (zp_screenline),y
	iny
	bne -
.done_erasing	
 	rts
s_erase_line_from_cursor
	jsr .update_screenpos
	ldy zp_screencolumn
	jmp .erase_line_from_any_col
} else {
    sec
    jsr s_pre_scroll
    ; Move the cursor down one line to force a scroll
    jsr set_os_normal_video ; new line must not be reverse video
    lda #vdu_down
    jsr oswrch
    ; Remove the text window
    lda #vdu_reset_text_window
    sta s_cursors_inconsistent ; vdu_reset_text_window moves cursor to home
    jmp oswrch

s_erase_line
	; registers: a,x,y
	lda #0
	sta zp_screencolumn
s_erase_line_from_cursor
    ; Define a text window covering the region to clear
    ; SFTODO: It may be possible to factor out the sequence of OSWRCH calls for
    ; the text window definition.
    lda #vdu_define_text_window
    jsr oswrch
    lda zp_screencolumn
    jsr oswrch
    lda zp_screenrow
    pha
    jsr oswrch
    lda screen_width_minus_1
    jsr oswrch
    pla
    jsr oswrch
    ; Clear it and reset the text window.
    jsr set_os_normal_video ; clear must not be to reverse video
    lda #vdu_cls
    jsr oswrch
    lda #vdu_reset_text_window
    sta s_cursors_inconsistent ; vdu_reset_text_window moves cursor to home
    jmp oswrch

    ; s_pre_scroll preserves X and Y if C is set on entry.
s_pre_scroll
    ; Define a text window covering the region to scroll.
    ; If C is set on entry, leave the OS text cursor at the bottom right of the
    ; text window.
    ; If C is clear on entry, leave the OS text cursor at zp_screen{row,column}.
    ; SF: ENHANCEMENT: If window_start_row+1 is 0 we are scrolling the whole
    ; screen, so defining the text window has no visible effect and will slow
    ; things down by preventing the OS doing a hardware scroll. It wouldn't be
    ; hard to avoid defining the text window in this case, but in reality I
    ; suspect there's nearly always a status bar or similar on the screen and
    ; this case won't occur. If a game where this would be useful turns up I
    ; can consider it.
    bcs .s_pre_scroll_leave_bottom_right
    lda #osbyte_read_cursor_position
    jsr osbyte
    tya
    pha
    txa
    pha
    jsr .s_pre_scroll_leave_bottom_right
    lda #vdu_goto_xy
    jsr oswrch
    pla
    jsr oswrch
    pla
    sec
    sbc window_start_row + 1
    jmp oswrch
.s_pre_scroll_leave_bottom_right
    lda #vdu_define_text_window
    jsr oswrch
    lda #0
    sta zp_screencolumn ; leave the ozmoo cursor at the start of the line
    jsr oswrch
    lda screen_height_minus_1
    sta zp_screenrow ; leave the ozmoo cursor on the last line
    jsr oswrch
    lda screen_width_minus_1
    jsr oswrch
    lda window_start_row + 1 ; how many top lines to protect
    jsr oswrch
    ; Move the cursor to the bottom right of the text window
    lda #vdu_goto_xy
    jsr oswrch
    lda screen_width_minus_1
    jsr oswrch
    lda screen_height_minus_1
    sec
    sbc window_start_row + 1
    jmp oswrch
}


!ifndef ACORN {
; colours		!byte 144,5,28,159,156,30,31,158,129,149,150,151,152,153,154,155
zcolours	!byte $ff,$ff ; current/default colour
			!byte COL2,COL3,COL4,COL5  ; black, red, green, yellow
			!byte COL6,COL7,COL8,COL9  ; blue, magenta, cyan, white
darkmode	!byte 0
bgcol		!byte BGCOL, BGCOLDM
fgcol		!byte FGCOL, FGCOLDM
bordercol	!byte BORDERCOL_FINAL, BORDERCOLDM_FINAL
!ifdef Z3 {
statuslinecol !byte STATCOL, STATCOLDM
}
cursorcol !byte CURSORCOL, CURSORCOLDM
current_cursor_colour !byte CURSORCOL
cursor_character !byte CURSORCHAR
}

; SFTODO: Eventually it might be nice if (e.g.) f0 cycled through the available
; background colours and f1 did the same for the foreground. (Perhaps SHIFT+f0/1
; instead to leave the unshifted ones available for *KEY and/or games which try
; to use function keys.)
!ifndef ACORN {
toggle_darkmode
!ifdef Z5PLUS {
	; We will need the old fg colour later, to check which characters have the default colour
	ldx darkmode ; previous darkmode value (0 or 1)
	ldy fgcol,x
	lda zcolours,y
	sta z_temp + 9 ; old fg colour
}
; Toggle darkmode
	lda darkmode
	eor #1
	sta darkmode
	tax
; Set cursor colour
	ldy cursorcol,x
	lda zcolours,y
	sta current_cursor_colour
; Set bgcolor
	ldy bgcol,x
	lda zcolours,y
	sta reg_backgroundcolour
!ifdef Z5PLUS {
	; We will need the new bg colour later, to check which characters would become invisible if left unchanged
	sta z_temp + 8 ; new background colour
}
; Set border colour 
	ldy bordercol,x
!ifdef BORDER_MAY_FOLLOW_BG {
	beq .store_bordercol
}
!ifdef BORDER_MAY_FOLLOW_FG {
	cpy #1
	bne +
	ldy fgcol,x
+	
}
	lda zcolours,y
.store_bordercol
	sta reg_bordercolour
!ifdef Z3 {
; Set statusline colour
	ldy statuslinecol,x
	lda zcolours,y
	ldy #39
-	sta $d800,y
	dey
	bpl -
}
; Set fgcolour
	ldy fgcol,x
	lda zcolours,y
	jsr s_set_text_colour
	ldx #4
	ldy #$d8
	sty z_temp + 11
	ldy #0
	sty z_temp + 10
!ifdef Z3 {
	ldy #40
}
!ifdef Z5PLUS {
	sta z_temp + 7
}
.compare
!ifdef Z5PLUS {
	lda (z_temp + 10),y
	and #$0f
	cmp z_temp + 9
	beq .change
	cmp z_temp + 8
	bne .dont_change
.change	
	lda z_temp + 7
}
	sta (z_temp + 10),y
.dont_change
	iny
	bne .compare
	inc z_temp + 11
	dex
	bne .compare
	jsr update_cursor
	rts 
}


!ifdef Z5PLUS {
z_ins_set_colour
!ifndef ACORN {
    ; set_colour foreground background [window]
    ; (window is not used in Ozmoo)
	jsr printchar_flush

; Load y with bordercol (needed later)
	ldx darkmode
	ldy bordercol,x

; Set background colour
    ldx z_operand_value_low_arr + 1
	beq .current_background
    lda zcolours,x
    bpl +
    ldx story_start + header_default_bg_colour ; default colour
    lda zcolours,x
+   sta reg_backgroundcolour
; Also set bordercolour to same as background colour, if bordercolour is set to the magic value 0
	cpy #0
	bne .current_background
	sta reg_bordercolour
.current_background

; Set foreground colour
    ldx z_operand_value_low_arr
	beq .current_foreground
    lda zcolours,x
    bpl + ; Branch unless it's the special value $ff, which means "default colour"
    ldx story_start + header_default_fg_colour ; default colour
    lda zcolours,x
+
; Also set bordercolour to same as foreground colour, if bordercolour is set to the magic value 1
	cpy #1
	bne +
	sta reg_bordercolour
+
    jsr s_set_text_colour ; change foreground colour
.current_foreground
    rts
} else {
    jmp printchar_flush
}
}

!ifdef TESTSCREEN {
!ifdef ACORN {
    !error "TESTSCREEN not supported on Acorn"
}

.testtext !pet 5,147,18,"Status Line 123         ",146,13
          !pet 28,"tesx",20,"t aA@! ",18,"Test aA@!",146,13
          !pet 155,"third",20,13
          !pet "fourth line",13
          !pet 13,13,13,13,13,13
          !pet 13,13,13,13,13,13,13
          !pet 13,13,13,13,13,13,13
          !pet "last line",1
          !pet "aaaaaaaaabbbbbbbbbbbcccccccccc",1
          !pet "d",1 ; last char on screen
          !pet "efg",1 ; should scroll here and put efg on new line
          !pet 13,"h",1; should scroll again and f is on new line
          !pet 0

testscreen
    lda #23 ; 23 upper/lower, 21 = upper/special (22/20 also ok)
    sta $d018 ; reg_screen_char_mode
    jsr s_init
    lda #1
    sta s_scrollstart
    ldx #0
-   lda .testtext,x
    bne +
    rts
+   cmp #1
    bne +
    txa
    pha
--  jsr kernal_getchar
    beq --
    pla
    tax
    bne ++
+   jsr s_printchar
++  inx
    bne -
}
}

