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
; SFTODO: TESCTSCREEN is not currently supported for Acorn, probably should be

;TESTSCREEN = 1

!zone screenkernal {

s_init
    ; init cursor
!IFNDEF ACORN {
    lda #$ff
    sta s_current_screenpos_row ; force recalculation first time
}
    lda #0
    sta zp_screencolumn
    sta zp_screenrow
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
!IFNDEF ACORN {
	jmp .update_screenpos
} ELSE {
    rts
}

!IFNDEF ACORN {
s_set_text_colour
	sta s_colour
	rts
}

s_delete_cursor
!IFNDEF ACORN {
	lda #$20 ; blank space
	ldy zp_screencolumn
	sta (zp_screenline),y
	rts
} ELSE {
    jsr s_cursor_to_screenrowcolumn
    lda #$20 ; blank space
    jmp oswrch ; SFTODO: ignore issue of scrolling at bottom right for now...
}

!IFDEF ACORN {
s_cursor_to_screenrowcolumn
    lda #vdu_goto_xy
    jsr oswrch
    lda zp_screencolumn
    jsr oswrch
    lda zp_screenrow
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
	cmp #$80
	bcc .normal_char
	cmp #$a0
	bcs .normal_char
+	
	cmp #$0d
    bne +
	; newline
!IFNDEF ACORN {
	; but first, check if the current character is the cursor so that we may delete it
	lda cursor_character
	ldy zp_screencolumn
	cmp (zp_screenline),y
	bne +++
	jsr s_delete_cursor
}
+++	jmp .perform_newline
+   
    ; SFTODO: Not too sure what to do here; let's see. 20 is PETSCII character
    ; for DEL. I can't see any obvious *code* which calls this with DEL, but
    ; a) maybe the running game can output a code (perhaps 127) which would
    ; normally get transformed into 20 (but won't on Acorn port as we don't
    ; translate to PETSCII) b) maybe we can get here with a "DEL" character when
    ; handling command line input. Need to come back to this, for now I'll guess
    ; changing 20 to 127 is right.
!IFNDEF ACORN {
    cmp #20
} ELSE {
    cmp #127
}
    bne +
    ; delete
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
++  
!IFNDEF ACORN {
    jsr .update_screenpos
    lda #$20
    ldy zp_screencolumn
    sta (zp_screenline),y
    lda s_colour
    sta (zp_colourline),y
} ELSE {
    jsr s_delete_cursor
}
    jmp .printchar_end
+
    ; SFTODO: I don't believe any of the following codes or their Acorn
    ; equivalents will come through this routine on the Acorn port.
!IFNDEF ACORN {
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
!IFNDEF ACORN {
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
} ELSE {
    ; SFTODO: I suspect "often" the OS text cursor will already be in the right
    ; place, but let's play it safe for now.
    ; SFTODO: We should probably take s_reverse into account here
    pha
    jsr s_cursor_to_screenrowcolumn
    pla
    jsr oswrch ; SFTODO THIS IS THE "MAIN" CHAR PRINT OSWRCH
    ldy zp_screencolumn
}
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
!IFNDEF ACORN {
	jsr .update_screenpos
}
	jmp .printchar_end
+	jsr .s_scroll
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
+	lda #0
    sta zp_screencolumn
    inc zp_screenrow
    jsr .s_scroll
!IFNDEF ACORN {
    jsr .update_screenpos
}
    jmp .printchar_end

!IFNDEF ACORN {
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

!IFNDEF ACORN {
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
!IFNDEF ACORN {
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
} ELSE {
    ; SFTODO: I think if window_start_row+1 is 0 we are scrolling the whole
    ; screen and by not defining a text window we will get a much faster
    ; hardware scroll. But let's get it working before we try to optimise it...
    ; Define a text window covering the region to scroll
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
    ; Move the cursor to the bottom line of the text window
    lda #vdu_goto_xy
    jsr oswrch
    lda #0
    jsr oswrch
    lda screen_height_minus_1
    sec
    sbc window_start_row + 1
    jsr oswrch
    ; Move the cursor down one line to force a scroll
    lda #vdu_down
    jsr oswrch
    ; Remove the text window
    ; SFTODO: Do we need to put the OS text cursor back somewhere? For now we will
    ; almost certainly get away with not doing this, but it may be optimal to
    ; do it to allow us to make assumptions about its positioning elsewhere in
    ; the code.
    lda #vdu_reset_text_window
    jmp oswrch
s_erase_line
	; registers: a,x,y
	lda #0
	sta zp_screencolumn
s_erase_line_from_cursor
    ; SFTODO: Maybe turn cursor off? Or maybe we'll have it off by default and
    ; only turn it on when we expect user input?
    ; SFTODO: Temporarily ignoring the question of screen scrolling if we
    ; output at bottom right of screen. It may be best - and perhaps fastest -
    ; to do this erase by defining a text window and clearing it, then we will
    ; avoid the auto-scrolling issue.
    ; SFTODO: This may not need to move the OS text cursor at the end, not sure
    jsr s_cursor_to_screenrowcolumn
    lda #' '
    ldy zp_screencolumn
-   jsr oswrch
    iny
    cpy screen_width
    bcc -
+   jmp s_cursor_to_screenrowcolumn
}


!IFNDEF ACORN {
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
; background colours and f1 did the same for the foreground.
!IFNDEF ACORN {
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
!IFNDEF ACORN {
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
} ELSE {
    ; SFTODO: Is this OK? Probably, but be good to test...
    rts
}
}

!ifdef TESTSCREEN {

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

