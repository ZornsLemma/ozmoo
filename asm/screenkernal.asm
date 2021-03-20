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

; The Commodore 64 and one Acorn build have a fixed screen size while other
; Acorn builds have a variable screen size. We don't want to penalise the
; former so we use these macros which assemble using either immediate values
; or memory accesses as appropriate.
; SFTODO: This probably needs tweaking/simplifying for 5.3 port; I shouldn't be
; gratuitously different from Commodore port now it has support for non-40x25
; screens. Probably best to avoid making any changes in this area until I look
; at supporting extra modes on non-shadow machines if they happen to have enough
; spare RAM (e.g. mode 6 on a model B, maybe even mode 3 on a B/Electron with an
; E00 DFS and a small game)

!ifndef ACORN {
    ; SFTODO: THIS IS NOT TRUE ANY MORE, OF COURSE
    FIXED_SCREEN_SIZE = 1
} else {
    !ifdef ACORN_SCREEN_HOLE { ; SFTODO: MAY OR MAY NOT WANT TO KEEP THIS - IT IS SEMI-TRUE IF WE ASSUME WE'LL ALWAYS BE IN MODE 6 OR 7
        FIXED_SCREEN_SIZE = 1
    }
}

!ifdef FIXED_SCREEN_SIZE {
!ifdef Z3 {
max_lines = 24
} else {
max_lines = 25
}
} else {
!ifdef Z3 {
max_lines = s_screen_height_minus_one
} else {
max_lines = s_screen_height
}
}

!macro cmp_screen_height {
    !ifdef FIXED_SCREEN_SIZE {
        cmp #25
    } else {
        cmp s_screen_height
    }
}
!macro cmp_screen_width {
    !ifdef FIXED_SCREEN_SIZE {
        cmp #40
    } else {
        cmp s_screen_width
    }
}
!macro cpx_screen_height {
    !ifdef FIXED_SCREEN_SIZE {
        cpx #25
    } else {
        cpx s_screen_height
    }
}
!macro cpx_screen_width {
    !ifdef FIXED_SCREEN_SIZE {
        cpx #40
    } else {
        cpx s_screen_width
    }
}
!macro cpx_max_lines {
    !ifdef FIXED_SCREEN_SIZE {
        cpx #max_lines
    } else {
        cpx max_lines
    }
}
!macro cpy_screen_width {
    !ifdef FIXED_SCREEN_SIZE {
        cpy #40
    } else {
        cpy s_screen_width
    }
}
!macro cpy_screen_width_plus_one {
    !ifdef FIXED_SCREEN_SIZE {
        cpy #41
    } else {
        cpy s_screen_width_plus_one
    }
}
!macro lda_screen_height_minus_one {
    !ifdef FIXED_SCREEN_SIZE {
        lda #24
    } else {
        lda s_screen_height_minus_one
    }
}
!macro lda_screen_height {
    !ifdef FIXED_SCREEN_SIZE {
        lda #25
    } else {
        lda s_screen_height
    }
}
!macro lda_screen_width_minus_one {
    !ifdef FIXED_SCREEN_SIZE {
        lda #39
    } else {
        lda s_screen_width_minus_one
    }
}
!macro ldx_screen_width_minus_one {
    !ifdef FIXED_SCREEN_SIZE {
        ldx #39
    } else {
        ldx s_screen_width_minus_one
    }
}
!macro lda_screen_width {
    !ifdef FIXED_SCREEN_SIZE {
        lda #40
    } else {
        lda s_screen_width
    }
}
!macro ldx_max_lines {
    !ifdef FIXED_SCREEN_SIZE {
        ldx #max_lines
    } else {
        ldx max_lines
    }
}
!macro ldx_screen_height_minus_one {
    !ifdef FIXED_SCREEN_SIZE {
        ldx #24
    } else {
        ldx s_screen_height_minus_one
    }
}
!macro ldx_screen_width {
    !ifdef FIXED_SCREEN_SIZE {
        ldx #40
    } else {
        ldx s_screen_width
    }
}
!macro ldy_screen_height_minus_one {
    !ifdef FIXED_SCREEN_SIZE {
        ldy #24
    } else {
        ldy s_screen_height_minus_one
    }
}
!macro ldy_screen_height {
    !ifdef FIXED_SCREEN_SIZE {
        ldy #25
    } else {
        ldy s_screen_height
    }
}
!macro ldy_screen_width {
    !ifdef FIXED_SCREEN_SIZE {
        ldy #40
    } else {
        ldy s_screen_width
    }
}

!ifdef TARGET_C128 {
SETBORDERMACRO_DEFINED = 1
!macro SetBorderColour {
	jsr C128SetBorderColour
}
!macro SetBackgroundColour {
	jsr C128SetBackgroundColour
}
}


!ifdef TARGET_PLUS4 {
SETBORDERMACRO_DEFINED = 1
!macro SetBorderColour {
	stx s_stored_x
	pha
	tax
	lda plus4_vic_colours,x
	sta reg_bordercolour
	pla
	ldx s_stored_x
}
!macro SetBackgroundColour {
	stx s_stored_x
	pha
	tax
	lda plus4_vic_colours,x
	sta reg_backgroundcolour
	pla
	ldx s_stored_x
}
}

!ifndef SETBORDERMACRO_DEFINED {
!macro SetBorderColour {
	sta reg_bordercolour
}
!macro SetBackgroundColour {
	sta reg_backgroundcolour
}
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
!ifdef USE_INPUTCOL {
inputcol	!byte INPUTCOL, INPUTCOLDM
}
!ifdef Z3 {
statuslinecol !byte STATCOL, STATCOLDM
}
cursorcol !byte CURSORCOL, CURSORCOLDM
current_cursor_colour !byte CURSORCOL
cursor_character !byte CURSORCHAR
}

!ifdef TARGET_PLUS4 {
plus4_vic_colours
	;     PLUS4  VIC-II
	!byte $00    ; black
	!byte $71  ; white
	!byte $32    ; red
	!byte $63   ; cyan
	!byte $54    ; purple
	!byte $55   ; green
	!byte $36    ; blue
	!byte $77  ; yellow
	!byte $28   ; orange
	!byte $18   ; brown 
	!byte $72   ; light red
	!byte $11   ; dark grey
	!byte $41   ; grey
	!byte $75   ; light green
	!byte $76   ; light blue
	!byte $61   ; light grey
}


!ifdef TARGET_C128 {
!source "vdc.asm"

.stored_a !byte 0
.stored_x_or_y !byte 0

; Mapping between VIC-II and VDC colours
; VDC:
; 00 = dark black
; 01 = light black (dark gray)
; 02 = dark blue
; 03 = light blue
; 04 = dark green
; 05 = light green
; 06 = dark cyan
; 07 = light cyan
; 08 = dark red
; 09 = light red
; 10 = dark purple
; 11 = light purple
; 12 = dark yellow (brown/orange)
; 13 = light yellow
; 14 = dark white (light gray)
; 15 = light white
vdc_vic_colours
	;     VDC    VIC-II
	!byte 0    ; black
	!byte 15   ; white
	!byte 8    ; red
	!byte 7    ; cyan
	!byte 10   ; purple
	!byte 4    ; green
	!byte 3    ; blue
	!byte 13   ; yellow
	!byte 12   ; orange
	!byte 12   ; brown 
	!byte 9    ; light red
	!byte 1    ; dark grey
	!byte 14   ; grey
	!byte 5    ; light green
	!byte 3    ; light blue
	!byte 14   ; light grey

C128SetBackgroundColour
	stx .stored_x_or_y
	ldx COLS_40_80
	beq +
	; 80 columns mode selected
	sta .stored_a
	tax
	lda vdc_vic_colours,x
	ldx #VDC_COLORS
	jsr VDCWriteReg
	lda .stored_a
	jmp ++
+	sta reg_backgroundcolour
++	ldx .stored_x_or_y
	rts

C128SetBorderColour
	stx .stored_x_or_y
	ldx COLS_40_80
	bne + ; no border in VDC, only use background
	; 40 column mode
	sta reg_bordercolour
+	ldx .stored_x_or_y
	rts

VDCGetChar
	; 80 columns, use VDC screen
	sty .stored_x_or_y
	lda zp_screenline + 1
	sec
	sbc #$04 ; adjust from $0400 (VIC-II) to $0000 (VDC)
	tay
	lda zp_screenline
	clc
	adc .stored_x_or_y
	bcc +
	iny
+	jsr VDCSetAddress
	ldy .stored_x_or_y
	ldx #VDC_DATA
	jmp VDCReadReg

VDCPrintChar
	; 80 columns, use VDC screen
	sty .stored_x_or_y
	sta .stored_a
	lda zp_screenline + 1
	sec
	sbc #$04 ; adjust from $0400 (VIC-II) to $0000 (VDC)
	tay
	lda zp_screenline
	clc
	adc .stored_x_or_y
	bcc +
	iny
+	jsr VDCSetAddress
	lda .stored_a
	ldy .stored_x_or_y
	ldx #VDC_DATA
	jmp VDCWriteReg

VDCPrintColour
	; 80 columns, use VDC screen
	sty .stored_x_or_y
	; adjust color from VIC-II to VDC format
	tax
	lda vdc_vic_colours,x
	ora #$80 ; lower-case
	sta .stored_a
	lda zp_colourline + 1
	sec
	sbc #$d0 ; adjust from $d800 (VIC-II) to $0800 (VDC)
	tay
	lda zp_colourline
	clc
	adc .stored_x_or_y
	bcc +
	iny
+	jsr VDCSetAddress
	lda .stored_a
	ldy .stored_x_or_y
	ldx #VDC_DATA
	jmp VDCWriteReg
}

!ifdef TARGET_MEGA65 {
mega65io
	; enable C65GS/VIC-IV IO registers
	;
	; (they will only be active until the first access
	; so mega65io needs to be called before any extended I/O)
	lda #$47
	sta $d02f
	lda #$53
	sta $d02f
	rts

init_mega65
	; MEGA65 IO enable
	jsr mega65io
	; set 40MHz CPU
	lda #65
	sta 0
	; set 80-column mode
	lda #$c0
	sta $d031
	lda #$c9
	sta $D016
	; set screen at $0800
	;lda #$26
	;sta $d018
	; disable VIC-II/VIC-III hot registers
	lda $d05d
	and #$7f
	sta $d05d
	rts
	
colour2k
	; start mapping 2nd KB of colour RAM to $DC00-$DFFF
	sei
	pha
	jsr mega65io
	lda #$01
	sta $d030
	pla
	rts

colour1k
	; stop mapping 2nd KB of colour RAM to $DC00-$DFFF
	pha
	jsr mega65io
	lda #$00
	sta $d030
	pla
	cli
	rts
}

; SFTODO: I should move these into page 4 to save a few bytes in main RAM.
s_screen_width !byte 0
s_screen_height !byte 0
s_screen_width_plus_one !byte 0
s_screen_width_minus_one !byte 0
s_screen_height_minus_one !byte 0
s_screen_size !byte 0, 0

!ifndef ACORN {
s_init
	; set up screen_width and screen_width_minus_one
!ifdef TARGET_C128 {
	lda #40
	ldx COLS_40_80
	beq +
	; 80 columns mode selected
	lda #80
+
} else {
	lda #SCREEN_WIDTH
}
	sta s_screen_width
	sta s_screen_width_plus_one
	sta s_screen_width_minus_one
	inc s_screen_width_plus_one
	dec s_screen_width_minus_one

	; set up screen_height and screen_width_minus_one
	lda #SCREEN_HEIGHT
	sta s_screen_height
	sta s_screen_height_minus_one
	dec s_screen_height_minus_one

	; calculate total screen size
	lda s_screen_height
	sta multiplier
	lda s_screen_width
	sta multiplicand
	lda #0
	sta multiplier + 1
	sta multiplicand + 1
	jsr mult16
	lda product
	sta s_screen_size;
	lda product + 1
	sta s_screen_size + 1;

	; init cursor
	lda #$ff
	sta s_current_screenpos_row ; force recalculation first time
	lda #0
	sta zp_screencolumn
	sta zp_screenrow
	; Set to 0: s_ignore_next_linebreak, s_reverse
	ldx #3
-	sta s_ignore_next_linebreak,x
	dex
	bpl -
	rts
} else {
!macro screenkernal_init_inline {
; SFTODO: PROB ALL ACORN BUILDS WANT THIS NOW !ifndef ACORN_NO_SHADOW {
    ; We don't want to be querying the OS for the screen resolution all the
    ; time, so initialise the relevant variables here. (The Commodore versions
    ; do the same; note that story_start + header_screen_{width,height}* aren't
    ; always valid, so we can't use those values.)
    lda #osbyte_read_vdu_variable
    ldx #vdu_variable_text_window_bottom
    jsr osbyte
    stx s_screen_height_minus_one
    inx
    stx s_screen_height
    sty s_screen_width_minus_one
    iny
    sty s_screen_width
    iny
    sty s_screen_width_plus_one
; SFTODO: SEE ABOVE }

    ; Pick up the current OS cursor position; this will improve readability if
    ; any errors occuring during initialization, and it doesn't affect the game
    ; because deletable_screen_init_2 calls set_cursor anyway.
    lda #osbyte_read_cursor_position
    sta s_cursors_inconsistent
    jsr osbyte
    stx zp_screencolumn
    sty zp_screenrow
	; Set to 0: s_ignore_next_linebreak, s_reverse, s_os_reverse
    lda #0
    ldx #4
-	sta s_ignore_next_linebreak,x
	dex
	bpl -

    ; If we didn't change mode after a restart, we may have been left with the
    ; OS colours set to reverse video. Force the OS settings to normal video so
    ; they agree with s_os_reverse.
    jsr force_set_os_normal_video
}
}

s_plot
	; y=column (0-(SCREEN_WIDTH-1))
	; x=row (0- (SCREEN_HEIGHT-1))
	bcc .set_cursor_pos
	; get_cursor
	ldx zp_screenrow
	ldy zp_screencolumn
	rts
.set_cursor_pos
+	cpx s_screen_height
	bcc +
	ldx s_screen_height_minus_one
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
!ifdef TARGET_C128 {
	ldx COLS_40_80
	beq +
	jmp VDCPrintChar
+
}
	ldy zp_screencolumn
	sta (zp_screenline),y
	rts
}

!ifdef ACORN {
!ifdef MODE_7_STATUS {
!ifdef Z4PLUS {
; Return with Z clear iff the cursor is on a mode 7 status line
; Preserves A and X.
.check_if_mode_7_status
    ldy zp_screenrow
    bne .check_if_mode_7_status2_rts
.check_if_mode_7_status2
    ldy window_start_row + 1 ; how many top lines to protect
    dey
    bne .check_if_mode_7_status2_rts
    ldy screen_mode
    cpy #7
.check_if_mode_7_status2_rts
    rts

check_and_add_mode_7_colour_code
    jsr .check_if_mode_7_status2
    bne .check_if_mode_7_status2_rts
    ; fall through to .output_mode_7_colour_code
}

.output_mode_7_colour_code
    lda #vdu_home
    sta s_cursors_inconsistent
    jsr oswrch
    clc
    lda fg_colour
    adc #mode_7_text_colour_base
    jmp oswrch
}

s_cursor_to_screenrowcolumn
    lda s_cursors_inconsistent
!ifndef DEBUG_CURSOR {
    beq .return
} else {
    beq .check_cursor
}
    lda #0
    sta s_cursors_inconsistent
    ldx zp_screencolumn
!ifdef MODE_7_STATUS {
!ifdef Z4PLUS {
    jsr .check_if_mode_7_status
    bne +
    inx
+
}
}
    ldy zp_screenrow
    jmp do_oswrch_vdu_goto_xy
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

set_os_normal_video
    lda s_os_reverse
    beq .rts
force_set_os_normal_video
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

!ifdef MODE_7_PROMPT {
; This must preserve X and Y.
handle_mode_7_colour_prompt_delete
	lda screen_mode
	cmp #7
	bne .rts
	; On second and subsequent lines there will be an invisible colour control
	; code in column 0. We want to leave that there when the user deletes the
	; only *visible* character on the line, so subsequent input is coloured, but
	; if the user deletes when the only character on the line is that control
	; code we want to do an extra delete to delete the visible character at the
	; end of the previous line and move the cursor back up.
	lda zp_screencolumn
	bne .rts
    lda #del
	bne s_printchar ; always branch - print the delete char again to delete the colour code
handle_mode_7_colour_prompt_new_line
	lda screen_mode
	cmp #7
	bne .rts
	lda zp_screencolumn
	bne .rts
	lda #mode_7_text_colour_base
	clc
	adc prompt_colour
	; fall through to s_printchar_unfiltered
; SFTODONOW: Experimental - this might also be useful for mode 7 status line
s_printchar_unfiltered
	stx s_stored_x
	sty s_stored_y
    jmp .normal_char
}
s_printchar
	; replacement for CHROUT ($ffd2)
	; input: A = byte to write (PETASCII)
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
	bcc ++ ; .normal_char
	cmp #$a0
	bcc + ; bcs .normal_char
++	jmp .normal_char
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
	ldy zp_screencolumn
	jsr s_delete_cursor
	dec zp_screencolumn ; move back
	bpl ++
	inc zp_screencolumn ; Go back to 0 if < 0
	lda zp_screenrow
	ldy current_window
	cmp window_start_row + 1,y
	bcc ++
	dec zp_screenrow
	lda s_screen_width_minus_one ; #SCREEN_WIDTH-1
	sta zp_screencolumn
++  jsr .update_screenpos
	lda #$20
	ldy zp_screencolumn
!ifdef TARGET_C128 {
	ldx COLS_40_80
	bne .col80_1
	; 40 columns, use VIC-II screen
	sta (zp_screenline),y
	lda s_colour
	sta (zp_colourline),y
	jmp .col80_1_end
.col80_1
	jsr VDCPrintChar
.col80_1_end
} else {
	sta (zp_screenline),y
	!ifdef TARGET_MEGA65 {
		jsr colour2k
	}
	lda s_colour
	sta (zp_colourline),y
	!ifdef TARGET_MEGA65 {
		jsr colour1k
	}
}
} else {
    jsr s_cursor_to_screenrowcolumn
    jsr s_reverse_to_os_reverse
    lda #del
    jsr oswrch
    dec zp_screencolumn ; move back
    bpl ++
	inc zp_screencolumn ; Go back to 0 if < 0
	lda zp_screenrow
	ldy current_window
	cmp window_start_row + 1,y
	bcc ++
	dec zp_screenrow
	+lda_screen_width_minus_one
	sta zp_screencolumn
++
}
!ifdef ACORN { ; SFTODO: CAN'T THIS BE MERGED INTO PRECEDING BLOCK, WHICH IS ALSO CONDITIONAL ON "ACORN"? BUT LET'S NOT FIDDLE WHILE I'M MERGING
    ; We don't have any reverse video handling here on Acorn as noted below, so
    ; we move the + label forward so anything which hasn't matched yet is
    ; ignored.
+
}
	jmp .printchar_end
    ; SF: Reverse video isn't handled by sending control codes through
    ; s_printchar on the Acorn. (It could be, but it seems simplest just to
    ; update s_reverse directly. This is a bit gratuitously incompatible with
    ; the Commodore though.)
!ifndef ACORN {
+   cmp #$93 
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
	beq +
	jmp .printchar_end
	; reverse off
+   ldx #0
	stx s_reverse
	bne .normal_char
	jmp .printchar_end ; Always jump
}

.normal_char
	; TODO: perhaps we can remove all testing here and just
	; continue at .resume_printing_normal_char	?
	ldx zp_screencolumn
	bpl +
	; Negative column. Increase column but don't print anything.
	inc zp_screencolumn
-	jmp .printchar_end
+	; Skip if column > SCREEN_WIDTH - 1
!ifdef MODE_7_STATUS {
!ifdef Z4PLUS {
    jsr .check_if_mode_7_status
    bne +
    cpx #39
    bcs - ; .printchar_end
+
}
}
	cpx s_screen_width ; #SCREEN_WIDTH
	bcs - ; .printchar_end
!ifdef ACORN_HW_SCROLL {
    ldy zp_screenrow
    bne +
    sta .top_line_buffer,x
    lda s_reverse
    sta .top_line_buffer_reverse,x
    lda .top_line_buffer,x
+
}
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
!ifdef TARGET_C128 {
	ldx COLS_40_80
	bne .col80_2
	; 40 columns, use VIC-II screen
	sta (zp_screenline),y
	lda s_colour
	sta (zp_colourline),y
	jmp .col80_2_end
.col80_2
	jsr VDCPrintChar
	lda s_colour
	jsr VDCPrintColour
.col80_2_end
} else {
	sta (zp_screenline),y
	!ifdef TARGET_MEGA65 {
		jsr colour2k
	}
	lda s_colour
!ifdef TARGET_PLUS4 {
	tax
	lda plus4_vic_colours,x
}
	sta (zp_colourline),y
	!ifdef TARGET_MEGA65 {
		jsr colour1k
	}
}
	iny
	sty zp_screencolumn
	ldx current_window
	bne .printchar_end ; For upper window and statusline (in z3), don't advance to next line.
	cpy s_screen_width ; #SCREEN_WIDTH
	bcc .printchar_end
	dec s_ignore_next_linebreak,x ; Goes from 0 to $ff
	lda #0
	sta zp_screencolumn
	inc zp_screenrow
	lda zp_screenrow
	cmp s_screen_height
	bcs +
	jsr .update_screenpos
	jmp .printchar_end
+
!ifdef TARGET_C128 {
	ldx COLS_40_80
	bne .col80_3
	; 40 columns, use VIC-II screen
	jsr .s_scroll
	jmp .col80_3_end
.col80_3
	jsr .s_scroll_vdc
.col80_3_end
} else {
	jsr .s_scroll
}
} else { ; ACORN
    ; OSWRCH may cause the screen to scroll if we're at the bottom right
    ; character. We therefore don't use .s_scroll to do the scroll, we just
    ; define a text window to tell the OS what to scroll. SFTODO: That comment
    ; is out of date and has been for ages, *if* we're using hardware scrolling
    ; we do things differently.

    pha
    jsr s_cursor_to_screenrowcolumn
    jsr s_reverse_to_os_reverse
    ldy zp_screencolumn
    iny
    sty zp_screencolumn
	ldx current_window
	bne .printchar_nowrap ; For upper window and statusline (in z3), don't advance to next line.
    +cpy_screen_width
    bcc .printchar_nowrap
	dec s_ignore_next_linebreak,x ; Goes from 0 to $ff
    lda #0
    sta zp_screencolumn
    inc zp_screenrow
	lda zp_screenrow
	+cmp_screen_height
	bcc .printchar_nowrap
!ifdef ACORN_HW_SCROLL {
    lda use_hw_scroll
    beq .no_hw_scroll0
    +lda_screen_height_minus_one
    sta zp_screenrow ; s_pre_scroll normally does this but we may not call it
    ldx window_start_row + 1 ; how many top lines to protect
    dex
    beq .no_pre_scroll
.no_hw_scroll0
}
    ; C is already set
    jsr s_pre_scroll
.no_pre_scroll
    pla
    jsr oswrch
    lda s_reverse
    beq .not_reverse
    ; Reverse video is on and the character we just output has caused the text
    ; window to scroll, so the OS will have added a blank line in reverse video.
    ; The Z-machine spec requires the blank line to be in normal video, so we
    ; need to fix this up.
    jsr s_erase_line_from_cursor
.not_reverse
!ifdef ACORN_HW_SCROLL {
    ldx window_start_row + 1 ; how many top lines to protect
    dex
    bne .no_hw_scroll1
    lda use_hw_scroll
    beq .no_hw_scroll1
    jsr .redraw_top_line
    jmp .printchar_oswrch_done
.no_hw_scroll1
}
    lda #vdu_reset_text_window
    sta s_cursors_inconsistent ; vdu_reset_text_window moves cursor to home
    pha
.printchar_nowrap
    pla
    ; This OSWRCH call is the one which actually prints most of the text on the
    ; screen.
    jsr oswrch
.printchar_oswrch_done
    ; Force the OS text cursor to move if necessary; normally it would be
    ; invisible at this point, but TerpEtude's "Timed full-line input" test can
    ; leave the OS text cursor visible at the top left if you type a character
    ; in the rightmost column and then wait for the timed message to appear.
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
	bcc +
	sty cursor_row
+	jmp .resume_printing_normal_char ; Always branch
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
!ifdef TARGET_C128 {
	ldx COLS_40_80
	bne .col80_4
	; 40 columns, use VIC-II screen
	jsr .s_scroll
	jmp .col80_4_end
.col80_4
	jsr .s_scroll_vdc
.col80_4_end
} else {
	jsr .s_scroll
}
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
	cmp s_screen_height
	bne -
	lda #0
	sta zp_screenrow
	sta zp_screencolumn
	rts
}

!ifndef ACORN { ; SFTODO: merge with prev ndef ACORN block?
.update_screenpos
	; set screenpos (current line) using row
	ldx zp_screenrow
	cpx s_current_screenpos_row
	beq .same_row
	; need to recalculate zp_screenline
	stx s_current_screenpos_row
!ifdef TARGET_MEGA65 {
	; calculate zp_screenline = zp_current_screenpos_row * 40
	; Use MEGA65's hardware multiplier
	jsr mega65io
	stx $d770
	lda #0
	sta $d771
	sta $d772
	sta $d773
	sta $d775
	sta $d776
	sta $d777
	lda s_screen_width ; #SCREEN_WIDTH
	sta $d774
	;
	; add screen offsets
	;
	lda $d778
	sta zp_screenline
	sta zp_colourline
	lda $d779
	and #$07
	clc
	adc #>SCREEN_ADDRESS ; add screen start ($0400 for C64)
	sta zp_screenline+1
	clc
	adc #>COLOUR_ADDRESS_DIFF ; add colour start ($d800 for C64)
	sta zp_colourline+1
} else {
	; calculate zp_screenline = zp_current_screenpos_row * s_screen_width
	stx product + 1
	txa
	asl ; 2x
	asl ; 4x
	adc product + 1 ; 5x
	asl ; 10x
	asl
	ldx #0
	stx product + 1
	rol product + 1 ; 20x
	asl
	rol product + 1 ; 40x
!ifdef TARGET_C128 {
	ldx COLS_40_80
	beq ++
	asl
	rol product + 1
++
}
	sta zp_screenline
	sta zp_colourline
	lda product + 1
	;
	; add screen offsets
	;
	adc #>SCREEN_ADDRESS ; add screen start ($0400 for C64)
	sta zp_screenline +1
	adc #>COLOUR_ADDRESS_DIFF ; add colour start ($d800 for C64)
	sta zp_colourline + 1
}
.same_row
	rts
}

!ifdef TARGET_C128 {
.s_scroll_vdc
	; scroll routine for 80 column C128 mode, using the blitter
	lda zp_screenrow
	cmp s_screen_height
	bpl +
	rts
+   ; set up copy mode
	ldx #VDC_VSCROLL
	jsr VDCReadReg
	ora #$80 ; set copy bit
	jsr VDCWriteReg
	; scroll characters
	lda #$00
	jsr .s_scroll_vdc_copy
	; scroll colours
	lda #$08
	jsr .s_scroll_vdc_copy
	; prepare for erase line
	sty zp_screenrow
	lda #$ff
	sta s_current_screenpos_row ; force recalculation
	jmp s_erase_line

.s_scroll_vdc_copy
	; input: a = offset (0 for characters, $08 for colours)
	;
	; calculate start position (start_row * screen_width)
	pha
	lda window_start_row + 1 ; how many top lines to protect
	sta multiplier
	lda s_screen_width
	sta multiplicand
	lda #0
	sta multiplier + 1
	sta multiplicand + 1
	jsr mult16
	; set up source and destination
	pla
	clc
	adc product + 1
	tay
	lda product
	jsr VDCSetAddress ; where to copy to (first line)
	clc
	adc s_screen_width
	bcc +
	iny
+	jsr VDCSetCopySourceAddress ; where to copy from (next line)
	; start copying
	ldy window_start_row + 1 ; how many top lines to protect
-	lda #80 ;copy 80 bytes
	ldx #VDC_COUNT
	jsr VDCWriteReg
	iny
	cpy s_screen_height_minus_one
	bne -
	rts
}

.s_scroll
	lda zp_screenrow
	cmp s_screen_height
	bpl +
	rts
+
!ifndef ACORN {
!ifdef TARGET_MEGA65 {
	jsr colour2k	
}
	ldx window_start_row + 1 ; how many top lines to protect
	stx zp_screenrow
	inc zp_screenrow
	jsr .update_screenpos
	lda zp_screenline
	sta .scroll_load_screen + 1
	lda zp_screenline + 1
	sta .scroll_load_screen + 2
	lda zp_colourline
	sta .scroll_load_colour + 1
	lda zp_colourline + 1
	sta .scroll_load_colour + 2
	dec zp_screenrow
	jsr .update_screenpos
	lda s_screen_height_minus_one
	sec
	sbc zp_screenrow
	tax
-
	ldy s_screen_width_minus_one
.scroll_load_screen
	lda $8000,y ; This address is modified above
	sta (zp_screenline),y
.scroll_load_colour
	lda $8000,y ; This address is modified above
	sta (zp_colourline),y
	dey
	bpl .scroll_load_screen
	dex
	beq .done_scrolling
	lda zp_screenline
	clc
	adc s_screen_width
	sta zp_screenline
	bcc +
	inc zp_screenline + 1
+		
	lda zp_colourline
	clc
	adc s_screen_width
	sta zp_colourline
	bcc +
	inc zp_colourline + 1
+	
	lda .scroll_load_screen + 1
	clc
	adc s_screen_width
	sta .scroll_load_screen + 1
	bcc +
	inc .scroll_load_screen + 2
+		
	lda .scroll_load_colour + 1
	clc
	adc s_screen_width
	sta .scroll_load_colour + 1
	bcc +
	inc .scroll_load_colour + 2
+	jmp -

.done_scrolling
!ifdef TARGET_MEGA65 {
	jsr colour1k
}
	lda s_screen_height_minus_one
	sta zp_screenrow
	lda #$ff
	sta s_current_screenpos_row ; force recalculation
s_erase_line
	; registers: a,x,y
	lda #0
	sta zp_screencolumn
	jsr .update_screenpos
	ldy #0
.erase_line_from_any_col	
!ifdef TARGET_C128 {
	ldx COLS_40_80
	bne .col80_5
	; 40 columns, use VIC-II screen
-	cpy s_screen_width
	bcs .done_erasing
	lda #$20
	sta (zp_screenline),y
	lda s_colour
	txa
	lda vdc_vic_colours,x
	sta (zp_colourline),y
	iny
	bne - ; Always branch
	jmp .done_erasing	
.col80_5
	; erase line in VDC
	tya
	pha
-	cpy s_screen_width
	bcs +
	lda #$20
	jsr VDCPrintChar
	iny
	bne -
	; also reset attributes/colours
+	pla
	tay
-	cpy s_screen_width
	bcs .done_erasing
	lda s_colour
	jsr VDCPrintColour
	iny
	bne -
} else {
-	cpy s_screen_width
	bcs .done_erasing
	; set character
	lda #$20
	sta (zp_screenline),y
    ; set colour
    !ifdef TARGET_MEGA65 {
        jsr colour2k
    }
!ifdef TARGET_PLUS4 {
	ldx s_colour
	lda plus4_vic_colours,x
} else {
	lda s_colour
}
	sta (zp_colourline),y
    !ifdef TARGET_MEGA65 {
        jsr colour1k
    }
	iny
	bne -
}
.done_erasing	
 	rts
s_erase_line_from_cursor
	jsr .update_screenpos
	ldy zp_screencolumn
	jmp .erase_line_from_any_col
} else { ; ACORN
!ifdef ACORN_HW_SCROLL {
    ldx window_start_row + 1 ; how many top lines to protect
    dex
    bne .no_hw_scroll2
    lda use_hw_scroll
    beq .no_hw_scroll2
    ldx #0
    +ldy_screen_height_minus_one
    sty zp_screenrow
    jsr do_oswrch_vdu_goto_xy
    jsr set_os_normal_video ; new line must not be reverse video
    lda #vdu_down
    jsr oswrch
    jmp .redraw_top_line
.no_hw_scroll2
}
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
!ifdef ACORN_HW_SCROLL {
    ldx zp_screenrow
    bne +
    ldx zp_screencolumn
-   lda #' '
    sta .top_line_buffer,x
    lda #0
    sta .top_line_buffer_reverse,x
    inx
    +cpx_screen_width
    bne -
+
}
    ; Define a text window covering the region to clear
    lda #vdu_define_text_window
    jsr oswrch
    lda zp_screencolumn
    jsr oswrch
    lda zp_screenrow
    pha
    jsr oswrch
    +lda_screen_width_minus_one
    jsr oswrch
    pla
    jsr oswrch
    ; Clear it and reset the text window.
    jsr set_os_normal_video ; clear must not be to reverse video
    lda #vdu_cls
    jsr oswrch
!ifdef MODE_7_STATUS {
!ifdef Z4PLUS {
    lda zp_screenrow
    bne +
    jsr check_and_add_mode_7_colour_code
+
}
}
    lda #vdu_reset_text_window
    sta s_cursors_inconsistent ; vdu_reset_text_window moves cursor to home
    jmp oswrch

s_pre_scroll
    ; Define a text window covering the region to scroll.
    ; If C is set on entry, leave the OS text cursor at the bottom right of the
    ; text window.
    ; If C is clear on entry, leave the OS text cursor where it was on the
    ; physical screen (its co-ordinates will be different because of the text
    ; window).
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
    pla
    tax
    pla
    sec
    sbc window_start_row + 1
    tay
    jmp do_oswrch_vdu_goto_xy
.s_pre_scroll_leave_bottom_right
    lda #vdu_define_text_window
    jsr oswrch
    lda #0
    ; We don't need to update zp_screencolumn, our caller will have done it.
    ; sta zp_screencolumn ; leave the ozmoo cursor at the start of the line
    jsr oswrch
    +lda_screen_height_minus_one
    sta zp_screenrow ; leave the ozmoo cursor on the last line
    jsr oswrch
    +lda_screen_width_minus_one
    jsr oswrch
    lda window_start_row + 1 ; how many top lines to protect
    jsr oswrch
    ; Move the cursor to the bottom right of the text window
    +ldx_screen_width_minus_one
    +lda_screen_height_minus_one
    sec
    sbc window_start_row + 1
    tay
    jmp do_oswrch_vdu_goto_xy

!ifdef ACORN_HW_SCROLL {
.redraw_top_line
    jsr turn_off_cursor
    lda #vdu_home
    jsr oswrch
    +ldy_screen_width
!ifdef MODE_7_STATUS {
!ifdef Z4PLUS {
    lda screen_mode
    cmp #7
    bne +
    jsr .output_mode_7_colour_code
    ldy #39
+
}
}
    ldx #0
-   lda .top_line_buffer_reverse,x
    beq +
    jsr set_os_reverse_video
    jmp ++
+   jsr set_os_normal_video
++  lda .top_line_buffer,x
    jsr oswrch
    inx
    dey
    bne -
    stx s_cursors_inconsistent
    jmp turn_on_cursor
}
}


!ifndef ACORN { ; SFTODO: CAN THIS BE MERGED WITH FIRST BRANCH OF PRECEDING BLOCK?
!ifndef NODARKMODE {
toggle_darkmode

; z_temp + 6: New foreground colour, as C64 colour 
; z_temp + 7: New foreground colour, tranformed for target platform
; z_temp + 8: New background colour, adapted to target platform
; z_temp + 9: Old foreground colour, adapted to target platform
; z_temp + 10, 11: Pointer into colour RAM

!ifdef Z5PLUS {
	; We will need the old fg colour later, to check which characters have the default colour
	ldx darkmode ; previous darkmode value (0 or 1)
	ldy fgcol,x
	lda zcolours,y
!ifdef TARGET_C128 {
	ldy COLS_40_80
	beq +
	; 80 columns mode selected
	tay
	lda vdc_vic_colours,y
+
}
!ifdef TARGET_PLUS4 {
	tay
	lda plus4_vic_colours,y
}
	sta z_temp + 9 ; old fg colour
} else { ; This is z3 or z4
!ifdef USE_INPUTCOL {

	; We will need the old input colour later, to check which characters are input text
	ldx darkmode ; previous darkmode value (0 or 1)
	ldy inputcol,x
	lda zcolours,y
!ifdef TARGET_C128 {
	ldy COLS_40_80
	beq +
	; 80 columns mode selected
	tay
	lda vdc_vic_colours,y
+
}
!ifdef TARGET_PLUS4 {
	tay
	lda plus4_vic_colours,y
}
	sta z_temp + 9 ; old input colour

	; If the mode we switch *from* has inputcol = fgcol, make sure inputcol is never matched
	lda inputcol,x
	cmp fgcol,x
	bne +
	inc z_temp + 9
+
} ; USE_INPUTCOL

} ; else (not Z5PLUS)



; Toggle darkmode
	lda darkmode
	eor #1
	sta darkmode
	tax
	
!ifdef USE_INPUTCOL {
	ldy inputcol,x
	lda zcolours,y
!ifdef TARGET_C128 {
	ldy COLS_40_80
	beq +
	; 80 columns mode selected
	tay
	lda vdc_vic_colours,y
+
}
!ifdef TARGET_PLUS4 {
	tay
	lda plus4_vic_colours,y
}
	sta z_temp + 8 ; new input colour

} ; USE_INPUTCOL	
	
; Set fgcolour
	ldy fgcol,x
	lda zcolours,y
	sta z_temp + 6 ; New foreground colour, as C64 colour 
	jsr s_set_text_colour
!ifdef TARGET_C128 {
	ldy COLS_40_80
	beq +
	; 80 columns mode selected
	tay
	lda vdc_vic_colours,y
+
}
!ifdef TARGET_PLUS4 {
	tay
	lda plus4_vic_colours,y
}
	sta z_temp + 7 ; New foreground colour, tranformed for target platform
!ifdef TARGET_MEGA65 {
	jsr colour2k
}
; Set cursor colour
	ldy cursorcol,x
	lda z_temp + 6
	cpy #1
	beq +
	lda zcolours,y
+	sta current_cursor_colour
; Set bgcolour
	ldy bgcol,x
	lda zcolours,y
	+SetBackgroundColour
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
	+SetBorderColour

	; update colour memory with new colours
!ifdef Z3 {

; For Z3: Set statusline colour
	ldy statuslinecol,x
	lda zcolours,y
!ifdef TARGET_C128 {
	ldy COLS_40_80
	beq +
	; 80 columns mode selected
	tay
	lda vdc_vic_colours,y
+
}
!ifdef TARGET_PLUS4 {
	tay
	lda plus4_vic_colours,y
}
	ldy s_screen_width_minus_one
-
!ifdef TARGET_C128 {
	ldx COLS_40_80
	bne +
	sta COLOUR_ADDRESS,y
	jmp ++
+
	; 80 columns mode selected
	sty s_stored_y
	pha
	ldy #$08
	lda s_stored_y
	jsr VDCSetAddress
	pla
	ora #$80 ; lower-case
	ldx #VDC_DATA
	jsr VDCWriteReg
	ldy s_stored_y
++
} else {
	sta COLOUR_ADDRESS,y
}
	dey
	bpl -
}
	;; Work out how many pages of colour RAM to examine
	ldx s_screen_size + 1
	inx
	ldy #>COLOUR_ADDRESS
	sty z_temp + 11
	ldy #0
	sty z_temp + 10
!ifdef Z3 {
	ldy s_screen_width ; Since we have coloured the statusline separately, skip it now
}
!ifndef Z5PLUS {
;	ldy #0 ; But y is already 0, so we skip this
	lda z_temp + 7 ; For Z3 and Z4 we can just load this value before the loop  
}
.compare
!ifdef TARGET_C128 {
	lda z_temp + 7  ; too much work to read old colour from VDC
	bit COLS_40_80
	bmi .toggle_80
} ; else {
!ifdef Z5PLUS {
	lda (z_temp + 10),y
!ifndef TARGET_PLUS4 {
	and #$0f
}
	cmp z_temp + 9
	beq .change
	cmp z_temp + 8
	bne .dont_change
.change	
	lda z_temp + 7
}

!ifdef USE_INPUTCOL {
	lda (z_temp + 10),y
!ifndef TARGET_PLUS4 {
	and #$0f
}
	cmp z_temp + 9
	bne .change
	lda z_temp + 8
	bne + ; Always branch
.change	
	lda z_temp + 7
+
}

.toggle_80

; }
!ifdef TARGET_C128 {
	pha
	lda COLS_40_80
	bne +
	pla
	sta (z_temp + 10),y
	jmp ++
+
	; 80 columns mode selected
	stx s_stored_x
	sty s_stored_y
	lda z_temp + 11
	sec
	sbc #$d0 ; adjust from $d800 (VIC-II) to $0800 (VDC)
	tay
	lda z_temp + 10
	clc
	adc s_stored_y
	bcc +
	iny
+	jsr VDCSetAddress
	pla
	ora #$80 ; lower-case
	ldy s_stored_y
	ldx #VDC_DATA
	jsr VDCWriteReg
	ldx s_stored_x
++
} else {
	sta (z_temp + 10),y
}
.dont_change
	iny
	bne .compare
	inc z_temp + 11
	dex
	bne .compare
!ifdef TARGET_MEGA65 {
	jsr colour1k
}

!ifdef USE_INPUTCOL {
	; Switch to the new input colour, if input colour is active (we could be at a MORE prompt or in a timed input)
	lda input_colour_active
	beq +
	jsr activate_inputcol
	ldx darkmode
+
}
	jmp update_cursor
} ; ifndef NODARKMODE
} else { ; ACORN
update_colours
!ifdef MODE_7_STATUS {
!ifdef Z4PLUS {
    jsr turn_off_cursor
    jsr check_and_add_mode_7_colour_code
    jsr turn_on_cursor
}
!ifdef Z3 {
    lda screen_mode
    cmp #7
    bne +
    jsr turn_off_cursor
    jsr .output_mode_7_colour_code
    jsr turn_on_cursor
+
}
}
!ifdef MODE_7_PROMPT {
    ; SFTODO: Not just here, there may be general code-saving opportunities by tidying up
    ; the mode 7 prompt code (e.g. the repeated 128+colour code calculation) and combining
    ; it with MODE_7_STATUS where applicable. But I want to see how well the mode 7 prompt
    ; works before integrating it too deeply.
    lda screen_mode
    cmp #7
    bne +
    sta s_cursors_inconsistent
    jsr turn_off_cursor
    ; SFTODO: We "should" update previous lines of this same prompt, but let's keep the
    ; code size and complexity down for now.
    ; SFTODO: Is it safe to use zp_temp here?
    lda #$ff
    sta zp_temp
-   inc zp_temp
    ldx zp_temp
    cpx #40
    beq .prompt_change_done
    ldy zp_screenrow
    jsr do_oswrch_vdu_goto_xy
    lda #osbyte_read_screen_mode ; SFTODO ALSO RETURN CHAR AT CURSOR
    jsr osbyte
    txa
    bpl -
    lda #mode_7_text_colour_base
    clc
    adc prompt_colour
    jsr oswrch
.prompt_change_done
    jsr turn_on_cursor
.not_colour_code
+
}
    ldx #0
    ldy bg_colour
    jsr .redefine_colour
    ldx #7
    ldy fg_colour
    ; fall through to .redefine_colour
.redefine_colour
    lda #vdu_redefine_colour
    jsr oswrch
    txa
    jsr oswrch
    tya
    jsr oswrch
    lda #0
    jsr oswrch
    jsr oswrch
    jmp oswrch

    ; SFTODO: It might be worth (for games like Border Zone where - check I'm not confused - hardware scrolling is not an option) allowing the build system to avoid showing CTRL-S on the loader screen and avoid having code for it in the Ozmoo binary. This might already be possible, I haven't checked. I suspect the user would have to specify a command line option for this, we can't really examine the game ourselves and infer it "always" uses a >1 line status area. - I guess this would come down to providing a --no-hw-scroll option in the build system
    ; SFTODO: Following on from that, for such games the mode 7 status line colouring won't work either, so we might want to have a --multiline-status-area option which is shorthand for --no-mode-7-colour and --no-hw-scroll.
    ; SFTODO: This only has one caller, we could inline it (via a macro), but it may be the ability to rts early is worth having.
check_user_interface_controls
    ; SFTODO: CTRL-F has no effect in mode 7 unless MODE_7_STATUS is defined,
    ; but we don't currently have the concept of a "mode 7 only" build so we
    ; need to include this code to handle modes 0-6 whether MODE_7_STATUS is
    ; defined or not. CTRL-F will be silently "processed with no effect" in mode
    ; 7 without MODE_7_STATUS, which looks like it being a no-op to the user. We
    ; could save a few bytes by omitting all this code in a mode 7 only build
    ; without MODE_7_STATUS, or omitting the CTRL-B/CTRL-S code in a mode 7 only
    ; build with MODE_7_STATUS.
    ldx #0
    cmp #'F' - ctrl_key_adjust
    beq .change_colour_x
!ifdef MODE_7_PROMPT {
    ; SFTODO: Show this on the loader screen!
    ldx #2
    cmp #'P' - ctrl_key_adjust
    beq .change_colour_x
}
    ; We can't change the background colour in mode 7, and we don't allow
    ; toggling hardware scrolling on (it defaults to off) because it looks ugly.
    ; Hardware scrolling looks ugly because the status line colour code isn't
    ; output via s_printchar so it doesn't get redrawn automatically; we could
    ; work round this, but it's extra code and complexity, software scrolling
    ; would still look nicer and it doesn't slow things down much in mode 7
    ; (which is why it's the default). (We could allow CTRL-B to be processed in
    ; mode 7, as it would just silently have no effect, but since we need to
    ; check explicitly to avoid CTRL-S making a beep when it hasn't done
    ; anything we might as well protect CTRL-B behind the same check.)
    ldy screen_mode
    cpy #7
    beq .done
!ifdef ACORN_HW_SCROLL {
    cmp #'S' - ctrl_key_adjust
    bne .not_scroll
    lda use_hw_scroll
    eor #1
    sta use_hw_scroll
    ; We make a sound here because this has no immediately visible effect, so
    ; it's nice to offer the user some sort of feedback.
    jsr sound_high_pitched_beep
    lda #0
    rts
}
.not_scroll
    cmp #'B' - ctrl_key_adjust
    bne .done
    ldx #1 ; SFTODO: Was inx, but with MODE_7_PROMPT code above we can't rely on this - can probably rationalise this later once MODE_7_PROMPT is not so experimental
.change_colour_x
    inc fg_colour,x
    lda fg_colour,x
    ; Wrap colour numbers; we need to wrap to 0 in modes 0-6, but if we support
    ; a coloured status line in mode 7 we need to wrap to 1.
    ; SFTODO: We also need this mode 7 wrapping if we have MODE_7_PROMPT without MODE_7_STATUS,
    ; if that is possible.
    cmp #8
    bne +
    lda #0
!ifdef MODE_7_STATUS {
    ldy screen_mode
    cpy #7
    bne +
    lda #1
}
+   sta fg_colour,x
    jsr update_colours
    lda #0
.done
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
	ldy #header_default_bg_colour
	jsr read_header_word
	lda zcolours,x
+   
	+SetBackgroundColour
; Also set bordercolour to same as background colour, if bordercolour is set to the magic value 0
	cpy #0
	bne .current_background
	+SetBorderColour
.current_background

; Set foreground colour
	ldx z_operand_value_low_arr
	beq .current_foreground
	lda zcolours,x
	bpl + ; Branch unless it's the special value $ff, which means "default colour"
	ldy #header_default_fg_colour
	jsr read_header_word
	lda zcolours,x
+
; Also set bordercolour to same as foreground colour, if bordercolour is set to the magic value 1
	cpy #1
	bne +
	+SetBorderColour
+
	jsr s_set_text_colour ; change foreground colour
.current_foreground

; Set cursor colour
	lda s_colour
	ldx darkmode
	ldy cursorcol,x
	cpy #1
	beq +
	lda zcolours,y
+	sta current_cursor_colour

	rts
} else {
    ; SFTODO: THIS IS PROBABLY RIGHT, BUT CHECK - CAN/SHOULD WE JUST DO RTS???
    jmp printchar_flush
}
}

; SFTODODATA 160 (BUT NOT IN ALL BUILDS)
!ifdef ACORN_HW_SCROLL {
.top_line_buffer
    !fill max_screen_width
.top_line_buffer_reverse
    !fill max_screen_width
}

!ifdef TESTSCREEN {
!ifdef ACORN {
    !error "TESTSCREEN not supported on Acorn"
}

.testtext
	!pet 2, 5,147,18,"Status Line 123         ",146,13    ; white REV
	!pet 3, 28,"tesx",20,"t aA@! ",18,"Test aA@!",146,13  ; red
	!pet 155,"third",20,13                                ; light gray
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
	jsr init_screen_colours
!ifdef TARGET_PLUS4 {
	lda #212 ; 212 upper/lower, 208 = upper/special
} else {
	lda #23 ; 23 upper/lower, 21 = upper/special (22/20 also ok)
}
	sta reg_screen_char_mode
	jsr s_init
	;lda #1
	;sta s_scrollstart
	lda #25
	sta window_start_row ; 25 lines in window 0
	lda #1
	sta window_start_row + 1 ; 1 status line
	sta window_start_row + 2 ; 1 status line
	lda #0
	sta window_start_row + 3
	ldx #0
-   lda .testtext,x
	bne +
	rts
+   cmp #2
	bne +
	; use upper window
	lda #1
	sta current_window
	jmp ++
+   cmp #3
	bne +
	; use lower window
	lda #0
	sta current_window
	jmp ++
+   cmp #1
	bne +
	txa
	pha
--  jsr kernal_getchar
	beq --
	pla
	tax
	bne ++
	; NOTE: s_printchar no longer recognizes the colour codes, so the
	; colours will not change. But rev on/off still works
+   jsr s_printchar
++  inx
	bne -
}
}

