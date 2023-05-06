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

; The low level details of accessing the screen are very different on Acorn
; compared to the Commodore machines. I've removed a lot of Commodore code to
; try to make the structure clearer on Acorn.
!ifndef ACORN {
    !error "Non-Acorn code has been removed from screenkernal.asm"
}

!ifdef MODE_7_STATUS {
    MODE_7_STATUS_OR_INPUT = 1
} else {
    !ifdef MODE_7_INPUT {
        MODE_7_STATUS_OR_INPUT = 1
    }
}
!ifdef Z3 {
    !ifdef MODE_7_STATUS_OR_INPUT {
        WANT_PRINTCHAR_UNFILTERED = 1
    }
} else {
    !ifdef MODE_7_INPUT {
        WANT_PRINTCHAR_UNFILTERED = 1
    }
}

!zone screenkernal {

!ifdef Z3 {
max_lines = s_screen_height_minus_one
} else {
max_lines = s_screen_height
}

; SFTODO: Could get rid of these macros now they're so simple?
!macro cmp_screen_height {
    cmp s_screen_height
}
!macro cmp_screen_width {
    cmp s_screen_width
}
!macro cpx_screen_height {
    cpx s_screen_height
}
!macro cpx_screen_width {
    cpx s_screen_width
}
!macro cpx_max_lines {
    cpx max_lines
}
!macro cpy_screen_width {
    cpy s_screen_width
}
!macro lda_screen_height_minus_one {
    lda s_screen_height_minus_one
}
!macro lda_screen_height {
    lda s_screen_height
}
!macro lda_screen_width_minus_one {
    lda s_screen_width_minus_one
}
!macro ldx_screen_width_minus_one {
    ldx s_screen_width_minus_one
}
!macro lda_screen_width {
    lda s_screen_width
}
!macro ldx_max_lines {
    ldx max_lines
}
!macro ldx_screen_height_minus_one {
    ldx s_screen_height_minus_one
}
!macro ldx_screen_width {
    ldx s_screen_width
}
!macro ldy_screen_height_minus_one {
    ldy s_screen_height_minus_one
}
!macro ldy_screen_height {
    ldy s_screen_height
}
!macro ldy_screen_width {
    ldy s_screen_width
}


; SFTODO: I could move these into page 4 to save a few bytes in main RAM. However, page 4 is getting quite full now we can use otherwise wasted space for history storage. It would be a bit conditionally messy but we could squeeze these into spare space in page 5 *if* vmap_max_size is small enough. Probably something to reconsider when I finally tidy up the low memory allocation/constants.asm. It might be possible in constants.asm to conditionally allocate in page 4/5 or to allocate "inline" (i.e. as part of the main executable), which might make it not too painful and would allow maximum flexibility to use spare parts of page 5 without complicating the code too much. I am thinking this would be useful not just for this, but for other uninitialised data which *could* live in low memory as well.
s_screen_height !byte 0
s_screen_width_minus_one !byte 0
s_screen_height_minus_one !byte 0

!macro screenkernal_init_inline {
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

    ; Pick up the current OS cursor position; this will improve readability if
    ; any errors occuring during initialization, and it doesn't affect the game
    ; because deletable_screen_init_2 calls set_cursor anyway.
    lda #osbyte_read_cursor_position
    sta s_cursors_inconsistent
    jsr osbyte
    stx zp_screencolumn
    sty zp_screenrow
	; Set to 0: s_ignore_next_linebreak, s_reverse, s_os_reverse
    ; SFTODONOW: I suspect we don't need any of this zero initialisation on Acorn, but let's play it safe while I'm in the middle of zp tweaking.
    lda #0
    ldx #2
-	sta s_ignore_next_linebreak,x
	dex
	bpl -
    sta s_reverse
    sta s_os_reverse

    ; If we didn't change mode after a restart, we may have been left with the
    ; OS colours set to reverse video. Force the OS settings to normal video so
    ; they agree with s_os_reverse.
    jsr force_set_os_normal_video
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
 	cpx s_screen_height
	bcc +
	ldx s_screen_height_minus_one
+	stx zp_screenrow
	sty zp_screencolumn
    lda #1
    sta s_cursors_inconsistent
.return
    rts

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
    ; SFTODO: *If* (not sure if true/could be made true) we didn't normally have
    ; a text window in effect, I think we could avoid the read_vdu_variable call
    ; here.
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

!ifdef MODE_7_INPUT {
; This must preserve X and Y.
handle_mode_7_colour_prompt_delete
    jsr s_printchar
    lda input_colour_code_or_0
    beq .rts
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
s_printchar_and_handle_mode_7_colour_prompt_new_line
    jsr s_printchar
    lda input_colour_code_or_0
    beq .rts
	lda zp_screencolumn
	bne .rts
    lda input_colour_code_or_0
	; fall through to s_printchar_unfiltered
} ; MODE_7_INPUT
!ifdef WANT_PRINTCHAR_UNFILTERED {
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
    cmp #$7f ; SF: was $80 on Commodore
	bcc .normal_char
	cmp #$a0
	bcs .normal_char
+   cmp #$0d
	bne +
	; newline
	jmp .perform_newline ; SFTODO: Any chance of using beq always on Acorn?
+
    ; SF: Note that .readkey will call s_printchar with the native delete
    ; character.
    cmp #127 ; 20 on Commodore
	bne +
	; delete
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
    ; SF: Reverse video isn't handled by sending control codes through
    ; s_printchar on the Acorn. It could be, but it seems simplest just to
    ; update s_reverse directly.
+
	jmp .printchar_end

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
    ; SFTODO: I'm sure it's not huge, but this feels like a bit of a performance
    ; drag on Z4+ games with MODE_7_STATUS. Can we optimise it?
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
    sta top_line_buffer,x
    lda s_reverse
    sta top_line_buffer_reverse,x
    lda top_line_buffer,x
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
    ; OSWRCH may cause the screen to scroll if we're at the bottom right
    ; character. We therefore don't use .s_scroll to do the scroll, we just
    ; define a text window to tell the OS what to scroll. SFTODO: That comment
    ; is out of date and has been for ages, *if* we're using hardware scrolling
    ; we do things differently.
    ; SFTODO: It feels completely wrong that when we scroll here because we're
    ; wrapping at the right margin we use a different code path than scrolling
    ; because we've done an explicit newline. Am I missing something? Surely the
    ; bulk of this operation should share the same code? There could be code size
    ; and/or performance improvements to be had.
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
    ; need to fix this up. SFTODO: Wouldn't it be better to turn reverse video
    ; off before the scroll? Maybe this is shorter but worth investigating. I
    ; suspect making "end of line forced scrolling" and "explicit newline" share
    ; code might also be helpful in simplifying/optimising this.
    jsr s_erase_line_from_cursor
.not_reverse
!ifdef ACORN_HW_SCROLL {
    ldx window_start_row + 1 ; how many top lines to protect
    dex
    bne .no_hw_scroll1
    lda use_hw_scroll
    beq .no_hw_scroll1
    ; SFTODO: Is there any value in trying to only redraw the top line after
    ; all "short term" updates are done? Suppose the game scrolls a
    ; five line room description onto the screen. At the moment we'll
    ; redraw the top line after every single line. It would be faster
    ; to just redraw once output has finished and we're waiting for user
    ; input. I don't know if this is feasible - what if the game decides to
    ; (making this up as a simple example) pause for 10 seconds, expecting
    ; the status line to be there but we haven't shown it because there's
    ; no user input happening? It might also look ugly even if there were
    ; no implementation concerns, but I'm just speculating - it might look
    ; nicer overall due to the faster scroll and reduced flicker.
    jsr .redraw_top_line
    jmp .printchar_oswrch_done
.no_hw_scroll1
}
    lda #vdu_reset_text_window
    sta s_cursors_inconsistent ; vdu_reset_text_window moves cursor to home
    ; SFTODO: Micro-optimisation: "bne just-after-following-pla ; always branch" would save four cycles here and only add one byte of code. Or "!byte $24 ; bit zp opcode" would also save four cycles and not add any bytes of code, while being a little less transparent.
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
    ; SFTODO: Is there any way we can optimise this? It seems a shame to have
    ; to do this after every character output when it's almost always unnecessary.
    ; I am wondering if a) we should be doing this *when we scroll*, not always
    ; b) if we are not turning the cursor off when timed events occur during
    ; input and we should be.
    jsr s_cursor_to_screenrowcolumn
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
+	
!ifdef SCROLLBACK {
	; Copy to scrollback buffer, if we're in lower window
	ldx current_window
	bne +
	jsr copy_line_to_scrollback
+
}
	lda #0
	sta zp_screencolumn
	inc zp_screenrow
	jsr .s_scroll
    lda #1
    sta s_cursors_inconsistent
	jmp .printchar_end


!ifdef SCROLLBACK {
s_reset_scrolled_lines
	pha
	lda #0
	sta s_scrolled_lines
	pla
	rts

s_scrolled_lines !byte 0
}

.s_scroll
	lda zp_screenrow
	cmp s_screen_height
	bpl +
	rts
+
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
    sta top_line_buffer,x
    lda #0
    sta top_line_buffer_reverse,x
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
    pha ; SFTODO: wouldn't it be same length, 4 cycles faster *and* clearer to drop pha and replace pla with "lda zp_screenrow"?
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
    ; SFTODO: hoglet points out that we define and clear this text window a lot - we may get better performance if we can leave it in place most of the time. However, do note that unless I've done something silly, we only have this text window when we're soft-scrolling (which is the default only in mode 7) - certainly an important case, but mode 7 is already nice and fast anyway. Definitely worth not doing things sub-optimally, but this may not be a massive win with respect to speed. If we can tweak/rewrite things to shorten the code that's always a win. (hoglet's observation was made in the context of an ongoing port to Atom+tube, when settings etc may have been a bit weird by our standards.)
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
    ; SFTODO: I suspect it seldom occurs, but this seems silly if we are in the
    ; case where the above code has jsr-ed in here and we really want to leave
    ; the cursor at the old position.
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
    ; SFTODO: We do call this code quite a lot - every time the screen scrolls - and are we maybe causing noticeable slowdown by constantly setting the correct foreground colours? Would it speed things up if we were smarter and only set normal/reverse video when there's an actual change. This might have even more effect on tube, where all these colour change codes will clog up the VDU FIFO.
-   lda top_line_buffer_reverse,x
    beq +
    jsr set_os_reverse_video
    jmp ++
+   jsr set_os_normal_video
++  lda top_line_buffer,x
    jsr oswrch
    inx
    dey
    bne -
    stx s_cursors_inconsistent
    jmp turn_on_cursor
}


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
!ifdef MODE_7_INPUT {
    lda input_colour_code_or_0
    beq +
    sta s_cursors_inconsistent
    jsr turn_off_cursor
    ; SFTODO: We "should" update previous lines of this same prompt, but let's keep the
    ; code size and complexity down for now.
    lda #$ff
    sta mode_7_input_tmp
-   inc mode_7_input_tmp
    ldx mode_7_input_tmp
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
    adc input_colour
    sta input_colour_code_or_0
    jsr oswrch
.prompt_change_done
    jsr turn_on_cursor
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
!ifdef MODE_7_INPUT {
    ldx #2
    cmp #'I' - ctrl_key_adjust
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
    ldx #1 ; SFTODO: Was inx, but with MODE_7_INPUT code above we can't rely on this - can probably rationalise this later once MODE_7_INPUT is not so experimental
.change_colour_x
    inc fg_colour,x
    lda fg_colour,x
    ; Wrap colour numbers; we need to wrap to 0 in modes 0-6, but if we support
    ; a coloured status line or input in mode 7 we need to wrap to 1.
    cmp #8
    bne +
    lda #0
!ifdef MODE_7_STATUS_OR_INPUT {
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


!ifdef Z5PLUS {
z_ins_set_colour
    ; SFTODO: THIS IS PROBABLY RIGHT, BUT CHECK - CAN/SHOULD WE JUST DO RTS???
    jmp printchar_flush
}

}
