; SFTODO: Temporary experimental - BBC B only for now

!source "acorn-shared-constants.asm"

; SFTODO: Constant names etc taken from TobyLobster's OS 1.20 disassembly
vdu_text_window_bottom = $309
vdu_text_cursor_y_position = $319
vdu_screen_top_left_address_low = $350
vdu_screen_top_left_address_high = $351
set_cursor_software_and_hardware_position = $c6af
hardware_scroll_up = $c9a4
move_text_cursor_to_next_line = $cd3f ; SFTODO: probably does more than strictly needed FWIW

wrchv = $20e


; SFTODO: for now just assume 80 column mode - though these values are available at $352/$353 as that's where OS keeps them
bytes_per_line = 640

zp = $db ; 5 bytes - VDU temporary storage, 6 bytes starting at $da SFTODO: can I get away with this? simply by experiment I think $da is used by the code I'm going to call in the OS, but the others are OK
src = zp ; 2 bytes
dst = zp+2 ; 2 bytes

    * = $b10
start
    ; SFTODO: Don't try to protect against being reinstalled for now
    lda wrchv
    sta parent_wrchv
    lda wrchv+1
    sta parent_wrchv+1
    lda #<our_wrchv
    sta wrchv
    lda #>our_wrchv
    sta wrchv+1
    rts

our_wrchv
    cmp #10
    beq lf
jmp_parent_wrchv
parent_wrchv = *+1
    jmp $ffff ; patched
lf
    lda vdu_text_cursor_y_position
    cmp vdu_text_window_bottom
    lda #10
    bcc jmp_parent_wrchv
    ; It's a line feed on the bottom line of the text window.
    txa
    pha
    tya
    pha
    lda vdu_screen_top_left_address_high
    sta src+1
    lda vdu_screen_top_left_address_low
    sta src
    clc
    adc #<bytes_per_line
    sta dst
    lda vdu_screen_top_left_address_high
    adc #>bytes_per_line
    sta dst+1
    jsr move_text_cursor_to_next_line
    jsr hardware_scroll_up
    ldx #>bytes_per_line
    ldy #255
copy_and_zero_loop
    lda (src),y
    sta (dst),y
    lda #%10101010
    sta (src),y
    dey
    cpy #255
    bne copy_and_zero_loop
    inc src+1
    inc dst+1
    dex
    bmi copy_and_zero_done
    bne copy_and_zero_loop
    ldy #(<bytes_per_line)-1
    bne copy_and_zero_loop ; always branch
copy_and_zero_done
    jsr set_cursor_software_and_hardware_position
    pla
    tay
    pla
    tax
    rts
end

    +assert * < $d00
