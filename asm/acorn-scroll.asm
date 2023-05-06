; SFTODO: Temporary experimental - BBC B only for now

!source "acorn-shared-constants.asm"

; SFTODO: Constant names etc taken from TobyLobster's OS 1.20 disassembly
vdu_text_window_bottom = $309
vdu_text_cursor_y_position = $319
vdu_screen_top_left_address_low = $350
vdu_screen_top_left_address_high = $351
vdu_screen_size_high_byte = $354
set_cursor_software_and_hardware_position = $c6af
hardware_scroll_up = $c9a4
move_text_cursor_to_next_line = $cd3f ; SFTODO: probably does more than strictly needed FWIW

wrchv = $20e


; SFTODO: for now just assume 80 column mode - though these values are available at $352/$353 as that's where OS keeps them
bytes_per_line = 640

zp = $db ; 5 bytes - VDU temporary storage, 6 bytes starting at $da SFTODO: can I get away with this? simply by experiment I think $da is used by the code I'm going to call in the OS, but the others are OK
src = zp ; 2 bytes
dst = zp+2 ; 2 bytes

; SFTODO: This kinda-sorta works, although if the *OS* scrolls the screen because we print a character at the bottom right cell, its own scroll routines kick and do the clearing that we don't want.

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
    lda vdu_screen_top_left_address_low
    sta src
    lda vdu_screen_top_left_address_high
    sta src+1
    ; lda #19:jsr osbyte ; SFTODO TEMP HACK TO SEE WHAT IT LOOKS LIKE - IT DOESN'T HELP MUCH...
    jsr move_text_cursor_to_next_line
    jsr hardware_scroll_up
    lda vdu_screen_top_left_address_low
    sta dst
    lda vdu_screen_top_left_address_high
    sta dst+1
    ; We copy and bump in chunks of 128 because the line length in bytes is
    ; always a multiple of 128, which means we can wrap at the top of screen
    ; memory between each chunk without any problems.
    ldx #(bytes_per_line / 128)
copy_and_zero_outer_loop
    ldy #127
copy_and_zero_loop
    lda (src),y
    sta (dst),y
    lda # 0 ; SFTODO HACK lda #%10101010
    sta (src),y
    dey
    bpl copy_and_zero_loop
    clc
    lda src
    adc #128
    sta src
    bcc no_src_wrap
    inc src+1
    bpl no_src_wrap
    sec
    lda src+1
    sbc vdu_screen_size_high_byte
    sta src+1
no_src_wrap
    clc
    lda dst
    adc #128
    sta dst
    bcc no_dst_wrap
    inc dst+1
    bpl no_dst_wrap
    sec
    lda dst+1
    sbc vdu_screen_size_high_byte
    sta dst+1
no_dst_wrap
    dex
    bne copy_and_zero_outer_loop
    jsr set_cursor_software_and_hardware_position
    pla
    tay
    pla
    tax
    rts
end

    +assert * < $d00
