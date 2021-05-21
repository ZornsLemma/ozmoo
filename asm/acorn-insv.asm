; Acorn-specific code; this is a simple INSV wrapper to adjust the *FX4 setting
; depending on whether SHIFT is held down in combination with the cursor keys or
; not. This allows the Ozmoo command history to be used with unshifted up/down
; cursor keys, while retaining access to the OS split cursor editing via
; SHIFT+cursor keys.

!source "acorn-constants.asm"

insv = $22a
cursor_key_status = $27d ; address updated by *FX4

; On entry:
;     A=140-143 for unshifted cursors
;     A=156-159 for shifter cursors
our_insv
    cpx #buffer_keyboard
    bne jmp_old_insv
    pha
    and #%11101100
    cmp #%10001100
    bne not_unshifted_or_shifted_cursor
    ; This is a shifted (b4 set) or unshifted (b4 clear) cursor key.
    pla
    pha
    and #%00010000
    eor #%00010000
    beq cursor_key_status_in_a
    lda #1
cursor_key_status_in_a
    sta cursor_key_status
not_unshifted_or_shifted_cursor
    pla
jmp_old_insv
    jmp $ffff ; patched by init

; This code is for initialisation and can be overwritten afterwards.
init
    lda insv
    sta jmp_old_insv + 1
    lda insv + 1
    sta jmp_old_insv + 2
    lda #<our_insv
    sta insv
    lda #>our_insv
    sta insv + 1
    rts
