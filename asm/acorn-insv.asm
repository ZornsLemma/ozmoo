; Acorn-specific code; this is a simple INSV wrapper to adjust the *FX4 setting
; depending on whether SHIFT is held down in combination with the cursor keys or
; not. This allows the Ozmoo command history to be used with unshifted up/down
; cursor keys, while retaining access to the OS split cursor editing via
; SHIFT+cursor keys.

!source "acorn-shared-constants.asm"

insv = $22a
vdu_status = $d0
cursor_key_status = $27d ; address updated by *FX4
copy_key = 139
up_key = 159
down_key = 158

; SFTODO: We could claw a byte back by moving nominal_cursor_key_status to a fixed location in page 4/5/6,
; although it's perhaps going to be fiddly to do so (we don't want to "waste" a byte for it on tube client side).
; We could also claw another byte back by moving nominal_cursor_key_status into zp, but that's probably a bit
; wasteful of a valuable byte of ZP.
; SFTODO: Hmm, we *could* change "lda nominal_cursor_key_status" - our *only* reference to this - to "lda #x" and
; make the immediate operand address nominal_cursor_key_status. That would save two bytes and is localised hackery
; rather than interfering with zp/low memory allocation which is more global.
; SFTODONOW: *Probably* nothing will do a VDU 7 when Ozmoo is running, but it might be possible - e.g. a * command
; might do it. Should we arrange (I think there's a *FX call) for VDU 7 beep to use sound channel 3 to avoid
; trampling on this code? (Might be good to confirm just at BASIC prompt this *can* be a problem first.) - OK, while a permanent
; comment is probably helpful, this seems to be a non-issue because VDU 7 uses channel 3 by default. Of course a
; utility ROM's *command might use one of the other channels, but that's not quite as likely.
; SFTODO: Not super keen, but we could maybe squeeze this code into CFS/RFS workspace &380-&3DF inclusive instead
; of the sound buffers. Cassette is currently probably not very interesting, it's *borderline* possible someone
; would want to use RFS to hold a fixed copy of a saved game and using that workspace might break this. This isn't
; super likely though.
!if * != nominal_cursor_key_status {
    !error "nominal_cursor_key_status incorrect"
}
    !byte 0 ;  nominal_cursor_key_status

; On entry:
;     A=140-143 for unshifted cursors
;     A=156-159 for shifted cursors
;     X=buffer number
;
; On exit:
;     A, X preserved
;     Y undefined
;     C indicates insertion success/failure, but parent INSV takes care of that
our_insv
    ; If this isn't a keyboard buffer insertion, leave things alone.
    cpx #buffer_keyboard
    bne jmp_old_insv
    ; If split cursor editing is in progress, leave things alone.
    bit vdu_status
    bvs jmp_old_insv
    ; Save A.
    pha
    ; If nominal_cursor_key_status is 1, we always use that.
    ldy nominal_cursor_key_status
    bne cursor_key_status_in_y
    ; If the Copy key is pressed, fake shifted-up followed by shifted-down
    ; keypresses; this enters split cursor mode without needing a real
    ; shift+cursor key press and without moving the cursor (except for a brief
    ; unavoidable flick up onto the previous row). This allows split cursor mode
    ; to be accessible on the Electron where the keyboard layout means INSV is
    ; never called with shifted cursor key codes. It also works on the BBC
    ; machines, if the user wants to use it.
    cmp #copy_key
    bne inserted_character_in_a
    ; SFTODO: We are ignoring the possibility of the keyboard buffer being full
    ; here... This is probably tolerable/unavoidable (what could we actually do about
    ; it, even if we weren't tight for code space here?) but think about it fresh.
    lda #up_key
    jsr jmp_old_insv
    lda #down_key
    jsr jmp_old_insv
    pla ; return with original A preserved as we should
    ; We leave C set from the second jmp_old_insv call. SFTOD: OK!?!?!
    rts
inserted_character_in_a
    ; The character being inserted is in A. For cursor keys, b4 is set iff this
    ; is a shifted cursor key. To save code (space is very tight here) we don't
    ; actually check if it *is* a cursor key; for other keys we will therefore
    ; end up setting cursor_key_status to arbitrary 0/1 values, but that won't
    ; be noticeable because it doesn't affect those keys. This does mean the
    ; Ctrl key will be ignored in combination with cursor keys instead of being
    ; treated as distinct, but I think that's OK.
    and #%00010000
    eor #%00010000
    ; Y is 0 at this point as we didn't "bne cursor_key_status_in_y" above.
    beq cursor_key_status_in_y
    iny ; Y=1
cursor_key_status_in_y
    sty cursor_key_status
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
