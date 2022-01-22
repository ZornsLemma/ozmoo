; Acorn-specific code.
;
; This is a simple INSV wrapper used by command history-enabled builds of Ozmoo
; to allow a mix of *FX4,0 and *FX4,1 behaviour to be available. These builds
; write to nominal_cursor_key_status instead of using *FX4 directly and this
; INSV handler uses that and the keys being pressed to decide whether to set the
; actual *FX4 status to 0 or 1.
;
; When Ozmoo sets nominal_cursor_key_status to 0 the custom behaviour is enabled:
; - cursor keys send key codes to Ozmoo by default, accessing command history
; - Shift+cursor keys or pressing Copy enters split cursor mode, which remains
;   enabled until the OS disables it (on Escape or a newline being printed)
;
; Shift+cursor keys only works on the BBC as on the Electron this combination
; types a regular character. Pressing Copy works on all machines.

!source "acorn-shared-constants.asm"

insv = $22a
vdu_status = $d0
cursor_key_status = $27d ; address updated by *FX4
copy_key = 139
cursor_up_key = 143
cursor_down_key = 142

; SFTODO: If this code needs to be shortened in the future,
; nominal_cursor_key_status could be moved into page 4/5 to save a byte. It
; could also be moved into zero page to save another byte, although that's
; probably not ideal as zero page is a valuable resource. Alternatively "ldy
; nominal_cursor_key_status" could be changed to "ldy #x" and the address of
; that x constant used as nominal_cursor_key_status; that's a little tricksy but
; still saves two bytes and avoids wasting a zero page location, so if this does
; need to be shortened that's probably the best of these options.
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

    pha ; save original A
    ; If nominal_cursor_key_status is 1, we always use that.
    ldy nominal_cursor_key_status
    bne cursor_key_status_in_a
    ; Check for Copy key.
    cmp #copy_key
    beq is_copy_key

    ; The character being inserted is in A. For cursor keys, b4 is set iff this
    ; is a shifted cursor key. To save code we don't actually check if it *is* a
    ; cursor key; for other keys we will therefore end up setting
    ; cursor_key_status to arbitrary 0/1 values, but that won't be noticeable
    ; because it doesn't affect those keys. This does mean the Ctrl key will be
    ; ignored in combination with cursor keys instead of being treated as
    ; distinct, but I think that's OK. (Space isn't as tight as it used to be
    ; and this could revert to testing more carefully if necessary.)
    and #%00010000
    eor #%00010000
    beq cursor_key_status_in_a
    lda #1
cursor_key_status_in_a
    sta cursor_key_status
    pla ; restore original A
jmp_old_insv
    jmp $ffff ; patched by init

is_copy_key
    ; If the Copy key is pressed, fake pressing cursor up followed by cursor
    ; down. This enters split cursor mode while leaving the cursor where it is
    ; (except for a brief flick onto the previous line).
    ;
    ; Set cursor_key_status to 0 so the parent INSV handler enters split cursor
    ; mode when it sees the cursor up we're about to send it.
    +assert buffer_keyboard = 0
    stx cursor_key_status
    ; If the first insert fails the second probably will as well and pressing
    ; Copy will be a no-op, which is probably the right outcome anyway; we don't
    ; bother to check the result of the first insert and explicitly avoid trying
    ; the second one.
    ;
    ; If the first insert succeeds and the second fails split cursor mode will
    ; be entered but the cursor will be left on the line above; this isn't ideal
    ; but it's not likely or a big problem, and it seems better than getting
    ; fancy trying to atomically insert both keycodes or none. (We'd have to
    ; disable interrupts and call CNPV to check the free space in the buffer is
    ; >= 2.) SFTODO: Should I do the CNPV call? We probably have space now. It's
    ; more a concern from a "will the code really ever be tested?" POV.
    lda #cursor_up_key
    jsr jmp_old_insv
    lda #cursor_down_key
    jsr jmp_old_insv
    pla ; restore original A
    rts

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
