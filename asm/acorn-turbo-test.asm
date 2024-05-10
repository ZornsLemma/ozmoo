; Executable with the "turbo 6502" flag set in the header to see if we're
; running on a turbo second processor.
;
; This should only be executed if we know we are running on a second processor
; of some kind. In that case, is_turbo is set to 0 for a non-turbo or $ff for a
; turbo second processor.

!source "acorn-shared-constants.asm"

start
    jmp entry
    jmp entry ; service entry point (irrelevant)
    !byte %01100001
    !byte copyright - start
copyright
    !text 0, "(C)"
    !word start, 0 ; relocation address
    !word entry - start, 0 ; offset to entry
entry
    lda #$ff
    sta is_turbo
    sta is_turbo_copy
    rts

    !fill is_turbo - *
    !byte 0
