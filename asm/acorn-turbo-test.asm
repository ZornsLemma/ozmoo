; Executable with the "turbo 6502" zp_temp_turbo_flag set in the header to see if we're
; running on a turbo secod processor.

zp_temp_turbo_flag = $8f

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
    sta zp_temp_turbo_flag
    rts

    !fill zp_temp_turbo_flag - *
    !byte 0
