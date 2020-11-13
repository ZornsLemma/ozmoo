!source "acorn-constants.asm"
osbyte_read_top_of_user_memory = $84
mrb_read_write = $fbfd
mrb_mode = $27f

BACKWARD_DECOMPRESS = 1

    lda #$ff
    sta LZSA_DST_LO
    lda #$7f
    sta LZSA_DST_HI
    lda #<(data_end - 1)
    sta LZSA_SRC_LO
    lda #>(data_end - 1)
    sta LZSA_SRC_HI
    jsr decompress

    ; If we're on an Electron with a Master RAM board in shadow mode we need to
    ; copy the decompressed image into video RAM. See preloader.bas for more on
    ; this.

    ; Are we on an Electron?
    lda #osbyte_read_host
    ldx #1
    jsr osbyte
    txa
    bne just_rts
    ; We're on an Electron. Are we in a shadow mode?
    lda #osbyte_read_top_of_user_memory
    jsr osbyte
    tya
    bpl just_rts
    ; We're in a shadow mode. Is this MRB shadow mode?
    lda mrb_mode
    cmp #$80
    bne just_rts
    ; Yes, it's MRB shadow mode, so copy the decompressed data.
ptr = $70
    lda #<SPLASH_SCREEN_ADDRESS
    sta ptr
    lda #>SPLASH_SCREEN_ADDRESS
    sta ptr + 1
loop
    ldy #0
    lda (ptr),y
    bit just_rts ; set V
    ldx ptr
    ldy ptr + 1
    jsr mrb_read_write
    inc ptr
    bne loop
    inc ptr + 1
    bpl loop

just_rts
    rts

decompress
!source "acorn-lzsa.asm"

data_start
    !binary "../temp/splash.lzsa2"
data_end
