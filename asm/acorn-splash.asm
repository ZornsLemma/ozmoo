!source "acorn-constants.asm"
osbyte_read_top_of_user_memory = $84
mrb_read_write = $fbfd

BACKWARD_DECOMPRESS = 1

    ; TODO: Since these are actually instructions in the decompression code being
    ; modified, can we simply assemble it with the correct values already in place?
    lda #$ff
    sta LZSA_DST_LO
    lda #$7f
    sta LZSA_DST_HI
    lda #<(data_end - 1)
    sta LZSA_SRC_LO
    lda #>(data_end - 1)
    sta LZSA_SRC_HI
    jsr decompress

    ; If we're in a shadow mode, we know we're on an Electron with a Master RAM
    ; board in shadow mode. (We know this because this is the only case where
    ; preloader.bas will call this code if we're in a shadow mode.)
    lda #osbyte_read_top_of_user_memory
    jsr osbyte
    tya
    bpl just_rts
    ; We are in a shadow mode, so use the MRB OS to copy the decompressed data
    ; into video RAM.
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
!source "acorn-lzsa-fast.asm"

data_start
    !binary "../temp/splash.lzsa2"
data_end
