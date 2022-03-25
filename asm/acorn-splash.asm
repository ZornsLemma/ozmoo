!source "acorn-shared-constants.asm"
osbyte_read_top_of_user_memory = $84
mrb_read_write = $fbfd

BACKWARD_DECOMPRESS = 1

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
ptr = lda_abs_x + 1
    +assert (<SPLASH_SCREEN_ADDRESS) = 0
    ldx #0
    stx ptr
    ldy #>SPLASH_SCREEN_ADDRESS
    sty ptr + 1
outer_loop
inner_loop
lda_abs_x
    lda $8000,x ; patched
    bit just_rts ; set V
    jsr mrb_read_write ; preserves X and Y
    inx
    bne inner_loop
    inc ptr + 1
    iny
    bpl outer_loop

just_rts
    rts

decompress
    !source "acorn-lzsa-fast.asm"

data_start
    !binary "../temp/splash.lzsa2"
data_end
