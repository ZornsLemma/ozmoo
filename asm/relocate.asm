; SFTODO: HEADERS OR WHATEVER FOR OZMOO STYLE

!zone {

relocstart = program_start
!if <relocstart != 0 {
    !error "relocstart must be on a page boundary"
}
; SFTODO: ALL THESE SHOULD START WITH A . TO MAKE THEM LOCAL
delta = $70
codep = $71 ; 2 bytes
deltap = $73 ; 2 bytes
count = $75 ; 2 bytes
src = $77 ; 2 bytes
dst = $79 ; 2 bytes

relocate
    lda relocate_target
    eor #>relocstart
    lsr
    bcc +
    ; relocate_target doesn't have the same double-page alignment as relocstart,
    ; so bump it up by one. It's harmless to modify relocate_target, because
    ; it just means if we're restarted by re-running the executable it will
    ; already have the right alignment.
    inc relocate_target
+

    lda relocate_target
    cmp #(>relocstart) + 1
    bcc +
    ; We don't support relocating upwards.
    brk
    !byte 0
    !text "PAGE too high"
    brk
+

    lda relocate_target
    sta dst + 1
    sec
    sbc #>relocstart
    sta delta
    lda #>relocstart
    sta codep + 1
    sta src + 1
    lda #0
    tay
    sta dst
    sta src
    sta codep
    lda #<vmreloc
    sta deltap
    lda #>vmreloc
    sta deltap + 1
    lda vmreloccount
    sta count
    lda vmreloccount + 1
    sta count + 1
    ora count
    bne +
    ; There are no relocations, so just carry on without relocating.
    jmp initialize
+

    ; Fix up the absolute addresses in the executable in place.
    ; None of the following code can contain absolute addresses, otherwise it
    ; will be modified while it's executing and may crash. This is why we copy
    ; vmreloccount down into zero page. (If we didn't generate relocations for
    ; this last bit of the executable we wouldn't have this issue, but it's
    ; easier for the build system not to have to work out where the "real"
    ; executable ends and the relocation code begins.)
.patch_loop
    lda (deltap),y
    beq .advance_255
    clc
    adc codep
    sta codep
    bcc +
    inc codep + 1
+   lda (codep),y
    clc
    adc delta
    sta (codep),y
.patch_next
    inc deltap
    bne +
    inc deltap + 1
+   lda count
    bne +
    dec count + 1
+   dec count
    bne .patch_loop
    lda count + 1
    bne .patch_loop

    ; Now copy the code down to relocate_target. We must not copy over the top of this
    ; code while it's executing, which is why it's right at the end and we're
    ; precise about how many bytes we copy. (We do round up to the nearest page,
    ; though.)
.bytes_to_copy = relocate - program_start
    ; SFTODO: We're rounding .bytes_to_copy up to a whole number of pages at
    ; run time, when we could trivially do it at assembly time.
    lda #>.bytes_to_copy
    sta count + 1
    ldy #<.bytes_to_copy
    beq +
    inc count + 1
    ldy #0
+   sty count ; SFTODO: We never actually read this, could get rid of it
.copy_loop
    lda (src),y
    sta (dst),y
    iny
    bne .copy_loop
    inc src + 1
    inc dst + 1
    dec count + 1
    bne .copy_loop

    ; Now execute the relocated code; this absolute address will have been
    ; fixed up.
    jmp initialize

.advance_255
    clc
    lda codep
    adc #255
    sta codep
    bcc +
    inc codep + 1
+   bne .patch_next ; Always branch
    



; The following must be right at the very end of the executable. A bit of
; padding is tolerable, though.
vmreloccount
    !word 0
vmreloc
}
