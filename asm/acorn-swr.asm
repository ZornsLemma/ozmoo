; Acorn sideways RAM macros and screen hole support; the screen hole is quite
; intimately connected with sideways RAM, since without sideways RAM the screen
; isn't in the middle of usable memory anyway.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sideways RAM paging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!ifdef ACORN_SWR {

; SF: ENHANCEMENT: It would probably be possible (even easy) to use an AQR
; cartridge for all sideways RAM on a Master or Electron; it has its own special
; paging register though, so we'd need a separate build to support it, and auto-
; detecting it in a way that doesn't interfere with the user's other use of it
; may be tricky. There's also some sort of unlock stuff to contend with, I think.

; These macros must leave the selected bank number in A or Y as appropriate.
; SFTODO These macros are probably quite space-consuming on the Electron; it's
; tempting to move them into subroutines. However, most of them *have* been
; moved indirectly, as part of the various dynamic memory *_slow* subroutines,
; so the remaining ones are mostly going to be performance-sensitive. So this is
; probably fine, but maybe worth thinking over later.

!ifndef ACORN_ELECTRON_SWR {

!macro acorn_page_in_bank_using_a .operand {
    lda .operand
    sta romsel_copy
    sta bbc_romsel
}

!macro acorn_page_in_bank_using_a_comma_x .operand {
    lda .operand,x
    sta romsel_copy
    sta bbc_romsel
}

!macro acorn_page_in_bank_using_a_comma_y .operand {
    lda .operand,y
    sta romsel_copy
    sta bbc_romsel
}

!macro acorn_page_in_bank_using_y .operand {
    ldy .operand
    sty romsel_copy
    sty bbc_romsel
}
} else { ; ACORN_ELECTRON_SWR

!macro acorn_page_in_bank_using_a .operand {
    lda #12
    sta romsel_copy
    sta electron_romsel
    lda .operand
    sta romsel_copy
    sta electron_romsel
}

!macro acorn_page_in_bank_using_a_comma_x .operand {
    lda #12
    sta romsel_copy
    sta electron_romsel
    lda .operand,x
    sta romsel_copy
    sta electron_romsel
}

!macro acorn_page_in_bank_using_a_comma_y .operand {
    lda #12
    sta romsel_copy
    sta electron_romsel
    lda .operand,y
    sta romsel_copy
    sta electron_romsel
}

!macro acorn_page_in_bank_using_y .operand {
    ldy #12
    sty romsel_copy
    sty electron_romsel
    ldy .operand
    sty romsel_copy
    sty electron_romsel
}
}

; SFTODONOW: acorn_swr->acorn in macro name? seems inconsistent
!macro acorn_swr_page_in_default_bank_using_y {
    +acorn_page_in_bank_using_y z_pc_mempointer_ram_bank
}
!macro acorn_swr_page_in_default_bank_using_a {
    +acorn_page_in_bank_using_a z_pc_mempointer_ram_bank
}

}

; In the Acorn port, I've deliberately renamed before_dynmem_read and
; after_dynmem_read so that any upstream changes which use these macros will
; fail to build and force me to inspect them manually. The Commodore versions
; don't modify any registers or flags (except I), but that would cost time and
; space on the Acorn port.

!macro before_dynmem_read_corrupt_a {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    +acorn_page_in_bank_using_a dynmem_ram_bank
}
}

!macro before_dynmem_read_corrupt_a_slow {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    jsr before_dynmem_read_corrupt_a_slow_sub
}
}

!macro before_dynmem_read_corrupt_y {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    +acorn_page_in_bank_using_y dynmem_ram_bank
}
}

!macro before_dynmem_read_corrupt_y_slow {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    jsr before_dynmem_read_corrupt_y_slow_sub
}
}

!macro after_dynmem_read_corrupt_a {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    +acorn_page_in_bank_using_a z_pc_mempointer_ram_bank
}
}

; SFTODO: A lot of the after_dynmem_*_slow calls are followed by rts; it may be worth introducing special wrappers to do after_dynmem_*+rts, it is a bit of extra complexity but not much and it would save time and space on bigdyn builds at a small complexity cost for other builds
!macro after_dynmem_read_corrupt_a_slow {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    jsr after_dynmem_read_corrupt_a_slow_sub
}
}

!macro after_dynmem_read_corrupt_y {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    +acorn_page_in_bank_using_y z_pc_mempointer_ram_bank
}
}

!macro after_dynmem_read_corrupt_y_slow {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    jsr after_dynmem_read_corrupt_y_slow_sub
}
}

; SFTODO: IT'S POSSIBLE SOME CALLERS OF THIS NON-A-CORRUPTING VERSION COULD USE AN X OR Y CORRUPTING VERSION - I HAVE REVIEWED MOST, BUT THINGS ARE STILL WIP SO WORTH RE-REVIEWING ANY REMAINING CALLERS OF THIS VERSION LATER
!macro after_dynmem_read_preserve_axy {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    pha
    +after_dynmem_read_corrupt_a
    pla
}
}

!macro after_dynmem_read_preserve_axy_slow {
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    jsr after_dynmem_read_preserve_axy_slow_sub
}
}

!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
before_dynmem_read_corrupt_a_slow_sub
    +before_dynmem_read_corrupt_a
    rts

before_dynmem_read_corrupt_y_slow_sub
    +before_dynmem_read_corrupt_y
    rts

after_dynmem_read_corrupt_a_slow_sub
    +after_dynmem_read_corrupt_a
    rts

after_dynmem_read_corrupt_y_slow_sub
    +after_dynmem_read_corrupt_y
    rts

after_dynmem_read_preserve_axy_slow_sub
    +after_dynmem_read_preserve_axy
    rts
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Screen hole support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!ifdef ACORN_SCREEN_HOLE {
!ifndef ACORN_SWR {
!error "ACORN_SCREEN_HOLE is only compatible with ACORN_SWR"
}
}

!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {

; We don't support DEBUG_BIG_DYNMEM here; these macros are already quite bloated
; and it would probably cause branch out of range errors. We should catch any
; bugs in this area using the non-screen hole version of the code.

; These macros will alter the carry, unlike a raw lda/sta (zp),y. The store
; macros will also alter N and Z. In practice this isn't a problem.

!macro lda_dynmem_ind_y .zp {
    lda .zp + 1
    cmp acorn_screen_hole_start_page_minus_one
    bcc .zp_y_ok
    bne .zp_y_not_ok
    ; We need to add Y to (.zp) to see if it's going to cause the high byte to
    ; increase and point into the screen hole.
    clc
    tya
    adc .zp
    bcc .zp_y_ok
    lda .zp + 1
.zp_y_not_ok
    ; A holds .zp + 1, C is set.
    adc acorn_screen_hole_pages_minus_one ; -1 because carry is set
    sta screen_hole_zp_ptr + 1
    lda .zp
    sta screen_hole_zp_ptr
    lda (screen_hole_zp_ptr),y
    jmp .done
.zp_y_ok
    lda (.zp),y
.done
}

; Dynamic memory reads which aren't performance critical use this macro, which
; calls a subroutine instead of inlining the code. We need to call a different
; version of the subroutine for each zero page address.
!macro lda_dynmem_ind_y_slow .zp {
    !if .zp = object_tree_ptr {
        jsr lda_dynmem_ind_y_slow_object_tree_ptr_sub
    } else {
        !if .zp = zp_mempos {
            jsr lda_dynmem_ind_y_slow_zp_mempos_sub
        } else {
            !if .zp = default_properties_ptr {
                jsr lda_dynmem_ind_y_slow_default_properties_ptr_sub
            } else {
                !if .zp = string_array {
                    jsr lda_dynmem_ind_y_slow_string_array_sub
                } else {
                    !if .zp = parse_array {
                        jsr lda_dynmem_ind_y_slow_parse_array_sub
                    } else {
                        !if .zp = z_low_global_vars_ptr {
                            jsr lda_dynmem_ind_y_slow_z_low_global_vars_ptr_sub
                        } else {
                            !error "Unsupported zp"
                        }
                    }
                }
            }
        }
    }
}

; SF: There would be some small performance gains here from allowing X to be
; corrupted, but few callers (and no performance-critical ones) would be able to
; take advantage, so it's not worth providing an X-corrupting version.
!macro sta_dynmem_ind_y .zp {
    sta screen_hole_tmp
    lda .zp + 1
    cmp acorn_screen_hole_start_page_minus_one
    bcc .zp_y_ok
    bne .zp_y_not_ok
    ; We need to add Y to (.zp) to see if it's going to cause the high byte to
    ; increase and point into the screen hole.
    clc
    tya
    adc .zp
    bcc .zp_y_ok
    lda .zp + 1
.zp_y_not_ok
    ; A holds .zp + 1, C is set.
    adc acorn_screen_hole_pages_minus_one ; -1 because carry is set
    sta screen_hole_zp_ptr + 1
    lda .zp
    sta screen_hole_zp_ptr
    lda screen_hole_tmp
    sta (screen_hole_zp_ptr),y
    jmp .done
.zp_y_ok
    lda screen_hole_tmp
    sta (.zp),y
.done
}

; Dynamic memory writes which aren't performance critical use this macro, which
; calls a subroutine instead of inlining the code. We need to call a different
; version of the subroutine for each 16-bit zero page address.
!macro sta_dynmem_ind_y_slow zp {
    !if zp = object_tree_ptr {
        jsr sta_dynmem_ind_y_slow_object_tree_ptr_sub
    } else {
        !if zp = zp_mempos {
            jsr sta_dynmem_ind_y_slow_zp_mempos_sub
        } else {
            !if zp = string_array {
                jsr sta_dynmem_ind_y_slow_string_array_sub
            } else {
                !if zp = parse_array {
                    jsr sta_dynmem_ind_y_slow_parse_array_sub
                } else {
                    !if zp = z_low_global_vars_ptr {
                        jsr sta_dynmem_ind_y_slow_z_low_global_vars_ptr_sub
                    } else {
                        !if zp = z_high_global_vars_ptr {
                            jsr sta_dynmem_ind_y_slow_z_high_global_vars_ptr_sub
                        } else {
                            !error "Unsupported zp"
                        }
                    }
                }
            }
        }
    }
}

!zone {
; This version of the lda/sta_dynmem_* subroutines is executed far more than the
; others, so we optimise it slightly by making it the one which has the code
; inline.
lda_dynmem_ind_y_slow_object_tree_ptr_sub
    stx screen_hole_tmp
    ldx #object_tree_ptr
lda_dynmem_ind_y_slow_x_sub
    lda $01,x
    cmp acorn_screen_hole_start_page_minus_one
    bcc .zp_y_ok
    bne .zp_y_not_ok
    ; We need to add Y to (zp) to see if it's going to cause the high byte to
    ; increase and point into the screen hole.
    clc
    tya
    adc $00,x
    bcc .zp_y_ok
    lda $01,x
.zp_y_not_ok
    ; A holds zp + 1, C is set.
    adc acorn_screen_hole_pages_minus_one ; -1 because carry is set
    sta screen_hole_zp_ptr + 1
    lda $00,x
    sta screen_hole_zp_ptr
    ldx screen_hole_tmp
    lda (screen_hole_zp_ptr),y
    rts
.zp_y_ok
    stx .lda_zp_ind_y + 1
    ldx screen_hole_tmp
.lda_zp_ind_y
    lda ($00),y ; patched at runtime
    rts
}

!macro lda_dynmem_ind_y_slow_helper zp {
    stx screen_hole_tmp
    ldx #zp
    !if zp = 0 {
        beq lda_dynmem_ind_y_slow_x_sub ; always branch
    } else {
        bne lda_dynmem_ind_y_slow_x_sub ; always branch
    }
}

lda_dynmem_ind_y_slow_zp_mempos_sub
	+lda_dynmem_ind_y_slow_helper zp_mempos

lda_dynmem_ind_y_slow_string_array_sub
	+lda_dynmem_ind_y_slow_helper string_array

lda_dynmem_ind_y_slow_parse_array_sub
	+lda_dynmem_ind_y_slow_helper parse_array

lda_dynmem_ind_y_slow_default_properties_ptr_sub
	+lda_dynmem_ind_y_slow_helper default_properties_ptr

lda_dynmem_ind_y_slow_z_low_global_vars_ptr_sub
	+lda_dynmem_ind_y_slow_helper z_low_global_vars_ptr

!zone {
sta_dynmem_ind_y_slow_object_tree_ptr_sub
    stx screen_hole_tmp_slow
    ldx #object_tree_ptr
sta_dynmem_ind_y_slow_x_sub
    sta screen_hole_tmp
    lda $01,x
    cmp acorn_screen_hole_start_page_minus_one
    bcc .zp_y_ok
    bne .zp_y_not_ok
    ; We need to add Y to (zp) to see if it's going to cause the high byte to
    ; increase and point into the screen hole.
    clc
    tya
    adc $00,x
    bcc .zp_y_ok
    lda $01,x
.zp_y_not_ok
    ; A holds zp + 1, C is set.
    adc acorn_screen_hole_pages_minus_one ; -1 because carry is set
    sta screen_hole_zp_ptr + 1
    lda $00,x
    sta screen_hole_zp_ptr
    ldx screen_hole_tmp_slow
    lda screen_hole_tmp
    sta (screen_hole_zp_ptr),y
    rts
.zp_y_ok
    stx .sta_zp_ind_y_instruction_to_patch + 1
    ldx screen_hole_tmp_slow
    lda screen_hole_tmp
.sta_zp_ind_y_instruction_to_patch
    sta ($00),y ; patched at runtime
    rts
}

!macro sta_dynmem_ind_y_slow_setup_x zp {
    stx screen_hole_tmp_slow
    ldx #zp
    !if zp = 0 {
        beq sta_dynmem_ind_y_slow_x_sub ; always branch
    } else {
        bne sta_dynmem_ind_y_slow_x_sub ; always branch
    }
}

sta_dynmem_ind_y_slow_zp_mempos_sub
	+sta_dynmem_ind_y_slow_setup_x zp_mempos

sta_dynmem_ind_y_slow_string_array_sub
	+sta_dynmem_ind_y_slow_setup_x string_array

sta_dynmem_ind_y_slow_parse_array_sub
	+sta_dynmem_ind_y_slow_setup_x parse_array

sta_dynmem_ind_y_slow_z_low_global_vars_ptr_sub
	+sta_dynmem_ind_y_slow_setup_x z_low_global_vars_ptr

sta_dynmem_ind_y_slow_z_high_global_vars_ptr_sub
	+sta_dynmem_ind_y_slow_setup_x z_high_global_vars_ptr

} else { ; !ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE

!ifdef ACORN_SWR_BIG_DYNMEM {

; Debugging macro to check that the SWR bank containing the upper part of dynmem
; is paged in when it should be.
!macro debug_dynmem .preserve {
!ifdef DEBUG_BIG_DYNMEM {
    !if .preserve {
        pha
    }
    lda romsel_copy
    cmp ram_bank_list
-   bne -
    !if .preserve {
        pla
    }
}
}
} else {
!macro debug_dynmem .preserve {
}
}

!macro lda_dynmem_ind_y zp {
    +debug_dynmem 0
    lda (zp),y
}

!macro lda_dynmem_ind_y_slow zp {
    +debug_dynmem 0
    lda (zp),y
}

!macro sta_dynmem_ind_y zp {
    +debug_dynmem 1
    sta (zp),y
}

!macro sta_dynmem_ind_y_slow zp {
    +debug_dynmem 1
    sta (zp),y
}

}
