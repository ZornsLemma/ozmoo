; Acorn-specific code factored out into its own file for readability.
; SFTODONOW: Maybe rename this file to acorn-something.asm now we've pulled some code out into other acorn-*.asm files

; Note that the code macros defined in here have the suffix "_inline" if control
; flows straight through them or "_subroutine" if they end by executing rts (or
; equivalent).

; A note on Acorn memory models - this affects code in many places, but I have
; to write this somewhere.
;
; The second processor build (ifndef ACORN_SWR) has a simple flat memory model
; with user RAM from $0400-$f7ff inclusive. It's rather like the C64 but without
; even the complication of paging the kernal ROM in and out.
; SFTODO: SAY SOMETHING ABOUT ACORN_TURBO_SUPPORTED
;
; The sideways RAM build (ifdef ACORN_SWR) is a bit more involved. The hardware
; situation here is that we have main RAM (not paged) from $0000-$7fff
; inclusive, with user RAM starting at OSHWM/PAGE, which varies between machines
; but will typically be in the range $e00-$1f00. Some builds hard-code a certain
; address, others use acorn-relocate.asm to accommodate this variation. We also
; have up to 16 different 16K banks of "sideways" RAM paged in at $8000-$bfff
; inclusive. (The BBC series and Electron have different ways to control paging;
; see the acorn_page_in_bank_* macros.) The OS is not paged and lives
; permanently at $c000-$ffff inclusive. The loader will have located any
; available sideways RAM banks, verified there's at least one and put the count
; and a list of bank numbers at ram_bank_{count,list} for us. SFTODO: MINOR QUIBBLE - LOADER WILL HAVE VERIFIED WE HAVE ENOUGH SWR BANKS, THERE MAY BE NONE IF WE CAN GET BY WITHOUT ANY
;
; SFTODO: This is outdated now I'm reworking the bigdyn model, needs rewriting.
; Also the memory hole stuff alters how Electron and B-no-shadow work.
;
; Acorn Ozmoo uses two slightly different sideways RAM models. Both of them
; allow static/high memory to be spread over approximately 9 sideways RAM banks
; (indexed in 512-byte chunks with indexes from 0-254, with chunk 0 starting
; at story_start+nonstored_pages). The standard Ozmoo mempointer (data) and 
; z_pc_mempointer (Z-machine PC) pointers are extended to each have an associated
; RAM bank (mempointer_ram_bank and z_pc_mempointer_ram_bank respectively). (If
; the relevant byte of Z-machine memory lives in main RAM, the bank number is
; irrelevant as main RAM is not affected by paging.)
;
; The "big dynamic RAM" model (ifndef ACORN_SWR_SMALL_DYNMEM) allows the game's
; dynamic memory (which starts in main RAM at story_start, as on any Ozmoo build)
; to be larger than main RAM and overflow into the first 16K sideways RAM bank.
; The first 16K sideways RAM bank therefore has to be paged in by default, so
; that miscellaneous Ozmoo code which might try to access dynamic memory can do
; so without any trouble. In this model, accesses to memory via read_next_byte
; and read_next_byte_at_z_pc temporarily page in the relevant bank to read the
; byte and then page the first 16K sideways RAM bank back in. (As an
; optimisation, read_next_byte_at_z_pc_unsafe* and friends are used during
; instruction decoding to avoid flipping back and forth excessively while
; reading a multi-byte instruction. This is possible because only a very limited
; set of cases can cause accesses to dynamic memory during instruction decoding.)
;
; The "small dynamic RAM" model (ifdef ACORN_SWR_SMALL_DYNMEM) requires the game's
; dynamic memory to fit in main RAM. Since dynamic memory can then be accessed
; regardless of the currently paged in bank, Ozmoo instead keeps the bank
; containing the Z-machine's PC paged in by default, temporarily paging it out
; only when reading a data byte.
;
; On a second processor or BBC series machine with shadow RAM, screen RAM is
; separate from user RAM and doesn't get in the way. On a BBC B with no shadow
; RAM, we use a trick (see ACORN_NO_SHADOW) to relocate the 1K screen RAM to
; $3c00, leave a gap in the Ozmoo binary to accommodate that and we can then
; mostly forget about screen RAM. Dynamic memory starts at story_start just
; after the Ozmoo stack (as on the C64) and is followed (with suitable paging
; for ACORN_SWR) directly by the virtual memory cache.
;
; On the Electron shadow RAM is rare and we can't use the ACORN_NO_SHADOW trick
; to get the screen memory (8K, from $6000-$8000) out of the way. Ozmoo really
; wants dynamic memory to be contiguous, and there isn't really enough RAM free
; between the Ozmoo stack and the screen memory to run all the games we'd like
; to. We therefore compromise by forcing the use of the big dynamic RAM model
; and making dynamic RAM start at $8000 instead of following the Ozmoo stack.
; This limits us to 16K of dynamic memory, which isn't too bad (and is more than
; we'd have free below screen RAM). We use the main RAM between the Ozmoo stack
; and the screen RAM as additional virtual memory cache so it isn't wasted. An
; Electron is therefore about 7K worse off than a BBC B with the same amount of
; sideways RAM as a result of its larger screen memory, in addition to not
; supporting games needing more than 16K of dynamic memory. The Electron save/
; restore code has to be slightly different because the data we need to save/
; restore is no longer contiguous in memory.

; Control a few small debug assertions and similar.
; SFTODO: Make this controllable via the build script?
; SFTODONOW: Should probably do some testing with these on - and make sure I turn them off after!
ACORN_DEBUG_ASSERT = 1 ; SFTODO: PERHAPS RENAME THIS ACORN_DEBUG_EXTRA OR SOMETHING?
DEBUG_BIG_DYNMEM = 1 ; SFTODO: RENAME ACORN_DEBUG_BIG_DYNMEM?

; Macro used to generate an OS error.
!macro os_error error_number, error_message {
    brk
    !byte error_number
    !text error_message, 0
}

; Macro used to catch cases where a supposedly unreachable execution path is
; taken. This is intended for use in discardable init code where we're not too
; space-conscious and so the check can be left in permanently. SFTODO: In some
; cases, maybe it would be better just to rewrite the code to avoid even a
; theoretical possibility of this macro being executed.
!macro assert_discardable_unreached {
    +os_error 0, "Unreachable"
}

; Macro used to catch cases where a supposedly unreachable execution path is
; taken. This is intended for use in space-conscious code and is a no-op unless
; ACORN_DEBUG_ASSERT is defined.
; SFTODONOW: Use this in more places?
!ifndef ACORN_DEBUG_ASSERT {
!macro assert_unreached {
}
} else {
assert_unreached_sub
    +assert_discardable_unreached

!macro assert_unreached {
    jsr assert_unreached_sub
}
}

; Macros used to detect "impossible" values of the carry flag. This are intended
; for use in space-conscious code and is a no-op unless ACORN_DEBUG_ASSERT is
; defined.
; SFTODONOW: Use this in more places?
!ifndef ACORN_DEBUG_ASSERT {
!macro assert_carry_set {
}

!macro assert_carry_clear {
}
} else {
assert_carry_set_sub
    bcs assert_carry_sub_rts
    !byte 0
    !text "C clear", 0
assert_carry_sub_rts
    rts

assert_carry_clear_sub
    bcc assert_carry_sub_rts
    !byte 0
    !text "C set", 0

!macro assert_carry_set {
    jsr assert_carry_set_sub
}

!macro assert_carry_clear {
    jsr assert_carry_clear_sub
}
}

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

; SFTODO: Don't define these in smalldyn? Any code using it is wasting time/space, I think. SFTODONOW: This is probably "potentially important" - I am not at all clear if this is true or not, need to think about it with a clear head - I *think* these are used usefully and correctly in smalldyn case, but review that again before deleting this TODO
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

!zone {

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OS error handling and associated routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

error_handler
    ldy .error_handler_newlines
    beq +
-   lda #13
    jsr .error_handler_print_char
    dey
    bne -
+   ldy #1
-   lda (error_ptr), y
    beq +
    jsr .error_handler_print_char
    iny
    bne - ; Always branch
    ; The following jmp will be patched by code which wants to gain control
    ; after an error.
.error_handler_jmp
+   jmp .press_break

default_error_handler_newlines = 2
.error_handler_newlines !byte default_error_handler_newlines

.press_break
    ; We don't use print_following_string here because we don't want to assume
    ; Ozmoo's own printing mechanisms are properly initialized.
    jsr error_print_following_string
    ; The following string is re-used by clean_up_and_quit_inline; this means a
    ; slight compromise on the punctuation of the output here, but it's not a
    ; big deal and normally this message should never be visible.
press_break_string
    !text cr
    !text "[Press BREAK]", cr, 0
-   jmp -

; Depending on what's happening when an error occurs, we need to output using
; different primitives. We therefore always use this subroutine and it gets
; patched at runtime.
.error_handler_print_char
    jmp s_printchar

; Like print_following_string, but using .error_handler_print_char.
error_print_following_string
    pla
    sta .error_print_following_string_lda + 1
    pla
    sta .error_print_following_string_lda + 2
-   inc .error_print_following_string_lda + 1
    bne +
    inc .error_print_following_string_lda + 2
+
.error_print_following_string_lda
    lda $ffff ; patched by code above
    beq +
    jsr .error_handler_print_char
    jmp -
+   lda .error_print_following_string_lda + 2
    pha
    lda .error_print_following_string_lda + 1
    pha
    rts

error_print_s_printchar = 0
error_print_osasci = 1
.error_print_table_l
    !byte <s_printchar
    !byte <osasci
.error_print_table_h
    !byte >s_printchar
    !byte >osasci

; Allow trapping of errors signalled by the OS via BRKV. Used like this:
;   ldx #2 ; number of newlines to print before any error
;   ldy #n ; type of printing to use if an error occurs (0 s_printchar, 1 osasci)
;   jsr setjmp
;   beq ok
;   ; error occurred, do something
;   jsr set_default_error_handler ; before returning
;   rts
; ok
;   ; do something that might cause an error
;   ; no error occurred
;   jsr set_default_error_handler ; errors after this point aren't our problem
setjmp
    stx .error_handler_newlines
    lda .error_print_table_l,y
    sta .error_handler_print_char + 1
    lda .error_print_table_h,y
    sta .error_handler_print_char + 2
    lda #<.setjmp_error_handler
    sta .error_handler_jmp + 1
    lda #>.setjmp_error_handler
    sta .error_handler_jmp + 2
!ifdef ACORN_SWR {
    ; The OS will page the current language back in on BRK, so we need to save
    ; the current bank and restore it in .setjmp_error_handler.
    lda romsel_copy
    sta jmp_buf_ram_bank
}
    ; We need to save the contents of the stack, because they may be corrupted
    ; when an error message is generated. (They probably won't be, but we can't
    ; rely on it.) As a nice side effect of this, the return address for our
    ; caller is saved so .setjmp_error_handler can simply rts after restoring
    ; the stack.
    tsx
!ifdef TRACE_SETJMP {
    cpx setjmp_min_s
    bcs +
    stx setjmp_min_s
+
}
    stx jmp_buf
.initial_y = (-jmp_buf_size) & $ff
    ldy #.initial_y
-   inx
    beq +
    lda stack,x
    sta jmp_buf+1-.initial_y,y
    iny
    bne - ; branch if we have't overflowed jmp_buf, which shouldn't happen in practice
    ; SFTODO: UNSAFE is not necessarily the best way to control this. Especially
    ; (do another code review) if Ozmoo doesn't use arbitrary amounts of stack
    ; depending on the Z-code being executed, this isn't something that should
    ; be triggered by buggy Z-code; it's internal to the interpreter.
!ifndef UNSAFE {
    ; Y starts at -jmp_buf_size and is incremented every time we store a byte,
    ; so if it reaches 0 will have written jmp_buf_size bytes starting at
    ; jmp_buf+1 and have therefore overflowed the buffer (remember jmp_buf holds
    ; a copy of S). This shouldn't happen in practice, as jmp_buf_size is chosen
    ; to accommodate the largest stack size we will have when setjmp is called.
    lda #ERROR_JMP_BUF_OVERFLOW
    jsr fatalerror
}
+   ; Z flag is set
    rts

.setjmp_error_handler
    ldx jmp_buf
    txs
    ldy #0
-   inx
    beq +
    lda jmp_buf+1,y
    sta stack,x
    iny
    bne -
+   
!ifdef ACORN_SWR {
    ; SFTODO: This could use a "slow" version of the paging macro, but it would be the
    ; only such user with this ram bank so probably no point?
    +acorn_page_in_bank_using_a jmp_buf_ram_bank
}
    lda #1 ; Z flag is clear
    rts

set_default_error_handler
    lda #default_error_handler_newlines
    sta .error_handler_newlines
    lda #<s_printchar
    sta .error_handler_print_char + 1
    lda #>s_printchar
    sta .error_handler_print_char + 2
    lda #<.press_break
    sta .error_handler_jmp + 1
    lda #>.press_break
    sta .error_handler_jmp + 2
.set_default_error_handler_rts
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C64 kernal_readtime emulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The Acorn OS time counter is a 5 byte value, whereas (ignoring the difference
; in resolution) the Commodore one is 3 bytes. Because the Acorn OS time counter
; may have an arbitrarily large value (perhaps some kind of soft RTC solution)
; when we start up and we don't want to zero it, we read the initial value and
; subtract that from any subsequent reads. SFTODO: We could get away without
; this and save a few bytes of runtime code, although it would mean you'd have
; to subtract the initial benchmark time from the final one, and you'd not be
; able to use the initial benchmark time as a measure of startup time.
!macro init_readtime_inline {
    lda #osword_read_clock
    ldx #<initial_clock
    ldy #>initial_clock
    jsr osword
}

; The Acorn kernal_readtime uses 100Hz ticks; a C64 uses 60Hz ticks. Since we
; subtract off the initial value of the OS time it would take 1.94 days from
; game startup for the value returned by kernal_readtime to wrap round, slightly
; quicker than a C64 at 3.24 days (from reset), but still not a concern. I think
; the Ozmoo timer code would handle a wraparound transparently anyway.
kernal_readtime
.current_clock = scratch_page
    lda #osword_read_clock
    ldx #<.current_clock
    ldy #>.current_clock
    jsr osword
    ldx #(256-5)
    sec
-   lda .current_clock-(256-5),x
    sbc (initial_clock-(256-5)) and $ffff,x ; 'and' in case initial_clock in zp
    sta .current_clock-(256-5),x
    inx
    bne -
    ldy .current_clock+2
    ldx .current_clock+1
    lda .current_clock+0
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Miscellaneous utility routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!macro clean_up_and_quit_inline {
!ifndef ACORN_ON_QUIT_COMMAND {
    lda #<press_break_string
    ldy #>press_break_string
    jsr printstring
-   jmp -
} else {
    !ifdef ACORN_ON_QUIT_COMMAND_SILENT {
        lda #vdu_disable
        jsr oswrch
    }
    ldx #<.on_quit_command
    ldy #>.on_quit_command
    jsr oscli
    ldx #<.basic_command
    ldy #>.basic_command
    jmp oscli ; never returns
.on_quit_command
    !source "../temp/on-quit-command.asm"
.basic_command
    !text "BASIC", 13
}
}

!ifndef ACORN_SWR {
    ; Re-enter the current language.
re_enter_language
    lda #osbyte_enter_language
re_enter_language_ldx_imm
    ldx #$ff ; patched by initialisation code
    jsr osbyte ; never returns
}

; Like printstring_raw, but using OSASCI.
printstring_os
    stx .printstring_os_lda + 1
    sta .printstring_os_lda + 2
-
.printstring_os_lda
   lda $ffff
   beq .calculate_crc_rts
   jsr osasci
   inc .printstring_os_lda + 1
   bne -
   inc .printstring_os_lda + 2
   bne - ; Always branch

; Calculate a CRC over A bytes of data at YX (A=0 => 256 bytes), returning it in
; YX.
calculate_crc
; This must not use the same location as .result in acorn-disk.asm as otherwise
; when .get_game_disc_back calls calculate_crc it will be overwritten.
.crc = zp_temp + 2 ; 2 bytes 
    sta .cpy_imm + 1
    stx .eor_abs + 1
    sty .eor_abs + 2
    lda #0
    sta .crc + 1
    sta .crc
    tay
.nbyt
    lda .crc + 1
.eor_abs
    eor $ffff,y ; patched by code above
    sta .crc + 1
    ldx #8
.loop
    lda .crc + 1
    rol
    bcc .b7z
    lda .crc + 1
    eor #8
    sta .crc + 1
    lda .crc
    eor #$10
    sta .crc
.b7z
    rol .crc
    rol .crc + 1
    dex
    bne .loop
    iny
.cpy_imm
    cpy #$ff
    bne .nbyt
    ldx .crc
    ldy .crc + 1
.calculate_crc_rts
    rts

; Two wrappers for calling osbyte_set_cursor_editing to reduce code size; we do
; this in several places.
; SFTODO: Take "osbyte" out of these names, now they may set this indirectly on
; USE_HISTORY builds?
do_osbyte_set_cursor_editing_x_0
    ldx #0
do_osbyte_set_cursor_editing
!ifndef USE_HISTORY {
    lda #osbyte_set_cursor_editing
    bne do_osbyte_y_0 ; Always branch
} else {
    ; We write X to nominal_cursor_key_status and let our INSV handler take care
    ; of the actual *FX4 state.
!ifdef ACORN_SWR {
    stx nominal_cursor_key_status
    rts
} else {
    ; We use temporary workspace in zero page here to keep the code size down.
    lda #<nominal_cursor_key_status
    sta osbyte_set_cursor_editing_tmp + 0
    lda #>nominal_cursor_key_status
    sta osbyte_set_cursor_editing_tmp + 1
    lda #$ff
    sta osbyte_set_cursor_editing_tmp + 2
    sta osbyte_set_cursor_editing_tmp + 3
    stx osbyte_set_cursor_editing_tmp + 4
    lda #osword_write_host
    ldx #<osbyte_set_cursor_editing_tmp
    ldy #>osbyte_set_cursor_editing_tmp
    jmp osword
}
}

; Two wrappers for calling osbyte_rw_escape_key to reduce code size; we do this
; in several places.
do_osbyte_rw_escape_key
    lda #osbyte_rw_escape_key
do_osbyte_y_0
    ldy #0
    jmp osbyte

; Move the OS cursor to (X, Y).
do_oswrch_vdu_goto_xy
    lda #vdu_goto_xy
    jsr oswrch
    txa
    jsr oswrch
    tya
    jmp oswrch

; SF: ENHANCEMENT: It would potentially be possible to support bold and
; underlined text in modes other than 7, although we'd either need to do it via
; OSWORD 10 and UDG redefinition (which is probably quite slow) or install
; some kind of custom driver around OSWRCH and have that run on the host even if
; we're using a second processor. As a "cheap" version, it would be possible to
; use colours in mode 1 for bold, but I'm not particularly keen to support this
; just for one mode which probably no one would use.
;
; SF: ENHANCEMENT: It would also with a bit more effort probably be possible
; to use a proportionally spaced font in modes other than 7, though this would
; be a more intrusive code change.
;
; SFTODO: Once the upstream dynamic memory access rework is completed and merged, it *may* be possible to do things like (on a turbo copro) use bank 0 free memory plus the whole of bank 1 for dynamic memory, and have banks 2 and 3 for virtual memory cache, or to promote the whole of a game <= ~210K to be pure dynamic memory. Or if we have more main+SWR than the game size, to promote the whole game to dynamic memory and do a simple absolute Z-machine to bank address conversion. I suspect it won't allow >64K dynamic memory, but just making this note so I can consider it when I see how upstream change works.

; SFTODO: If the bigdyn model where Z-machine PC bank is paged in by default works out, it may be a worthwhile saving for global variable accesses to avoid paging in the dynmem bank if we know the global variables live below $8000. ("know" might ultimately mean an initialisation-time check and dynamic code patching, but before that it might mean a cheap check in the code doing the global var access to avoid the paging. although the paging is maybe - do the cycle counting - cheap enough that by the time we do even a cheap check, we're not going to come out ahead.)

}

; SFTODO: Once things settle down and I don't have immediate plans to add new features, it would be good to look for opportunities to shrink the code - particularly on builds for smaller machines - as squeezing out an extra one or two 512 byte blocks of vmem cache in main RAM might make all the difference. Due to alignment it doesn't always need that much, a few bytes may tip it over the next alignment boundary.

; SFTODO: It's maybe a bit hacky, but perhaps we could offer an option to use pages 9 and &A on a B/B+ and those plus pages &B and &C on a Master as vmem cache if the user wants.

; SFTODO: We could perhaps avoid a proliferation of versions because of ROM paging by writing the code as "JSR page_in_bank_a:NOP:NOP:..." etc (wrapped in macros of course) and have those subroutines peek the return address from the stack and patch the JSR up to do the correct paging for the current platform directly. Of course the code size might vary, so the most likely use of this would be a) a third party model B SWR system which required no more code than standard paging b) making a BBC+Electron joint executable which penalises the Electron by making it JSR to subroutines to do paging but does direct paging on BBC. (The original subroutines would patch the JSR to a JSR to the Electron-specific subroutine on the Electron or the direct hardware code on the BBC.) Maybe a worthwhile idea in some other cases too.

; SFTODO: Perhaps do some timings to see how much of an impact replacing direct SWR paging code with a JSR to that same code has. It's probably significant, but it may be that some of the optimisations in Ozmoo over time mean this actually isn't a huge performance overhead, which would help make executables more shareable across different machines.

; SFTODO: Would there be any value in always using $400-$800 as two pages of VM cache, and using $900-B00 plus space allocated within the binary itself to substitute for existing uses of $400-800?

; SFTODONOW: Should I tweak the settings for the bRKV handler during the initial load so it uses an extra leading newline or two?

; SFTODONOW: This file has been extensively modified and I should review all SFTODOs to see if they need promoting to SFTODONOW
