; Acorn-specific code factored out into its own file for readability.

; Note that the code macros defined in here have the suffix "_inline" if control
; flows straight through them or "_subroutine" if they end by executing rts (or
; equivalent).

; A note on Acorn memory models - this affects code in many places, but I have
; to write this somewhere.
;
; The second processor build (ifndef ACORN_SWR) has a simple flat memory model
; with user RAM from $0400-$f7ff inclusive. It's rather like the C64 but without
; even the complication of paging the kernal ROM in and out, so it doesn't need
; the cache which the C64 code uses when ALLMEM is defined. SFTODO: SAY SOMETHING ABOUT ACORN_TURBO_SUPPORTED
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
; at story_start+nonstored_blocks). The standard Ozmoo mempointer (data) and 
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
; ACORN_DEBUG_ASSERT = 1 ; SFTODO: PERHAPS RENAME THIS ACORN_DEBUG_EXTRA OR SOMETHING?
; DEBUG_BIG_DYNMEM = 1 ; SFTODO: RENAME ACORN_DEBUG_BIG_DYNMEM?

; Zero page allocations for the initial load of game data.
dir_ptr = zp_temp ; 2 bytes
game_blocks = zp_temp + 2 ; 2 bytes
!ifndef ACORN_SWR_MEDIUM_DYNMEM {
catalogue = scratch_overlapping_game_start
} else {
; story_start will be in sideways RAM; we could make this work, but we'd need to
; make sure the right bank was paged in and it's simpler just to use
; scratch_double_page. We can't simply always use that, because it doesn't exist
; on second processor builds.
catalogue = scratch_double_page
}
; SFTODO: Probably not, but can the existence of vmap_sort_entries help simplify the normal tube+cache loading code?
vmap_sort_entries = vmem_temp ; 1 byte
!ifdef ACORN_TUBE_CACHE {
host_cache_size = memory_buffer
}
!ifdef VMEM {
ram_blocks = dir_ptr ; 2 bytes
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

; SF: These macros will alter the carry, unlike a raw lda/sta (zp),y. The store
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
    stx .SFTODO344LDA_ZP_IND_Y+1
    ldx screen_hole_tmp
.SFTODO344LDA_ZP_IND_Y
    lda ($00),y ; patched at runtime
    rts
}

!macro SFTODORENAMEME zp {
    stx screen_hole_tmp
    ldx #zp
    !if zp = 0 {
        beq lda_dynmem_ind_y_slow_x_sub ; always branch
    } else {
        bne lda_dynmem_ind_y_slow_x_sub ; always branch
    }
}

lda_dynmem_ind_y_slow_zp_mempos_sub
	+SFTODORENAMEME zp_mempos

lda_dynmem_ind_y_slow_string_array_sub
	+SFTODORENAMEME string_array

lda_dynmem_ind_y_slow_parse_array_sub
	+SFTODORENAMEME parse_array

lda_dynmem_ind_y_slow_default_properties_ptr_sub
	+SFTODORENAMEME default_properties_ptr

lda_dynmem_ind_y_slow_z_low_global_vars_ptr_sub
	+SFTODORENAMEME z_low_global_vars_ptr

!zone {
sta_dynmem_ind_y_slow_object_tree_ptr_sub
    stx screen_hole_tmp_slow
    ldx #object_tree_ptr
SFTODO3X1
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
    stx .SFTODO344STA_ZP_IND_Y + 1
    ldx screen_hole_tmp_slow
    lda screen_hole_tmp
.SFTODO344STA_ZP_IND_Y
    sta ($00),y ; patched at runtime
    rts
}

!macro SFTODOALSORENAMEME zp {
    stx screen_hole_tmp_slow
    ldx #zp
    !if zp = 0 {
        beq SFTODO3X1 ; always branch
    } else {
        bne SFTODO3X1 ; always branch
    }
}

sta_dynmem_ind_y_slow_zp_mempos_sub
	+SFTODOALSORENAMEME zp_mempos

sta_dynmem_ind_y_slow_string_array_sub
	+SFTODOALSORENAMEME string_array

sta_dynmem_ind_y_slow_parse_array_sub
	+SFTODOALSORENAMEME parse_array

sta_dynmem_ind_y_slow_z_low_global_vars_ptr_sub
	+SFTODOALSORENAMEME z_low_global_vars_ptr

sta_dynmem_ind_y_slow_z_high_global_vars_ptr_sub
	+SFTODOALSORENAMEME z_high_global_vars_ptr

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

; SF: In the Acorn port, I've deliberately renamed before_dynmem_read and
; after_dynmem_read so that any upstream changes which use these macros will
; fail to build and force me to inspect them manually. The Commodore versions
; don't modify any registers or flags (except I), but that would cost time
; and space on the Acorn port.

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
; Initialization and finalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialization code which may be placed after the end of the Z-machine stack;
; this means it will be overwritten as soon as anything is loaded at
; story_start.
!macro acorn_init_code_overlapping_game_data {
; Initialization performed very early during startup.
deletable_init_start
    ldx #1
    jsr do_osbyte_rw_escape_key

    jsr screenkernal_init

    ; Now screenkernal_init has been executed, it's safe to call s_printchar, so
    ; install our own error handler which will use s_printchar by default. No
    ; error should be able to occur before this point. If an error occurs during
    ; a restart, which will re-run the executable, there's not much we can do
    ; but it's probably OK because the load will just replace the code with an
    ; identical copy.
    lda #<error_handler
    sta brkv
    lda #>error_handler
    sta brkv + 1

!ifdef ACORN_CURSOR_PASS_THROUGH {
    ldx #1
    jsr do_osbyte_set_cursor_editing
}

!ifdef ACORN_FUNCTION_KEY_PASS_THROUGH {
    ; We're going to generate ZSCII codes for the unshifted function keys. We
    ; choose a base of 133 (=ZSCII f1) for f0 because if we set a base of 132 so
    ; Acorn f1=ZSCII f1, Acorn f0 would act like cursor right. If we want Acorn
    ; f1=ZSCII f1 we'll fix that up in the translation table. SFTODO: I HAVEN'T ADDED THIS OPTION TO THE TRANSLATION TABLE YET
    lda #osbyte_rw_function_key_status
    ldx #133
    jsr do_osbyte_y_0

    ; In order to allow the use of *KEY expansions, we'll make the shifted
    ; function keys generate those.
    lda #osbyte_rw_shift_function_key_status
    ldx #1 ; expand as normal soft key
    jsr do_osbyte_y_0
}

    ldx #0
    stx mempointer
!ifndef ACORN_SWR {
    ; Patch re_enter_language to enter the current language; reading it here
    ; saves a few bytes of non-deletable code.
    lda #osbyte_read_language
    ; X is already 0
    ldy #$ff
    jsr osbyte
    stx re_enter_language_ldx_imm + 1

    ; On a second processor, a soft break will transfer control back to our
    ; execution address. We will have thrown away our initialization code by
    ; that point and can't restart properly. In order to avoid random behaviour
    ; and probably a crash, we patch the jmp at the start of the executable to
    ; transfer control to re_enter_language. This means that although the
    ; language name gets printed twice, a soft break otherwise gives a clean
    ; result similar to that on a non-second processor. This code is harmless
    ; if we're not running on a second processor, but it's not necessary either.
    lda #<re_enter_language
    sta initial_jmp + 1
    lda #>re_enter_language
    sta initial_jmp + 2
}

!ifdef ACORN_TURBO_SUPPORTED {
    ; This executable doesn't have a ROM header indicating we want the turbo
    ; mode enabled, so it will have been disabled when were were executed. Turn
    ; it back on if we do want it.
    bit is_turbo
    bpl .dont_enable_turbo
    lda #$80
    sta turbo_control
.dont_enable_turbo
}

!ifdef VMEM {
    ; Copy the low bytes of the vmap at initial_vmap_z_l to vmap_z_l.
    ldx #vmap_max_size
-   lda initial_vmap_z_l - 1,x
    sta vmap_z_l - 1,x
    dex
    bne -

    ; SFTODO: We could also do the initialisation of quick_index here, saving a few
    ; bytes of stack space code.
}

!ifdef ACORN_SCREEN_HOLE {
    lda screen_mode
    ora #128 ; force shadow mode on SFTODO  MAGIC CONSTANT IN A FEW PLACES NOW?
    tax
    lda #osbyte_read_screen_address_for_mode
    jsr osbyte
    cpy #$80
    bcc .not_shadow_mode
    ; If we're in a shadow mode, the screen hole isn't needed; by setting the
    ; start page as high as possible, code which implements the screen hole will
    ; realise ASAP that it's not necessary. (If we didn't do this, we'd end up
    ; with a zero-size screen hole at $8000, which would work but but slower.)
    ldy #$ff
.not_shadow_mode
    sty acorn_screen_hole_start_page
    dey
    sty acorn_screen_hole_start_page_minus_one
    sec
    lda #>flat_ramtop
    sbc acorn_screen_hole_start_page
    bcs +
    lda #0 ; shadow RAM case (>flat_ramtop - $ff caused a borrow)
+   sta acorn_screen_hole_pages
    tax
    dex
    stx acorn_screen_hole_pages_minus_one
}

!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; This is used enough it's worth - if only for the code size saving -
    ; copying it into zero page. (Well, it saves 10 bytes at the time of
    ; writing; not huge, but not too bad. SFTODO: Maybe reconsider this later.)
    lda ram_bank_list
    sta dynmem_ram_bank
}

!ifdef ACORN_SHADOW_VMEM {
    ; SFTODONOW: IF WE HAVE "UNSUPPORTED" SHADOW RAM, WE MUST NOT TRY TO USE IT. I DON'T KNOW IF THE LOADER WILL INDICAT THIS TO US (BY SETTING VMEM_CACHE_COUNT_MEM TO 0, AND WE MUSTN'T TRAMPLE OVER IT AS WE CURRENTLY DO JUST BELOW) OR IF WE WILL DECIDE, BUT NEED TO DO SOMETHING.
    ; Set vmem_cache_count_mem to the number of 256-byte cache entries we have for
    ; holding data copied out of shadow RAM.
    ;
    ; If we're in mode 0, there's no spare shadow RAM anyway. The loader won't have
    ; allocated any space, but we might have one page available if we happened to
    ; load at PAGE+256, and we mustn't let that mislead us.
    ; SFTODONOW: MAKE SURE WE RESPECT THIS BEING 0 AND DON'T DO UNNECESSARY WORK OR CRASH IF IT IS 0! REVIEW CODE REFERENCING vmem_cache_count_mem AFTERWARDS TO CHECK... WHILE I'M AT IT, REVIEW TO CHECK HAVING JUST 1 OR 2 PAGES OF CACHE WILL ALSO WORK (NOT CRASH, THE PERFORMANCE MIGHT BE TERRIBLE OF COURSE)
    lda #0
    sta vmem_cache_count_mem
    lda screen_mode
    beq .mode_0
    ; We have as many pages of cache as there are between PAGE and
    ; program_start. In practice this is whatever the loader deliberately set
    ; aside for us plus maybe an extra page if we had to relocate down to
    ; PAGE+256 to keep the right alignment.
    lda #osbyte_read_oshwm
    jsr osbyte
    sty vmem_cache_start_mem
    lda #>program_start
    sec
    sbc vmem_cache_start_mem
    sta vmem_cache_count_mem
.mode_0
}

    +prepare_static_high_memory_inline
    +init_readtime_inline
    jmp init_cursor_control

; Do as much as we can to prepare for the initial load of game data from disk,
; without actually loading anything (which would overwrite this code).
prepare_for_initial_load
!ifdef TRACE_FLOPPY {
    ; Call streams_init so the tracing is able to show the readblocks calls
    ; performed here.
	jsr streams_init
}

!ifndef ACORN_ADFS {
    ; Examine the disc catalogue and determine the first sector occupied by the
    ; DATA file containing the game.
    ; Because this is initialisation code, we know the following variables are
    ; already set to predictable values. This optimisation was useful at one
    ; point but it's not really important right now; it isn't too confusing so
    ; let's keep it anyway now it's been tested.
!if 0 {
    lda #2
    sta readblocks_numblocks
    lda #0
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
    sta readblocks_mempos ; catalogue is page-aligned
!ifdef ACORN_TURBO_SUPPORTED {
    ; In reality this is redundant but let's play it safe.
    sta readblocks_mempos + 2
}
}
    lda #>catalogue
    sta dir_ptr + 1
    sta readblocks_mempos + 1
    lda #0
    sta readblocks_base
!ifndef ACORN_DSD {
    sta readblocks_base + 1
}
    ; Note that because we're reading the first few sectors, this works
    ; correctly whether this is an ACORN_DSD build or not.
    jsr readblocks
    lda #8
    sta dir_ptr
.find_file_loop
    ldy #0
    ldx #(-8 & $ff)
.name_compare_loop
    lda (dir_ptr),y
    ; The directory name will have the top bit set iff the file is locked; we
    ; don't care about that here, so we need to strip it off. It's harmless to
    ; just do this for all characters.
    and #$7f
    cmp .data_filename-(-8 & $ff),x
    bne .file_not_found
    iny
    inx
    beq .file_found
    bne .name_compare_loop
.data_filename
    !text "DATA   $"
.file_not_found
    clc
    lda dir_ptr
    adc #8
    sta dir_ptr
    bne .find_file_loop
    brk
    !byte 0
    !text "DATA not found"
    !byte 0
.file_found
    ; We found the file's name using sector 0, we now want to look at the
    ; corresponding part of sector 1. Adjust dir_ptr for convenience.
    inc dir_ptr + 1
    ; Determine the start sector of the DATA file and set readblocks_base.
    ldy #6
    lda (dir_ptr),y
    and #$3
!ifndef ACORN_DSD {
    sta readblocks_base + 1
    iny
    lda (dir_ptr),y
    sta readblocks_base
} else {
    sta dividend + 1
    iny
    lda (dir_ptr),y
    sta dividend
    lda #0
    sta divisor + 1
    lda #10
    sta divisor
    jsr divide16
    lda division_result
    sta readblocks_base
}
    ; Determine the length of the DATA file in blocks.
    ldy #6
    lda (dir_ptr),y
    and #%00110000
    lsr
    lsr
    lsr
    lsr
    sta game_blocks + 1 ; high byte of length in blocks
    dey
    lda (dir_ptr),y
    sta game_blocks ; low byte of length in blocks
    dey
    lda (dir_ptr),y ; low byte of length in bytes
    beq +
    inc game_blocks
    bne +
    inc game_blocks + 1
+
!ifdef ACORN_DSD {
    ; If this is a double-sided game, there will be *approximately* (definitely
    ; no more, possibly a track's worth of data less) the same amount of data
    ; on the second side. We don't look up :2.$.DATA and determine its length,
    ; we just double game_blocks. The absolute worst case here is we read a
    ; track's worth of junk which won't be accessed because it's past the end
    ; of the game.
    asl game_blocks
    rol game_blocks + 1
!ifndef VMEM {
    ; If we don't have virtual memory, the logic below to cap game_blocks at
    ; ram_blocks won't kick in. Since we don't have virtual memory, we know the
    ; game will fit in RAM - but due to the doubling of game_blocks we just did,
    ; it might be larger than RAM, causing us to read too much and corrupt
    ; things. TODO: If we simply passed in the game size as a build parameter
    ; this sort of thing would go away.
    lda #0
    sta game_blocks + 1
    lda #>(flat_ramtop - story_start)
    cmp game_blocks
    bcs +
    sta game_blocks
+
}
}
} else { ; ACORN_ADFS
    lda #<game_data_filename
    sta scratch_page
    lda #>game_data_filename
    sta scratch_page + 1
    lda #osfile_read_catalogue_information
    ldx #<scratch_page
    ldy #>scratch_page
    jsr osfile
    bne +
    ; The wording of this error is technically incorrect - we're trying to read
    ; information about the file, not open it - but I don't think it's a big
    ; deal. It shouldn't be happening at all, of course.
    jmp cant_open_data_error
+   lda scratch_page + $a
    beq +
    inc scratch_page + $b
    bne +
    inc scratch_page + $c
+   lda scratch_page + $b
    sta game_blocks
    lda scratch_page + $c
    sta game_blocks + 1
}

    ; If game_blocks is odd, increment it by one so the game data is always
    ; considered to be a multiple of 512 bytes. This avoids having to worry
    ; about some corner cases and doesn't cause any problems; on DFS we're doing
    ; raw sector reads and the extra sector will always exist, on ADFS we may try
    ; to do a 512-byte read when only 256 bytes are available but that's fine.
    lda game_blocks
    and #1
    beq +
    inc game_blocks
    bne +
    inc game_blocks + 1
+

SFTODOXX89
!ifdef VMEM {
    ; How much RAM do we have available for game data?
    ; We have 64 (2^6) 256-byte blocks per sideways RAM bank, if we have any.
    lda #0
    sta ram_blocks + 1
!ifdef ACORN_SWR {
    lda ram_bank_count
    ldx #6
-   asl
    rol ram_blocks + 1
    dex
    bne -
}
    sta ram_blocks

!ifdef ACORN_SHADOW_VMEM {
    ; We may have some additional RAM blocks in shadow RAM not being used for the
    ; screen display.
SFTODOLM2
    lda vmem_cache_count_mem
    beq .no_spare_shadow
    lda #osbyte_read_screen_address_for_mode
    ldx screen_mode ; note we don't force shadow mode on here
    jsr osbyte
    ; SFTODONOW: I wonder if this will go wrong for things like Electron Master RAM board, where shadow can't be turned off by software and so this OSBYTE probably always returns $8000. For now let me deliberately hang if this happens - if this is the case, we just need to use a hard-coded table of start addresses rather than this OSBYTE, not a big deal.
    tya
!if 1 { ; SFTODO TEMP
-   bmi -
}
    sec
    sbc #$30 ; SFTODO MAGIC NUMBER USED IN COUPLE OF PLACES
    clc
    adc ram_blocks
    sta ram_blocks
    bcc +
    inc ram_blocks + 1
+
.no_spare_shadow
}

!ifdef ACORN_TUBE_CACHE {
    ; We have some blocks of cache in the host, which aren't directly accessible
    ; but are almost as good as our own RAM and which will benefit from
    ; preloading if we're on a normal second processor. We count them for now so
    ; we process more of the initial vmap and fix up the inflated value of
    ; vmap_max_entries later.
    lda screen_mode
    ora #128 ; force shadow mode on
    tax
    lda #osbyte_initialise_cache
    jsr osbyte
    stx host_cache_size
!ifdef ACORN_TURBO_SUPPORTED {
    ; A turbo second processor has enough RAM to preload everything in vmap
    ; without touching the host cache. The host cache will still work, but we
    ; don't have anything to preload into it, so having initialised it there's
    ; nothing else to do.
    bit is_turbo
    bmi .host_cache_initialised
}
    ; X is cache size in 512-byte blocks, but we want to count 256-byte blocks here.
    txa
    asl
    rol ram_blocks + 1
    sta ram_blocks
.host_cache_initialised
SFTODOLABELX1
}

!ifdef ACORN_TURBO_SUPPORTED {
    ; On a turbo second processor, we will use all 128K in banks 1 and 2 as
    ; virtual memory cache.
    bit is_turbo
    bpl .dont_count_turbo_ram
    inc ram_blocks + 1
    inc ram_blocks + 1
.dont_count_turbo_ram
}

    ; We also have some blocks between flat_ramtop and data_start.
    ; SF: We're doing a constant subtraction in code here, but a) this is
    ; deletable init code so it doesn't really cost anything b) if we don't,
    ; the relocation code fails because we have a variation which doesn't follow
    ; the simple fixed relationship we expect.
    lda #(>flat_ramtop)
    sec
    sbc #>data_start
!ifdef ACORN_SCREEN_HOLE {
    sec
    sbc acorn_screen_hole_pages
}
    clc
    adc ram_blocks
    sta ram_blocks
    bcc +
    inc ram_blocks + 1
+

!ifdef ACORN_SWR {
    ; This value might be changed below.
    lda #0
    sta vmem_blocks_in_main_ram
}

    ; ram_blocks now contains the number of 256-byte blocks of RAM we have
    ; available, including RAM which will be used for dynamic memory. The build
    ; system and the loader will have worked together to guarantee that:
    ; - ram_blocks >= ACORN_INITIAL_NONSTORED_BLOCKS + 2*vmem_block_pagecount,
    ;   i.e. that we have enough RAM for the game's dynamic memory and two
    ;   512-byte blocks of virtual memory cache.
    ; - the game always has at least one block of non-dynamic memory.

    ; In order to avoid accessing nonexistent game data in an attempt to use all
    ; that RAM, set ram_blocks = min(ram_blocks, game_blocks).
SFTODOEE2
    ldx game_blocks + 1
    lda game_blocks
    cpx ram_blocks + 1
    bne +
    cmp ram_blocks
+   bcs +
    stx ram_blocks + 1
    sta ram_blocks
+

    ; Now we know how much data we are going to load (ram_blocks' worth), we can
    ; calculate how many blocks correspond to each progress indicator position.
    ; SFTODONOW: ISN'T THERE A RISK HERE (AND WAS EVEN BEFORE I ADDED THE SHADOW VMEM STUFF? OR DID THE RESTRICTION TO 9 BANKS OF SWR PLUS DYNMEM ADJUST - ALTHO THAT COULD BE DISABLED FROM CMDLINE! - MEAN THIS *WAS* SAFE BEFORE, BUT ISN'T NOW?????) THAT RAM_BLOCKS-ADJUSTED_NONSTORED_BLOCKS IS MORE MEMORY THAN THE VMAP CAN ADDRESS, AND THEREFORE WE *AREN'T* NEC GOING TO LOAD RAM_BLOCKS' WORTH OF DATA? IF I'M RIGHT, IS THIS ONLY AN ISSUE FOR THE PROGRESS BAR OR DOES IT HAVE MORE SERIOUS CONSEQUENCES? THIS WOULD PROBABLY ONLY SHOW UP FOR GAMES>=127K+THEIRDYNMEM LONG (PROB NEED TO BE LONGER, TO ALLOW FOR OUR DYNMEM ADJUST), AND I PROBABLY HAVEN'T TESTED WITH SUCH A GAME RECENTLY, IF AT ALL.
    jsr init_progress_indicator

    ; Set nonstored_blocks to the number of 256-byte blocks of RAM we are going
    ; to treat as dynamic memory. This is normally the game's actual dynamic
    ; memory rounded up to a 512-byte boundary, i.e.
    ; ACORN_INITIAL_NONSTORED_BLOCKS.
    lda #ACORN_INITIAL_NONSTORED_BLOCKS
    sta nonstored_blocks
!ifdef VMEM {
!ifndef ACORN_NO_DYNMEM_ADJUST {
!ifdef ACORN_TURBO_SUPPORTED {
    ; SFTODO: REVIEW THIS FRESH
    ; On a turbo second processor, we can increase nonstored_blocks to promote
    ; some additional data into dynamic memory and make full use of bank 0. We
    ; don't need to keep any of bank 0 free for virtual memory cache because we
    ; have banks 1 and 2 for that. So we set nonstored_blocks = min(game_blocks
    ; - vmem_block_pagecount, available blocks in bank 0); see below for why we
    ; subtract vmem_block_pagecount. This subtraction can't cause
    ; nonstored_blocks < ACORN_INITIAL_NONSTORED_BLOCKS because the build system
    ; guarantees the game has at least one block of non-dynamic memory. The
    ; subtraction is otherwise harmless; it just means that for small games one
    ; 512-byte block of RAM will have to be accessed via the slower virtual
    ; memory code when it could maybe have been promoted to be dynamic memory.
    ;
    ; This adjustment will make it faster to access the part of the game which
    ; has been promoted into dynamic memory, so we do it even if this is a small
    ; game and we could address enough RAM for the entire game without it.
    bit is_turbo
    bpl .no_turbo_dynmem_adjust
SFTODOLABEL1
    lda game_blocks
    sec
    sbc #vmem_block_pagecount
    tax
    lda game_blocks + 1
    sbc #0
    bne .available_blocks_is_smaller
    cpx #>(flat_ramtop - story_start)
    bcc game_blocks_is_smaller
.available_blocks_is_smaller
    ldx #>(flat_ramtop - story_start)
game_blocks_is_smaller
    stx nonstored_blocks
.no_turbo_dynmem_adjust
}
!ifdef ACORN_SWR {
    ; It may be useful to increase nonstored_blocks to promote some additional
    ; data into dynamic memory, either for performance or to make use of more
    ; sideways RAM. We must not make it too large for the memory model we're
    ; using. (This optimisation has relatively limited scope in the small or
    ; medium model, but it's trivial to support it anyway. It's just conceivable
    ; some games and/or machines might benefit from --force-big-dynmem to give
    ; this optimisation more headroom, but of course the big model has its own
    ; performance drawbacks so it's probably best not using it unless we're
    ; forced to.)
.max_dynmem = zp_temp + 4 ; 1 byte
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    lda #>swr_ramtop
} else {
    lda #>flat_ramtop
}
!ifndef ACORN_SWR_MEDIUM_DYNMEM {
!ifdef ACORN_SCREEN_HOLE {
    sec
    sbc acorn_screen_hole_pages
}
}
    sec
    sbc #>story_start
    sta .max_dynmem

    ; If game_blocks == ram_blocks, we want to set nonstored_blocks as high as
    ; possible; there's no downside as we have enough RAM for the entire game
    ; and this will allow as much of the game as possible to be accessed via the
    ; faster dynamic memory code path. Set nonstored_blocks = min(game_blocks -
    ; vmem_block_pagecount, .max_dynmem).
    ldy ram_blocks + 1
    lda ram_blocks
    cpy game_blocks + 1
    bne game_blocks_ne_ram_blocks
    cmp game_blocks
    bne game_blocks_ne_ram_blocks
    sec
    sbc #vmem_block_pagecount
    tax
    tya
    sbc #0
    bne .max_dynmem_is_smaller
    cpx .max_dynmem
    bcc game_blocks_is_smaller
.max_dynmem_is_smaller
    ldx .max_dynmem
game_blocks_is_smaller
    bne .dynmem_adjust_done ; Always branch

game_blocks_ne_ram_blocks
    ; Note that we can't have game_blocks < ram_blocks because we reduced
    ; ram_blocks to match earlier, so game_blocks > ram_blocks. We don't want
    ; to reduce flexibility by locking parts of the game into RAM instead of
    ; allowing the virtual memory system to choose what lives in RAM. It's only
    ; a clear win to increase nonstored_blocks if it brings otherwise unusable
    ; RAM into play. Set nonstored_blocks =
    ; max(min(ram_blocks - vmap_max_size * vmem_block_pagecount, .max_dynmem),
    ;     ACORN_INITIAL_NONSTORED_BLOCKS)
.min_lhs_sub = vmap_max_size * vmem_block_pagecount
    sec
    sbc #<.min_lhs_sub
    tax
    tya
    sbc #>.min_lhs_sub
    bmi .use_acorn_initial_nonstored_blocks
    bne .use_min_rhs
    cpx .max_dynmem
    bcc .use_min_lhs
.use_min_rhs
    ldx .max_dynmem
.use_min_lhs
    cpx #ACORN_INITIAL_NONSTORED_BLOCKS
    bcs .use_max_lhs
.use_acorn_initial_nonstored_blocks
    ldx #ACORN_INITIAL_NONSTORED_BLOCKS
.use_max_lhs
.dynmem_adjust_done
    stx nonstored_blocks
}
}

    ; Set ram_blocks -= nonstored_blocks, i.e. set ram_blocks to the number of
    ; RAM blocks we have available as virtual memory cache.
    lda ram_blocks
    sec
    sbc nonstored_blocks
    sta ram_blocks
    bcs +
    dec ram_blocks + 1
+
    ; SFTODO: REVIEW ALL THE FOLLOWING FRESH, ESP "NEW" SWR CASE
    ; It's important ram_blocks >= vmem_block_pagecount now so that we will set
    ; vmap_max_entries >= 1 below. Conceptually it makes sense for
    ; vmap_max_entries to be 0 but in practice lots of code assumes it isn't.
    ;
    ; The build system and loader work together to guarantee (initial)
    ; ram_blocks >= ACORN_INITIAL_NONSTORED_BLOCKS + 2 * vmem_block_pagecount.
    ; If nonstored_blocks has not been adjusted, there are two cases:
    ; a) If we didn't set ram_blocks = game_blocks above, the build system and
    ;    loader guarantee means we now have ram_blocks >= 2 *
    ;    vmem_block_pagecount. QED.
    ; b) If we did set ram_blocks = game_blocks above, we know that the
    ;    game has at least one block of non-dynamic memory, so before the
    ;    subtraction we had ram_blocks = game_blocks >=
    ;    ACORN_INITIAL_NONSTORED_BLOCKS + vmem_block_pagecount. QED.
    ;
    ; On a turbo second processor, we may have adjusted nonstored_blocks. There are
    ; two cases:
    ; a) If we didn't set ram_blocks = game_blocks above, as nonstored_blocks
    ;    lives in bank 0 and we also have banks 1 and 2 for virtual memory
    ;    cache, after the subtraction we have ram_blocks ~= 128K >=
    ;    vmem_block_pagecount. QED.
    ; b) If we did set ram_blocks = game_blocks above, combine that with the
    ;    fact the adjustment to nonstored_blocks left nonstored_blocks <=
    ;    game_blocks - vmem_block_pagecount. QED.
    ;
    ; On a sideways RAM build, we may have adjusted nonstored_blocks. There are
    ; two cases:
    ; a) If we didn't set ram_blocks = game_blocks above, we either:
    ;    1) set nonstored_blocks = ACORN_INITIAL_NONSTORED_BLOCKS; see the "not
    ;       been adjusted" case above.
    ;    2) set nonstored_blocks <= ram_blocks - vmap_max_size *
    ;       vmem_block_pagecount, so after the subtraction we have ram_blocks
    ;       >= vmap_max_size * vmem_block_pagecount. QED
    ; b) exactly the same as for the turbo second processor
}

!ifndef ACORN_SWR {
    ; vmap_first_ram_page is set at build time to suit a normal second processor
    ; and it's not used on a turbo second processor, so we don't need any code
    ; to initialise it. SFTODO: BUT WE COULD MOVE IT INTO PAGE 4 AND INITIALISE IT IN DISCARDABLE INIT CODE (IE HERE)
}

    ; Now set vmap_max_entries = min(ram_blocks / vmem_block_pagecount,
    ; vmap_max_size), i.e. the number of vmap entries we have RAM to support.
    ; (If we're in the ACORN_TUBE_CACHE case on a normal second processor, we
    ; have that much RAM in total but the number of vmap entries we can support
    ; is lower. It's convenient to work with this larger value while we do the
    ; initial load, then vmap_max_entries is fixed up later.)
    ldx #vmap_max_size
    lda ram_blocks
    lsr ram_blocks + 1
    bne .cap_at_vmap_max_size
    ror
    cmp #vmap_max_size
    bcs .cap_at_vmap_max_size
    tax
.cap_at_vmap_max_size
    stx vmap_max_entries

SFTODOLABEL5
!ifndef ACORN_NO_DYNMEM_ADJUST {
    ; If we've adjusted nonstored_blocks, we may need to sort more than
    ; vmap_max_entries elements of vmap and it's definitely safe to sort all
    ; vmap_max_size entries, because we either have enough RAM for vmap_max_size
    ; blocks of virtual memory cache or we have enough RAM for the entire game.
    ; (Note that we're only talking about *sorting* here; it's important we
    ; don't sort "useful" things into parts of the vmap we don't have the memory
    ; to load. We're not talking about *how much of the vmap contains useful
    ; entries* - that is specified by vmap_max_entries.)
    ; SFTODO: We *will* sort the dummy 0 entries put at the end of vmap by the
    ; build script into the initial positions if the game didn't use the entire
    ; vmap. This is "wrong" but they will be pruned away when we discard vmap
    ; entries for memory promoted to dynmem (since 0 entries represent the first
    ; page of dynmem) and start using only the first vmap_max_entries; since
    ; this is a sort (we're just reordering things) they haven't actually
    ; displaced anything useful in the meantime. All the same, it might be
    ; neater to make the build script use $ffff for the dummy entries.
    lda nonstored_blocks
    cmp #ACORN_INITIAL_NONSTORED_BLOCKS
    beq +
    ldx #vmap_max_size
+
}
    stx vmap_sort_entries
} else { ; !VMEM
    ; Now we know how much data we are going to load (game_blocks' worth), we can
    ; calculate how many blocks correspond to each progress indicator position.
    ; SFTODO: It would be possible to pre-calculate this at build time, but it's
    ; probably best for consistency just to always use this code.
    jsr init_progress_indicator
}
    rts

; We use 16-bit fixed point arithmetic to represent the number of blocks per
; progress bar step, in order to get avoid the bar under-filling or over-filling
; the screen width. .blocks_to_load can't be more than 64K dynamic memory plus
; 128K virtual memory cache (and that's not going to happen in practice), so it
; is effectively a 9-bit value in 512-byte blocks. We can therefore afford 7
; bits for the fractional component.
progress_indicator_fractional_bits=7

; Initialise progress_indicator_blocks_left_in_chunk and
; progress_indicator_blocks_per_chunk. This is only called once in any given
; build so we could make it a macro and inline it, but since this code overlaps
; the game data we're not under that much memory pressure and it's more readable
; to just use a subroutine.
; SFTODO: It might be nice to use "half steps" to double the resolution of the
; progress bar; we'd just need to alternate between "print a half-width
; character" and "print backspace-then-full-width-character".
init_progress_indicator
!ifdef VMEM {
.blocks_to_load = ram_blocks
} else {
.blocks_to_load = game_blocks
}
    ; If we're not on the bottom line of the screen, set divisor = screen_width
    ; - cursor_x, otherwise set divisor = (screen_width - 1) - cursor_x. This
    ; way we don't have to worry about causing a mildly ugly scroll if we print
    ; in the bottom right position on the screen.
    ;
    ; (We haven't called screenkernal_init yet; that would be wrong because we
    ; might be in mode 6/7 from the loader and not yet have changed into the
    ; final mode for running the game. So we can't use s_screen_width here.)
    lda #osbyte_read_cursor_position
    jsr osbyte ; set X=cursor X, Y=cursor Y
    cpx #0
    bne .cursor_not_in_first_column
    ; The cursor's in the first column, so this is a restart; the loader prints
    ; the "Loading: " prefix for us but a restart doesn't, so do it now. By
    ; doing this here instead of in the restart code, this can be discardable
    ; init code.
    ldx #<.loading_string
    lda #>.loading_string
    jsr printstring_os
    lda #osbyte_read_screen_mode
    jsr osbyte ; set Y=current screen mode
    lda #' '
    cpy #7
    bne .not_mode_7
    ; Text on the lower part of the screen is always white in mode 7, so we
    ; follow suit with the graphics for the progress bar.
    lda #mode_7_graphics_colour_base + 7
.not_mode_7
    jsr oswrch
    lda #osbyte_read_cursor_position
    jsr osbyte ; set X=cursor X, Y=cursor Y
.cursor_not_in_first_column
    stx divisor
    sty divisor + 1
    lda #osbyte_read_vdu_variable
    ldx #vdu_variable_text_window_bottom
    jsr osbyte ; set X=screen_height - 1, Y=screen width - 1
    cpx divisor + 1
    beq .cursor_on_bottom_line
    iny
.cursor_on_bottom_line
    tya
    sec
    sbc divisor
    sta divisor
    lda #0
    sta divisor + 1

    ; .blocks_to_load is expressed in 256-byte blocks, but loading is done in
    ; 512-byte blocks, so we want to divide by two to convert this. We want to
    ; shift right by 1 for the division by two, then left by
    ; progress_indicator_fractional_bits bits.
    ldx #progress_indicator_fractional_bits - 1
    ; Set dividend = .blocks_to_load << X.
    lda .blocks_to_load + 1
    sta dividend + 1
    lda .blocks_to_load
-   asl
    rol dividend + 1
    dex
    bne -
    sta dividend
SFTODOOOL
    jsr divide16
    lda division_result
    sta progress_indicator_blocks_per_chunk
    sta progress_indicator_blocks_left_in_chunk
    lda division_result + 1
    sta progress_indicator_blocks_per_chunk + 1
    sta progress_indicator_blocks_left_in_chunk + 1
    rts

.loading_string
    !text "Loading:", 0

; SFTODO: Don't forget more code can go here if it can be executed before we
; start to put data at story_start.

!ifdef VMEM {
initial_vmap_z_l
    !FILL vmap_max_size, 'V'
}
} ; End of acorn_init_code_overlapping_game_data_inline

; Initialization subroutines which will be placed inside the Z-machine stack.
!macro acorn_init_code_in_stack {
screenkernal_init
    +screenkernal_init_inline
.screenkernal_init_rts
    rts

update_progress_indicator
progress_indicator_block_size = 1 << progress_indicator_fractional_bits
    sec
    lda progress_indicator_blocks_left_in_chunk
    sbc #<progress_indicator_block_size
    sta progress_indicator_blocks_left_in_chunk
    lda progress_indicator_blocks_left_in_chunk + 1
    sbc #>progress_indicator_block_size
    sta progress_indicator_blocks_left_in_chunk + 1
    bcc +
    ora progress_indicator_blocks_left_in_chunk
    bne .screenkernal_init_rts
+   ; progress_indicator_blocks_left_in_chunk <= 0
    clc
    lda progress_indicator_blocks_left_in_chunk
    adc progress_indicator_blocks_per_chunk
    sta progress_indicator_blocks_left_in_chunk
    lda progress_indicator_blocks_left_in_chunk + 1
    adc progress_indicator_blocks_per_chunk + 1
    sta progress_indicator_blocks_left_in_chunk + 1
    lda #255 ; solid block graphic
    jmp oswrch
}

; Initialization performed shortly after startup, just after
; acorn_deletable_init_start. (The distinction is not that important on Acorn
; as the Ozmoo executable itself doesn't generate a splash screen.)
!macro acorn_deletable_init_inline {
    jsr prepare_for_initial_load
    ; Load the nonstored blocks, or all the blocks if we're not using virtual
    ; memory. We don't need to worry about reading past the end of the game data
    ; here, because at worst we will read a final 512-byte block when we don't
    ; have a full block and that's fine.
.blocks_to_read = zp_temp + 4 ; 1 byte
    ; Because this is initialisation code, we know the following variables are
    ; already set to predictable values. This optimisation was useful at one
    ; point but it's not really important right now; it isn't too confusing so
    ; let's keep it anyway now it's been tested.
!if 0 {
    lda #2
    sta readblocks_numblocks
    lda #0
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
    sta readblocks_mempos ; story_start is page-aligned
!ifdef ACORN_TURBO_SUPPORTED {
    ; On a turbo second processor readblocks_mempos + 2 is significant and will
    ; vary; it's probably still zero at this point but play it safe. On a normal
    ; second processor readblocks_mempos + 2 will always be 0 so this is
    ; redundant but harmless.
    sta readblocks_mempos + 2
}
} else {
    ; If we're on DFS the earlier read of the catalogue will have bumped
    ; readblocks_currentblock and readblocks_mempos, so they have to be set
    ; explicitly.
    lda #0
    sta readblocks_currentblock
}
    lda #>story_start
    sta readblocks_mempos + 1
!ifdef VMEM {
    lda nonstored_blocks
} else {
    lda game_blocks
}
    sta .blocks_to_read

!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; Page in the first bank as dynamic memory may overflow into it.
    +acorn_page_in_bank_using_a dynmem_ram_bank
}

.dynmem_load_loop
!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
    lda readblocks_mempos + 1
    cmp acorn_screen_hole_start_page
    bne +
    clc
    adc acorn_screen_hole_pages
    sta readblocks_mempos + 1
+
}
    jsr update_progress_indicator
    jsr readblocks
    lda .blocks_to_read
    sec
    sbc readblocks_numblocks
    sta .blocks_to_read
    bne .dynmem_load_loop

!ifdef ACORN_SWR {
    ; Calculate vmem_blocks_in_main_ram and vmem_blocks_stolen_in_first_bank.
!ifndef ACORN_SWR_MEDIUM_DYNMEM {
    lda #0
    sta vmem_blocks_stolen_in_first_bank
    ; Set A = (>story_start + nonstored_blocks) - (>flat_ramtop - acorn_screen_hole_pages)
    lda #>story_start
    clc
    adc nonstored_blocks
!ifdef ACORN_SCREEN_HOLE {
    clc
    adc acorn_screen_hole_pages
}
    sec
    sbc #(>flat_ramtop)
    bcc .some_vmem_in_main_ram
    lsr
    sta vmem_blocks_stolen_in_first_bank
    bpl + ; Always branch
.some_vmem_in_main_ram
    ; Carry is clear; negate A
    eor #$ff
    adc #1
    lsr
    sta vmem_blocks_in_main_ram
+
} else {
    lda nonstored_blocks
    lsr
    sta vmem_blocks_stolen_in_first_bank
    lda #>flat_ramtop
    sec
    sbc #>vmem_start
!ifdef ACORN_SCREEN_HOLE {
    sec
    sbc acorn_screen_hole_pages
}
    lsr
    sta vmem_blocks_in_main_ram
}

!ifdef ACORN_SHADOW_VMEM {
    ; Calculate vmem_blocks_in_sideways_ram = 32 * ram_bank_count -
    ; vmem_blocks_stolen_in_first_bank. (Each 16K bank has 32 512-byte blocks.)
    ; This is used in convert_index_x_to_ram_bank_and_address to decide when a
    ; vmem block is in shadow RAM and it doesn't matter if we actually use fewer
    ; blocks than this. This value is just used to ensure that if a vmem block
    ; index *would* access past the end of sideways RAM, it's handled via shadow
    ; RAM. SFTODONOW: I think that's true, but revisit later.
    lda #0
    sta scratch_page
    lda ram_bank_count
    ldx #5 ; 32 = 2^5
-   asl
    rol scratch_page
    dex
    bne -
    sec
    sbc vmem_blocks_stolen_in_first_bank
    sta vmem_blocks_in_sideways_ram
    lda scratch_page
    sbc #0
    beq +
    ; We have a result which won't fit in a single byte, but since we know the
    ; maximum vmap index is 254, we can just set vmem_blocks_in_sideways_ram to
    ; 255.
    lda #255
    sta vmem_blocks_in_sideways_ram
+
}
}

!ifdef VMEM {
    ; vmem_highbyte_mask might be 0 and that enables some small optimisations, but
    ; in this one-off discardable init code we favour simplicity and don't bother.

!ifndef PREOPT {
    ; Sort vmap into ascending order, preserving the timestamps but using just the
    ; addresses as keys. This avoids the drive head jumping around during the
    ; initial load. The build system can't do this sort, because we're sorting
    ; the truncated list with just vmap_sort_entries not the full list of
    ; vmap_max_size entries.
    ;
    ; This only happens once and it's not a huge list so while we don't want it
    ; to be really slow, compactness and simplicity of code are also important.
    ; This is an insertion sort, implemented based on the pseudocode from
    ; https://en.m.wikipedia.org/wiki/Insertion_sort, which I've relabelled here
    ; to match the register use in the following code:
    ;
    ;     x = 1
    ;     while x < length(vmap_z)
    ;         temp = vmap_z[x]
    ;         y = x - 1
    ;         while y >= 0 and vmap_z[y] > temp 
    ;             vmap_z[y+1] = vmap_z[y]
    ;             y = y - 1
    ;         end while
    ;         vmap_z[y+1] = temp
    ;         x = x + 1
    ;     end while
    ;
    ; Invariants:
    ;       1 <= x <= length(vmap_z) <= vmap_max_size <= 255
    ;      -1 <= y < x, so -1 <= y <= 254
    ;
    ; This takes about 0.42 seconds to sort 255 shuffled entries at 2MHz; that's
    ; not great but it's not terrible. It takes about 0.1 seconds to sort 122
    ; shuffled entries, which is probably a more typical case. Given a sorted
    ; list - as will happen if the preopt mode has not been used - it takes
    ; about 0.02 seconds to "sort" 255 entries, so there's no significant
    ; performance penalty when this is not doing anything useful.
    ; (We could simply not include this code if we don't have any preopt data,
    ; but it's discardable init code so it's not really harmful and it seems
    ; best for support purposes to keep the code identical whether or not preopt
    ; data is supplied or not.)
    ldx #1
.outer_loop
    lda vmap_z_l,x
    sta zp_temp
    lda vmap_z_h,x
    sta zp_temp + 1
    and #vmem_highbyte_mask
    sta zp_temp + 4
    txa
    tay
.inner_loop
    dey
    lda vmap_z_h,y
    and #vmem_highbyte_mask
    cmp zp_temp + 4
    bne +
    lda vmap_z_l,y
    cmp zp_temp
    beq .exit_inner_loop
+   bcc .exit_inner_loop
    lda vmap_z_l,y
    sta vmap_z_l + 1,y
    lda vmap_z_h,y
    sta vmap_z_h + 1,y
    tya
    bne .inner_loop
    dey
.exit_inner_loop    
    ; We can't omit this iny and use vmap_z_[lh] + 1,y below to compensate
    ; because Y may be -1 (255) and so they're not equivalent.
    iny
    lda zp_temp
    sta vmap_z_l,y
    lda zp_temp + 1
    sta vmap_z_h,y
    inx
    cpx vmap_sort_entries
    bne .outer_loop

!ifndef ACORN_NO_DYNMEM_ADJUST {
    ; The initial vmap created by the build system assumes nonstored_blocks ==
    ; ACORN_INITIAL_NONSTORED_BLOCKS, so if we changed nonstored_blocks earlier
    ; we need to adjust the vmap to compensate. If we didn't adjust it, this
    ; code is a no-op. As the vmap is now sorted by address we just need to find
    ; the first entry which doesn't correspond to dynamic memory and move
    ; everything down so that entry becomes the first entry in the vmap. The
    ; space freed up at the end of the vmap by this move is filled with dummy
    ; entries so those entries will be used first when the game needs to load
    ; more blocks from disc.
SFTODOLABEL2
    ldx #255
.find_first_non_promoted_entry_loop
    ; We need to shift the 16-bit vmap entry left one bit before comparing it
    ; against nonstored_blocks.
    inx
    lda vmap_z_l,x
    asl
    lda vmap_z_h,x
    and #vmem_highbyte_mask
    rol
    bne .found_first_non_promoted_entry
    lda vmap_z_l,x
    asl
    cmp nonstored_blocks
    bcc .find_first_non_promoted_entry_loop
.found_first_non_promoted_entry
    txa
    beq .no_dynmem_promotion
    ldy #0
.vmap_move_down_loop
    cpx #vmap_max_size
    beq .use_dummy_entry
    lda vmap_z_h,x
    sta vmap_z_h,y
    lda vmap_z_l,x
    inx
    bne + ; Always branch
.use_dummy_entry
    ; We use $0000 as a dummy entry; this has the oldest possible timestamp so
    ; the entry will be re-used ASAP and because $0000xx is always dynamic
    ; memory the virtual memory code will never match against the dummy entry.
    lda #0
    sta vmap_z_h,y
+   sta vmap_z_l,y
    iny
    cpy vmap_max_entries
    bne .vmap_move_down_loop
.no_dynmem_promotion
}

    ; Debugging code - use this in conjunction with --print-vm.
!if 0 {
    jsr streams_init
    ; vmap_used_entries is set later in normal use, but set it early here so
    ; print_vm shows the entire vmap.
    lda vmap_max_entries
    sta vmap_used_entries
    lda #'X'
    jsr s_printchar
    lda #>story_start
    jsr print_byte_as_hex
    lda nonstored_blocks
    jsr print_byte_as_hex
    jsr newline
    jsr osrdch
}

    ; Now we've got vmap how we want it, load the corresponding blocks into
    ; memory and initialise vmap_used_entries. (This roughly corresponds to the
    ; C64 load_suggested_pages subroutine.)

!ifdef ACORN_TURBO_SUPPORTED {
    bit is_turbo
    bpl .normal_tube_load
    ; On a turbo second processor we don't do any preloading of the host cache
    ; so we just use a straightforward load loop like the non-tube-cache case
    ; below.
    stz vmap_index
!ifdef ACORN_TUBE_CACHE {
    lda #$ff
    sta osword_cache_index_offered
    sta osword_cache_index_offered + 1
}
.turbo_load_loop
    jsr update_progress_indicator
    jsr load_blocks_from_index
    inc vmap_index
    lda vmap_index
    cmp vmap_max_entries
    bne .turbo_load_loop
    sta vmap_used_entries
    jmp .all_loading_done

.normal_tube_load
}

!ifdef ACORN_TUBE_CACHE {
; SFTODO: These labels should probably start with a "."
inflated_vmap_max_entries = zp_temp
from_index = zp_temp + 1
to_index = vmap_index
load_scratch_space = flat_ramtop - vmem_blocksize
SFTODOLABELX2

    ; vmap_max_entries was deliberately artificially high up to this point so
    ; we'd retain and sort more of the initial vmap; set it to the correct value
    ; reflecting the size of the vmap Ozmoo's virtual memory has to work with.
    ; SFTODO: Worth noting that here - and might be useful in some other code
    ; too - we are calculating using known-at-build-time values. I could
    ; potentially simplify/shorten the code in a few places by not treating this
    ; dynamically, e.g. we wouldn't need the code to populate game_blocks in
    ; the first place. (on SWR builds the dynmem growth optimisation means
    ; nonstored_blocks is not precisely known at build time, but that's not an
    ; issue for a tube build) This might also simplify some corner cases in the
    ; "grow nonstored_blocks" logic, because the game size is no longer a
    ; runtime variable. I just worry a little bit about this breaking
    ; already-not-supposed-to-work-but-sort-of-does-just-about things where a
    ; game developer wants to switch in an updated data file without going
    ; through the Ozmoo build process.
    lda vmap_max_entries
    sta inflated_vmap_max_entries
    lda #>(flat_ramtop - story_start)
    ldx game_blocks + 1
    bne +
    cmp game_blocks
    bcc +
    lda game_blocks
+   sec
    sbc nonstored_blocks
    lsr
    sta vmap_max_entries
    sta vmap_used_entries
    ; Adjust host_cache_size so the following load loop won't try to put "too
    ; much" into the host cache; if this happens we might not have enough blocks
    ; to load into the local virtual memory cache and so some vmap entries would
    ; be present but not have actually been loaded. (This can happen because the
    ; cutover timestamp isn't that precise due to limited timestamp resolution,
    ; especially for Z4+ games.) Note that this doesn't actually stop us using
    ; more of the host cache; we will offer it blocks willy-nilly during play
    ; and if it has space it will hold onto them.
SFTODOLABELX3
    lda inflated_vmap_max_entries
    sec
    sbc vmap_max_entries
    sta host_cache_size

    ; We now need to load the inflated_vmap_max_entries blocks in the vmap from
    ; disk; vmap_max_entries blocks will go into our local memory as normal, the
    ; remainder need to be handed over to the host cache. We want the newer
    ; (higher timestamp) blocks in local memory, but remember vmap is sorted by
    ; block address to avoid the disk head jumping around. We use knowledge of
    ; how the build system generates the timestamps on vmap to identify a
    ; timestamp cutover point which will (except for the fact that the same
    ; timestamp can occur on multiple entries) separate the vmap into two chunks
    ; of the required sizes. (If the cutover timestamp is calculated
    ; incorrectly, we will still load everything we should, but newer blocks
    ; will tend to be in the host cache when we'd prefer them to be in local
    ; memory.)
.vmem_blocks = ((>(flat_ramtop - story_start)) - ACORN_INITIAL_NONSTORED_BLOCKS) / vmem_block_pagecount
.cutover_timestamp = int(ACORN_TUBE_CACHE_MAX_TIMESTAMP + ((float(.vmem_blocks) / vmap_max_size) * (ACORN_TUBE_CACHE_MIN_TIMESTAMP - ACORN_TUBE_CACHE_MAX_TIMESTAMP))) and ($ff xor vmem_highbyte_mask)

    ; Work through the blocks in vmap, loading each in turn and offering it to
    ; the host cache if it's old and there's room, and keeping it loaded into
    ; local memory otherwise. We keep doing this until we've loaded
    ; vmap_max_entries blocks into local memory (blocks offered to the host
    ; cache don't count) or until we've loaded all the blocks in vmap.
    lda #0
    sta from_index
    sta to_index
    lda #$ff
    sta osword_cache_index_offered
    sta osword_cache_index_offered + 1
.first_load_loop
    ldx from_index
    ldy to_index
    lda vmap_z_l,x
    sta vmap_z_l,y
    lda vmap_z_h,x
    sta vmap_z_h,y
    jsr update_progress_indicator
    jsr load_blocks_from_index
    ldy to_index
    lda vmap_z_h,y
    ; and #$ff xor vmem_highbyte_mask ; not necessary as we're doing a >= test
    cmp #.cutover_timestamp + 1
    bcs .dont_put_in_cache
+   ldx host_cache_size
    beq .dont_put_in_cache
    dec host_cache_size
    and #$ff xor vmem_highbyte_mask
    sta osword_cache_index_offered_timestamp_hint
    ; load_blocks_from_index will have set osword_cache_index_requested
    lda osword_cache_index_requested
    sta osword_cache_index_offered
    lda osword_cache_index_requested + 1
    sta osword_cache_index_offered + 1
    jmp .continue
.dont_put_in_cache
    lda #$ff
    sta osword_cache_index_offered
    sta osword_cache_index_offered + 1
    inc to_index
.continue
    inc from_index
    lda to_index
    cmp vmap_max_entries
    bne .first_load_loop

    ; We may have already loaded all the blocks, but if we haven't we now need
    ; to start re-using vmap[vmap_max_entries - 1] to load the rest. (We can
    ; only load into local memory and we've used it all.) We no longer have the
    ; luxury of (easily) putting the blocks with older timestamps into the host
    ; cache and keeping the younger ones in local memory, but if we chose the
    ; cutover timestamp correctly we should end up with at most one misplaced
    ; block in each cache. SFTODO: I'M NOT SURE IT'S AS PRECISE AS ONE MISPLACED BLOCK, DUE TO LACK OF TIMESTAMP RESOLUTION - IT MAY BE ONE MISPLACED *TIMESTAMP*. IT ISN'T THAT BIG A DEAL, BUT THINK ABOUT THIS AND UPDATE THIS COMMENT. The misplaced block in the host cache will be the
    ; newest block in the host cache so it will spend a long time in there
    ; before being discarded, which should give plenty of opportunity for it to
    ; be requested during gameplay and moved into local memory before it's lost.
    ; SFTODO: vmap_index is an alias for to_index, maybe use to_index in the follow loop to match the previous loop? This (double check) is just a labelling change, the code would be identical.
    dec vmap_index ; set vmap_index = vmap_max_entries - 1
.second_load_loop
    ldx from_index
    cpx inflated_vmap_max_entries
    beq .second_load_loop_done
    ldy vmap_index
    lda vmap_z_l,y
    sta osword_cache_index_offered
    lda vmap_z_h,y
    and #vmem_highbyte_mask
    sta osword_cache_index_offered + 1
    lda vmap_z_h,y
    and #$ff xor vmem_highbyte_mask
    sta osword_cache_index_offered_timestamp_hint
    lda vmap_z_l,x
    sta vmap_z_l,y
    lda vmap_z_h,x
    sta vmap_z_h,y
    jsr update_progress_indicator
    jsr load_blocks_from_index
    inc from_index
    jmp .second_load_loop
.second_load_loop_done
    ; Now we've finished the initial load, specify no timestamp hint for cache
    ; operations; this setting will remain untouched for the rest of the game.
    lda #osword_cache_no_timestamp_hint
    sta osword_cache_index_offered_timestamp_hint
} else { ; not ACORN_TUBE_CACHE
    ; Load the blocks in vmap.
    lda #0
    sta vmap_index
-   jsr update_progress_indicator
    jsr load_blocks_from_index
    inc vmap_index
    lda vmap_index
    cmp vmap_max_entries
    bne -
SFTODOLABEL4
    sta vmap_used_entries
}
.all_loading_done

} ; End of !ifndef PREOPT
} ; End of !ifdef VMEM

    ; Calculate CRC of block 0 before it gets modified, so we can use it later
    ; to identify the game disc after a save or restore.
!ifdef ACORN_SWR_MEDIUM_DYNMEM {
    +acorn_page_in_bank_using_a dynmem_ram_bank
}
    lda #0
    ldx #<story_start
    ldy #>story_start
    jsr calculate_crc ; corrupts some zp_temp locations
    stx game_disc_crc
    sty game_disc_crc + 1

!ifdef ACORN_SWR {
    ; The load loop or the above CRC may have left a non-default bank of sideways
    ; RAM paged in; we need to page the default bank back in. SFTODO: I am not
    ; sure this is necessary, as we should page in the right bank when we first
    ; try to get the page containing the initial Z-machine PC, but it doesn't
    ; really hurt to do this anyway.
    +acorn_swr_page_in_default_bank_using_y ; SFTODO: Should prob remove _swr_ from this macro for consistency
}
} ; End of acorn_deletable_init_inline

; SFTODO: Move this to be with the other paging macros?
!ifdef ACORN_SWR {
; SFTODO: Don't define these in smalldyn? Any code using it is wasting time/space, I think.
!macro acorn_swr_page_in_default_bank_using_y {
    +acorn_page_in_bank_using_y z_pc_mempointer_ram_bank
}
!macro acorn_swr_page_in_default_bank_using_a {
    +acorn_page_in_bank_using_a z_pc_mempointer_ram_bank
}
}

; Acorn-specific initialization to carry out in deletable_screen_init_2. This is
; quite late in the initialization process - in particular it happens after the
; lengthy loading process in acorn_deletable_init_inline.
!macro acorn_deletable_screen_init_2_inline {
    ; Set the desired mode. If we're already in the right mode we don't reselect
    ; it, to avoid the screen flashing briefly to black. This is deletable init
    ; code so we can afford minor luxuries like this.
    lda #osbyte_read_screen_mode
    jsr osbyte
    cpy screen_mode
    beq .already_in_right_mode
    lda #vdu_set_mode
    jsr oswrch
    lda screen_mode
    ora #128 ; force shadow mode on
    jsr oswrch
    jmp .mode_set
.already_in_right_mode
    ; Clear the screen; this is mostly unnecessary, but for Z3 games which are
    ; loading from the loader in mode 7 it clears any leftover text on the top
    ; line of the screen.
    lda #vdu_cls
    jsr oswrch
.mode_set
    ; Setting the mode will have turned the cursor back on, so fix that.
    jsr init_cursor_control
    ; We must re-initialise screenkernal to pick up the details of the new mode.
    jsr screenkernal_init
    ; We must also reset the window sizes; we do this by re-executing
    ; deletable_screen_init_1.
    jsr deletable_screen_init_1

!ifdef ACORN_HW_SCROLL {
    ldx #1
    ldy screen_mode
    cpy #7
    bne +
    dex
+   stx use_hw_scroll
}
    jsr update_colours
} ; End of acorn_deletable_screen_init_2_inline

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
    ldx #$ff
    jsr osbyte ; never returns
}

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
    lda $ffff
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
    ; SFTODO: If jmp_buf is made smaller, we could probably fairly easily
    ; detect overflow - initialize y with -buffer_size, do sta jmp_buf+1+buffer_size,y
    ; and if the bne after the iny isn't taken we've overflowed. There might be
    ; an off by one error in that, I'm just sketching the idea out. This is
    ; tempting, *but* at the moment jmp_buf is going to live in $400-800 and
    ; (except for the possibility of starting code at say $600 on 2P) we have
    ; loads of free space down there, so adding a few bytes of code to the VM
    ; to detect overflow and cause a fatal error will eat into valuable memory
    ; for the sake of optimising use of a currently not-scare resource. Think
    ; about it, maybe convert this to an SF: comment.
    tsx
    stx jmp_buf
    ldy #0
-   inx
    beq +
    lda stack,x
    sta jmp_buf+1,y
    iny
    bne -
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

    ; The Acorn OS time counter is a 5 byte value, whereas (ignoring the
    ; difference in resolution) the Commodore one is 3 bytes. Because the Acorn
    ; OS time counter may have an arbitrarily large value (perhaps some kind of
    ; soft RTC solution) when we start up and we don't want to zero it, we read
    ; the initial value and subtract that from any subsequent reads.
!macro init_readtime_inline {
    lda #osword_read_clock
    ldx #<initial_clock
    ldy #>initial_clock
    jsr osword
}

kernal_readtime
.current_clock = scratch_page
    lda #osword_read_clock
    ldx #<.current_clock
    ldy #>.current_clock
    jsr osword
    ldx #(256-5)
    sec
-   lda .current_clock-(256-5),x
    sbc initial_clock-(256-5),x
    sta .current_clock-(256-5),x
    inx
    bne -
    ; The Acorn clock is in centiseconds; a PAL C64 would use fiftieths of a
    ; second, so halve the value before we return it.
    ldx #4
    clc
-   ror .current_clock,x
    dex
    bpl -
    ; All the above means we're at no more or less risk than a C64 of having the
    ; time roll over during a game. It would take >3.8 days for this to happen.
    ldy .current_clock+2
    ldx .current_clock+1
    lda .current_clock+0
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Miscellaneous utility routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    eor $ffff,y
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
do_osbyte_set_cursor_editing_x_0
    ldx #0
do_osbyte_set_cursor_editing
    lda #osbyte_set_cursor_editing
    bne do_osbyte_y_0 ; Always branch

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
