; Shadow RAM detection/driver installation code
;
; Using shadow RAM for the screen memory is easy, as we just use the OS routines
; and it just works - the OS (on a B+ or Master) or the driver ROM (on a B or
; Electron) supplied by the shadow RAM manufacturer handles it.
;
; Ozmoo is capable of using spare shadow RAM (i.e. the part of the 20K shadow
; RAM not needed by screen modes smaller than 20K) for caching, but there's no
; standard way to access that RAM. This code tries to detect the type of shadow
; RAM we have and if it's one we know about, installs a driver routine to
; abstract the details away from the core Ozmoo code. If we can't detect the
; precise shadow RAM type we disable using spare shadow RAM for cache, but
; continue to allow the use of shadow RAM for screen memory.
;
; This always runs in the host; if a second processor is present we want to
; install the driver in the host for use with the host cache. (This also
; sidesteps concerns with older Watford DFS's use of *FX111 not being
; tube-compatible, although I think it is actually tube-compatible anyway. See
; the code at bbc_b_not_integra_b.)

; SFTODO: This code barely fits in the space it has available at &900-&B00
; and if we want to add a new shadow driver there is no way it's going to be
; squashable. We really ought to be running this from somewhere in main RAM,
; creating space by moving HIMEM down in the loader if necessary. (The runtime
; shadow driver is fine, it's this code which decides which driver to install
; in the runtime shadow driver space that's the problem.)

!source "acorn-shared-constants.asm"

max_shadow_driver_size = shadow_driver_end - shadow_driver_start

!macro assert_shadow_driver_fits .start {
    +assert * - .start <= max_shadow_driver_size
    ;!warn "QQQA FREE ",max_shadow_driver_size - (* - .start) ; SFTODO TEMP, DELETE
}

; The following addresses are only used internally by this code as we install the
; shadow driver; the shadow driver itself doesn't use them.
src = $72 ; 2 bytes
tmp = $74

extended_vector_table = $d9f

; NB: If an OSBYTE call fails because it's not supported, it returns normally
; with X=$FF. It does *not* generate an OS error via BRK; that is a function of
; the *FX command, not OSBYTE itself. I have used *FX a lot in comments, but
; remember we're really dealing with raw OSBYTE.

start
    ; Do we have shadow RAM?
    lda #shadow_state_none
    sta shadow_state
    +assert shadow_state_none = 0
    sta private_ram_in_use
    lda #osbyte_read_screen_address_for_mode
    ldx #shadow_mode_bit + 0
    jsr osbyte
    tya
    bpl no_shadow_ram
    ; We have shadow RAM, which we can use for screen memory via the OS without
    ; any special knowledge on our part.
    +assert shadow_state_screen_only = shadow_state_none + 1
    inc shadow_state
    ; Are we running on an Integra-B? We check for this explicitly as the
    ; Integra-B spoofs the result of osbyte_read_host.
    lda #$49
    ldx #$ff
    ldy #0
    jsr osbyte
    cpx #$49
    bne not_integra_b
    ; We're running on an Integra-B.
    lda #64
!ifndef ACORN_IGNORE_INTEGRA_B_PRIVATE_RAM {
    jsr test_private_ram_in_use
} else {
    sta private_ram_in_use
}
    lda #shadow_state_integra_b
    bne set_shadow_state_from_a_and_install_driver ; always branch
not_integra_b
    ; Determine the host type.
    lda #osbyte_read_host
    ldx #1
    jsr osbyte
    cpx #2
    beq bbc_b_plus
    bcs master
    ; We're on an Electron or BBC B, and not an Integra-B.
    txa
    bne bbc_b_not_integra_b
    ; We're on an Electron. Check for Master RAM Board shadow RAM. As we know
    ; we're running on the host, we can just check $27F directly as mentioned in
    ; the MRB manual. We check for it being exactly $80 because that's what the
    ; MRB manual says.
    lda $27f
    cmp #$80
    bne electron_not_mrb
    ; We're on an Electron with Master RAM Board shadow RAM.
    lda #shadow_state_mrb
    bne set_shadow_state_from_a_and_install_driver ; always branch
bbc_b_plus
    lda #128
    jsr test_private_ram_in_use
    lda #shadow_state_b_plus_os
    ldy private_ram_in_use
    bne set_shadow_state_from_a_and_install_driver
    lda #shadow_state_b_plus_private
    bne set_shadow_state_from_a_and_install_driver ; always branch
master
    lda #shadow_state_master
    bne set_shadow_state_from_a_and_install_driver ; always branch
electron_not_mrb
    ; We're on an Electron with shadow RAM but it's not a Master RAM board. We
    ; don't know how to access spare shadow RAM on this hardware, so leave
    ; shadow_state set alone; it's already shadow_state_screen_only.
    ; SFTODO: Arguably we could/should fall through to the following BBC B
    ; tests, which *might* work. However, my suspicion is there are no BITD
    ; shadow RAM solutions for the Electron other than the MRB, and any new one
    ; is likely to use Master-style control which we don't currently have any
    ; test code for (since we only use it on Integra-B and Master where we can
    ; explicitly detect the system type).
osbyte_111_failed
    ; *FX111 didn't return the expected result, so neither *FX34 nor *FX111
    ; works and we therefore don't know how to access the spare shadow RAM.
    ; Leave shadow_state alone; it's already shadow_state_screen_only.
no_shadow_ram
    ; Leave shadow_state alone; it's already shadow_state_none.
    rts
bbc_b_not_integra_b
    ; We're on a BBC B with non-Integra B shadow RAM. We currently support two
    ; competing standards for controlling shadow RAM paging on such a machine:
    ; one using *FX34 ("Watford") and the other using *FX111 ("Aries").
    ;
    ; With the Watford 32K RAM card, *FX34 is supported by Watford ROMs 2.00 and
    ; 2.40. *FX111 is only supported by 2.40.
    ;
    ; With the Aries B20, *FX34 is not supported; *FX111 is supported.
    ;
    ; As an extra complication, Watford DFS versions prior to 1.43/Watford DDFS
    ; versions prior to 1.53 use *FX111 to read the drive number of the last
    ; *LOAD or *RUN read.
    ;
    ; In order to try to cope with all this:
    ;
    ; We try *FX34 first, because probably the only thing that implements this
    ; is the Watford RAM card driver ROM (either version). So we should with
    ; luck avoid any false positives and if *FX34 works, we can use it to
    ; control shadow RAM paging.
    ;
    ; If *FX34 doesn't work, we try using *FX111 to query the shadow state and
    ; see if it returns the result we expect. If it does, we use it.
    ;
    ; If neither *FX34 nor *FX111 works we don't try to use spare shadow RAM and
    ; just content ourselves with using shadow RAM for the screen.
    ;
    ; If the user has an older version of Watford (D)DFS, Ozmoo's preference for
    ; using *FX34 will reduce the chances of a clash over the two different uses
    ; of *FX111. Only a user with an older version of Watford (D)DFS and a
    ; non-Watford shadow RAM card will experience problems, and probably only if
    ; they have the DFS in a higher priority bank than the shadow RAM support
    ; ROM as well. Anyone with that kind of setup is likely to run into problems
    ; with other software trying to use *FX111 as well. Our test for the return
    ; value of *FX111 should reliably detect a clash, and in that case we'll run
    ; with shadow RAM for screen memory only. To allow full use of shadow RAM in
    ; this case, the user either needs to upgrade the DFS or, probably, reorder
    ; their ROMs so the shadow RAM driver ROM gets dibs on *FX111.
    ;
    ; We *could* generate an error if *FX111 is claimed by Watford DFS, so the
    ; user can maybe fix it, but they'd probably rather have the game run.

    ; We use *FX34,64 to read the current shadow state but we don't check the
    ; return value precisely; all we care about is whether the call is
    ; recognised or not.
    lda #34
    ldx #64
    jsr osbyte
    inx
    beq not_watford
    lda #shadow_state_watford
    bne set_shadow_state_from_a_and_install_driver ; always branch

    ; This code lives at this illogical place in the source because otherwise
    ; the +copy_data_checked macro call below won't assemble. Sigh.
shadow_driver_b_plus_private_high
!pseudopc shadow_copy_private_ram {
    sta .lda_abs_y+2
    sty .sta_abs_y+2
    ldy #0
.copy_loop
.lda_abs_y
    lda $ff00,Y ; patched
.sta_abs_y
    sta $ff00,y ; patched
    dey
    bne .copy_loop
    jmp stub_finish
}
shadow_driver_b_plus_private_high_end

not_watford
    ; *FX34 doesn't work. Try *FX111. As suggested by JGH in
    ; https://stardot.org.uk/forums/viewtopic.php?p=432945#p432945, we make a
    ; couple of OSBYTE 111 calls which will distinguish the case where this is
    ; controlling shadow RAM from the case where it's returning the current
    ; drive. We check specifically for the values of X being 1 and 0 after each
    ; call, because we're only interested in *FX111 if it is controlling shadow
    ; RAM. If OSBYTE 111 is reading the current drive, X will not change between
    ; these two calls.
    ; SFTODONOW: GAH! Now this is running at &5C00, these OSBYTE 111 calls page it out as it is running on hardware implementing OSBYTE 111. So I really need to run it below &3000, *but* there isn't enough room in &900-&AFFF and I really don't want to have to make it relocatable and run it at PAGE. I suppose I could run it at &2D00-ish from !BOOT. I'm going to sleep on this.
    lda #111
    ldx #$80
    jsr osbyte
    stx tmp
    ; SFTODONOW: Should I perhaps (be very careful about what mght overwrite the installed driver later on) install the shadow driver from PRELOAD if present and "ASAP" in LOADER if no PRELOAD? This way there would be option in the future to use the shadow driver for helping with displaying a loading screen (don't do this now). It's tempting to do it from !BOOT but that is probably not a great idea as e.g. hard drive/Econet installations may use a custom !BOOT and not just copy ours, so the less it does the better.
    ; I think A *should* still be 111, but at least testing this on MAME with
    ; an Aries B20 it is not always preserved. SFTODONOW: This needs testing - before this fix, booting the game fresh would show Aries shadow RAM, but pressing (non CTRL) BREAK and rebooting would show "shadow (screen only)". I hope this fix will address this, but need to confirm once I fix the "the code is paging itself out" problem.
    lda #111
    ldx #$c0
    jsr osbyte
    txa
    bne osbyte_111_failed
    dec tmp
    bne osbyte_111_failed
    ; *FX111 is controlling shadow RAM.
    lda #shadow_state_aries
set_shadow_state_from_a_and_install_driver
    ; fall through to set_shadow_state_from_a_and_install_driver

set_shadow_state_from_a_and_install_driver
!zone {
    sta shadow_state
    tax ; SFTODO: pass value in X in first place?
    cmp #shadow_state_b_plus_private
    bne .no_private_ram_driver
    ; For shadow_state_b_plus_private, we need to copy driver code into the 12K
    ; private RAM as well as installing a driver in main RAM.
    lda romsel_copy
    pha
    lda #128
    sta romsel_copy
    sta bbc_romsel
    +copy_data_checked shadow_driver_b_plus_private_high, shadow_driver_b_plus_private_high_end, shadow_copy_private_ram, shadow_copy_private_ram_end
    pla
    sta romsel_copy
    sta bbc_romsel
.no_private_ram_driver
    lda shadow_driver_table_low-shadow_state_first_driver,x
    sta src
    lda shadow_driver_table_high-shadow_state_first_driver,x
    sta src+1
    +assert (max_shadow_driver_size - 1) < 128
    ldy #max_shadow_driver_size-1
.copy_loop
    lda (src),y
    sta shadow_driver_start,y
    dey
    bpl .copy_loop
    rts
}

; SFTODO: Need to keep this in sync with shadow_state_* enum
shadow_driver_table_low
    !byte <shadow_driver_b_plus_os
    !byte <shadow_driver_b_plus_private_low
    !byte <shadow_driver_master
    !byte <shadow_driver_electron_mrb
    !byte <shadow_driver_integra_b
    !byte <shadow_driver_watford
    !byte <shadow_driver_aries
shadow_driver_table_high
    !byte >shadow_driver_b_plus_os
    !byte >shadow_driver_b_plus_private_low
    !byte >shadow_driver_master
    !byte >shadow_driver_electron_mrb
    !byte >shadow_driver_integra_b
    !byte >shadow_driver_watford
    !byte >shadow_driver_aries

; The shadow driver API consists of two calls. Before trying to use either, the
; driver executable must have been run to examine the current hardware and
; install the appropriate driver (if any) and it must have returned with
; shadow_state >= shadow_state_first_driver.
;
; shadow_ram_copy:
;    Enter with:
;        A=page to copy from
;        Y=page to copy to
;
;    One of A or Y will be in the range $30-$7f inclusive, indicating the
;    relevant page of shadow RAM.
;
;    The other of A or Y will be in the range $00-$2f inclusive, indicating the
;    relevant page of main RAM.
;
;    The entire page of data at A is copied to page Y.
;
;    All registers are corrupt on exit.
;
; shadow_paging_control:
;    A pointer to this routine is held at shadow_paging_control_ptr; it may be 0
;    if the current hardware can't support shadow RAM paging. (Check this rather
;    than trying to infer the capabilities from the precise value of
;    shadow_state, which is provided mainly for display purposes.)
;
;    Enter with:
;        A=0 to page in main RAM at $3000-$7fff inclusive
;        A=1 to page in shadow RAM at $3000-$7fff inclusive
;        (any other value of A will result in undefined behaviour)
;
;    All registers are corrupt on exit.

shadow_driver_integra_b
!pseudopc shadow_driver_start {
!zone {
    !word .shadow_paging_control

    ; SFTODO: Since the Ozmoo executable pokes directly at Integra-B hardware
    ; registers, we might as well do so here to page shadow RAM in and out; it
    ; would be faster. But I'll stick with this for now.
    ; SFTODO: This is *similar* to the Watford/Aries driver (though OSBYTE is
    ; 108, and the 1/0 codes are swapped) so there might be potential for
    ; sharing code, if only via a macro. But this is moot if I switch to driving
    ; the hardware direct.
    sta .lda_abs_y+2
    sty .sta_abs_y+2
    ; Page in shadow RAM
    lda #$6c ; SFTODO: named constant?
    ldx #1
    jsr osbyte
    ldy #0
.copy_loop
.lda_abs_y
    lda $ff00,y ; patched
.sta_abs_y
    sta $ff00,y ; patched
    dey
    bne .copy_loop
    ; Page out shadow RAM
    lda #$6c ; SFTODO: name constant
    ldx #0
    jmp osbyte

.shadow_paging_control
    tax
    lda #$6c
    jmp osbyte
}
}
+assert_shadow_driver_fits shadow_driver_integra_b

; SFTODO: https://beebwiki.mdfs.net/Paging_in_video_memory has some untested
; code for paging in shadow RAM on the Electron. Could this work on the MRB?
shadow_driver_electron_mrb
!pseudopc shadow_driver_start {
!zone {
    !word 0 ; shadow paging is not supported

    cmp #$30
    bcs .copy_from_shadow
    ; We're copying to shadow RAM.
    sta .lda_abs_x+2
    ldx #0
    ; SFTODO: Can we assume V is preserved by $FBFD and move the set/clear outside the loop?
.copy_to_shadow_loop
.lda_abs_x
    lda $ff00,x ; patched
    bit .our_rts ; set V
    jsr $fbfd ; write to shadow RAM SFTODO: use named constant?
    inx
    bne .copy_to_shadow_loop
.our_rts
    rts
.copy_from_shadow
    ; We're copying from shadow RAM.
    sty .sta_abs_x+2
    tay
    ldx #0
.copy_from_shadow_loop
    clv
    jsr $fbfd ; read from shadow RAM SFTODO: use named constant?
.sta_abs_x
    sta $ff00,x ; patched
    inx
    bne .copy_from_shadow_loop
    rts
}
}
+assert_shadow_driver_fits shadow_driver_electron_mrb

shadow_driver_b_plus_os
!pseudopc shadow_driver_start {
!zone {
oswrsc = $ffb3
oswrsc_ptr = $d6
osrdsc = $ffb9
osrdsc_ptr = $f6

    !word 0 ; shadow paging is not supported

    cmp #$30
    bcs .copy_from_shadow
    ; We're copying to shadow RAM.
    sta .lda_abs_y+2
    sty oswrsc_ptr+1
    ldy #0
    sty oswrsc_ptr
.copy_to_shadow_loop
.lda_abs_y
    lda $ff00,y ; patched
    jsr oswrsc ; preserves Y - equivalent to sta (oswrsc_ptr),y - note Y is used!
    iny
    bne .copy_to_shadow_loop
    rts
.copy_from_shadow
    ; We're copying from shadow RAM.
    sta osrdsc_ptr+1
    sty .sta_abs+2
    ldy #0
    sty osrdsc_ptr
.copy_from_shadow_loop
    jsr osrdsc ; ignores and corrupts Y - equivalent to lda (osrdsc_ptr)
.sta_abs
    sta $ff00 ; patched
    inc osrdsc_ptr
    inc .sta_abs+1
    bne .copy_from_shadow_loop
    rts
}
}
+assert_shadow_driver_fits shadow_driver_b_plus_os

!zone {
shadow_driver_b_plus_private_low
!pseudopc shadow_driver_start {
    !word 0 ; shadow paging is not supported

    ldx romsel_copy
    stx .lda_imm_bank+1
    ldx #128
    stx romsel_copy
    stx bbc_romsel
    jmp shadow_copy_private_ram
stub_finish
.lda_imm_bank
    lda #0 ; patched
    sta romsel_copy
    sta bbc_romsel
    rts
}
+assert_shadow_driver_fits shadow_driver_b_plus_private_low

b_plus_high_driver_size = shadow_driver_b_plus_private_high_end - shadow_driver_b_plus_private_high
    ; We use a dey...bpl loop to copy this driver. This also acts as an over-tight check that the
    ; driver fits in the last page of private RAM.
    +assert b_plus_high_driver_size < 128
}

shadow_driver_master
!pseudopc shadow_driver_start {
!zone {
    !word .shadow_paging_control

    !cpu 65c02

    sta .lda_abs_y+2
    sty .sta_abs_y+2
    lda #4
    tsb $fe34 ; page in shadow RAM SFTODO: named constant?
    ldy #0
.copy_loop
.lda_abs_y
    lda $ff00,y ; patched
.sta_abs_y
    sta $ff00,y ; patched
    dey
    bne .copy_loop
    lda #4
    trb $fe34 ; page out shadow RAM
    rts

.shadow_paging_control
    tax
    beq .page_in_main_ram
    lda #4
    tsb $fe34
    rts
.page_in_main_ram
    lda #4
    trb $fe34
    rts

    !cpu 6502
}
}
+assert_shadow_driver_fits shadow_driver_master

!macro shadow_driver_watford_aries shadow_osbyte {
!pseudopc shadow_driver_start {
!zone {
    !word .shadow_paging_control

    sta .lda_abs_y+2
    sty .sta_abs_y+2
    lda #shadow_osbyte
    ldx #0
    jsr osbyte ; page in shadow RAM
    ldy #0
.copy_loop
.lda_abs_y
    lda $ff00,y ; patched
.sta_abs_y
    sta $ff00,y ; patched
    dey
    bne .copy_loop
    lda #shadow_osbyte
    ldx #1
    jmp osbyte ; page out shadow RAM

.shadow_paging_control
    eor #1
    tax
    lda #shadow_osbyte
    jmp osbyte
}
}
}

shadow_driver_watford
    +shadow_driver_watford_aries 34
    +assert_shadow_driver_fits shadow_driver_watford

shadow_driver_aries
    +shadow_driver_watford_aries 111
    +assert_shadow_driver_fits shadow_driver_aries

; Determine if the private 12K is free on an Integra-B or B+ by checking for any
; extended vectors pointing into it. On entry A is the bit which is set in the
; extended vector ROM byte to select the private RAM. private_ram_in_use is
; non-0 on exit iff the private RAM is in use.
; SFTODO: Make sure to test this (e.g. with SWMMFS+)
test_private_ram_in_use
!zone {
    sta tmp
    ldx #26*3
.test_loop
    lda extended_vector_table+2,x
    and tmp
    bne .in_use
    dex
    dex
    dex
    bpl .test_loop
.in_use
    sta private_ram_in_use
    rts
}

end

; SFTODONOW: Must test that this works for all the different shadow options

; SQUASH: If it's desirable to shrink the shadow drivers to free up more low
; memory for other purposes, don't forget the drivers with shadow paging support
; could jsr into that support from their shadow copy code instead of duplicating
; the code inline.
