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
; install the driver in the host for use with the host cache. (This also avoids
; the problem with the Watford DFS use of *FX111 not being tube-compatible; see
; the code at SFTODOREFTHELABEL.)

!source "acorn-shared-constants.asm"

max_shadow_driver_size = shadow_ram_copy_max_end - shadow_ram_copy

!macro assert_shadow_driver_fits .start {
    +assert * - .start <= max_shadow_driver_size
}

!macro set_brkv .target {
    lda #<.target
    sta brkv
    lda #>.target
    sta brkv+1
}

; SFTODONOW: Decide if I prefer to reorder these for some cosmetic reason before I embed them in the loader
shadow_state_none        = 0 ; no shadow RAM
shadow_state_screen_only = 1 ; shadow RAM with no driver for spare shadow RAM access
shadow_state_first_driver = 2
shadow_state_integra_b   = 2 ; Integra-B shadow RAM
shadow_state_mrb         = 3 ; Electron Master RAM Board shadow RAM
shadow_state_b_plus_os = 4 ; BBC B+ shadow RAM accessed via OS
shadow_state_b_plus_private = 5 ; BBC B+ shadow RAM via code in 12K private RAM
shadow_state_master = 6 ; BBC Master shadow RAM
shadow_state_watford = 7 ; BBC B Watford shadow RAM
shadow_state_aries = 8 ; BBC B Aries shadow RAM

; We don't need zero page for all of these, but we have it free so with might as
; well use it to keep the code size down.
shadow_state = $70
private_ram_in_use = $71
old_brkv = $72 ; 2 bytes
src = $74 ; 2 bytes

extended_vector_table = $d9f

; On a B+, code running at $axxx in the 12K private RAM can access shadow RAM
; directly. If the private RAM is free, we copy some code there for use as part
; of the shadow driver - it's faster than going via the OSRDSC/OSWRSC routines.
shadow_copy_private_ram = $af00

start
    lda brkv
    sta old_brkv
    lda brkv+1
    sta old_brkv+1
    jsr main
restore_brkv ; SFTODO: If this is never jsr-ed to, we can just save old brkv on stack using pha/pla to save code size
    lda old_brkv
    sta brkv
    lda old_brkv+1
    sta brkv+1
    rts

main
    ; Do we have shadow RAM?
    lda #shadow_state_none
    sta shadow_state
    +assert shadow_state_none == 0
    sta private_ram_in_use
    lda #osbyte_read_screen_address_for_mode
    ldx #shadow_mode_bit + 0
    jsr osbyte
    tya
    bpl no_shadow_ram
    ; We have shadow RAM, which we can use for screen memory via the OS without
    ; any special knowledge on our part.
    +assert shadow_state_screen_only == shadow_state_none + 1
    inc shadow_state
    ; Are we running on an Integra-B? We check for this explicitly as the
    ; Integra-B spoofs the result of osbyte_read_host.
    +set_brkv not_integra_b
    lda #$49 ; SFTODO: USE NAMED CONSTANT
    ldx #$ff
    ldy #0
    jsr osbyte
    cpx #$49
    bne not_integra_b
    ; We're running on an Integra-B.
    lda #64
    jsr test_private_ram_in_use
    lda #shadow_state_integra_b
    jmp set_shadow_state_from_a_and_install_driver
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
    bne bbc_b
    ; We're on an Electron. Check for Master RAM Board shadow RAM. As we know
    ; we're running on the host, we can just check &27F directly as mentioned in
    ; the MRB manual. We check for it being exactly &80 because that's what the
    ; MRB manual says.
    lda $27f
    cmp #$80
    bne electron_not_mrb
    ; We're on an Electron with Master RAM Board shadow RAM.
    lda #shadow_state_mrb
    jmp set_shadow_state_from_a_and_install_driver
bbc_b_plus
    lda #128
    jsr test_private_ram_in_use
    lda #shadow_state_b_plus_os
    ldy private_ram_in_use
    bne +
    lda #shadow_state_b_plus_private
+   jmp set_shadow_state_from_a_and_install_driver
master
    lda #shadow_state_master
    jmp set_shadow_state_from_a_and_install_driver
electron_not_mrb
    ; We're on an Electron with shadow RAM but it's not a Master RAM board. We
    ; don't know how to access spare shadow RAM on this hardware, so leave
    ; shadow_state set alone; it's already shadow_state_screen_only. SFTODO: Arguably we could/should fall through to the following BBC B tests, which *might* work. However, my suspicion is there are no BITD shadow RAM solutions for the Electron other than the MRB, and any new one is likely to use Master-style control which we don't currently have any test code for (since we only use it on Integra-B and Master where we can explicitly detect the system type).
fx111_failed
    ; *FX111 generated an error, so neither *FX34 nor *FX111 works and we
    ; therefore don't know how to access the spare shadow RAM. Leave
    ; shadow_state alone; it's already shadow_state_screen_only.
no_shadow_ram
    ; Leave shadow_state alone; it's already shadow_state_none.
    rts
bbc_b
    ; We're on a BBC B with non-Integra B shadow RAM. There are two competing
    ; standards for controlling shadow RAM paging on a BBC B, one using *FX34
    ; ("Watford") and the other using *FX111 ("Aries").
    ;
    ; With the Watford 32K RAM card, *FX34 is supported by Watford ROMs 2.00 and
    ; 2.40. *FX111 is only supported by 2.40.
    ;
    ; With the Aries B20, *FX34 is not supported, *FX111 is supported.
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
    ; If *FX34 doesn't work, we assume we can use *FX111, which I believe is
    ; reasonable. SFTODONOW: WOULD IT BE PRUDENT TO TRY *FX111 AND IF IT GENERATES AN ERROR, REVERT TO "SHADOW BUT WITH NO SCREEN DRIVER"?
    ;
    ; If the user has an older version of Watford (D)DFS, Ozmoo's preference for
    ; using *FX34 will reduce the chances of a clash over the two different uses
    ; of *FX111. Only a user with an older version of Watford (D)DFS and a
    ; non-Watford shadow RAM card will experience problems, and probably only if
    ; they have the DFS in a higher priority bank than the shadow RAM support
    ; ROM as well. Anyone with that kind of setup is likely to run into problems
    ; with other software trying to use *FX111 as well. We try to detect this
    ; (in a way that's probably fairly reliable in practice, but not guaranteed)
    ; and generate an error rather than crashing when *FX111 doesn't do what we
    ; expect during the game. To solve this, the user either needs to upgrade
    ; the DFS, disable their shadow RAM or, probably, reorder their ROMs so the
    ; shadow RAM driver ROM gets dibs on *FX111. SFTODONOW: DON'T FORGET TO DO THIS BEST EFFORT FX111 STOLE BY DFS TEST

    +set_brkv not_watford
    ; Whether shadow mode is currently in operation is a little fuzzy, so use
    ; *FX34,64 to read the current state and do nothing with it rather than
    ; *setting a state. All we care about is whether an error occurs.
    lda #34
    ldx #64
    jsr osbyte
    lda #shadow_state_watford
    jmp set_shadow_state_from_a_and_install_driver
not_watford
    ; *FX34 doesn't work. Try *FX111. We do this for two reasons. Firstly, if it generates
    ; an error, we obviously have some kind of unknown shadow RAM and we must content ourselves with using shadow RAM only for screen memory. Secondly, this gives us a chance to see if *FX111 is being picked up by an older Watford DFS.
    ; If OSBYTE 111 is controlling shadow RAM state, X after *FX111,&40 will be the shadow
    ; state (i.e. 1, as we're in a shadow mode at this point). If OSBYTE 111 is being
    ; picked up by an older Watford DFS and used to return the current drive,
    ; returned_x will *probably* be 0. (There's no guarantee; although Ozmoo assumes
    ; elsewhere it's being run from drive 0, it's possible Watford DFS is present but
    ; not the current filing system, in which case the "current drive" might not be
    ; 0.)
    ; SFTODO: *If* we eventually allow running the shadow driver executable from a preloader
    ; (counting the time taken to run it against any default delay the user requested),
    ; note that we may well *not* be in a shadow mode then, which will interfere with this
    ; test.
    ; SFTODO: Can we make more/cleverer OSBYTE 111 calls to establish with confidence that it is controlling shadow RAM? Since only b0 of the returned X is used we always get 0 or 1 back to reflect current displayed RAM, and thus without video glitching I am not sure we can. Could we issue a *DRIVE 0 command after checking DFS is the current filing system? This feels error prone/annoying for the user though - someone is bound to get bitten by it.
    +set_brkv fx111_failed
    lda #111
    ldx #$40
    jsr osbyte
    cpx #1
    beq fx111_probably_ok
    brk ; SFTODONOW: What should we do? Set a flag so the loader can generate the error it previously generated, or just revert to using shadow RAM for screen memory only? THe latter at least means the user can get some benefit. We could also maybe make the hardware detected line something like "shadow RAM (screen only; *FX111 failure)" which might prompt the user to investigate but not force them to.
fx111_probably_ok
    ; *FX111 seems to work; we have no absolute guarantee it isn't being intercepted by an older Watford DFS, but we have to assume it's fine.
    lda #shadow_state_aries
    jmp set_shadow_state_from_a_and_install_driver

set_shadow_state_from_a_and_install_driver
!zone {
    sta shadow_state
    tax ; SFTODO: pass value in X in first place?
    cmp #shadow_state_b_plus_private
    bne .no_private_ram_driver
    lda romsel_copy
    pha
    lda #128
    sta romsel_copy
    sta bbc_romsel
    ldy #b_plus_high_driver_size-1
.copy_high_loop
    lda shadow_driver_b_plus_private_high,y
    sta shadow_copy_private_ram,y
    dey
    bpl .copy_high_loop
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
    sta shadow_ram_copy,y
    dey
    bpl .copy_loop
    rts
}

; SFTODO: Need to keep this in sync with shadow_state_* enum
shadow_driver_table_low
    !byte <shadow_driver_integra_b
    !byte <shadow_driver_electron_mrb
    !byte <shadow_driver_b_plus_os
    !byte <shadow_driver_b_plus_private_low
    !byte <shadow_driver_master
    !byte <shadow_driver_watford
    !byte <shadow_driver_aries
    ; SFTODO: MORE
shadow_driver_table_high
    !byte >shadow_driver_integra_b
    !byte >shadow_driver_electron_mrb
    !byte >shadow_driver_b_plus_os
    !byte >shadow_driver_b_plus_private_low
    !byte >shadow_driver_master
    !byte >shadow_driver_watford
    !byte >shadow_driver_aries
    ; SFTODO: MORE

; The shadow driver API is very simple - the core Ozmoo executable calls the
; subroutine at shadow_ram_copy with A=source page and Y=destination page, and
; that subroutine copies 256 bytes from page A to page Y. One of A or Y will be
; in the &30-&7F inclusive range, indicating it's spare shadow RAM, and the
; other will be <&30, indicating it's main RAM.

; SFTODO: Looks like non-2P shadow driver only copies a 256 byte page. *May* want to define a completely separate shadow driver API for 2P host cache case. Let's get this working first anyway. (Probably best to make code changes to host cache to support shadow RAM first and see what I "want" to be able to do, then decide on driver API based on that, rather than guessing and implementing the API *first*.)

shadow_driver_integra_b
!pseudopc shadow_ram_copy {
!zone {
    ; SFTODO: Since the Ozmoo executable pokes directly at Integra-B hardware
    ; registers, we might as well do so here to page shadow RAM in and out; it
    ; would be faster. But I'll stick with this for now.
    ; SFTODO: This is *similar* to the Watford/Aries driver (though OSBYTE is
    ; 108, and the 1/0 codes are swapped) so there might be potential for
    ; sharing code. But this is moot if I switch to driving the hardware direct.
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
}
}
+assert_shadow_driver_fits shadow_driver_integra_b

shadow_driver_electron_mrb
!pseudopc shadow_ram_copy {
!zone {
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

; SFTODONOW: Must also port the faster B+ shadow driver which uses code at &A000, but let's keep it simple for the moment.
shadow_driver_b_plus_os
!pseudopc shadow_ram_copy {
!zone {
oswrsc = $ffb3
oswrsc_ptr = $d6
osrdsc = $ffb9
osrdsc_ptr = $f6
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
    jsr oswrsc ; preserves Y - equivalent to STA (&D6),Y - note Y is used!
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
    jsr osrdsc ; ignores and corrupts Y
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
!pseudopc shadow_ram_copy {
    ldx romsel_copy
    stx .lda_imm_bank+1
    ldx #128
    stx romsel_copy
    stx bbc_romsel
    jmp shadow_copy_private_ram
.stub_finish
.lda_imm_bank
    lda #0 ; patched
    sta romsel_copy
    sta bbc_romsel
    rts
}
+assert_shadow_driver_fits shadow_driver_b_plus_private_low

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
    jmp .stub_finish
}
shadow_driver_b_plus_private_high_end

b_plus_high_driver_size = shadow_driver_b_plus_private_high_end - shadow_driver_b_plus_private_high
    ; We use a dey...bpl loop to copy this driver. This also acts as an over-tight check that the
    ; driver fits in the last page of private RAM.
    +assert b_plus_high_driver_size < 128
}

shadow_driver_master
!pseudopc shadow_ram_copy {
!zone {
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
    !cpu 6502
}
}
+assert_shadow_driver_fits shadow_driver_master

!macro shadow_driver_watford_aries shadow_osbyte {
!pseudopc shadow_ram_copy {
!zone {
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
}
}
}

shadow_driver_watford
    +shadow_driver_watford_aries 34
    +assert_shadow_driver_fits shadow_driver_watford

shadow_driver_aries
    +shadow_driver_watford_aries 111
    +assert_shadow_driver_fits shadow_driver_aries


end

; Determine if the private 12K is free on an Integra-B or B+ by checking for any
; extended vectors pointing into it. On entry A is the bit which is set in the
; extended vector ROM byte to select the private RAM. private_ram_in_use is
; non-0 on exit iff the private RAM is in use.
; SFTODO: Make sure to test this (e.g. with SWMMFS+)
test_private_ram_in_use
!zone {
    sta .and_immediate+1
    ldx #26*3
.test_loop
    lda extended_vector_table+2,x
.and_immediate
    and #$ff ; patched
    bne .in_use
    dex
    dex
    dex
    bpl .test_loop
.in_use
    sta private_ram_in_use
    rts
}

; SFTODONOW: assert the executable hasn't overflowed page $9/$a, if that's where it's going to live

; SFTODONOW: Must test that this works for all the different shadow options
