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
; the code at bbc_b_not_integra_b.)

!source "acorn-shared-constants.asm"

max_shadow_driver_size = shadow_ram_copy_max_end - shadow_ram_copy

!macro assert_shadow_driver_fits .start {
    +assert * - .start <= max_shadow_driver_size
    ;!warn "QQQA FREE ",max_shadow_driver_size - (* - .start) ; SFTODO TEMP, DELETE
}

; SFTODONOW: Decide if I prefer to reorder these for some cosmetic reason before I embed them in the loader
shadow_state_none           = 0 ; no shadow RAM
shadow_state_screen_only    = 1 ; shadow RAM with no driver for spare shadow RAM access
shadow_state_first_driver   = 2 ; shadow_state >= this means we have a driver
shadow_state_b_plus_os      = 2 ; BBC B+ shadow RAM accessed via OS
shadow_state_b_plus_private = 3 ; BBC B+ shadow RAM via code in 12K private RAM
shadow_state_master         = 4 ; BBC Master shadow RAM
shadow_state_mrb            = 5 ; Electron Master RAM Board shadow RAM
shadow_state_integra_b      = 6 ; Integra-B shadow RAM
shadow_state_watford        = 7 ; BBC B Watford shadow RAM
shadow_state_aries          = 8 ; BBC B Aries shadow RAM

; We don't need zero page for all of these, but we have it free so with might as
; well use it to keep the code size down. The first two addresses are used to
; pass information to the BASIC loader.
shadow_state = $70
private_ram_in_use = $71
; The following addresses are only used internally by this code.
src = $72 ; 2 bytes
tmp = $74

extended_vector_table = $d9f

; On a B+, code running at $axxx in the 12K private RAM can access shadow RAM
; directly. If the private RAM is free, we copy some code there for use as part
; of the shadow driver - it's faster than going via the OSRDSC/OSWRSC routines.
shadow_copy_private_ram = $af00

; NB: If an OSBYTE call fails, it returns normally with X=$FF. It does *not*
; generate an OS error via BRK; that is a function of the *FX command, not
; OSBYTE itself. I have used *FX a lot in comments, but remember we're really
; dealing with raw OSBYTE.

start
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
    lda #$49
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
    ; SFTODO: This is different from earlier versions of Ozmoo, where if *FX111
    ; returned an unexpected value we failed with an informative error. Is this
    ; change OK? It is friendlier to owners of non-Aries/Watford shadow RAM, who
    ; at least get to use it for screen memory instead of an error, but it might
    ; allow users vulnerable to the *FX111 use by an older Watford DFS to live in
    ; ignorance until they eventually get bitten by it. Gut feeling is doing it
    ; the new way is best, but think about it fresh.
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
    ; value of *FX111 will probably detect a clash here, but there's no
    ; guarantee. If a clash is detected, we will run with shadow RAM for screen
    ; memory only; if we fail to detect a clash we'll try to use spare shadow
    ; RAM via *FX111 and the game is likely to crash at runtime. To solve this,
    ; the user either needs to upgrade the DFS, disable their shadow RAM or,
    ; probably, reorder their ROMs so the shadow RAM driver ROM gets dibs on
    ; *FX111.

    ; We use *FX34,64 to read the current shadow state but we don't check the
    ; return value precisely; all we care about is whether the call is
    ; recognised or not.
    lda #34
    ldx #64
    jsr osbyte
    inx
    beq not_watford
    lda #shadow_state_watford
    jmp set_shadow_state_from_a_and_install_driver
not_watford
    ; *FX34 doesn't work. Try *FX111. We do this for two reasons. Firstly, if it
    ; fails, we obviously have some kind of unknown shadow RAM and we must
    ; content ourselves with using shadow RAM only for screen memory. Secondly,
    ; this gives us a chance to test (imperfectly) if *FX111 is being picked up
    ; by an older Watford DFS.
    ;
    ; If OSBYTE 111 is controlling shadow RAM state, X after *FX111,&40 will be
    ; the shadow state (i.e. 1, as we're in a shadow mode at this point). If
    ; OSBYTE 111 is being picked up by an older Watford DFS and used to return
    ; the current drive, X will *probably* be afterwards 0. (There's no
    ; guarantee; although Ozmoo assumes elsewhere it's being run from drive 0,
    ; it's possible Watford DFS is present but not the current filing system, in
    ; which case Watford DFS's current drive might not be 0 even if Ozmoo is
    ; running from drive 0.)
    ;
    ; SFTODO: *If* we eventually allow running the shadow driver executable from
    ; a preloader (counting the time taken to run it against any default delay
    ; the user requested), note that we may well *not* be in a shadow mode then,
    ; which will interfere with this test.
    ;
    ; SFTODO: Can we make more/cleverer OSBYTE 111 calls to establish with
    ; confidence that it is controlling shadow RAM? Since only b0 of the
    ; returned X is used we always get 0 or 1 back to reflect current displayed
    ; RAM, and thus without video glitching I am not sure we can. Could we issue
    ; a *DRIVE 0 command after checking DFS is the current filing system? This
    ; feels error prone/annoying for the user though - someone is bound to get
    ; bitten by it.
    lda #111
    ldx #$40
    jsr osbyte
    cpx #1
    bne osbyte_111_failed
    ; *FX111 seems to work; we have no absolute guarantee it isn't being
    ; intercepted by an older Watford DFS, but we have to assume it's fine.
    lda #shadow_state_aries
    jmp set_shadow_state_from_a_and_install_driver

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

; The shadow driver API is very simple - the core Ozmoo executable calls the
; subroutine at shadow_ram_copy with A=source page and Y=destination page, and
; that subroutine copies 256 bytes from page A to page Y. One of A or Y will be
; in the $30-$7F inclusive range, indicating it's spare shadow RAM, and the
; other will be <$30, indicating it's main RAM.

; SFTODO: Looks like non-2P shadow driver only copies a 256 byte page. *May*
; want to define a completely separate shadow driver API for 2P host cache case.
; Let's get this working first anyway. (Probably best to make code changes to
; host cache to support shadow RAM first and see what I "want" to be able to do,
; then decide on driver API based on that, rather than guessing and implementing
; the API *first*.)

shadow_driver_integra_b
!pseudopc shadow_ram_copy {
!zone {
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

; For now this code executes in pages $9/$a, so make sure it doesn't overflow.
+assert * <= $b00

; SFTODONOW: Must test that this works for all the different shadow options

; SFTODONOW: Gut feeling based on quick look at code is that for shadow paging driver, entering with A=0 for main and A=1 for shadow is a good API. This mirrors X in OSBYTE &6C - just for "mnemonic" value really. Entering with it in A makes it easier to do swizzling on it (e.g. EOR #1 or ASL A:ASL A) to tweak it to the value actually required within any given driver.
; - probably change !pseudopc to a new shadow_driver_start (=$8c4) and make shadow_ram_copy 2 higher than currently (i.e. $8c6), then $8c4 (which we'd call shadow_driver_page_in_out=$8c4 again) is a two byte value which is zero if the driver doesn't support paging and can be lda #n:jmp (shadow_driver_page_in_out) if it's been checked to be non-zero) - callers obviously have option to e.g. self-modify to make a directly jsr-able version or whatever if they prefer
