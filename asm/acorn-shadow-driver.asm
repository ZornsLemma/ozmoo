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

max_shadow_driver_size = shadow_ram_copy_max - shadow_ram_copy

!macro assert_shadow_driver_fits start {
    +assert * - start <= max_shadow_driver_size
}

shadow_state_none        = 0 ; no shadow RAM
shadow_state_screen_only = 1 ; shadow RAM with no driver for spare shadow RAM access
shadow_state_first_driver = 2
shadow_state_integra_b   = 2 ; Integra-B shadow RAM
shadow_state_mrb         = 3 ; Electron Master RAM Board shadow RAM

; SFTODO: Putting shadow_state "inline" here assumes this code lives at $900-ish and the fact can be set on load and will persist here - if this code ends up loading high, we need to set shadow_state=$900 or similar so it's at a fixed location for the loader to access. (Note that we *don't* need access to this value in the Ozmoo executable, so actually we probably don't want to waste a persistent byte on it - we can maybe stick it in the resident integer space somewhere it will be safe until the loader finishes, but let Ozmoo executable scribble all over it.)
shadow_state
    !byte shadow_state_none
start
    ; Do we have shadow RAM?
    lda #osbyte_read_screen_address_for_mode
    ldx #shadow_mode_bit + 0
    jsr osbyte
    tya
    bpl no_shadow_ram
    ; We have shadow RAM, which we can use for screen memory via the OS without
    ; any special knowledge on our part.
    lda #shadow_state_screen_only
    sta shadow_state
    ; Are we running on an Integra-B? We check for this explicitly as the
    ; Integra-B spoofs the result of osbyte_read_host.
    lda brkv
    sta old_brkv
    lda brkv+1
    sta old_brkv+1
    lda #<not_integra_b
    sta brkv
    lda #>not_integra_b
    sta brkv+1
    lda #$49 ; SFTODO: USE NAMED CONSTANT
    ldx #$ff
    ldy #0
    jsr osbyte
    jsr restore_brkv
    cpx #$49
    bne not_integra_b
    ; We're running on an Integra-B.
    lda #shadow_state_integra_b
    jmp set_shadow_state_from_a_and_install_driver
not_integra_b
    jsr restore_brkv
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
    ; We're on an Electron. Check for Master RAM Board shadow RAM.
    lda #$ef ; SFTODO: USE NAMED CONSTANT
    ldx #0
    ldy #$ff
    jsr osbyte ; SFTODO: Could this BRK if we have shadow RAM of non-MRB type???
    ; SFTODO: Loader checks for &80 exactly, beebwiki talks about b7 being set - I suspect I do it this way in the loader because the MRB's own docs say that, but check and comment/fix this.
    cpx #$80
    bne electron_not_mrb
    ; We're on an Electron with Master RAM Board shadow RAM.
    lda #shadow_state_mrb
    jmp set_shadow_state_from_a_and_install_driver
electron_not_mrb
    ; We're on an Electron with shadow RAM but it's not a Master RAM board. We
    ; don't know how to access spare shadow RAM on this hardware, so leave
    ; shadow_state set to shadow_state_screen_only. SFTODO: Arguably we could/should fall through to the following BBC B tests, which *might* work. However, my suspicion is there are no BITD shadow RAM solutions for the Electron other than the MRB, and any new one is likely to use Master-style control which we don't currently have any test code for (since we only use it on Integra-B and Master where we can explicitly detect the system type).
no_shadow_ram
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

    jsr save_brkv
    lda #<not_watford
    sta brkv
    lda #>not_watford
    sta brkv+1
    ; Whether shadow mode is currently in operation is a little fuzzy, so use
    ; *FX34,64 to read the current state and do nothing with it rather than
    ; *setting a state. All we care about is whether an error occurs.
    lda #34
    ldx #64
    jsr osbyte
    jsr restore_brkv ; sftodo: Fold this into set_shadow_state...
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
    ; SFTODO: Can we make more/cleverer OSBYTE 111 calls to establish with confidence that it is controlling shadow RAM? Since only b0 of the returned X is used we always get 0 or 1 back to reflect current displayed RAM, and thus without video glitching I am not sure we can. Could we issue a *DRIVE 0 command after checking DFS is the current filing system? This feels error prone/annoying for the user though - someone is bound to get bitten by it.
    lda #<fx111_failed
    sta brkv
    lda #>fx111_failed
    sta brkv+1
    lda #111
    ldx #$40
    jsr osbyte
    cpx #1
    beq fx111_probably_ok
    +assert FALSE ; SFTODO: What should we do? Set a flag so the loader can generate the error it previously generated, or just revert to using shadow RAM for screen memory only? THe latter at least means the user can get some benefit. We could also maybe make the hardware detected line something like "shadow RAM (screen only; *FX111 failure)" which might prompt the user to investigate but not force them to.
fx111_probably_ok
    jsr restore_brkv
    ; *FX111 seems to work; we have no absolute guarantee it isn't being intercepted by an older Watford DFS, but we have to assume it's fine.
    lda #shadow_state_aries
    jmp set_shadow_state_from_a_and_install_driver
fx111_failed
    ; *FX111 generated an error, so neither *FX34 nor *FX111 works and we
    ; *therefore don't know how to access the spare shadow RAM. Leave
    ; *shadow_state at the default shadow_state_screen_only.
    jmp restore_brkv

set_shadow_state_from_a_and_install_driver
    sta shadow_state
    +assert FALSE ; SFTODO INSTALL DRIVER
    tax ; SFTODO: pass value in X in first place?
    lda shadow_driver_table_low-shadow_state_first_driver,x
    sta src
    lda shadow_driver_table_high-shadow_state_first_driver,x
    sta src+1
    +assert (max_shadow_driver_size-1) < 128
    ldy #max_shadow_driver_size-1
copy_loop
    lda (src),y
    sta shadow_ram_copy,y
    dey
    bpl copy_loop
    rts

; SFTODO: Need to keep this in sync with shadow_state_* enum
shadow_driver_table_low
    !byte <shadow_driver_integra_b
    !byte <shadow_driver_electron_mrb
    ; SFTODO: MORE
shadow_driver_table_high
    !byte >shadow_driver_integra_b
    !byte >shadow_driver_electron_mrb
    ; SFTODO: MORE

; SFTODO: Document shadow driver API
; SFTODO: Looks like non-2P shadow driver only copies a 256 byte page. *May* want to define a completely separate shadow driver API for 2P host cache case. Let's get this working first anyway. (Probably best to make code changes to host cache to support shadow RAM first and see what I "want" to be able to do, then decide on driver API based on that, rather than guessing and implementing the API *first*.)

shadow_driver_integra_b
!pseudopc shadow_ram_copy {
    ; SFTODO: Since the Ozmoo executable pokes directly at Integra-B hardware
    ; registers, we might as well do so here to page shadow RAM in and out; it
    ; would be faster. But I'll stick with this for now.
    ; SFTODO: This is *similar* to the Watford/Aries driver (though OSBYTE is
    ; 108, and the 1/0 codes are swapped) so there might be potential for
    ; sharing code. But this is moot if I switch to driving the hardware direct.
    sta lda_abs_y+2
    sty sta_abs_y+2
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
    bne copy_loop
    ; Page out shadow RAM
    lda #$6c ; SFTODO: name constant
    ldx #0
    jmp osbyte
}
+assert shadow_driver_fits shadow_driver_integra_b

shadow_driver_electron_mrb
!pseudopc shadow_ram_copy {
    cmp #$30
    bcs copy_from_shadow
    ; We're copying to shadow RAM.
    sta lda_abs_x+2
    ldx #0
    ; SFTODO: Can we assume V is preserved by $FBFD and move the set/clear outside the loop?
.copy_to_shadow_loop
.lda_abs_x
    lda $ff00,x ; patched
    bit our_rts ; set V
    jsr $fbfd ; write to shadow RAM SFTODO: use named constant?
    inx
    bne copy_to_shadow_loop
.our_rts
    rts
.copy_from_shadow
    ; We're copying from shadow RAM.
    sty sta_abs_x+2
    tay
    ldx #0
.copy_from_shadow_loop
    clv
    jsr $fbfd ; read from shadow RAM SFTODO: use named constant?
.sta_abs_x
    sta $ff00,x ; patched
    inx
    bne copy_from_shadow_loop
    rts
}
+assert shadow_driver_fits shadow_driver_electron_mrb

; SFTODONOW: Must also port the faster B+ shadow driver which uses code at &A000, but let's keep it simple for the moment.
shadow_driver_b_plus_os
!pseudopc shadow_ram_copy {
oswrsc = $ffb3
osrdsc = $ffb9
    cmp #$30
    bcs copy_from_shadow
    ; We're copying to shadow RAM.
    sta lda_abs_y+2
    sty $d7 ; SFTODO: named constant for d6/d7?
    ldy #0
    sty $d6
.copy_to_shadow_loop
.lda_abs_y
    lda $ff00,y ; patched
    jsr osrdsc ; preserves Y - equivalent to STA (&D6),Y - note Y is used!
    iny
    bne copy_to_shadow_loop
    rts
.copy_from_shadow
    ; We're copying from shadow RAM.
    sta $f7 ; SFTODO: named constant for f6/f7?
    sty sta_abs+2
    ldy #0
    sty $F6
.copy_from_shadow_loop
    jsr osrdsc ; ignores and corrupts Y
.sta_abs
    sta $ff00 ; patched
    inc $f6
    inc sta_abs+1
    bne copy_from_shadow_loop
    rts
}
+assert shadow_driver_fits shadow_driver_b_plus_os

; SFTODO: Do a code review at end to ensure this is called on every relevant code path. Just possibly we should always call it before we return (make the "main()" do jsr actual_code:fall_through_to restore_brkv) and make sure every function we install on brkv starts by executing this.
restore_brkv
    lda old_brkv
    sta brkv
    lda old_brkv+1
    sta brkv+1
    rts


end



; SFTODONOW: Make sure we set and clear any necessary flags about private RAM in use - this may be a mostly internal detail to this code but not sure (we may want shadow status values to differ based on this so we can report to user)
