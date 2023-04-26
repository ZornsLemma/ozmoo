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

shadow_state_none        = 0 ; no shadow RAM
shadow_state_screen_only = 1 ; shadow RAM with no driver for spare shadow RAM access
shadow_state_integra_b   = 2 ; Integra-B shadow RAM
shadow_state_mrb         = 3 ; Electron Master RAM Board shadow RAM

shadow_state
    !byte shadow_state_none
start
    ; Do we have shadow RAM?
    lda #osbyte_read_screen_address_for_mode
    ldx #shadow_mode_bit + 0
    jsr osbyte
    tya
    bmi have_shadow_ram
have_shadow_ram
    ; We have shadow RAM, which we can use for screen memory via the OS without
    ; any special knowledge on our part.
    lda #shadow_state_screen_only
    sta shadow_state
    ; Are we running on an Integra-B?
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
    ; shadow_state set to shadow_state_screen_only.
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

    b
    SFTODO
    rts

set_shadow_state_from_a_and_install_driver
    sta shadow_state
    +assert FALSE ; SFTODO INSTALL DRIVER

restore_brkv
    lda old_brkv
    sta brkv
    lda old_brkv+1
    sta brkv+1
    rts


end
