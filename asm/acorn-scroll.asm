; SFTODO: Temporary experimental - BBC B only for now

!source "acorn-shared-constants.asm"

; SFTODO: Constant names etc taken from TobyLobster's OS 1.20 disassembly
vdu_text_window_bottom = $309
vdu_text_cursor_y_position = $319
vdu_screen_top_left_address_low = $350
vdu_screen_top_left_address_high = $351
vdu_screen_size_high_byte = $354
vdu_status_byte = $D0       ; Each bit holds part the VDU status:

wrchv = $20e


; SFTODO: for now just assume 80 column mode - though these values are available at $352/$353 as that's where OS keeps them
bytes_per_line = 640

zp = $db ; 5 bytes - VDU temporary storage, 6 bytes starting at $da SFTODO: can I get away with this? simply by experiment I think $da is used by the code I'm going to call in the OS, but the others are OK
src = zp ; 2 bytes
dst = zp+2 ; 2 bytes

; SFTODO: This kinda-sorta works, although if the *OS* scrolls the screen because we print a character at the bottom right cell, its own scroll routines kick and do the clearing that we don't want.
; SFTODO: Damn! My strategy so far has been to just not do that - we control the printing most of the time. But what about during user text input? Oh no, it's probably fine, because we are doing that via s_printchar too. Yes, a quick test suggests it is - but test this with final version, and don't forget to test the case where we're doing split cursor editing on the command line... - I think this is currently broken, copying at the final prompt at the end of thed benchmark ccauses cursor editing to go (non-crashily) wrong when copying into bottom right and causing a scroll

; SFTODO: A quick test on a B+ in non-shadow mode suggests this works. It kinda-sorta works on a M128 in non-shadow mode, but some bits of text end up being invisible, so I must still be doing something wrong. This might be a good thing to investigate next. DFS 2.24 does *HELP (which is what I was testing with) oddly, and even on a M128 with this code *not* running, *SPOOL output doesn't contain DFS *HELP. I suspect this is something to do with its logic to avoid infinite recursion when *SPOOLing. It may or may not be related to this code's problems, but it may be this code works fine on a M128 in "normal" use, and it only has to deal with Ozmoo's output. Yes, superficial testing (can't test Ozmoo itself as it forces shadow RAM and this doesn't support that yet) suggests ordinary output works. Would be curious to find out how DFS 2.24 is feeding these characters to the screen in a way that bypasses WRCHV, but ultimately it's probably not an issue.

; SFTODO: If we inline the single-use subroutines, I think this code is completely relocatable, which is handy, as we ideally don't want multiple copies of it inside the installation executable, and we need to be able to copy part of it into private RAM on the B+ and to insert shadow page in/out tsb/trb instructions before/after the main body of the "our new logic" part of the WRCHV handler.

    * = $b10 ; SFTODO: SUPER TEMP LOCATION, NOT ACCEPTABLE FOR FINAL VSN ON ANY PLATFORM
start
    ; SFTODO: Don't try to protect against being reinstalled for now
    ; SFTODO: A final version would discard this code afterwards rather than having it stick around.
    lda wrchv
    sta parent_wrchv
    lda wrchv+1
    sta parent_wrchv+1
    lda #<our_wrchv
    sta wrchv
    lda #>our_wrchv
    sta wrchv+1
    rts

our_wrchv
    ; We want to minimise overhead on WRCHV, so we try to get cases we're not interested in passed through ASAP.
    cmp #10
    beq lf
jmp_parent_wrchv
parent_wrchv = *+1
    jmp $ffff ; patched
lf
    lda vdu_text_cursor_y_position
    cmp vdu_text_window_bottom
    lda #10
    bcc jmp_parent_wrchv
    ; It's a line feed on the bottom line of the text window.
    ; SFTODO: This code probably needs to disable itself if there is an OS text window defined. We *might* get away without it - we'd just perhaps be optimising some scrolls, and any which go through to the OS as a result of printing at bottom right would have desired effect, just slower. However, the DFS 2.24 *HELP weirdness on M128 would manifest (I think) if you did this during save/restore - remember we do and probably will continue to have an OS text window in effect then, because we can't control printing at bottom right - and there might be other weirdness.
    ; SFTODO: It's not a huge deal, but FWIW - I think this LF character will be omitted from any *SPOOL file being produced, unless we take extra steps to include it.
    txa
    pha
    tya
    pha
    lda vdu_screen_top_left_address_low
    sta src
    lda vdu_screen_top_left_address_high
    sta src+1
    ; lda #19:jsr osbyte ; SFTODO TEMP HACK TO SEE WHAT IT LOOKS LIKE - IT DOESN'T HELP MUCH...
    jsr .hardwareScrollUp
    lda vdu_screen_top_left_address_low
    sta dst
    lda vdu_screen_top_left_address_high
    sta dst+1
    jsr .setCursorSoftwareAndHardwarePosition
!if 0 { ; SFTODO TEMP SKETCHING OUT POSS OPTIMISED COPY CODE
    lda $ffff,y ; 4+ - without y is still 4
    sta $ffff,y ; ;5 - without y is 4


    ldy dst
loop2
    lda dst+1:sta sta_patched+1
    y = max(y, 0x8000 - 16bitdst)
    y = max(y, 0x8000 - 16bitsrc)
loop
    ; SFTODO: If we can keep src and $ffff-patched page-aligned, we can save one cycle on each of these instructions in some cases
    ; SFTODO: What I'm sketching here is wrong, because we can't page-align both src and dst - we should probably prioritise page-aligning src, given we access it twice per loop - no, that's not actually true, because sta has no page-crossing penalty in any form - but I am right for the wrong reason, as that means we need to page-align src as we use it with lda
    lda (src),y
.sta_patched
    sta $ff00,y ; we save one cycle over sta (dst),y here - can't do it for the other two because we need two instructions referencing same address
    lda #0
    sta (src),y
    iny
    bne loop
    count -= y before start of loop
    y = max(255, count)
    bne loop2



    ldy src:lda #0:sta src ; SFTODO: correct-ish, but following anti-wrap-in-inner-loop checks will possibly *reduce* y, and that's wrong for first pass
loop2
    y = max(y, 0x8000-16bitsrcwith0lowbyte)
    y = max(y, 0x8000-(16bitdst+y)) ; ???
    count -= y
loop
    lda (src),y
dst = * + 1
    sta $ffff,y ; patched
    lda #0
    sta (src),y
    iny
    bne loop
    inc-with-wrap src
    inc-with-wrap dst
    y = max(256, count) ; 256 means Y=0, of course
    branch-if-count-ne loop2



    ; SFTODO: note to self - we care about speed, but we also care about code size. we want this to be as fast as is reasonable without it getting ultra-mega-complex.
    ; SFTODO: *maybe* if dst>=$7f00, we do a slow-but-simple copy a byte at a time? it won't be a super common case so no massive performance impact and it may save a lot of complexity elsewhere
    ; We keep src page-aligned because lda (src),y has a page-crossing penalty; the sta instructions do not.
    ldy src:lda #0:sta src
loop2
    ; SFTODO: *if* dst+y>=$8000, the following loop will go too far and we need to do something to stop it wrapping
    ; SFTODO: Note that because src is page aligned, (src),y cannot need wrapping inside the inner loop - dst,y can though
    count -= y
loop
    lda (src),y
dst = *+1
    sta $ffff,y ; patched
    lda #0
    sta (src),y
    iny
    bne loop
    inc-with-wrap src
    inc-with-wrap dst - but this is too simplistic, because dst can wrap part-way through loop
    ldy #max(256, count) ; 256 means Y=0, of course
    branch-if-count-ne loop2



    ; SFTODO: note to self - we care about speed, but we also care about code size. we want this to be as fast as is reasonable without it getting ultra-mega-complex.
    ; SFTODO: *maybe* if dst>=$7f00, we do a slow-but-simple copy a byte at a time? it won't be a super common case so no massive performance impact and it may save a lot of complexity elsewhere
    ; We keep src page-aligned because lda (src),y has a page-crossing penalty; the sta instructions do not.
    min_y=src(low byte):lda #0:sta src
loop2
    ; SFTODO: *if* dst+y>=$8000, the following loop will go too far and we need to do something to stop it wrapping
    ; SFTODO: Note that because src is page aligned, (src),y cannot need wrapping inside the inner loop - dst,y can though
    lda dst:cmp #$7f:bcs slow_loop_prep
    y = min(min_y, 0)
    min_y = 0 ; only relevant on first pass
    count -= y ; SFTODO NO THIS IS BROKEN AS WRITTEN, WHAT IF WE DON'T WANT TO COPY A FULL 256 BYTES IN THE INNER LOOP? WE ONLY BREAK OUT IF Y WRAPS TO 0
loop
    lda (src),y
dst = *+1
    sta $ffff,y ; patched
    lda #0
    sta (src),y
    iny
    bne loop
    inc-with-wrap src
    inc-with-wrap dst - but this is too simplistic, because dst can wrap part-way through loop
    ldy #max(256, count) ; 256 means Y=0, of course
    branch-if-count-ne loop2

slow_loop_prep
    clc:tya:adc dst:sta dst2:lda dst+1:adc #0:sta dst2
    ldy #0
    ; SFTODO: I'm temporarily ignoring the fact we set src low byte to 0 earlier
slow_loop
    lda (src),y
dst2 = *+1
    sta $ffff ; patched
    lda #0
    sta (src),y
    inc src-with-wrap ; because we can avoid src-related wrapping the in fast loop, we don't take any special action (beyond applying any wrap) when incing src
    inc dst-with-wrap *and* if it wraps, dec16bitcount and escape back to fast loop
    dec 16bitcount
    bne slow_loop
    ; we've done the whole copy

}
    ; We copy and bump in chunks of 128 because the line length in bytes is
    ; always a multiple of 128, which means we can wrap at the top of screen
    ; memory between each chunk without any problems.
    ; SFTODO: The "tearing" seen on the screen might be more visually pleasing
    ; if this loop copied with Y advancing rather than decrementing, and the
    ; performance impact wouldn't be huge. (We could maybe even offset src/dst
    ; and run Y from 128 to 0 to avoid an extra cpy# at the end of the loop.)
    ; SFTODO: In 320-bytes-per-line mode, we need to use 64 byte chunks to have
    ; the same guarantee. But note that the following ldx is "constant" because 640/128==320/64, so we kind of have "ldx #number_of_lines_to_preserve*5"
    ldx #(bytes_per_line / 128)
copy_and_zero_outer_loop
    ldy #127
copy_and_zero_loop
    lda (src),y
    sta (dst),y ; SFTODO: we can save a cycle by doing dst=*+1:sta $ffff,y - this comes at no extra complexity cost really
    lda #0 ; SFTODO HACK lda #%10101010
    sta (src),y
    dey
    bpl copy_and_zero_loop
    clc
    lda src
    adc #128
    sta src
    bcc no_src_wrap
    inc src+1
    bpl no_src_wrap
    sec
    lda src+1
    sbc vdu_screen_size_high_byte
    sta src+1
no_src_wrap
    clc
    lda dst
    adc #128
    sta dst
    bcc no_dst_wrap
    inc dst+1
    bpl no_dst_wrap
    sec
    lda dst+1
    sbc vdu_screen_size_high_byte
    sta dst+1
no_dst_wrap
    dex
    bne copy_and_zero_outer_loop
    pla
    tay
    pla
    tax
    rts

; SFTODO: A LOT OF THESE AREN'T NEEDED ANY MORE (AND ANY I KEEP SHOULD BE RENAMED TO MY foo_bar_baz STYLE)
; Code copied from OS 1.2 (TobyLobster disassembly).
.vduWriteCursorScreenAddressLow             = $D8       ; } address of the top of the cell
.vduWriteCursorScreenAddressHigh            = $D9       ; } on screen for the write cursor
; SFTODO: Row multiplication table probably doesn't exist on Master so need to find alternative
.vduMultiplicationTableLow                  = $E0       ; stores which multiplication table
.vduMultiplicationTableHigh                 = $E1       ; to use
;.vduTextWindowBottom = $309
.vduTextCursorXPosition                     = $0318     ; } text cursor position
.vduTextCursorYPosition = $319
.vduTextCursorCRTCAddressLow                = $034A     ; CRTC address of the cursor
.vduTextCursorCRTCAddressHigh               = $034B     ;
.vduBytesPerCharacter                       = $034F     ;
.vduScreenTopLeftAddressLow = $350
.vduScreenTopLeftAddressHigh = $351
.vduScreenSizeHighByte = $354
.vduCurrentScreenMODE                       = $0355     ;
.vduCurrentScreenMODEGroup                  = $0356     ; MODE group = screen memory size:
.vduTextInputCursorYCoordinate              = $0365     ;
.vduTempStoreDA                             = $DA       ; }
.crtcAddressRegister                        = $FE00     ;
.crtcAddressWrite                           = $FE01     ;
.vduStatusByte                              = $D0       ; Each bit holds part the VDU status:
.vduTextWindowTop                           = $030B     ; }
.vduBytesPerCharacterRowLow                 = $0352     ;
.vduBytesPerCharacterRowHigh                = $0353     ;



.crtcStartScreenAddressHighRegister         = 12        ;

.crtcCursorPositionHighRegister             = 14        ;


; SFTODO: Only one caller, could and probably should inline
.setCursorSoftwareAndHardwarePosition
; This is not generic code (unlike the OS routine of the same name); it's
; special-cased because we know we are moving down one row from the current
; position.
    clc
    ; We start with vduTextCursorCRTCAddress{Low,High} as the base, since it doesn't wrap so we don't "accumulate" wrapping.
    ; SFTODO: I am not sure this is strictly right. Remember *really* we want to set vduTextCursorCRTCAddress = screen base (&3000 or whatever) + 320 (or 640) * text cursor Y. If this code was the *only* think updating ...CursorCRTCAddress..., it would soar off infinitely high as we did newline after newline after newline. In reality the OS driver will get involved as we output other text to the screen and that will pull it back into range, but it's technically *possible* a game would emit a long stream of nothing but newlines and this would probably go wrong. Test it and see about fixing it if necessary.
    lda .vduTextCursorCRTCAddressLow
    adc .vduBytesPerCharacterRowLow
    sta .vduWriteCursorScreenAddressLow
    sta .vduTextCursorCRTCAddressLow
    tax
    lda .vduTextCursorCRTCAddressHigh
    adc .vduBytesPerCharacterRowHigh
    sta .vduTextCursorCRTCAddressHigh                   ; store the text cursor CRTC address (before any wraparound)
!if 1 { ; SFTODO EXPERIMENTAL
    ; vduTextCursorCRTCAddress works in a logical address space running from the screen start address to the screen start address plus vduScreenSizeHighByte pages. It therefore doesn't wrap at $8000, but does need to wrap at $8000+screen size. The actual OS routines never encounter this case, because they calculate it from scratch here based on the text cursor Y, which can never generate a value high enough to need to wrap in that logical address space. Since we are just adding vduBytesPerCharacterRow every time, if the proper OS version of this code doesn't happen to get called to do the multiplication-based version, vduTextCursorCRTCAddress will advance past the point where it needs wrapping. This is unlikely put possible; you can see it with a test program that does PRINT "Hello";:REPEAT:VDU 10:UNTIL FALSE (perhaps with a small delay after VDU 10 to help see what's going on). We don't want to rely on the OS routine happening to be called before this goes wrong, so we apply some wrapping of our own. SFTODO: I believe this is correct, but review it fresh.
    ; SFTODO: If we calculated $80+.vduScreenSizeHighByte earlier and saved it somewhere (even patching cmp # instruction here, maybe), we wouldn't need to do the and #$7f here and therefore we wouldn't corrupt A and we could avoid the lda to restore it before we subtract just below. We could also probably get rid of the bpl + - that might (only *might*) slow things down, but just a tiny fraction - it might also be a negligible win, depending on statistics of how often the bpl + avoids the following check. Plus of course if just do a straight cmp we could avoid the sta immediately preceding us, and just store after any wrapping.
    bpl +
    and #$7f ; subtract $80
    cmp .vduScreenSizeHighByte
    bcc +
    ; sec - redundant
    lda .vduTextCursorCRTCAddressHigh
    sbc .vduScreenSizeHighByte
    sta .vduTextCursorCRTCAddressHigh
+
}
    bpl +
    sec
    sbc .vduScreenSizeHighByte
+
    sta .vduWriteCursorScreenAddressHigh
    LDA .vduTextCursorCRTCAddressHigh                   ; text cursor CRTC address SFTODO: we could save a byte by tay-ing above (as well as sta-ing) and doing tya here
    LDY #.crtcCursorPositionHighRegister                ; Y=14
    ; fall through...
.setHardwareScreenOrCursorAddress
    STX .vduTempStoreDA                                 ; store X
    LSR                                                 ; divide X/A by 8
    ROR .vduTempStoreDA                                 ;
    LSR                                                 ;
    ROR .vduTempStoreDA                                 ;
    LSR                                                 ;
    ROR .vduTempStoreDA                                 ;
    LDX .vduTempStoreDA                                 ;
    STY .crtcAddressRegister                            ; set which CRTC register to write into
    STA .crtcAddressWrite                               ; write A into CRTC register
    INY                                                 ; increment Y to the next CRTC register
    STY .crtcAddressRegister                            ; set which CRTC register to write into
    STX .crtcAddressWrite                               ; write X into CRTC register
    RTS                                                 ;

.hardwareScrollUp
    LDA .vduScreenTopLeftAddressLow                     ; screen top left address low
    CLC                                                 ;
    ADC .vduBytesPerCharacterRowLow                     ; add bytes per character row
    TAX                                                 ; put low byte back into X
    LDA .vduScreenTopLeftAddressHigh                    ; screen top left address high
    ADC .vduBytesPerCharacterRowHigh                    ; add bytes per character row high byte (and carry)
    BPL +                                               ;
    SEC                                                 ; wrap around
    SBC .vduScreenSizeHighByte                          ; screen RAM size high byte
+
    STA .vduScreenTopLeftAddressHigh                    ; screen top left address high
    STX .vduScreenTopLeftAddressLow                     ; screen top left address low
    LDY #.crtcStartScreenAddressHighRegister            ; Y = value to change screen address
    BNE .setHardwareScreenOrCursorAddress               ; ALWAYS branch to set screen address

; SFTODO: Of course (assuming they survive) some of these subroutines are called only once and can be inlined.

; SFTODO: If we have a "number of lines to protect" variable which Ozmoo can poke to control this, it being 0 would naturally mean "disable this code". I am not sure off top of my head if we want/need that, but I guess we might.

; SFTODO: OK, right now *without* this driver, using split cursor editing to copy when the inputs cause the screen to scroll causes split cursor editing to terminate. I am surprised - we are not emitting a CR AFAIK - but this is acceptable (if not absolutely ideal) and if it happens without this driver being in the picture I am not going to worry about it too much. But may want to investigate/retest this later. It may well be that some of the split cursor stuff I've put in this code in a voodoo-ish ways turns out not to actually matter after all.

; SFTODO: We should probably disable (in as few bytes of code as possible) our custom OSWRCH routine if we quit and don't force press break - or maybe this should just be the job of any custom code that runs on quit and BREAK is the default and all we care about in detail?

end

    +assert * < $d00

; SFTODO: Some thoughts on making this work properly without hacks:
;
; If https://stardot.org.uk/forums/viewtopic.php?f=54&t=12242&p=155982&hilit=fc7f#p155982 is correct (and looking at the Elkulator source seems to back it up), code running at &8000 or above can toggle MRB access for the *whole* of the lower 32K. So we could steal 512 bytes from the last sideways RAM bank, treat that as short and install a variant on the above code in there if we're running with MRB shadow. We could probably also shove a B+ private RAM style shadow driver in there, although it still wouldn't be paging capable it would be faster at copying whole pages. The biggest faff factor there is really the problem of the loader deciding whether we can "afford" to lose 512 bytes of sideways RAM to this - or are we willing to say "if you have MRB, you must be willing to give this up?" Not sure. Ah, and there's also a post by Sarah Walker pointing out that you can (probably? not sure if she tried it) use the MRB OS routine to copy your code across to the corresponding location in the "other" RAM beforehand, and then you *can* toggle MRB access (carefully - may need to disable interrupts too, not sure) while running from <&8000 without crashing. (As per notes below, it would still be faster - bear in mind the "shadow" RAM is probably the slow RAM, so we'd be running in fast RAM by default but when our driver toggled over into MRB mode, we'd be running from the slow RAM - the video memory will always be slow, but if we were running from sideways RAM we'd not suffer any slowdown in our code. So running from sideways RAM is still better, except it raises the - maybe awkward, maybe not that hard really, not thought in detail - issue of how to decide if we can spare 512 bytes of sideways RAM for our driver.
;
; I do wonder if the "cursor down to scroll, cursor back up, then print last character on line" printing method we are using with this new style code will look "ugly" - did I deliberately choose not to do this in Ozmoo up to now (because it is equally valid with the OS driver) or did I just not think of it? Gut feeling is that with the scrolling happening the human eye is not going to notice, in my brief play with the new hacked code I didn't notice this.
;
; Working on the assumption that's OK, it's probably best to start by tidying up the existing HW scrolling to use this way of handling printing on the bottom right of the screen. That should simplify the code a bit (e.g. we will probably then be mostly sharing code for newline and scroll-when-printing-at-bottom-right, and/or the code will be so simple there's little to share) and will make it cleaner to integrate support for scrolling with the assistance of a custom scroll handler like the one in this file while still allowing a build-time option to have old-style HW scrolling.
;
; For the B+ I can probably use part of the 512 byte page of private RAM I'm already stealing for a shadow-based part of a driver like the above. If we don't have private RAM, maybe I'd fall back to existing HW scrolling? So maybe it wouldn't be a build-time option but we'd have to allow the loader to choose between them? This complicates things in some ways, but it's also quite nice as it opens up the possibility of saying "for now, Electron with MRB and B+ with private RAM used by something else don't have the new scrolling", rather than *having* to implement it for everything straight away.
;
; I called OS routines in the above because as a hack it only had to work on 1.2 and I had Toby's disassembly right there with all the subroutines the actual OS VDU 10 scroll driver uses, so it was natural and easy. In reality I'd probably want to copy tweaked/minimised versions of that code into this driver - if nothing else, with minor tweaks (calling into our shadow driver to page shadow ram in/out, or to call into a copy loop in private RAM on B+), given the basic similarities of the OS and the VDU variable locations, I suspect a single version of this code would run on B/B+/Master (including all the OS variants 3.2/3.5/5.0).
;
; Worth noting that even without MRB issues, it would be *nicer* to implement this on the Electron by stealing 512 bytes of sideways RAM because then this code would be running at 2MHz (although it would still be contending with the ULA for the screen data itself, but that's no worse than the OS doing it). But maybe for a first cut, I'd support Electron running only from low RAM and only if there's no MRB (shadow; MRB turbo is absolutely fine of course, assuming the machine is "big" enough to run the game OK without the extra shadow RAM), and then this could be tweaked further later on.
;
; Given we always use software scrolling in mode 7 and will continue to do so, plus we will continue to use the OS text window facility for save/restore (when arbitrary * commands can execute), our OSWRCH handler needs to disable itself when an OS text window is in effect. I don't think we need to explicitly check for mode 7, because the text window check will subsume it.
;
; If it's not obvious above, this driver is *not* general purpose because if a character is printed at the bottom right of the screen, the OS does the scroll itself in the usual way.
;
; Maybe this driver *doesn't* need to disable itself if there's an OS text window defined (but if that's true it should probably disable itself in mode 7 to avoid confusion). Or at least if there's an OS text window defined covering all but the top row of the screen. (Games with "bigger" top rows were never compatible with our hw scrolling use and will force sw scrolling with suitable OS text windows, and we need to make sure that doesn't break.) If there is an OS text window defined which covers all but the top window of the screen, the scrolling performed by this code is faster and uglier, but compatible, and if the OS decides to do the scroll itself because of printing at bottom right the text window will mean it does the right thing. However, this would mean we'd need some other way to allow Ozmoo to toggle between software and hardware scrolling at runtime, and it also gives us a more complex check to perform before triggering our new behaviour. So gut feeling is that all in all it's simpler both here and in the Ozmoo code for this driver to just disable itself if there is a software text window in effect, then everything should "just work" nicely.
;
; The OSWRCH driver installer would be quite similar in style to the shadow driver installer (though it may not fit in 512 bytes, in which case it might have to be handled slightly differently as it runs in main RAM and would need to run suitably high but not so high it clashes with mode 6 screen etc etc) - check the hardware, copy/tweak the relevant OSWRCH driver into some bit of low memory and install it
;
; It is a bit of a shame we can't get rid of the in-Ozmoo code to handle old-style hardware scrolling, but at least it isn't a huge quantity of code - still, it would have been nice to shrink the Ozmoo executable a bit and just maybe move towards getting an extra 512 bytes of RAM free at runtime (it all adds up, right?). I half wonder if the OSWRCH driver installer could install a copy of the old-style hw scrolling code from Ozmoo somewhere if it doesn't have a new-style driver, but that feels like it could get messy.
;
; What I've somewhat overlooked in all the above and in the hacky version of the code is that core Ozmoo needs to to be able to turn this new behaviour on and off, because it's only appropriate where there's a single line window on the top line. So we either need some sort of flag we can poke (across the tube if necessary) to say "yes, do your stuff" or "don't do your stuff", or we need to make the logic conditional on the OS text window dimensions, and in that case we *still* need some way to tell this code to disable itself so we can get software scrolling with the OS text window in place if that's what the user wants.
;
; I half wonder if (by peeking OS text cursor Y and text fg/bg colours) it wouldn't be *too* hard for a pure-OS OSWRCH version of the current HW scrolling implementation to be added, although to start with I should definitely try simplifying the pure OS HW scrolling in Ozmoo itself and if that doesn't add too much complexity it may be silly to go to the trouble and risk of bugs by trying to implement the same thing on the OSWRCH vector with less information to hand and less "nicely allocated" memory available etc.
;
; Hmm. Given we're copying in place, there's no real reason this new style approach can't be used for games with arbitrarily large upper windows, is there? I guess as the lower window shrinks and the upper window grows, the ugliness of this technique increases and the sluggishness of the OS software scrolling decreases, but we could allow this technique to work on any size window, maybe implementing a threshold above which we fall back to software scrolling. (This would mean the internal software scrolling state would be "software scroll if user explicitly set it, or if upper window is over threshold".) For now it might be best to just stick to upper windows of size 1, but if it's easy or maybe even *easier* to allow this from the start, maybe go for it.
;
; Since I am going to have to keep the current text window based scrolling around for soft scroll mode, I should make sure to tidy that up as much as possible while doing the tidy up of the existing hw scroll approach. I suspect unfortunately I *will* need/want to allow the OS to scroll after printing a text character at bottom right in soft scroll mode, as the slower software scrolling might make it more obvious that the last letter doesn't appear until after the scroll. I should maybe test it, it might not be obvious. Hmm, a quick test with a BASIC program doing it in mode 3 suggests it might look OK. Perhaps go with it, and if I need to reintroduce the old approach later it may well be done better starting from a cleaner simpler code base anyway.

; SFTODO: Although that code was probably written without much care about code size, STEM has a highly-optimised line-oriented memmove which would potentially be ideal for copying the line data when we scroll (especially if we start to do it for >1 line of data). Definitely worth taking a look. (It does also have to run from ROM, so there may be more scope for better or simpler optimisation using self-modifying code.)
