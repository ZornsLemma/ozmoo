; SFTODO: Temporary experimental - BBC B only for now

!source "acorn-shared-constants.asm"

; SFTODO: Constant names etc taken from TobyLobster's OS 1.20 disassembly
vdu_text_window_bottom = $309
vdu_text_cursor_y_position = $319
vdu_screen_top_left_address_low = $350
vdu_screen_top_left_address_high = $351
vdu_screen_size_high_byte = $354
set_cursor_software_and_hardware_position = $c6af
hardware_scroll_up = $c9a4
move_text_cursor_to_next_line = $cd3f ; SFTODO: probably does more than strictly needed FWIW

wrchv = $20e


; SFTODO: for now just assume 80 column mode - though these values are available at $352/$353 as that's where OS keeps them
bytes_per_line = 640

zp = $db ; 5 bytes - VDU temporary storage, 6 bytes starting at $da SFTODO: can I get away with this? simply by experiment I think $da is used by the code I'm going to call in the OS, but the others are OK
src = zp ; 2 bytes
dst = zp+2 ; 2 bytes

; SFTODO: This kinda-sorta works, although if the *OS* scrolls the screen because we print a character at the bottom right cell, its own scroll routines kick and do the clearing that we don't want.

    * = $b10
start
    ; SFTODO: Don't try to protect against being reinstalled for now
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
    txa
    pha
    tya
    pha
    lda vdu_screen_top_left_address_low
    sta src
    lda vdu_screen_top_left_address_high
    sta src+1
    ; lda #19:jsr osbyte ; SFTODO TEMP HACK TO SEE WHAT IT LOOKS LIKE - IT DOESN'T HELP MUCH...
    jsr move_text_cursor_to_next_line
    jsr hardware_scroll_up
    lda vdu_screen_top_left_address_low
    sta dst
    lda vdu_screen_top_left_address_high
    sta dst+1
    ; We copy and bump in chunks of 128 because the line length in bytes is
    ; always a multiple of 128, which means we can wrap at the top of screen
    ; memory between each chunk without any problems.
    ; SFTODO: The "tearing" seen on the screen might be more visually pleasing
    ; if this loop copied with Y advancing rather than decrementing, and the
    ; performance impact wouldn't be huge. (We could maybe even offset src/dst
    ; and run Y from 128 to 0 to avoid an extra cpy# at the end of the loop.)
    ldx #(bytes_per_line / 128)
copy_and_zero_outer_loop
    ldy #127
copy_and_zero_loop
    lda (src),y
    sta (dst),y
    lda # 0 ; SFTODO HACK lda #%10101010
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
    jsr set_cursor_software_and_hardware_position
    pla
    tay
    pla
    tax
    rts
end

    +assert * < $d00

; SFTODO: Some thoughts on making this work properly without hacks:
;
; If https://stardot.org.uk/forums/viewtopic.php?f=54&t=12242&p=155982&hilit=fc7f#p155982 is correct (and looking at the Elkulator source seems to back it up), code running at &8000 or above can toggle MRB access for the *whole* of the lower 32K. So we could steal 512 bytes from the last sideways RAM bank, treat that as short and install a variant on the above code in there if we're running with MRB shadow. We could probably also shove a B+ private RAM style shadow driver in there, although it still wouldn't be paging capable it would be faster at copying whole pages. The biggest faff factor there is really the problem of the loader deciding whether we can "afford" to lose 512 bytes of sideways RAM to this - or are we willing to say "if you have MRB, you must be willing to give this up?" Not sure.
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
