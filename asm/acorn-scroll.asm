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
evntv = $220

; SFTODO: These are hacks; I need to sort out the locations used for this kind of early stage initialisation, because at the moment things trample on each other.
fast_scroll_screen_mode = $90
fast_scroll_status = $91

; SFTODO: for now just assume 80 column mode - though these values are available at $352/$353 as that's where OS keeps them
bytes_per_line = 640

zp = $db ; 5 bytes - VDU temporary storage, 6 bytes starting at $da SFTODO: can I get away with this? simply by experiment I think $da is used by the code I'm going to call in the OS, but the others are OK
src = zp ; 2 bytes
dst = zp + 2 ; 2 bytes

irq1v = $204
us_per_scanline = 64
us_per_row = 8*us_per_scanline
vsync_position = 35
total_rows = 39
SFTODO99 = total_rows * us_per_row ; SFTODO: idea is that as we're comparing against timer most of the time (not loading it) we don't want the 2*us_per_scanline adjustment
frame_time_us = SFTODO99 - 2 * us_per_scanline
first_safe_start_row_time_us = SFTODO99 - ((total_rows - vsync_position) + 2) * us_per_row
last_safe_start_row_time_us = SFTODO99 - ((total_rows - vsync_position) + 20) * us_per_row ; SFTODO: ARBITRARY 20

DEBUG_COLOUR_BARS = 1
;DEBUG_COLOUR_BARS2 = 1
; 1=red=post-vsync
; 3=yellow=first timer 2
; 2=green=second timer 2
; 4=blue=safe raster, starting copy
; 6=cyan=first timer 2 if occurs during copy
; 5=magenta=second timer 2 if occurs during copy
; 0=black=copy finished

; SFTODO: This kinda-sorta works, although if the *OS* scrolls the screen because we print a character at the bottom right cell, its own scroll routines kick and do the clearing that we don't want.
; SFTODO: Damn! My strategy so far has been to just not do that - we control the printing most of the time. But what about during user text input? Oh no, it's probably fine, because we are doing that via s_printchar too. Yes, a quick test suggests it is - but test this with final version, and don't forget to test the case where we're doing split cursor editing on the command line... - I think this is currently broken, copying at the final prompt at the end of thed benchmark ccauses cursor editing to go (non-crashily) wrong when copying into bottom right and causing a scroll

; SFTODO: A quick test on a B+ in non-shadow mode suggests this works. It kinda-sorta works on a M128 in non-shadow mode, but some bits of text end up being invisible, so I must still be doing something wrong. This might be a good thing to investigate next. DFS 2.24 does *HELP (which is what I was testing with) oddly, and even on a M128 with this code *not* running, *SPOOL output doesn't contain DFS *HELP. I suspect this is something to do with its logic to avoid infinite recursion when *SPOOLing. It may or may not be related to this code's problems, but it may be this code works fine on a M128 in "normal" use, and it only has to deal with Ozmoo's output. Yes, superficial testing (can't test Ozmoo itself as it forces shadow RAM and this doesn't support that yet) suggests ordinary output works. Would be curious to find out how DFS 2.24 is feeding these characters to the screen in a way that bypasses WRCHV, but ultimately it's probably not an issue.

; SFTODO: If we inline the single-use subroutines, I think this code is completely relocatable, which is handy, as we ideally don't want multiple copies of it inside the installation executable, and we need to be able to copy part of it into private RAM on the B+ and to insert shadow page in/out tsb/trb instructions before/after the main body of the "our new logic" part of the WRCHV handler.

; We hook evntv to detect vsync rather than checking for this on irq1v. This
; avoids missing vsync events where they occur after we check but before the OS
; irq handler that we chain onto checks. Thanks to Coeus for help with this! See
; https://stardot.org.uk/forums/viewtopic.php?f=54&t=26939.
evntv_handler
    ; SFTODO: If we're pushed for space we don't need to chain to parent evntv or check it's our event
    cmp #4:bne jmp_parent_evntv
    lda #<frame_time_us:sta $fe68
    lda #>frame_time_us:sta $fe69
    lda #4
jmp_parent_evntv
old_evntv = *+1
    jmp $ffff ; patched

     ; SFTODO: *SOMETIMES* (DOING REP:PRINT:UN.FA. IN BASIC DOESN'T SEEM TO TRIGGER IT, DOING *HELP IN A LOOP ON B-EM'S B 1770 CONFIG DOES) WE GET STUCK  - I REALLY DON'T KNOW WHY

!ifdef DEBUG_COLOUR_BARS_SFTODO {
debug_set_bg
    sta SFTODO9
    txa
    pha
    ldx #7
SFTODO9 = *+1
    lda #0 ; patched
loopSFTODO
    sta $fe21
    clc
    adc #16
    dex
    bpl loopSFTODO
    pla
    tax
    rts
}

; SFTODO: move this to a better location
lines_to_move
    !byte 2 ; SFTODO TEMP HACK
lines_to_move_working_copy
    !byte 0

add_line_x
!zone {
    clc
    lda $00,x:adc #<bytes_per_line:sta $00,x
    lda $01,x:adc #>bytes_per_line
    bpl .no_wrap
    sec:sbc vdu_screen_size_high_byte
.no_wrap
    sta $01,x
    rts
}

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
    ; Page in shadow RAM; this is a no-op if we have no shadow RAM.
    lda #1
jsr_shadow_paging_control1
    jsr $ffff ; patched
!if 0 { ; SFTODO: EXPERIMENT - NEED TO REWORK FOR NEW APPROACH
    ; SFTODO: WE SHOULD PROBABLY *NOT* DO THIS WAIT IF WE ARE PROTECTING >=2 LINES - THAT'S SLOW ENOUGH WE CAN'T RELIABLY STOP FLICKER, SO WE SHOULD JUST GO AHEAD AT MAX SPEED - JUST POSSIBLY FOR TWO LINES IT IS MOSTLY GOING TO NOT FLICKER, MAY BE PAINFUL TO DO RASTER WAIT OR MAY NOT
wait_for_safe_raster_position
    lda current_crtc_row
    bne wait_for_safe_raster_position
} else { ; SFTODO
SFTODOLOOP
    lda $fe69 ; timer 2 high-order counter
    cmp #>first_safe_start_row_time_us
    bcs SFTODOLOOP
    cmp #>last_safe_start_row_time_us
    bcc SFTODO44
    ;sta $ffff ; SFTODO HACK
    and #7:eor #7:sta $fe21
SFTODO44
    jmp SFTODOLOOP
}
!ifdef DEBUG_COLOUR_BARS {
!ifdef DEBUG_COLOUR_BARS2 {
    lda #6:sta SFTODOHACK
}
    lda #4 xor 7:sta $fe21 ;jsr debug_set_bg
}
    ; SFTODO: I think this is a good spot to do the hardware scroll. We shouldn't start this until after vsync, and if we start a little too close to the next vsync, the chances are we'll finish our update before the raster reaches the last bit of data we're moving around. If we did the hw scroll at the end of the copy, we might end up getting a frame where the top window has been moved in  screen RAM but the hw scroll hasn't happened yet and we'd see it in the wrong place.
    jsr .hardwareScrollUp
    lda vdu_screen_top_left_address_low
    sta dst
    lda vdu_screen_top_left_address_high
    sta dst+1
    jsr .setCursorSoftwareAndHardwarePosition

    ; We need this code to be reasonably fast and also reasonably small. SFTODO: I am sure it's possible to do better, particularly once I have a better idea of exactly how much code size I can tolerate.
    ; We work with chunks of data which are 1/5 of a line, i.e. 64 bytes in 320 bytes per line modes and 128 bytes in 640 bytes per line modes. This works out so that wrapping at the end of screen memory can only ever occur between chunks, not within a chunk.

chunk_size = 128 ; SFTODO: hard-coded for 640 byte per line modes for now
chunks_per_page = 256 / chunk_size
chunks_per_line = 5

    ldy lines_to_move:sty lines_to_move_working_copy
    dey:beq line_loop
    ; SFTODO: It's not exactly efficient to add bytes_per_line n times instead
    ; of adding n*bytes_per_line, but we don't want to be doing a generic
    ; multiply, n is small (and not generally a power of two) and as n grows the
    ; amount of data we have to copy grows proportionally so the extra overhead
    ; of doing this adding is always a small fraction of the total work.
    ;
    ; Because the wrapping makes everything so painful, we push the addresses on
    ; the stack as we calculate them so we can pop them off again to work
    ; through them backwards, rather than decrementing-with-wrapping. SFTODO:
    ; This does mean that the multiply-by-adding is kind of necessary.
add_loop
    lda src:pha:lda src+1:pha
    ldx #src:jsr add_line_x
    lda dst:pha:lda dst+1:pha
    ldx #dst:jsr add_line_x
    dey:bne add_loop

line_loop
    ldx #chunks_per_line
chunk_loop
    ldy #chunk_size - 1
    ; SFTODO: It may be worth using self-modifying code and abs,y addressing for byte_loop, especially in 128 byte chunks, but let's avoid that complexity for now as I write something.
byte_loop
    lda (src),y
    sta (dst),y
    lda #0
    sta (src),y
    dey
    bpl byte_loop
    ; SFTODO: experimental - this will be a bit slower, but will save code size
!macro bump .ptr {
    clc:lda .ptr:adc #chunk_size:sta .ptr
    bcc .no_carry
    inc .ptr+1
    bpl .no_wrap
    sec:lda .ptr+1:sbc vdu_screen_size_high_byte:sta .ptr+1
.no_carry
.no_wrap
}
    +bump src
    +bump dst
    dex
    bne chunk_loop
    dec lines_to_move_working_copy
    beq done
    pla:sta dst+1:pla:sta dst
    pla:sta src+1:pla:sta src
    jmp line_loop
done

!ifdef DEBUG_COLOUR_BARS {
!ifdef DEBUG_COLOUR_BARS2 {
    lda #3:sta SFTODOHACK
}
    lda #0 xor 7:sta $fe21 ;jsr debug_set_bg
}
    ; Page in main RAM; this is a no-op if we have no shadow RAM.
    lda #0
jsr_shadow_paging_control2
    jsr $ffff ; patched
    pla
    tay
    pla
    tax
    lda #10
null_shadow_driver
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
    ; SFTODO: We could (and used to) do this ourselves, but the code is relatively bulky even if we take advantage of the fact we know we are moving down exactly one line from the current position and can use the current values as a starting point instead of working things out from first principles as the OS does. As we're short of space, let's be a bit slower but force the OS to do the calculation for us by telling it to move the cursor to the desired location.
    lda #31 ; SFTODO: vdu_goto_xy
    jsr jmp_parent_wrchv
    lda .vduTextCursorXPosition
    jsr jmp_parent_wrchv
    lda .vduTextCursorYPosition
    jmp jmp_parent_wrchv

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

; SFTODO: Of course (assuming they survive) some of these subroutines are called only once and can be inlined.

; SFTODO: If we have a "number of lines to protect" variable which Ozmoo can poke to control this, it being 0 would naturally mean "disable this code". I am not sure off top of my head if we want/need that, but I guess we might.

; SFTODO: OK, right now *without* this driver, using split cursor editing to copy when the inputs cause the screen to scroll causes split cursor editing to terminate. I am surprised - we are not emitting a CR AFAIK - but this is acceptable (if not absolutely ideal) and if it happens without this driver being in the picture I am not going to worry about it too much. But may want to investigate/retest this later. It may well be that some of the split cursor stuff I've put in this code in a voodoo-ish ways turns out not to actually matter after all.

; SFTODO: We should probably disable (in as few bytes of code as possible) our custom OSWRCH routine if we quit and don't force press break - or maybe this should just be the job of any custom code that runs on quit and BREAK is the default and all we care about in detail?


init
    ; Examine the current hardware and decide if we can support fast hardware
    ; scrolling on it; if not the Ozmoo executable will fall back to slow
    ; hardware scrolling or software scrolling as appropriate.
    lda #0
    sta fast_scroll_status
!if 0 { ; SFTODO TEMP REMOVED WHILE WE'RE SHORT OF SPACE
    ; Are we running on an Integra-B? We check for this explicitly as the
    ; Integra-B spoofs the result of osbyte_read_host.
    lda #$49
    ldx #$ff
    ldy #0
    jsr osbyte
    cpx #$49
    bne not_integra_b
    ; We don't need to handle the Integra-B specially here, except for
    ; recognising that we are on a model B despite its spoofing.
    ldx #1
    bne host_type_in_x ; always_branch
}
not_integra_b
    ; Determine the host type.
    lda #osbyte_read_host
    ldx #1
    jsr osbyte
host_type_in_x
    cpx #1
    bcs not_electron
    ; We're on an Electron.
    ; SFTODONOW: For the moment this doesn't support fast hardware scrolling. I intend to implement this, so later we will need to check for MRB in shadow mode here and use Electron fast hw scrolling code provided we are't in MRB shadow mode.
    ; Return with fast_scroll_status 0.
just_rts
    rts
not_electron
    ; SFTODO: We should probably just rts if we're in mode 7 - there's no point slowing things down with vsync events we don't care about.
    ; We can't support fast hardware scrolling unless we can page in any shadow RAM. If we have no shadow RAM, that's fine but for consistency we provide a null shadow paging driver. SFTODO: This slightly penalises a machine with no shadow RAM, but probably not so much that we really need to care.
    ; SFTODO: We need to special case the B+ and *if* we have private RAM available  (we can check shadow_state_b_plus_private) we need to copy the scroll code in there with a low memory stub. For the moment this will "work" and nicely disable fast hw scrolling on B+.
    ; SFTODO: Rather than explicitly test for Electron MRB shadow mode, the fact we have no shadow RAM paging control is probably what we should check - this hides some of the logic in a single place (in the shadow driver) instead of duplicating tests here.
    ldx #<null_shadow_driver
    ldy #>null_shadow_driver
    lda shadow_state
    cmp #shadow_state_none
    beq supported_bbc_with_shadow_driver_yx
    cmp #shadow_state_first_driver
    bcc just_rts ; we have shadow RAM but no driver
    ldy shadow_paging_control_ptr + 1
    beq just_rts ; we have no shadow paging control
    ldx shadow_paging_control_ptr
supported_bbc_with_shadow_driver_yx
    ; We have shadow paging control, so patch up this code to call it.
    stx jsr_shadow_paging_control1 + 1
    stx jsr_shadow_paging_control2 + 1
    sty jsr_shadow_paging_control1 + 2
    sty jsr_shadow_paging_control2 + 2
    lda #1
    sta fast_scroll_status ; SFTODO: maybe this variable should have _host appended, to avoid confusion if I accidentally use it in the core Ozmoo code (which runs on tube if we have one, of course)

    ; SFTODONOW: At some point this code will load in one place (probably page &9/&A) and copy the relevant driver to the final place (wherever that is), as the shadow driver executable does, but for now it just loads into the final place.

    ; Check if we've already claimed vectors and do nothing if so. We don't
    ; expect this to happen, but since this is discardable init code we might as
    ; well check.
    lda wrchv
    ldx wrchv+1
!if 0 { ; SFTODO TEMP REMOVED AS WE STRUGGLE FOR SPACE
    cmp #<our_wrchv
    bne not_already_claimed
    cpx #>our_wrchv
    beq just_rts ; branch if we've already claimed wrchv
}
not_already_claimed
    ; We haven't already claimed vectors, so set up ready for action and claim the vectors.
    sei
    ; SFTODO: Hacky interrupt support - cpied from Kieran's screen-example.asm Need proper cvomments
    sta parent_wrchv
    stx parent_wrchv+1
    lda #<our_wrchv
    sta wrchv
    lda #>our_wrchv
    sta wrchv+1
    ; SFTODO: inconsistent - old__evntv but parent_wrchv
    lda evntv:sta old_evntv
    lda evntv+1:sta old_evntv+1
    lda #<evntv_handler:sta evntv
    lda #>evntv_handler:sta evntv+1
    lda #0
    sta $fe6b ; user via ACR
    cli
    lda #14:ldx #4:jmp osbyte ; SFTODO: MAGIC NUM

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

; SFTODO: Thinking out loud - this code needs to know what mode it is in, because it needs to tweak its timings accordingly. We don't really want to waste bytes or cycles (especially bytes, I suspect) adapting on the fly, unless it's very cheap, because this code is probably fairly large already and it's best if the installation executable can set this up and be done with it to keep the runtime memory requirement down. The trouble is that we aren't in the final screen mode when the installation executable will be loaded, so we may need to pass it through as we do with the host cache. I guess this is fine-ish, because we can poke it into memory from the loader and it only has to remain valud there until this code initisalises itself.

!if 0 { ; SFTODO SKETCHING/THINKING OUT LOUD
    ; Because we need to account for wrapping at the top of screen RAM, we *don't* handle multiple lines just by copying bytes_per_line*number of line bytes in a single operation. By copying only a line's worth of data at a time, most of the time our inner loop doesn't have to worry about line wrapping.
    lda #n:sta working_lines_to_copy

multi_line_loop
    ldx #bytes_per_line / 256
    ; If the high byte of (pre-modification) src or dst is $7f, we will wrap in the middle of a line.
    lda #0:ldy src+1:cmp #$7f:rol:ldy dst+1:cmp #$7f:rol:php
    ; On the first pass we are potentially copying less than 256 bytes, but the code to increment src/src2/dst assumes we work a page at a time. To compensate for this, we adjust Y and src/dst.
    initial_y = (256 - (bytes_per_line % 256)) & $ff
    ldy #initial_y
    sec:lda src:sbc #initial_y:sta src:deccc src+1
    sec:lda dst:sbc #initial_y:sta dst:deccc dst+1
    plp:bne awkward
copy_and_zero_outer_loop
    ; Patch the lda/sta abs instructions in the inner loop to use the values in src/dst; this allows us to use (zp),y addressing in the awkward case loop and saves us a handful of bytes/cycles elsewhere by using zp instructions to modify src/dst, while still giving us the performance benefits of abs,y in the inner loop.
    lda src+0:sta src1abs+0:sta src2abs+0 ; 3+4+4
    lda src+1:sta src1abs+1:sta src2abs+1 ; 3+4+4
    lda dst+0:sta dstabs+0 ; 3+4
    lda dst+1:sta dstabs+1 ; 3+4
    ; so 36 cycles to do that copy, and we then save 3 cycles on each pass round the loop, so we break even after 12 bytes, so this is a win for speed.
copy_and_zero_inner_loop
src1abs = *+1
    lda $ffff,y ; patched
dstabs = *+1
    sta $ffff,y ; patched
    ; SFTODO: If working_lines_to_copy>1, the lda #0:sta are redundant - we would just be zeroing out memory we are going to deal with on the next line pair
    lda #0
src2abs = *+1
    sta $ffff,y ; patched
    iny
    bne copy_and_zero_inner_loop
    ; These cannot wrap because they are within a line and we already handed the awkward case off to a different piece of code.
    inc src+1
    inc dst+1
    dex
    bne copy_and_zero_outer_loop
done ; SFTODO: done_line?

    dec working_lines_to_copy:beq done_all
    dst = src
    src += bytes_per_line:wrap src if nec
    jmp multi_line_loop
done_all



awkward
    ldy #initial_y:sty working_y ; SFTODO: ldy # probably redundant, Y probably already set
awkward_outer_loop
awkward_inner_loop
    ldy #0
    lda (src),y
    sta (dst),y
    lda #0 ; SFTODO: tya
    sta (src),y
    inc src:bne no_wrap_src:inceq src+1:bpl no_wrap_src:wrap:.no_wrap_src
    inc dst:bne no_wrap_dst:inceq dst+1:bpl no_wrap_dst:wrap:.no_wrap_dst:inceq dst+1
    inc working_y
    bne awkward_inner_loop
    dex
    bne awkward_outer_loop
    beq done ; always branch



    ; SFTODO: IS THIS TOO CLEVER? THE SIMPLE INNER LOOP AS IT STANDS LOOPS WITH INY:BNE INNER - 5 cycles
    ; IF WE KEEP Y 0, DO INCS AND TEST FOR WRAPPING WE HAVE (looking at common case only):
    ;     inc src:beq not_simple
    ;     inc dst:bne inner
    ; that is 5+2+5+3=15 cycles, so 10 cycles worse per byte copied (ignoring the 1-in-256 non-simple cases).
    ; OK, it's slightly worse, because that assumes src/dst are in zp, so we're paying an extra cycle on each lda/sta in the inner loop as we can't use the abs,y mode.
    ; An advantage of just blatting round the inner loop using inc-and-test-for-wrap is that in the multi-line case, we don't need to treat things any differently - we just keep going. Oh, but I forgot we need to handle counting how many bytes we copy. Gah.


}
