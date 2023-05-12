; Fast hardware scroll driver installation code
;
; This executable examines the current hardware (including shadow RAM, as
; reported by acorn-shadow-driver.asm, which must have been run first) to see if
; we can support fast hardware scrolling. If we can we copy code into low RAM,
; hook WRCHV and EVNTV to point into that code and work with the core Ozmoo
; executable to enable/disable the fast hardware scrolling as appropriate.
;
; Normal OS hardware scrolling only works on the full screen. the OS updates the
; CRTC screen start address to be the currently second-from-top line of the screen,
; and blanks the current top line; these become the top and bottom lines respectively
; with the new CRTC screen start address. This code detects when a LF is being
; used to scroll the screen up and replaces the OS behaviour with a
; copy-and-blank operation which effectively protects one or more lines of the
; screen at the top from scrolling. This means Ozmoo does not need to worry
; about redrawing the top line or using a text window to protect it. For a
; single protected line (where there isn't too much data to copy) we also use
; the VSYNC event to try to avoid flicker by deferring our updates until the
; raster is in a safe place.
;
; This is *not* a general implementation of hardware scrolling with a protected
; area at the top of the screen. If the screen scrolls because a character is
; printed at the bottom right, the OS hardware scroll code will be invoked as
; normal and the protected area will not be protected. We can get away with this
; here because it is our code running to output text to the screen, and we just
; make sure it never prints in the bottom right character position (the screen
; is scrolled first, then we go back and print at the rightmost position on the
; now second-to-bottom line). (In principle we could trap more OSWRCH calls and
; detect this case and handle it, but it would bloat the code and slow down
; normal output, and it's easier all round to just not print at the bottom
; right.)

; SFTODONOW: ELECTRON SUPPORT

!source "acorn-shared-constants.asm"

; The constant names and fragments of OS 1.20 code which have been hacked up to
; form part of this code are based on TobyLobster's OS 1.20 disassembly.
vdu_text_window_bottom = $309
vdu_text_cursor_x_position = $318
vdu_text_cursor_y_position = $319
vdu_screen_top_left_address_low = $350
vdu_screen_top_left_address_high = $351
vdu_bytes_per_character_row_low = $0352
vdu_bytes_per_character_row_high = $0353
vdu_screen_size_high_byte = $354
vdu_status_byte = $d0
vdu_temp_store_da = $da

crtc_address_register = $fe00
crtc_address_write = $fe01
crtc_start_screen_address_high_register = 12
crtc_cursor_position_high_register = 14

user_via_t2_low_order_latch_counter = $fe68
user_via_t2_high_order_counter = $fe69

; SFTODO RENAME THIS - "driver" IS UNHELPFUL
b_plus_private_ram_driver = $ae80 ; SFTODONOW JUST GUESSING THIS FITS WITH SHADOW DRIVER - ALSO WE MAY GET BAD ALIGNMENT ON LOOPS IF WE DON'T "CHECK" SOMEHOW

opcode_rts = $60

wrchv = $20e
evntv = $220

; SFTODO: for now just assume 80 column mode - though these values are available at $352/$353 as that's where OS keeps them
bytes_per_line = 640


; 6 bytes of zero page are allocated to the VDU driver starting at $da, but as
; we're not too tight for space, we permanently allocate $da to
; vdu_temp_store_da, imitating how it is used in OS 1.20. This could be shared
; if necessary.
zp = $db ; 5 bytes
src = zp ; 2 bytes
dst = zp + 2 ; 2 bytes
lines_to_move_working_copy = zp + 4 ; 1 byte

irq1v = $204
us_per_scanline = 64
us_per_row = 8*us_per_scanline
vsync_position = 35
total_rows = 39
SFTODO99 = total_rows * us_per_row ; SFTODO: idea is that as we're comparing against timer most of the time (not loading it) we don't want the 2*us_per_scanline adjustment
frame_time_us = SFTODO99 - 2 * us_per_scanline
; SFTODO: I think it would be possible to auto-tune these parameters (although first_safe_start_row_time_us is probably more sensible kept fixed) - we could examine timer 2 (and an optional frame counter, to detect really bad overruns) after we finish our scroll copy and if we overran we could nudge last_safe_start_row_time_us towards the top of the screen (with some kind of min value so we don't push it up ridiculously far). I am not actually sure this is a good idea - do we *really* want outliers to slow everything down afterwards? Are we really hell-bent on getting no flicker if humanly possible, or are we trying to compromise and get virtually no flicker without killing performance? We *might* want to behave differently if we have tube - if we are on tube the fifo smoothes things out and stops us doing a lot of busy waiting delaying things too badly.
first_safe_start_row_time_us = SFTODO99 - ((total_rows - vsync_position) + 2) * us_per_row ; SFTODO: SHOULD PROBABLY BE 1 AND MIGHT NEED FURTHER TWEAKING, SINCE WE ONLY REALLY CARE ABOUT AVOIDING FLICKER FOR SINGLE PROTECTED ROW
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

runtime_start
!pseudopc fast_scroll_start {

; We hook evntv to detect vsync rather than checking for this on irq1v. This
; avoids missing vsync events where they occur after we check but before the OS
; irq handler that we chain onto checks. Thanks to Coeus for help with this! See
; https://stardot.org.uk/forums/viewtopic.php?f=54&t=26939.
evntv_handler
    ; SFTODO: If we're pushed for space we don't need to chain to parent evntv or check it's our event
    cmp #4:bne jmp_parent_evntv
    lda #<frame_time_us:sta user_via_t2_low_order_latch_counter
    lda #>frame_time_us:sta user_via_t2_high_order_counter
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
    ; We want to minimise overhead on WRCHV, so we try to get cases we're not
    ; interested in passed through ASAP. We only care about LF characters.
    cmp #10
    beq is_lf
jmp_parent_wrchv
parent_wrchv = *+1
    jmp $ffff ; patched
is_lf
    ; If we're not on the bottom line of the screen (the "text window" is the
    ; whole screen by default) we're not interested.
    lda vdu_text_cursor_y_position
    cmp vdu_text_window_bottom
    bcc jmp_parent_wrchv_with_lf
    ; If we're not protecting any lines at the top of the screen we're not interested.
    lda fast_scroll_lines_to_move
    bne protecting_some_lines
jmp_parent_wrchv_with_lf
    lda #10
    jmp jmp_parent_wrchv
protecting_some_lines
    ; If there's a text window in effect, just pass through to the parent.
    lda #%00001000
    bit vdu_status_byte
    bne jmp_parent_wrchv_with_lf

    ; It's a line feed on the bottom line of the screen with no text window in
    ; effect. That's what we're here for!
    ; SFTODO: It's not a huge deal, but FWIW - I think this LF character will be
    ; omitted from any *SPOOL file being produced, unless we take extra steps to
    ; include it.

    ; We must preserve X and Y (as well as A, although we know that's 10).
    txa
    pha
    tya
    pha

    ; Save the current screen top left address before we scroll.
    lda vdu_screen_top_left_address_low
    sta src
    lda vdu_screen_top_left_address_high
    sta src+1

    ; Page in shadow RAM; this is a no-op if we have no shadow RAM.
    lda #1
jsr_shadow_paging_control1
    jsr $ffff ; patched

    ; If we're protecting a single row at the top of the screen, wait for a
    ; "safe" raster position - one where we expect to be able to complete the
    ; movement of data in screen memory before the raster reaches that data.
    ; This should give a flicker-free appearance, like software scrolling but
    ; much faster. If we have more than one row to protect, the copying takes so
    ; long that we don't try to be flicker-free and just go right ahead to get
    ; the job done ASAP.
    lda fast_scroll_lines_to_move
    cmp #2
    bcs dont_wait_for_raster
raster_wait_loop
    ; We only examine the high-order counter; this is good enough and avoids
    ; complications trying to read a two byte counter which is counting down as
    ; we read it a byte at a time.
    lda user_via_t2_high_order_counter
    cmp #>first_safe_start_row_time_us
    bcs raster_wait_loop
    cmp #>last_safe_start_row_time_us
    bcc raster_wait_loop
dont_wait_for_raster
!ifdef DEBUG_COLOUR_BARS {
!ifdef DEBUG_COLOUR_BARS2 {
    lda #6:sta SFTODOHACK
}
    lda #4 xor 7:sta $fe21 ;jsr debug_set_bg
}

    ; We update the CRTC screen start address now; we don't want to leave it too
    ; late otherwise we might miss the next vsync. We also don't want to have a
    ; frame where the protected line temporarily appears at the bottom of the
    ; screen; it's probably less annoying for it to flicker (because we can't
    ; keep up with the raster) than have it jump around that much.
    ;
    ; As we're doing this we also save the post-scroll top left address to dst
    ; ready for use in the data copy loops below.
    lda vdu_screen_top_left_address_low
    clc
    adc vdu_bytes_per_character_row_low
    sta vdu_screen_top_left_address_low
    sta dst
    sta vdu_temp_store_da
    lda vdu_screen_top_left_address_high
    adc vdu_bytes_per_character_row_high
    bpl +
    sec
    sbc vdu_screen_size_high_byte
+
    sta vdu_screen_top_left_address_high
    sta dst+1
    ldy #crtc_start_screen_address_high_register
    lsr
    ror vdu_temp_store_da
    lsr
    ror vdu_temp_store_da
    lsr
    ror vdu_temp_store_da
    ldx vdu_temp_store_da
    sty crtc_address_register
    sta crtc_address_write
    iny
    sty crtc_address_register
    stx crtc_address_write

    ; Tell the OS to move the cursor to the same co-ordinates it already has,
    ; but taking account of the hardware scrolled screen. We could do this
    ; ourselves, but it isn't that slow (compared to all the copying we do) to
    ; get the OS to do it for us, which saves a lot of code here.
    lda #vdu_goto_xy
    jsr jmp_parent_wrchv
    lda vdu_text_cursor_x_position
    jsr jmp_parent_wrchv
    lda vdu_text_cursor_y_position
    jsr jmp_parent_wrchv

    ; We need this code to be reasonably fast and also reasonably small. SFTODO: I am sure it's possible to do better, particularly once I have a better idea of exactly how much code size I can tolerate.
    ; We work with chunks of data which are 1/5 of a line, i.e. 64 bytes in 320 bytes per line modes and 128 bytes in 640 bytes per line modes. This works out so that wrapping at the end of screen memory can only ever occur between chunks, not within a chunk.

chunk_size = 128 ; SFTODO: hard-coded for 640 byte per line modes for now
chunks_per_line = 5

    ; Code from b_plus_copy_start to b_plus_copy_end is copied into private RAM
    ; on the B+ and this code in main RAM is patched to execute it from private
    ; RAM. This allows it to access screen memory directly. Because of this, we
    ; must not use any absolute addresses within this code. SFTODO: DO THAT!
    ; SFTODO: COULD WE MAKE BETTER USE OF X IN THIS LOOP?
b_plus_copy_start
    ldy fast_scroll_lines_to_move:sty lines_to_move_working_copy
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
    lda src+1:pha:lda src:pha
    ldx #src:jsr add_line_x
    lda dst+1:pha:lda dst:pha
    ldx #dst:jsr add_line_x
    dey:bne add_loop

!macro bump .ptr {
    clc:lda .ptr:adc #chunk_size:sta .ptr
    bcc .no_carry
    inc .ptr+1
    bpl .no_wrap
    sec:lda .ptr+1:sbc vdu_screen_size_high_byte:sta .ptr+1
.no_carry
.no_wrap
}

    ; SFTODO EXPERIMENTAL HACK - LOTS OF COPY AND PASTE, MAYBE ACCEPTABLE, MAYBE NOT
line_loop2 ; SFTODO: "2" suffix on labels here is hacky
    ldx #chunks_per_line
chunk_loop2
    ldy #chunk_size - 1
    ; SFTODO: It may be worth using self-modifying code and abs,y addressing for byte_loop, especially in 128 byte chunks, but let's avoid that complexity for now as I write something. - BE CAREFUL, THIS MIGHT BREAK B+
    ; SFTODO: This loop may benefit from being unrolled somewhat - there are sufficiently few cycles per iteration that the bpl overhead is non-negligible. (The other loop with the extra lda #0:sta doesn't show the same kind of gains, at least when I do calculations about the possible benefits - I haven't experimented with it.) Worth noting that just "double-unrolling" reduces per loop cost from 16 cycles to 14.5 cycles and only costs five bytes of extra code. If we play games to use lda/sta abs,y addressing we save two cycles and that adds two bytes to the loop and (finger in air) 20-ish bytes of setup code; we'd probably (given our relatively tight space budget) come out ahead unrolling the loop a couple of extra times.
byte_loop2
    lda (src),y
    sta (dst),y
    dey
    bpl byte_loop2
    +assert_no_page_crossing byte_loop2
    ; SFTODO: The overhead of moving this double-bump into a subroutine so it can be shared with the loop below might well be acceptable and could give a worthwhile saving on code size, allowing us to actually fit and/or have more space for other optimisations. The "dex" could be shared as well, saving one more byte. Each bump is 21 bytes, so ignoring the jsr+rts overhead we'd save 2*21+1=43 bytes factoring this out - minus 1+2*3 for the rts+jsrs, so 36 bytes overall. We'd pay an extra 12 cycles in each case for the jsr+rts; double-unrolling each loop would cost 5+9=14 bytes, so we could in fact afford to 4-unroll and still come out ahead. 4-unrolling the more complex loop saves 11.25 *scanlines* of time per line of screen, whereas we only pay 10 jsr+rts penalties (120 cycles total) for moving the bumps into a subroutine. This seems such a clear win I half wonder if I've got confused. We *would* have to take special care to patch up the absolute jsr when copying to the B+ private RAM, but that is a small-ish bit of complexity and is handled in discardable init code.
    +bump src
    +bump dst
    dex
    bne chunk_loop2
    dec lines_to_move_working_copy
    pla:sta dst:pla:sta dst+1
    pla:sta src:pla:sta src+1
    ldx lines_to_move_working_copy
    cpx #1:bne line_loop2

line_loop
    ldx #chunks_per_line
chunk_loop
    ldy #chunk_size - 1
    ; SFTODO: It may be worth using self-modifying code and abs,y addressing for byte_loop, especially in 128 byte chunks, but let's avoid that complexity for now as I write something. - BE CAREFUL, THIS MIGHT BREAK B+
    ; SFTODO: Unrolling this loop twice would cost 9 bytes and would drop the per-iteration cost from 24 cycles to 22.5 cycles. That doesn't sound much, but over a 640 byte copy it is 960 cycles, which is 7.5 scanlines - *nearly* an entire text row. Going to a 4-unroll drops it to 21.75, saving an additional 3.75 scanlines.
    ; SFTODO: Not sure it is worth it, but if we stashed X elsewhere we could load it with 0 and replace "lda #0" with "txa". This saves no cycles, but it saves a byte, so if we 4-unroll we save four bytes - though that only breaks even with stx zp:...:ldx zp. We could possibly store the chunk counter in zp location all the time, we'd pay an extra two bytes to store to it and one extra byte to dec it (and some cycles, but those are outside the core loop) - and then by the time we ldx #0:tax instead of lda #0 we've burned an extra byte, so we're breakig even again, and we've slowed down the outer loop slightly. If we were to 8-unroll we'd probably come out ahead, but gut feeling is this just isn't worth it.
byte_loop
    lda (src),y
    sta (dst),y
    lda #0
    sta (src),y
    dey
    bpl byte_loop
    +assert_no_page_crossing byte_loop
    +bump src
    +bump dst
    dex
    bne chunk_loop

!ifdef DEBUG_COLOUR_BARS {
!ifdef DEBUG_COLOUR_BARS2 {
    lda #3:sta SFTODOHACK
}
    lda #0 xor 7:sta $fe21 ;jsr debug_set_bg
}
b_plus_copy_end
    ; Page in main RAM; this is a no-op if we have no shadow RAM.
    lda #0
jsr_shadow_paging_control2
    jsr $ffff ; patched
finish_oswrch
    pla
    tay
    pla
    tax
    lda #10
null_shadow_driver
    rts
}
runtime_end ; SFTODO: label names in this file are a bit crappy in general, e.g. this is the runtime *code* but this label is its non-runtime (end) address
; The code between runtime_start and runtime_end is copied down to runtime_start
; by our discardable initialisation code. Make sure it fits!
runtime_size = runtime_end - runtime_start
+assert runtime_size <= fast_scroll_end - fast_scroll_start

; SFTODO: Of course (assuming they survive) some of these subroutines are called only once and can be inlined.

; SFTODO: OK, right now *without* this driver, using split cursor editing to copy when the inputs cause the screen to scroll causes split cursor editing to terminate. I am surprised - we are not emitting a CR AFAIK - but this is acceptable (if not absolutely ideal) and if it happens without this driver being in the picture I am not going to worry about it too much. But may want to investigate/retest this later. It may well be that some of the split cursor stuff I've put in this code in a voodoo-ish ways turns out not to actually matter after all.

; SFTODO: We should probably disable (in as few bytes of code as possible) our custom OSWRCH routine if we quit and don't force press break - or maybe this should just be the job of any custom code that runs on quit and BREAK is the default and all we care about in detail?


init
    ; Before we do anything else, we copy our code from inside this executable
    ; to its final location - we can then patch it in-place in the following
    ; code and copy it from in-place to the B+ private RAM if necessary. (The
    ; way acme's !pseudpc directive works means that it's fiddly to patch the
    ; copy embedded in this executable, as we don't have labels within the block
    ; of code addressing the embedded copy so we'd have to manually apply
    ; offsets.) If we decide not to support fast hardware scrolling this is
    ; still OK; we own this block of memory so there's no problem with us
    ; corrupting it.
    ldx #>runtime_size
    ldy #<runtime_size
SFTODOCOPYLOOP
lda_runtime_start_abs
    lda runtime_start
sta_fast_scroll_start_abs
    sta fast_scroll_start
    +inc16 lda_runtime_start_abs+1
    +inc16 sta_fast_scroll_start_abs+1
    dey
    bne SFTODOCOPYLOOP
    dex
    bpl SFTODOCOPYLOOP
    ; Examine the current hardware and decide if we can support fast hardware
    ; scrolling on it; if not the Ozmoo executable will fall back to slow
    ; hardware scrolling or software scrolling as appropriate.
    lda #0
    sta fast_scroll_status_host
    sta fast_scroll_lines_to_move ; SFTODO: I THINK THIS IS RIGHT, MEANS WE START OFF DISABLED
!if 0 { ; SFTODO?
    ; If we're going to run the game in mode 7, we will use software scrolling. SFTODO: Technically we shouldn't *need* to do this, as the game shouldn't try to use it. Don't bother checking?
    lda screen_mode_host
    cmp #7
    beq just_rts
}
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
    ; Return with fast_scroll_status_host 0.
just_rts
    rts
not_electron

    ; If we're going to run in mode 7, we don't bother installing anything. This
    ; would be mostly harmless - the core Ozmoo executable will not enable the
    ; vsync events in mode 7, and we will have a screen window in effect for
    ; software scrolling so our OSWRCH code will never execute - but it seems
    ; better to avoid the unnecessary overhead on the OSWRCH vector. This might
    ; (although it probably won't) help to ensure that "there is no fast scroll
    ; support" code paths get at least a little bit of routine testing.
    lda screen_mode_host
    cmp #7
    beq just_rts
    ; We special-case the B+. The shadow driver can't page in the shadow RAM,
    ; but we can copy our code into private RAM to execute it, as long as we
    ; have use of the private RAM.
    cpx #2 ; SFTODO: magic number - generally do this, maybe it's OK
    bne not_b_plus
    lda shadow_state
    cmp #shadow_state_b_plus_private
    bne just_rts
    ; If we're on a B+, copy the relevant code into private RAM so it can access
    ; the shadow screen. We do this after copying our code to fast_scroll_start,
    ; so we can copy it from there.
    lda romsel_copy
    pha
    lda #128
    sta romsel_copy
    sta bbc_romsel
    ; SFTODO: SHOULD WE FACTOR OUT THE COPY LOOP IF IT'S DUPLICATED?
    ; SFTODO: ASSERT THIS COPY COPIES OK
    ldy #b_plus_copy_end-b_plus_copy_start
-   lda b_plus_copy_start-1,y
    sta b_plus_private_ram_driver-1,y
    dey
    bne -
    lda #opcode_rts
    sta b_plus_private_ram_driver + (b_plus_copy_end - b_plus_copy_start)
    ldy #b_plus_copy_start_patch_end-b_plus_copy_start_patch_start ; SFTODO: THESE LABELS ARE INSANE
-   lda b_plus_copy_start_patch_start-1,y
    sta b_plus_copy_start-1,y
    dey
    bne -
    pla
    sta romsel_copy
    sta bbc_romsel
    jmp supported_bbc_with_null_shadow_driver
not_b_plus
    ; We can't support fast hardware scrolling unless we can page in any shadow RAM. If we have no shadow RAM, that's fine but for consistency we provide a null shadow paging driver. SFTODO: This slightly penalises a machine with no shadow RAM, but probably not so much that we really need to care.
    ; SFTODO: We need to special case the B+ and *if* we have private RAM available  (we can check shadow_state_b_plus_private) we need to copy the scroll code in there with a low memory stub. For the moment this will "work" and nicely disable fast hw scrolling on B+.
    ; SFTODO: Rather than explicitly test for Electron MRB shadow mode, the fact we have no shadow RAM paging control is probably what we should check - this hides some of the logic in a single place (in the shadow driver) instead of duplicating tests here.
    ldx #<null_shadow_driver
    ldy #>null_shadow_driver
    lda shadow_state
    cmp #shadow_state_none
    beq supported_bbc_with_null_shadow_driver
    cmp #shadow_state_first_driver
    bcc just_rts ; we have shadow RAM but no driver
    ldx shadow_paging_control_ptr
    ldy shadow_paging_control_ptr + 1
    beq just_rts ; we have no shadow paging control
    bne supported_bbc_with_shadow_driver_yx ; always branch
supported_bbc_with_null_shadow_driver
    ldx #<null_shadow_driver
    ldy #>null_shadow_driver
supported_bbc_with_shadow_driver_yx
    ; We have shadow paging control, so patch up this code to call it.
    stx jsr_shadow_paging_control1 + 1
    stx jsr_shadow_paging_control2 + 1
    sty jsr_shadow_paging_control1 + 2
    sty jsr_shadow_paging_control2 + 2
    lda #1
    sta fast_scroll_status_host ; SFTODO: maybe this variable should have _host appended, to avoid confusion if I accidentally use it in the core Ozmoo code (which runs on tube if we have one, of course)

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
    ; We haven't already claimed vectors.
    ; Claim vectors; this patches the code we just copied down to
    ; fast_scroll_start.
    sei
    lda wrchv
    ldx wrchv+1
    sta parent_wrchv
    stx parent_wrchv+1
    ; SFTODO: Hacky interrupt support - cpied from Kieran's screen-example.asm Need proper cvomments
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
    ; We don't enable vsync events; we don't need them while
    ; fast_scroll_lines_to_move is zero, and the Ozmoo executable takes care of
    ; turning them on and off as that changes.
    rts

; This code is copied over b_plus_copy_start in main RAM after we've copied that code into the private RAM.
b_plus_copy_start_patch_start
    lda romsel_copy
    pha
    lda #128
    sta romsel_copy
    sta bbc_romsel
    jsr b_plus_private_ram_driver
    pla
    sta romsel_copy
    sta bbc_romsel
    jmp finish_oswrch
b_plus_copy_start_patch_end

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
    ; SFTODO: If working_lines_to_copy>1, the lda #0:sta are redundant - we would just be zeroing out memory we are going to deal with on the next line pair - apart from being slower I also suspect this looks really bad (e.g. might be responsible for ugliness of Border Zone?)
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

; SFTODO: Quick fiddle with Border Zone suggests the multi-line hardware scrolling works but it looks quite nasty. It may still be better than software scrolling. This is just a note to go back and experiment with this later. Just maybe we should never use the new scrolling for anything except a single line (as we do with the redraw-via-OS based hw scrolling).

; SFTODONOW: It *might* actually be possible for us to do a two line protected area in 40 column modes, or maybe even in 80 column modes if the code is optimised further (at the very least, not doing the redundant zero stores - which is probably cosmetically desirable - might make this work). Core Ozmoo code would need to change to enable vsync events in this case as well as this code deciding to check raster position in this case.

; SFTODONOW: Suspect I already have a TODO about this, but note that we need to be careful when copying the copy loops into B+ private RAM that we don't accidentally end up making the copy loops cross pages. I suspect the best way to handle this is (space permitting) just to always copy code into the B+ private RAM at the same sub-page offset as it lives at in low RAM.
