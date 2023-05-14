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

; SFTODO: I should probably make an effort to go through and rename
; constants/labels and tweak comments to talk consistently about an "upper
; window" rather than the alternative phrasing of "protecting n lines" or (even
; more confusingly; it works from the internal perspective of this code, where
; we move lines in RAM to keep the fixed in place on the screen) "moving n
; lines". (I think it's fine, if it feels helpful, to talk about "moving" inside
; this file, where we're actually doing it, but outside of this file we probably
; shouldn't.)

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

electron_screen_start_address_low = $fe02
electron_screen_start_address_high = $fe03

user_via_t2_low_order_latch_counter = $fe68
user_via_t2_high_order_counter = $fe69
user_via_auxiliary_control_register = $fe6b

opcode_jmp = $4c
opcode_rts = $60

wrchv = $20e
evntv = $220

; 6 bytes of zero page are allocated to the VDU driver starting at $da, but as
; we're not too tight for space, we permanently allocate $da to
; vdu_temp_store_da, imitating how it is used in OS 1.20. This could be shared
; if necessary.
zp = $db ; 5 bytes
src = zp ; 2 bytes
dst = zp + 2 ; 2 bytes
lines_to_move_without_clearing = zp + 4 ; 1 byte

; Timings here are based on Kieran's "Intro to Interrupts" talk and example
; code.
us_per_scanline = 64
us_per_row = 8 * us_per_scanline
vsync_position = 35
total_rows = 39
frame_us = total_rows * us_per_row
initial_t2_value = frame_us - 2 * us_per_scanline

; SFTODO: At the moment colour bar writes are BBC-specific; can/should we add
; Electron support?
; DEBUG_COLOUR_BARS = 1
!ifdef DEBUG_COLOUR_BARS {
    bbc_palette = $fe21
}

runtime_start
!pseudopc fast_scroll_start {

; We hook evntv to detect vsync rather than checking for this on irq1v. This
; avoids missing vsync events where they occur after we check but before the OS
; irq handler that we chain onto checks. Thanks to Coeus for help with this! See
; https://stardot.org.uk/forums/viewtopic.php?f=54&t=26939.
evntv_handler
    ; SQUASH: If we're pushed for space we don't need to chain to parent evntv
    ; or check it's our event - we are the foreground application and we
    ; more-or-less know there aren't any other events. Of course it's possible
    ; some kind of utility ROM a user wants to use is using events so it's nice
    ; to play nice.
    cmp #event_vsync:bne jmp_parent_evntv
    lda #<initial_t2_value:sta user_via_t2_low_order_latch_counter
    lda #>initial_t2_value:sta user_via_t2_high_order_counter
    lda #event_vsync
jmp_parent_evntv
old_evntv = *+1
    jmp $ffff ; patched

add_line_x
!zone {
    clc
    lda $00,x:adc vdu_bytes_per_character_row_low:sta $00,x
    lda $01,x:adc vdu_bytes_per_character_row_high
    bpl .no_wrap
    sec:sbc vdu_screen_size_high_byte
.no_wrap
    sta $01,x
    rts
}

bump_src_dst_and_dex
!macro bump .ptr {
    ; .ptr += chunk_size
    sec ; not clc as we want to offset the minus 1 just below
    lda .ptr
    adc ldy_imm_chunk_size_minus_1_a+1 ; add chunk_size - 1
    sta .ptr
    bcc .no_carry
    inc .ptr+1
    bpl .no_wrap
    ; .ptr has gone past the top of screen memory, wrap it.
    sec
    lda .ptr+1
    sbc vdu_screen_size_high_byte
    sta .ptr+1
.no_carry
.no_wrap
}
    +bump src
    +bump dst
    dex
    rts

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

    ; This code is patched out at runtime on the Electron.
    ; SFTODO: It is *just* possible - would need to time it - that in e.g. 40 column modes we *can* do this quickly enough to avoid flicker on the Electron if we wait for vsync here (we can't time it more precisely). That said, even if that works, it would essentially add "on average" 0.01s to every line feed (since we'd hit at a random point during the 50Hz frame and have to wait half that on average for vsync) and might slow things down too painfully. Still, if it is flicker free, worth trying some timings to see if it's actually a problem in practice.
check_raster
    ; If we're protecting a single row at the top of the screen, wait for a
    ; "safe" raster position - one where we expect to be able to complete the
    ; movement of data in screen memory before the raster reaches that data.
    ; This should give a flicker-free appearance, like software scrolling but
    ; much faster. If we have more than one row to protect, the copying takes so
    ; long that we don't try to be flicker-free and just go right ahead to get
    ; the job done ASAP. SFTODO: WE MAY BE ABLE TO DO MORE THAN ONE ROW WITHOUT FLICKER NOW, WOULD NEED TO EXPERIMENT AND TUNE THIS, AND PROBABLY THE FIRST/LAST SAFE ROWS WOULD BE SELECTED FROM TABLES INDEXED BY NUMBER OF PROTECTED ROWS - THAT SAID, IT MAY BE THAT PROTECTING >1 ROW AND WAITING FOR FLICKER FREE SAFE REGIONS WILL SLOW THINGS DOWN TOO MUCH
    ldx fast_scroll_lines_to_move
    cpx #raster_wait_table_entries + 1
    bcs dont_wait_for_raster
raster_wait_loop
    ; We only examine the high-order counter; this is good enough and avoids
    ; complications trying to read a two byte counter which is counting down as
    ; we read it a byte at a time.
    lda user_via_t2_high_order_counter
    cmp raster_wait_table_first-1,x
    bcs raster_wait_loop
    cmp raster_wait_table_last-1,x
    bcc raster_wait_loop
dont_wait_for_raster
!ifdef DEBUG_COLOUR_BARS {
    lda #4 xor 7:sta bbc_palette
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
    ; The following code is patched at runtime on the Electron.
update_hardware_screen_start_address
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
update_hardware_screen_start_address_done

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
    ; SFTODO: If we are in an 80 column mode and thus have 128 byte chunks, should we patch this code at init time to do ldy #0 instead of ldy #chunk_size -1 and change the dey to iny? (We'd do it for both copy loops.) This would not change the behaviour (because we test with bpl) and it *might* look slightly nicer. This gets awkward with the unrolled loops though, as patching the dey->iny is faffy. Still, we *could* do it, and it's discardable init code. We are already patching the ldy # so changing what we patch with is not hard.

chunks_per_line = 5
chunk_size_40 = 320 / chunks_per_line
chunk_size_80 = 640 / chunks_per_line
min_chunk_size = chunk_size_40

    ; Code from b_plus_copy_start to b_plus_copy_end is copied into private RAM
    ; on the B+ and this code in main RAM is patched to execute it from private
    ; RAM. This allows it to access screen memory directly. Because the code is
    ; also present in main RAM we can use absolute addresses here, as long as
    ; they don't cause us to execute screen memory-accessing code from main RAM
    ; instead of private RAM. We preserve the within-page alignment when we copy
    ; the code into private RAM so loops don't incur page crossing penalties
    ; despite our checks.
b_plus_copy_start
fast_scroll_private_ram_aligned = (fast_scroll_private_ram & $ff00) | (b_plus_copy_start & $ff)
+assert fast_scroll_private_ram_aligned >= fast_scroll_private_ram
    ldy fast_scroll_lines_to_move ; SFTODO: RENAME THIS "fast_scroll_lines_to_protect" or "fast_scroll_top_window_size"?
    dey:beq line_loop
    sty lines_to_move_without_clearing

    ; SFTODO: COULD WE MAKE BETTER USE OF X IN THIS LOOP?
    ; We need to copy lines_to_move_without_clearing lines up by one line,
    ; working from the bottom-most line to the top-most line. We just add
    ; repeatedly to generate the addresses; this is relatively easy and saves
    ; using a general-purpose multiplication routine. We push the addresses onto
    ; the stack as we generate them; this saves having to write
    ; subtract-with-wrap code as well.
add_loop
    lda src+1:pha:lda src:pha
    ldx #src:jsr add_line_x
    lda dst+1:pha:lda dst:pha
    ldx #dst:jsr add_line_x
    dey:bne add_loop

    ; SFTODO EXPERIMENTAL HACK - LOTS OF COPY AND PASTE, MAYBE ACCEPTABLE, MAYBE NOT
line_loop2 ; SFTODO: "2" suffix on labels here is hacky
    ldx #chunks_per_line
chunk_loop2
ldy_imm_chunk_size_minus_1_a ; SFTODO: perhaps be good to rename these labels (one on each loop) ldy_imm_chunk_size_minus_1 - that would help clarify the use of it in the bump macro too
    ldy #chunk_size_80 - 1 ; patched
byte_loop2
byte_loop2_unroll_count = 8
+assert min_chunk_size % byte_loop2_unroll_count == 0
!for i, 1, byte_loop2_unroll_count {
    lda (src),y
    sta (dst),y
    dey
}
    bpl byte_loop2
    +assert_no_page_crossing byte_loop2
    jsr bump_src_dst_and_dex
    bne chunk_loop2
    pla:sta dst:pla:sta dst+1
    pla:sta src:pla:sta src+1
    dec lines_to_move_without_clearing
    bne line_loop2

    ; Copy the final (possibly only, if fast_scroll_lines_to_move is 1) line,
    ; clearing the source afterwards.
    ;
    ; It is somewhat tempting to add a special case here, where if src and dst
    ; are both not the one line on the screen which can wrap part-way through,
    ; we execute a different version of the code which just copies the 320/640
    ; bytes in one loop without breaking it into chunks. This would be a bit
    ; faster (a back-of-envelope calculation suggests about 220 cycles saved
    ; IIRC), *but* it wouldn't improve the worst-case time and so to be flicker
    ; free we'd still have to keep the raster timings tight enough for the
    ; chunk-based copy. I therefore don't think it's worth it - it doesn't help
    ; us be flicker free or use looser raster bounds, and saving about 220
    ; cycles on about 23/25ths of the scrolling line feeds isn't huge - there
    ; are 2518 such line feeds in the benchmark, so we'd save about 0.25 seconds
    ; on the benchmark even if none of our savings just got thrown away waiting
    ; on the raster.
line_loop
    ldx #chunks_per_line
chunk_loop
ldy_imm_chunk_size_minus_1_b
    ldy #chunk_size_80 - 1 ; patched
byte_loop
; The body of this loop is slow enough that we fairly rapidly hit diminishing
; returns by unrolling it, *but* we do have spare space for the unroll at the
; moment and this loop executes for every single screen scroll. A scanline is
; only 128 cycles and in 80 column modes we go round this inner loop body 640
; times, so small savings really do add up and contribute towards increasing our
; chances of maintaining a flicker free display without slowing things down too
; much by having over-tight safe raster bounds.
byte_loop_unroll_count = 8
+assert min_chunk_size % byte_loop_unroll_count == 0
!for i, 1, byte_loop_unroll_count {
    lda (src),y
    sta (dst),y
    lda #0
    sta (src),y
    dey
}
    bpl byte_loop
    +assert_no_page_crossing byte_loop
    jsr bump_src_dst_and_dex
    bne chunk_loop

!ifdef DEBUG_COLOUR_BARS {
    lda #0 xor 7:sta bbc_palette
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

; For a window of n lines at the top of the screen to protect from scrolling,
; raster_wait_table_first[n-1] is the first "safe" timer 2 high byte and
; raster_wait_table_last[n-1] is the last "safe" timer 2 high byte. To disable
; raster waiting and just scroll regardless, specify $ff and $00 respectively.
;
; This macro helps create bytes in terms of scan lines from the top of the
; visible screen. Don't forget this is only approximate as we only check the
; high byte of the timer. There are 312 scanlines per frame.
!macro scan_line .scan_lines_from_visible_top {
    .vsync_to_visible_top_scan_lines = (total_rows - vsync_position) * 8
    .scan_lines_to_wait = (.vsync_to_visible_top_scan_lines + .scan_lines_from_visible_top) % 312
    !byte >(frame_us - .scan_lines_to_wait * us_per_scanline)
}
; SFTODO: In modes 3 and 6, we *might* want to use a different start position. In general we're talking about time to execute code and a constant 50hz refresh rate and the line height doesn't matter. However, in terms of "not starting too early", if (say) we start at scan line 16 in mode 0 we can never (barring overrun) modify top two lines while they are being drawn, *but* in mode 3 the top two lines cover scan lines 0-19 inclusive. My inclination is to not worry about this; if I tune the start safe scan line in the 25 line modes I will be playing it safe and not costing that much performance in 32 line modes. Should probably have a perm comment regarding this though.
; SFTODO: PERM COMMENT - I THINK HAVING A COPRO HITS THE HOST WITH MORE FREQUENT LINE FEEDS, SO IT SHOWS UP INTERMITTENT FLICKER MORE OFTEN - THE FASTER THE COPRO THE BETTER FOR TESTING, PROBABLY

; SFTODO: DO WE NEED TO BE ADJUSTING THESE COUNTS IF THE USER HAS USED *TV TO MOVE SCREEN UP/DOWN?

; This table is for 80 column modes; the 40 column table at raster_wait_table_40
; is copied over this by the discardable init code if necessary.
raster_wait_table_entries = fast_scroll_max_upper_window_size
raster_wait_table
raster_wait_table_first
    +scan_line 1*8 ; 1 line window
    +scan_line 2*8 ; 2 line window
    +scan_line 3*8 ; 3 line window
raster_wait_table_last
    +scan_line 8*8 ; 1 line window - this seems solid
    +scan_line 8*8 ; 2 line window - seems surprisingly solid
    +scan_line 4*8 ; 3 line window - not great, maybe nicer than with no raster check
raster_wait_table_end
    +assert raster_wait_table_last - raster_wait_table_first == raster_wait_table_entries
    +assert raster_wait_table_end - raster_wait_table_last == raster_wait_table_entries
}
runtime_end ; SFTODO: label names in this file are a bit crappy in general, e.g. this is the runtime *code* but this label is its non-runtime (end) address
; The code between runtime_start and runtime_end is copied down to runtime_start
; by our discardable initialisation code. Make sure it fits!
runtime_size = runtime_end - runtime_start
+assert runtime_size <= fast_scroll_end - fast_scroll_start
!warn "SFTODO TEMP: free space ", (fast_scroll_end - fast_scroll_start) - runtime_size

; This table is for 40 column modes; it is copied over raster_wait_table by the
; discardable init code if appropriate.
raster_wait_table_40
raster_wait_table_first_40
    +scan_line 1*8 ; 1 line window
    +scan_line 2*8 ; 2 line window
    +scan_line 3*8 ; 3 line window
raster_wait_table_last_40
    +scan_line 21*8 ; 1 line window - this seems solid
    +scan_line 21*8 ; 2 line window - seems surprisingly solid
    +scan_line 4*8 ; 3 line window - not perfect, maybe nicer than with no raster check
raster_wait_table_end_40
    +assert raster_wait_table_last_40 - raster_wait_table_first_40 == raster_wait_table_entries
    +assert raster_wait_table_end_40 - raster_wait_table_last_40 == raster_wait_table_entries

; It is vaguely tempting to try to have the code auto-tune the safe window end
; position. We could maintain a frame count to disambiguate the timer wrapping
; round. By inspecting (frame count, timer value) before and after executing our
; copy code, we could tell if we got the job done before hitting the first
; visible line on the following frame, and if we didn't we could (subject to a
; minimum value) nudge the safe end position closer to the top of the screen.
; This is probably not too complex, even in the limited code space we have here.
; However, we don't necessarily want to let rare outlying cases force us to use
; an extremely tight safe window when 99% of the time we can get away with
; something looser. And of course, auto-tuning is also potentially error prone
; and since manual tuning seems to have worked fairly well so far I don't want
; to rush into implementing this.

; SFTODO: OK, right now *without* this driver, using split cursor editing to copy when the inputs cause the screen to scroll causes split cursor editing to terminate. I am surprised - we are not emitting a CR AFAIK - but this is acceptable (if not absolutely ideal) and if it happens without this driver being in the picture I am not going to worry about it too much. But may want to investigate/retest this later. It may well be that some of the split cursor stuff I've put in this code in a voodoo-ish ways turns out not to actually matter after all.

; SFTODO: We should probably disable (in as few bytes of code as possible) our custom OSWRCH routine if we quit and don't force press break - or maybe this should just be the job of any custom code that runs on quit and BREAK is the default and all we care about in detail?

!zone { ; discardable init code

; This is copied over the code at update_hardware_screen_start_address on an
; Electron.
.electron_update_hardware_screen_start_address
!pseudopc update_hardware_screen_start_address {
    ; For the record, I have briefly experimented with putting a *FX19 call in
    ; here to wait for vsync on the Electron. It looks worse that way, unless
    ; I've got confused. I think this isn't too surprising, because even on the
    ; BBC (with faster RAM and no memory contention with the screen) for a
    ; flicker-free update of even a single line upper window we have to start in
    ; the visible region of the screen. So by waiting for vsync here, we
    ; deliberately force the Electron to start in a position that practically
    ; guarantees flicker, whereas by not waiting we will start somewhere better
    ; some of the time just by luck.
    lsr
    ror vdu_temp_store_da
    sta electron_screen_start_address_high
    lda vdu_temp_store_da
    sta electron_screen_start_address_low
    jmp update_hardware_screen_start_address_done
}
.electron_update_hardware_screen_start_address_end

; This code is copied over b_plus_copy_start in main RAM after we've copied that code into the private RAM.
.b_plus_copy_start_patch_start
!pseudopc b_plus_copy_start {
    lda romsel_copy
    pha
    lda #128
    sta romsel_copy
    sta bbc_romsel
    jsr fast_scroll_private_ram_aligned
    pla
    sta romsel_copy
    sta bbc_romsel
    jmp finish_oswrch
}
.b_plus_copy_start_patch_end

; SFTODO: Feels a bit crap to have to shove this here rather than re-using one, though it's all discardable init
.just_rts
    rts

init
    ; Check if we've already claimed vectors and do nothing if so. We don't
    ; expect this to happen, but since this is discardable init code we might as
    ; well check.
    lda wrchv
    cmp #<our_wrchv
    bne not_already_claimed
    lda wrchv+1
    cmp #>our_wrchv
    beq .just_rts
not_already_claimed
    ; Before we do anything else, we copy our code from inside this executable
    ; to its final location - we can then patch it in-place in the following
    ; code and copy it from in-place to the B+ private RAM if necessary. (The
    ; way acme's !pseudpc directive works means that it's fiddly to patch the
    ; copy embedded in this executable, as we don't have labels within the block
    ; of code addressing the embedded copy, so we'd have to manually apply
    ; offsets.) If we decide not to support fast hardware scrolling this is
    ; still OK; we own this block of memory so there's no problem with us
    ; corrupting it.
    ldx #>runtime_size
    ldy #<runtime_size
copy_runtime_code_loop
lda_runtime_start_abs
    lda runtime_start
sta_fast_scroll_start_abs
    sta fast_scroll_start
    +inc16 lda_runtime_start_abs+1
    +inc16 sta_fast_scroll_start_abs+1
    dey
    bne copy_runtime_code_loop
    dex
    bpl copy_runtime_code_loop
    ; Examine the current hardware and decide if we can support fast hardware
    ; scrolling on it; if not the Ozmoo executable will fall back to slow
    ; hardware scrolling or software scrolling as appropriate.
    lda #0
    sta fast_scroll_status_host
    sta fast_scroll_lines_to_move
    ; If we're going to run in mode 7, we don't bother installing anything. It
    ; would be mostly harmless if we did - the core Ozmoo executable will not
    ; enable the vsync events in mode 7, and we will have a screen window in
    ; effect for software scrolling so our OSWRCH code will do anything - but it
    ; seems better to avoid the unnecessary overhead on the OSWRCH vector. This
    ; might (although it probably won't) also help to ensure that "there is no
    ; fast scroll support" code paths get at least a little bit of routine
    ; testing.
    lda screen_mode_host
    cmp #7
    beq .just_rts
    ; The runtime code has constants for 80 column modes; patch some code/data
    ; if we're going to run in a 40 column mode.
    cmp #4
    bcc +
    ldx #chunk_size_40 - 1
    stx ldy_imm_chunk_size_minus_1_a + 1
    stx ldy_imm_chunk_size_minus_1_b + 1
    +copy_data raster_wait_table_40, raster_wait_table_end_40, raster_wait_table
+

    ; If we don't have shadow RAM, that's fine. If we do have shadow RAM, we
    ; must have a shadow driver for it which is capable of paging. The only
    ; exception is a B+ with the private RAM available for our use.
    ldx #<null_shadow_driver
    ldy #>null_shadow_driver
    lda shadow_state
    cmp #shadow_state_none
    beq use_null_shadow_driver
    cmp #shadow_state_first_driver
    bcc .just_rts ; we have shadow RAM but no driver
    ldx shadow_paging_control_ptr
    ldy shadow_paging_control_ptr + 1
    bne use_shadow_driver_yx ; we have shadow RAM with a paging-capable driver
    ; We don't have a paging-capable shadow driver. Unless this is the B+
    ; special case, we can't support fast scrolling.
    cmp #shadow_state_b_plus_private
    bne .just_rts
    ; It's the B+ special case. Copy the relevant code into private RAM so it
    ; can access the shadow screen.
    lda romsel_copy
    pha
    lda #128
    sta romsel_copy
    sta bbc_romsel
    ; -1 in the next line to allow for the RTS opcode we patch on afterwards.
    +copy_data_checked b_plus_copy_start, b_plus_copy_end, fast_scroll_private_ram_aligned, fast_scroll_private_ram_end - 1
    lda #opcode_rts
    sta fast_scroll_private_ram_aligned + (b_plus_copy_end - b_plus_copy_start)
    +copy_data .b_plus_copy_start_patch_start, .b_plus_copy_start_patch_end, b_plus_copy_start ; SFTODO: THESE LABELS ARE INSANE
    pla
    sta romsel_copy
    sta bbc_romsel
    ; fall through to use_null_shadow_driver
use_null_shadow_driver
    ldx #<null_shadow_driver
    ldy #>null_shadow_driver
use_shadow_driver_yx
    ; We have shadow paging control, so patch up this code to call it.
    stx jsr_shadow_paging_control1 + 1
    stx jsr_shadow_paging_control2 + 1
    sty jsr_shadow_paging_control1 + 2
    sty jsr_shadow_paging_control2 + 2
    ; Determine the host type. We don't care about the Integra-B spoofing this
    ; here, as all we're checking for is Electron vs non-Electron.
    lda #osbyte_read_host
    ldx #1
    jsr osbyte
    txa
    bne not_electron
    ; We're on an Electron with no shadow RAM or a paging-capable shadow driver.
    ; We can support fast scrolling, but we need to tweak the code to remove
    ; BBC-specific aspects.
    ; Disable the check for the raster position.
    lda #opcode_jmp:sta check_raster
    lda #<dont_wait_for_raster:sta check_raster + 1
    lda #>dont_wait_for_raster:sta check_raster + 2
    ; Overwrite the BBC hardware screen start address update with the Electron code.
    +copy_data_checked .electron_update_hardware_screen_start_address, .electron_update_hardware_screen_start_address_end, update_hardware_screen_start_address, update_hardware_screen_start_address_done
    jmp common_init
not_electron
    ; Install our EVNTV handler so we can tell where the raster is. We don't
    ; install this on the Electron as it doesn't have a user VIA, and there
    ; isn't an alternative timer we can use.
    ; SFTODO: inconsistent - old__evntv but parent_wrchv
    sei
    lda evntv:sta old_evntv
    lda evntv+1:sta old_evntv+1
    lda #<evntv_handler:sta evntv
    lda #>evntv_handler:sta evntv+1
    lda #0
    sta user_via_auxiliary_control_register
    cli
common_init
    lda #1
    sta fast_scroll_status_host

    ; We haven't already claimed vectors, so claim them. This patches the code
    ; we just copied down to fast_scroll_start.
    sei
    lda wrchv
    sta parent_wrchv
    lda wrchv+1
    sta parent_wrchv+1
    lda #<our_wrchv
    sta wrchv
    lda #>our_wrchv
    sta wrchv+1
    cli
    ; We don't enable vsync events; we don't need them while
    ; fast_scroll_lines_to_move is zero, and the Ozmoo executable takes care of
    ; turning them on and off as that changes.
    rts

}

; SFTODO: Some thoughts on making this work properly without hacks:
;
; If https://stardot.org.uk/forums/viewtopic.php?f=54&t=12242&p=155982&hilit=fc7f#p155982 is correct (and looking at the Elkulator source seems to back it up), code running at &8000 or above can toggle MRB access for the *whole* of the lower 32K. So we could steal 512 bytes from the last sideways RAM bank, treat that as short and install a variant on the above code in there if we're running with MRB shadow. We could probably also shove a B+ private RAM style shadow driver in there, although it still wouldn't be paging capable it would be faster at copying whole pages. The biggest faff factor there is really the problem of the loader deciding whether we can "afford" to lose 512 bytes of sideways RAM to this - or are we willing to say "if you have MRB, you must be willing to give this up?" Not sure. Ah, and there's also a post by Sarah Walker pointing out that you can (probably? not sure if she tried it) use the MRB OS routine to copy your code across to the corresponding location in the "other" RAM beforehand, and then you *can* toggle MRB access (carefully - may need to disable interrupts too, not sure) while running from <&8000 without crashing. (As per notes below, it would still be faster - bear in mind the "shadow" RAM is probably the slow RAM, so we'd be running in fast RAM by default but when our driver toggled over into MRB mode, we'd be running from the slow RAM - the video memory will always be slow, but if we were running from sideways RAM we'd not suffer any slowdown in our code. So running from sideways RAM is still better, except it raises the - maybe awkward, maybe not that hard really, not thought in detail - issue of how to decide if we can spare 512 bytes of sideways RAM for our driver.
;
; Worth noting that even without MRB issues, it would be *nicer* to implement this on the Electron by stealing 512 bytes of sideways RAM because then this code would be running at 2MHz (although it would still be contending with the ULA for the screen data itself, but that's no worse than the OS doing it). But maybe for a first cut, I'd support Electron running only from low RAM and only if there's no MRB (shadow; MRB turbo is absolutely fine of course, assuming the machine is "big" enough to run the game OK without the extra shadow RAM), and then this could be tweaked further later on.
;
; Maybe this driver *doesn't* need to disable itself if there's an OS text window defined (but if that's true it should probably disable itself in mode 7 to avoid confusion). Or at least if there's an OS text window defined covering all but the top row of the screen. (Games with "bigger" top rows were never compatible with our hw scrolling use and will force sw scrolling with suitable OS text windows, and we need to make sure that doesn't break.) If there is an OS text window defined which covers all but the top window of the screen, the scrolling performed by this code is faster and uglier, but compatible, and if the OS decides to do the scroll itself because of printing at bottom right the text window will mean it does the right thing. However, this would mean we'd need some other way to allow Ozmoo to toggle between software and hardware scrolling at runtime, and it also gives us a more complex check to perform before triggering our new behaviour. So gut feeling is that all in all it's simpler both here and in the Ozmoo code for this driver to just disable itself if there is a software text window in effect, then everything should "just work" nicely.
;
; It is a bit of a shame we can't get rid of the in-Ozmoo code to handle old-style hardware scrolling, but at least it isn't a huge quantity of code - still, it would have been nice to shrink the Ozmoo executable a bit and just maybe move towards getting an extra 512 bytes of RAM free at runtime (it all adds up, right?). I half wonder if the OSWRCH driver installer could install a copy of the old-style hw scrolling code from Ozmoo somewhere if it doesn't have a new-style driver, but that feels like it could get messy.
;
; I half wonder if (by peeking OS text cursor Y and text fg/bg colours) it wouldn't be *too* hard for a pure-OS OSWRCH version of the current HW scrolling implementation to be added, although to start with I should definitely try simplifying the pure OS HW scrolling in Ozmoo itself and if that doesn't add too much complexity it may be silly to go to the trouble and risk of bugs by trying to implement the same thing on the OSWRCH vector with less information to hand and less "nicely allocated" memory available etc.
;
; Should we cap the maximum size of upper window we support? As the upper window gets bigger, this technique will look uglier and software scrolling will be less annoyingly slow. Experiment with a game with a big upper window - Seastalker?? (The capping would probably need to be done in the core Ozmoo binary, although just *maybe* the driver would write a "recommended max upper window size" somewhere in ZP, just like it indicates its status (maybe that status is repurposed, with 0 meaning nothing but >0 meaning "max recommended upper window size") which the loader would copy somewhere for Ozmoo to use - that way it can switch to software scrolling for upper windows over a size based on our judgement of the current HW capabilities. It may not really be useful to do this extra complexity though, perhaps at least to start with just hard-code a particular value in the Ozmoo binary.
;
; SFTODO: Although that code was probably written without much care about code size, STEM has a highly-optimised line-oriented memmove which would potentially be ideal for copying the line data when we scroll (especially if we start to do it for >1 line of data). Gut feeling is this is too complex/bug for the memory we have here, but I'll leave this note around for now.

; SFTODO: Move vdu_down constant to shared constants header and use it in this code instead of literal 10 all over the place?

; SFTODO: Give Electron support a good test at some point once this settles down. Does split cursor mode work? Do we need to hide the (software generated) cursor when we are scrolling? I suspect we don't, but perhaps test a bit more thoroughly.
