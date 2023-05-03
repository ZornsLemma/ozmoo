; Cache to use memory on the host in Acorn second processor builds.
; SFTODO: Eventually this should be capable of using spare shadow RAM too

!source "acorn-shared-constants.asm"

userv = $200
osword_a = $ef
osword_x = $f0
osword_y = $f1
osword_block_ptr = $f0
bbc_tube_data = $fee5
electron_tube_data = $fce5
tube_entry = $406
tube_reason_claim = $c0
tube_reason_release = $80
tube_reason_256_byte_to_io = 6
tube_reason_256_byte_from_io = 7
osbyte_read_oshwm = $83
osbyte_read_screen_address_for_mode = $85

; SFTODO: Why don't we just use $70-8f for zp_temp? In practice tube host code leaves this free.
zp_temp = $f5 ; 3 bytes

; $70-74 inclusive are used by the shadow driver so we keep them free; we could
; potentially reuse some of these addresses with care, but while we're not short
; of zero page it seems best just to steer clear.
; SFTODO: Should I move all "shared" low memory allocations into acorn-shared-constants and comment them all there, to make it more obvious what is being used simultaneously?

; We use this address for the loader to communicate the intended screen mode to the cache.
cache_screen_mode = $75 ; 1 byte

; SFTODO: Arbitrarily chosen magic number for tube claims. I don't know if there is
; some standard number allocated to the foreground application.
our_tube_reason_claim_id = $25 ; a six bit value

; When current_tick would wrap round to 0, we subtract timestamp_adjustment from
; all the existing timestamps (setting any which would become negative to 0). A
; small value of timestamp_adjustment preserves more useful range in the
; timestamps, but at the cost of having to do the adjustment more often.
timestamp_adjustment = $40 ; SFTODO: $80?

page_in_swr_bank_a_electron_size = 13

!macro assert_no_page_crossing .target {
    !if (>*) <> (>.target) {
        !error "Unacceptable page crossing"
    }
}

; SFTODONOW: For the moment I'm hardcoding these, but we should really make them available by moving them from acorn-shadow-driver.asm to acorn-shared-constants to avoid keeping things in sync being a problem.
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
shadow_state = $70
private_ram_in_use = $71

program_start
    jmp relocate_setup

    ; SFTODO: ON A RESTART OZMOO BINARY WILL PROBABLY INVOKE THIS OSBYTE, SHOULD WE MAKE IT PRESERVE ITS CACHE WHEN CALLED WITH "SAME" X OR ON A SUBSEQUENT CALL OR SOMETHING? THIS MIGHT HELP SPEED UP A RESTART AS THIS CACHE MAY BE USABLE FOR LOADING. OTOH THIS MAY BE AN EXTRA SOURCE OF OBSCURE BUGS ON A RESTART.
; OSBYTE $88 - host cache initialisation
;
; On entry: N/A
;
; On exit:
;   X contains the number of 512-byte blocks the cache holds
; SFTODO: Test (perhaps just force X=0 on return) to see nothing crashes if the cache happens to have no space - I don't think this is likely (especially if we start checking OSHWM in host is something like <=&2500 and refusing to run if noot) but worth a go.
our_osbyte
cache_entries_high = zp_temp ; 1 byte
    ; The cache size doesn't change at runtime, it's always full with
    ; cache_entries entries, but it starts full of very old entries with the
    ; invalid block ID. This means those entries will be evicted in preference
    ; to any real entries and will never be returned in response to a cache
    ; lookup.
    ldy cache_entries
-   lda #0
    sta cache_timestamp - 1,y
    lda #$ff
    sta cache_id_low - 1,y
    sta cache_id_high - 1,y
    dey
    bne -

    ; All the blocks are free, so let's set free_block_index to 0.
    sty free_block_index

    ; The initial dummy cache entries have timestamp 0, so we start current_tick at 1.
    lda #1
    sta current_tick ; SFTODO: Rename "current_timestamp"??

    ; Because in practice the cache will always be offered a block when a block
    ; is requested from it and processing that offered block will involve
    ; discarding the oldest block even if it could have satisfied the request
    ; being made, the cache is effectively one block smaller than its nominal
    ; size, so that's what we report to the caller. (The only practical upshot
    ; of this is that the preload won't load a block which can never be
    ; successfully retrieved, which saves a tiny amount of time.)
    ;
    ; To avoid this we'd need to do a swap when a block was offered and the
    ; cache could satisfy the request, but that would complicate the code
    ; significantly. If we set aside a 512 byte buffer for use in the swap we'd
    ; be no better off than we are now, because the cache would have to be one
    ; block smaller to make room for that buffer, so the swap would have to be
    ; written to work in smaller chunks as well.
    ldx cache_entries
    dex ; SFTODO: Any risk in corner cases this goes negative?
    rts

our_userv
    cmp #$E0
    beq our_osword
    cmp #0
    beq our_osbyte ; *CODE/OSBYTE $88
    jmp (old_userv)

    ; Waste some space so we avoid unwanted page crossing in time-critical loops.
    !fill 4 ; SFTODONOW: Don't forget to tweak this as necessary once code has been updated for new features

; OSWORD &E0 - host cache access
;
; On entry:
;   YX?0:     12 (send block length)
;   YX?1:     12 (receive block length)
;   YX!2:     address of 512-byte data block (high word must be 0) SFTODO: GET RID OF () BIT, IT'S TRUE-ISH BUT NOT REALLY THAT HELPFUL AND POSS NOT QUITE TRUE ONCE WE HAVE TURBO SUPPORT
;   YX?6..7:  ID of 512-byte block currently held at YX!2 (offered to cache)
;             (ID $FFFF means no block is being offered to the cache.)
;             SFTODO: We could use 0 for this special case, Ozmoo will never pass 0 as it will be in dynamic memory - FF does have the small (?) advantage that as it's odd, it can never match the low byte of anything Ozmoo will pass, which may slightly help to optimise searching
;   YX?8:     Timestamp hint for offered block, or $FF for no hint.
;   YX?9..10: ID of 512-byte block wanted at YX!2 (requested from cache)
;
; On exit:
;   YX?11:    0 ("no error") means data at YX!2 replaced with block YX?9..10
;             non-0 means block YX?9..10 is not in cache and data at YX!2 not
;             modified
;
; In practice a block ID of $ABCD will correspond to the 512-byte block of
; Z-machine address space starting at $ABCD00 >> 1, but this code doesn't
; actually care.
;
; The implementation removes blocks from the cache when they are provided to the
; caller. This means that a block is either in this cache or in the caller's own
; cache, not both. It also means that when the caller offers us a block, we know
; it isn't in our cache already, provided (as is the case in practice) the
; caller always queries us before loading a block from disk itself.
;
; SFTODO: PSEUDOCODE NEEDS UPDATING TO REFLECT TIMESTAMP HINTS
; Pseudocode:
;   if block_offered != 0xffff:
;       if g_free_block_index is None:
;           g_free_block_index = find_oldest_block_index()
;       cache_timestamp[g_free_block_index] = current_tick
;       cache_id[g_free_block_index] = block_offered
;       cache[g_free_block_index] = block
;       g_free_block_index = None
;       advance_tick()
;   for i in range(0, cache_entries):
;       if cache_id[i] == block_requested:
;           cache_timestamp[i] = 0
;           cache_id[i] = 0xffff
;           g_free_block_index = i
;           return cache[i]
;   return None

our_osword_data_offset = 2
our_osword_block_offered_offset = 6
our_osword_block_offered_timestamp_hint_offset = 8
our_osword_block_requested_offset = 9
our_osword_result_offset = 11
our_osword
our_cache_ptr = zp_temp ; 2 bytes
count = zp_temp + 2 ; 1 byte

    lda romsel_copy
    pha

    ; Is a block being offered to us by the caller?
    ldy #our_osword_block_offered_offset + 1
    lda (osword_block_ptr),y
    tax
    dey
    lda (osword_block_ptr),y
    cmp #$ff ; SFTODO MAGIC NUMBER
    bne block_offered
    cpx #$ff ; SFTODO MAGIC NUMBER
    bne block_offered
    jmp no_block_offered ; SFTODO: can we avoid indirect branch?
block_offered
    ; A block is being offered to us. If free_block_index doesn't already identify
    ; somewhere in the cache to store it, find the oldest entry in the cache so we
    ; can re-use it.
    ldx free_block_index
    cpx #free_block_index_none
    bne have_free_block_index_in_x
    ldy cache_entries
    dey
    ; Start off by assuming the 0th entry is the oldest.
oldest_index = zp_temp
    lda #0
    sta oldest_index
    lda cache_timestamp
    ; Now examine all the other entries (Y=cache_entries - 1 down to 1
    ; inclusive) to find the oldest.
find_oldest_loop
    cmp cache_timestamp,y
    bcc newer_than_a
    sty oldest_index
    lda cache_timestamp,y
    ; SFTODO: We could beq to escape if we've found a maximally-aged block already, not sure if this is worth it or not
newer_than_a
    dey
    bne find_oldest_loop
    ldx oldest_index
have_free_block_index_in_x

    ; Update the cache metadata for the new block at index X.
    ldy #our_osword_block_offered_offset
    lda (osword_block_ptr),y
    sta cache_id_low,x
    iny
    lda (osword_block_ptr),y
    sta cache_id_high,x
    iny
    +assert our_osword_block_offered_offset + 2 = our_osword_block_offered_timestamp_hint_offset
    lda (osword_block_ptr),y
    cmp #$ff ; SFTODO: share osword_cache_no_timestamp_hint constant?
    beq no_timestamp_hint
    ; We have a timestamp hint. current_tick isn't incremented automatically in
    ; this case (otherwise the initial cache population may end up squashing
    ; some of the timestamps down to zero as we populate a large cache a block
    ; at a time), but we ensure current_tick is strictly greater than the
    ; timestamp hint so blocks added to the cache during normal use are
    ; correctly treated as more recent.
    cmp current_tick
    bcc timestamp_hint_less_than_current_tick
    sta current_tick
    inc current_tick ; can't wrap as hint != $ff
timestamp_hint_less_than_current_tick
    sta cache_timestamp,x
    jmp timestamp_updated
no_timestamp_hint
    lda current_tick
    sta cache_timestamp,x
    ; Increment current_tick.
    inc current_tick
    bne tick_not_wrapped
    ; SFTODO: FWIW in the benchmark this tick adjust case only occurs once (rather dated comment, may no longer be true)
    ldy cache_entries
tick_adjust_loop
    lda cache_timestamp - 1,y
    sec
    sbc #timestamp_adjustment
    bcs adjusted_timestamp_not_negative
    lda #0
adjusted_timestamp_not_negative
    sta cache_timestamp - 1,y
    dey
    bne tick_adjust_loop
    lda #($100 - timestamp_adjustment)
    sta current_tick
tick_not_wrapped
timestamp_updated

    ; Reset free_block_index.
    lda #free_block_index_none
    sta free_block_index

    ; If we're using a shadow bounce buffer for this block, we need to copy 256
    ; bytes from the bounce buffer into shadow RAM after the tube read loop.
    ; This flag tells set_our_cache_ptr_to_index_y to do nothing special and
    ; do_loop_tail_common to do both copies.
    ; SFTODO: IT'S A BIT SILLY HAVING THIS FLAG TO TELL "COMMON" CODE TO BEHAVE DIFFERENTLY - WE SHOULD JUST PULL THE CODE OUT OF DO_LOOP_TAIL_COMMON FOR THE RELEVANT CASES. I THINK WE STILL NEED THIS FLAG FOR ONE OTHER USE, BUT IT'S STILL A WIN AND IT MAY BE WE COULD HANDLE THE FLAG DIFFERENTLY IN THAT ONE REMAINING CASE. - NOT A TERRIBLE IDEA, BUT THERE IS A MODERATE AMOUNT OF "COMMON" LOGIC EVEN SO, EG CHECKING IF WE ARE ACTUALLY DOING A SHADOW BLOCK, SO IT MAY NOT BE A GOOD CHANGE - THINK ABOUT IT
    lda #0
    sta SFTODOSHADOWCOPYBEFORE

    ; Set our_cache_ptr to the block's data so we can update it.
    txa
    tay
    jsr set_our_cache_ptr_to_index_y

    ; Copy the 512-byte block offered to the cache into the block pointed to by
    ; our_cache_ptr.
    jsr claim_tube
    lda #2
    sta count
copy_offered_block_loop
    jsr set_yx_to_tube_transfer_block
    lda #tube_reason_256_byte_to_io
    jsr tube_entry
    ; We now need a 19 microsecond/38 cycle delay.
    ldx #7     ; 2 cycles
wait_x
    dex        ; 2*7=14 cycles
    bne wait_x ; 3*6+2=20 cycles
    +assert_no_page_crossing wait_x
    ldy #0     ; 2 cycles
    ; SFTODO: Kind of stating the obvious, but the tube loops obviously burn loads of CPU time, so even if it feels inefficient to be iterating over 255 cache entries once or twice per call to check timestamps or whatever, remember we have to do 512 iterations of this loop and/or the other similar tube loop, so that other code isn't negligible but is diluted. Plus of course we are saving a trip to disc.
tube_read_loop
lda_abs_tube_data
    ; We must not read from bbc_tube_data more often than once every 10
    ; microseconds/20 cycles.
    lda bbc_tube_data     ; 4 cycles
    sta (our_cache_ptr),y ; 6 cycles
    lda (our_cache_ptr),y ; 5 cycles (dummy, cache is page-aligned so not 6)
    iny                   ; 2 cycles
    bne tube_read_loop    ; 3 cycles if we branch
    +assert_no_page_crossing tube_read_loop
    ldy shadow_ptr_high
    beq not_shadow_in_tube_read_loop
    lda shadow_bounce_buffer
    ; SFTODO: IF SQUASHING CODE, SHARING THE FOLLOWING TWO INSNS MAY SAVE A BYTE
    jsr shadow_ram_copy
    inc shadow_ptr_high
    dec our_cache_ptr + 1 ; counteract do_loop_tail_common incrementing this
not_shadow_in_tube_read_loop
    jsr do_loop_tail_common
    bne copy_offered_block_loop
    jsr release_tube
    jsr reset_osword_block_data_offset
no_block_offered

    ; Do we have the requested block in cache?
    ldy #our_osword_block_requested_offset + 1
    lda (osword_block_ptr),y
    tax
    dey
    lda (osword_block_ptr),y
    ; XA now contains the requested block ID.
    ldy cache_entries
    ; Although it's probably not a big deal, this loop accounts for most of our
    ; cycles outside the tube copy loops. We therefore save a cycle in the
    ; common case where the low bytes don't match by making that the straight
    ; line case.
find_cache_entry_by_id_loop
    cmp cache_id_low - 1,y
    beq possible_match
not_match
    dey
    bne find_cache_entry_by_id_loop
    +assert_no_page_crossing find_cache_entry_by_id_loop
    ; We don't have the requested block.
    lda #1
    ldy #our_osword_result_offset
    sta (osword_block_ptr),y
    bne our_osword_done ; Always branch
possible_match
    sta zp_temp
    txa
    cmp cache_id_high - 1,y
    beq match
    lda zp_temp
    jmp not_match
match
    dey ; so Y contains a 0-based index not a 1-based index as in the loop above

    ; We do have the requested block, at index Y. Remove it from our cache,
    ; since the caller will be taking ownership of it.
    lda #0
    sta cache_timestamp,y
    lda #$ff ; SFTODO: MAGIC NUMBER
    sta cache_id_low,y
    sta cache_id_high,y
    sty free_block_index

    ; If we're using a shadow bounce buffer for this block, we need to copy 256
    ; bytes from shadow RAM into the bounce buffer before the tube write loop.
    ; This flag tells set_our_cache_ptr_to_index_y to do the first 256 byte copy
    ; and do_loop_tail_common to do the second 256 byte copy at the end of the
    ; first pass round the loop. SFTODO: THIS ISN'T TRUE WITH CURRENT REWORK, set_our_cache_ptr... NO LONGER FIDDLES WITH THIS - TWEAK OTHER COMMENT ON OTHER TUBE COPY LOOP TOO
    lda #1
    sta SFTODOSHADOWCOPYBEFORE

    ; Set our_cache_ptr to point to the block's data.
    jsr set_our_cache_ptr_to_index_y

    ; Set the result to say we were able to provide the requested block.
    ldy #our_osword_result_offset
    lda #0
    sta (osword_block_ptr),y

    ; Copy the 512-byte block in our cache pointed to by our_cache_ptr back to
    ; the caller.
    jsr claim_tube
    lda #2
    sta count
copy_requested_block_loop
    lda shadow_ptr_high
    beq not_shadow_in_tube_write_loop
    ldy shadow_bounce_buffer
    sty our_cache_ptr + 1 ; counteract do_loop_tail_common incrementing this
    jsr shadow_ram_copy
    inc shadow_ptr_high
not_shadow_in_tube_write_loop
    jsr set_yx_to_tube_transfer_block
    lda #tube_reason_256_byte_from_io
    jsr tube_entry
    ; We don't need an initial delay with this reason code.
    ldy #0
tube_write_loop
    ; We must not write to bbc_tube_data more often than once every 10
    ; microseconds/20 cycles.
    lda (our_cache_ptr),y    ; 5 cycles
    sta (our_cache_ptr),y    ; 6 cycles (dummy)
sta_abs_tube_data
    sta bbc_tube_data        ; 4 cycles
    iny                      ; 2 cycles
    bne tube_write_loop      ; 3 cycles if we branch
    +assert_no_page_crossing tube_write_loop
    jsr do_loop_tail_common
    bne copy_requested_block_loop
    jsr release_tube
    jsr reset_osword_block_data_offset

our_osword_done
    ; We don't need to preserve A, X or Y. We just need to leave the same sideways
    ; ROM paged in as when we were entered.
    pla
    ; fall through to page_in_swr_bank_a
page_in_swr_bank_a
    sta romsel_copy
    sta bbc_romsel
    rts
    ; Leave room for the larger Electron version of page_in_swr_bank_a to be copied
    ; over the BBC version.
!fill page_in_swr_bank_a_electron_size - (* - page_in_swr_bank_a)

claim_tube
    lda #tube_reason_claim + our_tube_reason_claim_id
    jsr tube_entry
    bcc claim_tube
    rts

release_tube
    lda #tube_reason_release + our_tube_reason_claim_id
    jmp tube_entry

; Set YX to point to the data block address within our OSWORD block.
set_yx_to_tube_transfer_block
    clc
    lda osword_x
    adc #our_osword_data_offset
    tax
    ldy osword_y
    bcc no_carry
    iny
no_carry
    rts

do_loop_tail_common
    ; Bump the source and destination addresses by one page.
    inc our_cache_ptr + 1
    lda #1
    jsr adjust_osword_block_data_offset
    dec count ; must be last
    rts

SFTODOSHADOWCOPYBEFORE !byte 0
shadow_ptr_high !byte 0 ; SFTODO MOVE ETC

; Undo the changes made to the high byte of the data address during a copy loop.
reset_osword_block_data_offset
    lda #(-2 and $ff)
    ; fall through to adjust_osword_block_data_offset

; Add A to the high byte of the data address in the OSWORD block.
adjust_osword_block_data_offset
    ldy #our_osword_data_offset + 1
    clc
    adc (osword_block_ptr),y
    sta (osword_block_ptr),y
    rts

; Set our_cache_ptr to point to the block with index Y.
set_our_cache_ptr_to_index_y
    lda #0
    sta our_cache_ptr
    sta shadow_ptr_high ; SFTODO TEMP HACK SO WE CAN USE IT TO DECIDE IF WE'RE DOING SHADOW OR NOT
    tya
    sec
    sbc low_cache_entries
    bcs index_in_high_cache
    tya
    asl
    clc
    adc #>low_cache_start
    sta our_cache_ptr + 1
anrts
    rts
index_in_high_cache ; SFTODO: rename in_swr_cache? Altho that's really just below after cmp swr_cache_entries
; SFTODO: COMMENT?
    cmp swr_cache_entries
    bcs index_in_shadow_cache
    ; A now contains the 512-byte block offset from the start of our first
    ; sideways RAM bank. Each 16K bank has 32 512-byte blocks, so we need to
    ; divide by 32=2^5 to get the bank index.
swr_base = our_cache_ptr + 1
    pha
    lsr
    lsr
    lsr
    lsr
    lsr
    tay
    lda #$80
    sta swr_base
    lda ram_bank_list,y
    jsr page_in_swr_bank_a
    ; If b6 of A is set, we've paged in the Integra-B private RAM and we need to
    ; skip over the 1K of IBOS workspace at $8000 by changing swr_base. Making
    ; swr_base variable like this will slow down other machines but it's a
    ; handful of cycles and our execution time is dominated by the tube copy
    ; loops and the cache searching anyway. SFTODONOW TEST THIS ON IBOS!
    asl
    bpl not_integra_b_private_ram
    lda #$84
    sta swr_base
not_integra_b_private_ram
    ; Now get the low 5 bits of the block offset, multiply by two to convert to
    ; 256 byte pages and that gives us the page offset within the bank.
    pla
    and #31
    asl
    ; Carry is already clear
    adc swr_base
    sta our_cache_ptr + 1
    rts
index_in_shadow_cache
    ; Carry is already set
    sbc swr_cache_entries
    asl
    ; Carry is already clear
    adc #>shadow_start
    sta shadow_ptr_high
    lda shadow_bounce_buffer
SFTODOHANG
    beq SFTODOHANG ; SFTODO TEMP HACK - IT SHOULDN'T BE POSSIBLE TO GET HERE IF WE HAVE NO SHADOW BOUNCE BUFFER
    sta our_cache_ptr + 1
    rts


    ; SFTODO: ALL SHADOW SUPPORT CODE FOR CACHE OFFER/RESPONSE NEEDS COMMENTING PROPERLY AND TIDYING

    ; If this executable is re-loaded, old_userv will be overwritten by 0 as
    ; part of the re-load and things will break. In reality this isn't going to
    ; happen and I don't think there's any great solution to it. We can't easily
    ; put this just before the binary because the relocation likes to work on
    ; page boundaries, and since we have discardable init code at the end of the
    ; binary we can't put it there either.
    ; SFTODO: Maybe worth thinking about this, it may be we could save code and
    ; complexity by not doing some stuff that won't work anyway.
old_userv
    !word 0

    ; Permanent code ends here; the following memory contains some one-off
    ; initialization code and some overlapping data allocations.
code_end

max_cache_entries = 255
free_block_index_none = max_cache_entries ; real values are 0-(max_cache_entries - 1)

low_cache_entries
    !byte 0
swr_cache_entries
    !byte 0
shadow_cache_entries
    !byte 0
shadow_bounce_buffer ; SFTODO: rename to indicate this is the start page not the actual buffer address?
    !byte 0
; SFTODO: If we need further variables, they can go here before we do !align.

; The following allocations try to avoid page crossing and assume
; max_cache_entries == 255.
+assert max_cache_entries == 255
; To avoid page crossing we want to allocate each of the 255-byte arrays here
; inside its own page; we really want them to start one byte into the page so
; foo-1,y addressing doesn't cross a page boundary (particularly important for
; cache_id_low as it's used in the hot find_cache_entry_by_id loop). We pair
; three single-byte variables with the 255-byte blocks so as to get the desired
; alignment.
    !align 255, 0, 0 ; SFTODO: If this is *just* over a page boundary try hard to optimise to pull it back
cache_entries
    !byte 0
cache_id_low
    !fill max_cache_entries
free_block_index
    !byte 0
cache_id_high
    !fill max_cache_entries
current_tick
    !byte 0
cache_timestamp
    !fill max_cache_entries
low_cache_start = *
!if low_cache_start & $ff <> 0 {
    !error "low_cache_start must be page-aligned"
}

initialize
    ; We claim iff we haven't already claimed USERV; this avoids crashes if
    ; we're executed twice, although this isn't expected to happen.
    lda #<our_userv
    ldx #>our_userv
!if 0 { ; This is pointless, as old_userv will be lost on a second execution.
    cmp userv
    bne initialize_needed
    cpx userv + 1
    beq initialize_done
initialize_needed
}
    ; Install ourselves on USERV, saving any previous claimant so we can forward
    ; calls we're not handling.
    ldy userv
    sty old_userv
    ldy userv + 1
    sty old_userv + 1
    sta userv
    stx userv + 1

    ; If we're running on an Electron, patch the code accordingly.
    lda #osbyte_read_host
    ldx #1
    jsr osbyte
    txa
    bne not_electron

    ; Patch the tube_data accesses.
    lda #<electron_tube_data
    sta lda_abs_tube_data + 1
    sta sta_abs_tube_data + 1
    lda #>electron_tube_data
    sta lda_abs_tube_data + 2
    sta sta_abs_tube_data + 2

    ; Patch page_in_swr_bank_a
    ldx #page_in_swr_bank_a_electron_size - 1
-   lda page_in_swr_bank_a_electron,x
    sta page_in_swr_bank_a,x
    dex
    bpl -
not_electron

    ; Calculate low_cache_entries. We have RAM available between low_cache_start
    ; and the screen.
    lda cache_screen_mode
    ora #shadow_mode_bit
    tax
    lda #osbyte_read_screen_address_for_mode
    jsr osbyte
    ; The following calculation will correctly discard any odd pages, so we
    ; don't need to be double-page aligned in order to make things work nicely.
    tya
    sec
    sbc #>low_cache_start
    lsr
    sta low_cache_entries

    ; Each 16K sideways RAM bank holds 32*512-byte blocks.
    lda #0
    sta cache_entries_high
    lda ram_bank_count
    ldx #5 ; 32 == 2^5
-   asl
    rol cache_entries_high
    dex
    bne -
    adc low_cache_entries
    sta cache_entries
    bcc +
    inc cache_entries_high
+

    ; Adjust to allow for the last sideways RAM bank possibly being short if
    ; it's Integra-B or B+ private RAM.
    +assert b_plus_private_ram_size == integra_b_private_ram_size + 512
    ldy ram_bank_count
    beq no_private_ram
    ldx #(b_plus_private_ram_size - 16*1024)/512
    lda ram_bank_list-1,y
    bmi b_plus_private_ram
    asl
    bpl no_private_ram
    ; This is the Integra-B private 12K.
    dex ; we have one 512 byte block less of private RAM on the Integra-B.
    ; Set up RAMSEL so we can access the private 12K by setting b6 (PRVEN) of
    ; ROMSEL, much as we can access it by setting b7 on the B+.
    ; SFTODO: Copy and paste from core Ozmoo code - probably OK, but think.
    ; SFTODO: Extra bulk of this code and other code added recently is annoying,
    ; probably can't do much - we need to be able to re-init the cache after
    ; a RESTART, which won't re-run CACHE2P, so we can't discard all this code
    ; - but we could maybe discard *some* code, including this one-off config,
    ; eg by jsring to discardable init code and nopping out the jsr afterwards
    ; from the discardable init code
    lda $37f
    ora #%00110000 ; set PRVS4 and PRVS8 to make all 12K visible
    sta $37f
    sta $fe34
b_plus_private_ram
    ; X contains the negative number of cache entries we need to subtract
    ; because of the short bank.
    clc
    txa
    adc cache_entries
    sta cache_entries
    ; SFTODO: It's annoying to have to deal with the 16-bit value here. I can't
    ; help feeling there might be a way to avoid this by doing this adjustment
    ; differently, but this will do for now - think about this fresh later.
    lda cache_entries_high
    adc #$ff
    sta cache_entries_high
no_private_ram

    ; SFTODONOW COMMENT - THIS IS WRONG BECAUSE IT WILL PROBABLY FAIL WHERE WE'RE CAPPING AT 255 CACHE ENTRIES BELOW, BUT LET'S JUST HACK IT FOR NOW AND IT WILL BE FINE ON MACHINES WITH LITTLE SWR FOR INITIAL TESTING
    lda cache_entries
    sec
    sbc low_cache_entries ; SFTODO: rename this main_ram_cache_entries??
    sta swr_cache_entries

    ; Add in shadow_cache_entries.
    clc
    lda cache_entries
    adc shadow_cache_entries
    sta cache_entries
    bcc +
    inc cache_entries_high
+

    ; We don't support more than 255 cache entries.
    ldy cache_entries
    lda cache_entries_high
    beq +
    ldy #255
+   sty cache_entries

; SFTODO: DELETE THIS LINE initialize_done
    rts

    ; This is copied over page_in_swr_bank_a, so must not contain absolute
    ; addresses within this executable.
page_in_swr_bank_a_electron
    ldx #12
    stx romsel_copy
    stx electron_romsel
    sta romsel_copy
    sta electron_romsel
    rts
page_in_swr_bank_a_electron_end
+assert page_in_swr_bank_a_electron_size = page_in_swr_bank_a_electron_end - page_in_swr_bank_a_electron

; SFTODO: Copy and paste from acorn-init-preload.asm - possibly better just to duplicate it than faff sharing it, but have a think
screen_start_page_by_mode
    !byte $30 ; mode 0
    !byte $30 ; mode 1
    !byte $30 ; mode 2
    !byte $40 ; mode 3
    !byte $58 ; mode 4
    !byte $58 ; mode 5
    !byte $60 ; mode 6
    !byte $7c ; mode 7

relocate_target
    !byte 0
    ; SFTODONOW: NOW WE KNOW THE SCREEN MODE HERE RATHER THAN DURING INIT OSBYTE, WE CAN DO MORE INITIALISATION IN DISCARDABLE INIT CODE
relocate_setup
    ; Set Y (relocate_target) to OSHWM to start with.
    lda #osbyte_read_oshwm
    jsr osbyte

    ; Calculate shadow_cache_entries, the number of 512-byte blocks we can hold
    ; in spare shadow RAM. If we have no shadow driver or are running in mode 0
    ; this will be 0. If we need a bounce buffer to copy data between main RAM
    ; and shadow RAM, bump Y (relocate_target) by one to allocate a page of
    ; bounce buffer at OSHWM. If OSHWM is so high the bounce buffer would be at
    ; or above $3000, we set shadow_cache_entries to 0 to disable use of spare
    ; shadow RAM.
    ; SFTODO: ONCE SHADOW DRIVER ALLOWS PAGE IN/OUT ON SUITABLE HW, WE WON'T NEED BOUNCE BUFFER FOR THAT CASE
    cpy #>(shadow_start - $100)
    bcs spare_shadow_init_done ; branch if no room for bounce buffer
    lda shadow_state
    cmp #shadow_state_first_driver
    bcc spare_shadow_init_done ; branch if no shadow driver
    ldx cache_screen_mode
    sec
    lda screen_start_page_by_mode,x
    sbc #>shadow_start
    lsr ; convert pages to 512-byte blocks
    sta shadow_cache_entries
    beq spare_shadow_init_done ; branch if no spare shadow RAM (mode 0)
    sty shadow_bounce_buffer
    iny
spare_shadow_init_done

    sty relocate_target

    ; This must be the last thing in the executable.
    !source "acorn-relocate.asm"

; SFTODO: Is this code small enough that it could run in what's left of pages &9/A after the list of sideways RAM banks? That would make better use of memory as we'd have an extra two pages above OSHWM for cached data. Don't forget though that the INSV handler currently lives in page &A.

; SFTODONOW: If possible, it would be good if we had same host cache size on a non-shadow machine after adding all this private RAM/shadow support as we did beforehand, i.e. that non-shadow machine is not losing out (slightly)

; SFTODO: Just thinking out loud...
; Although it's way nicer if we can page in shadow RAM and access it directly - it's simpler *and* faster, and I definitely want to support this - in order to handle shadow RAM in general we need to copy data between shadow RAM and a bounce buffer in low main RAM. We could in theory do this one byte at a time, but that would be slow. The real choice is whether to do 256 bytes or 512 bytes at a time. 256 bytes frees up a page (and depending on how PAGE is aligned and the size of this code, that might mean we have an extra 512 bytes of cache, i.e. one more block) compared to 512, but it means we have to switch to doing a copy in the middle of our 512 byte tube transfer. 256 bytes is also a natural fit for the existing shadow driver, although that's not a huge deal as I will probably have separate "Ozmoo itself" vs "host cache" shadow drivers, given I want to expose page in/out for the host cache (which isn't really useful in Ozmoo itself). I suppose since we *are* doing 2x256 byte tube transfers just because of how the protocol works, it may not be excessively complex to do two 256 byte copies in/out of shadow RAM. Doing 2x256 shadow RAM copies instead of 1x512 bytes does slightly increase the overhead, because we're doing the OSBYTE to page in/out twice, but I suppose once I only copy if the page in/out isn't possible (MRB, B+) that goes away, and for those non-pageable systems there *is* no real setup/finish overhead, the overhead is all in the per byte calls to copy the data.
;
; So I guess I'm thinking it's worth trying to use a 256 byte bounce buffer to start with. Of course ultimately if the shadow driver allows page in/out, we don't want to allocate a bounce buffer at all.
