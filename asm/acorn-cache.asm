; SFTODO: WITH ACME I CAN'T USE "BYTE GENERATING" THINGS TO ALLOCATE UNINITIALISED DATA AS THEY GET INCLUDED IN THE BINARY

userv = $200
osword_a = $ef
osword_x = $f0
osword_y = $f1
osword_block_ptr = $f0
tube_data = $fee5 ; SFTODO: possibly different on Electron
tube_entry = $406
tube_reason_claim = $c0
tube_reason_release = $80
tube_reason_256_byte_to_io = 6
tube_reason_256_byte_from_io = 7
osbyte_read_oshwm = $83
osbyte_read_screen_address_for_mode = $85
osbyte = $fff4

zp_temp = $f5 ; 3 bytes

; SFTODO: Arbitrarily chosen magic number for tube claims. I don't know if there is
; some standard number allocated to the foreground application.
our_tube_reason_claim_id = $25 ; a six bit value

; When current_tick would wrap round to 0, we subtract timestamp_adjustment from
; all the existing timestamps (setting any which would become negative to 0). A
; small value of timestamp_adjustment preserves more useful range in the
; timestamps, but at the cost of having to do the adjustment more often.
timestamp_adjustment = $40 ; SFTODO: $80?

program_start
    jmp relocate_setup

; SFTODO: It might be worth putting this initialisation code at the end where
; it can be overwritten, but let's not worry about that for now. (If we do overwrite
; it *and* we want to support being re-run, we'd need to ensure this reset all the
; cache state appropriately.)
initialize
    ; We claim iff we haven't already claimed USERV; this avoids crashes if
    ; we're executed twice, although this isn't expected to happen.
    ; SFTODO: AND IF ANY "STATE" IS STORED IN THE AREA COVERED BY THE EXECUTABLE
    ; THEN RE-RUNNING THE EXECUTABLE COULD CAUSE US TO CRASH IN OTHER WAYS
    lda #<our_userv
    ldx #>our_userv
    cmp userv
    bne initialize_needed
    cpx userv + 1
    beq initialize_done
initialize_needed
    ; Install ourselves on USERV, saving any previous claimant so we can forward
    ; calls we're not handling.
    ldy userv
    sty old_userv
    ldy userv + 1
    sty old_userv + 1
    sta userv
    stx userv + 1
initialize_done
    rts

our_userv
    cmp #$E0
    beq our_osword
    cmp #0
    beq our_osbyte ; *CODE/OSBYTE $88
    ; SFTODO: We might be able to use that OSBYTE to trigger initialisation of the cache size as well - at that point we'll still be in mode 7 on the loader screen but Ozmoo knows what mode it will select, so it could pass that over in the OSBYTE and receive in return the available cache size
    jmp (old_userv)

    ; SFTODO: EXPERIMENTAL
    ; SFTODO: ON A RESTART OZMOO BINARY WILL PROBABLY INVOKE THIS OSBYTE, SHOULD WE MAKE IT PRESERVE ITS CACHE WHEN CALLED WITH "SAME" X OR ON A SUBSEQUENT CALL OR SOMETHING? THIS MIGHT HELP SPEED UP A RESTART AS THIS CACHE MAY BE USABLE FOR LOADING. OTOH THIS MAY BE AN EXTRA SOURCE OF OBSCURE BUGS ON A RESTART.
; OSBYTE $88 - host cache initialisation
;
; On entry:
;   X is the screen mode the cache needs to leave space for
;
; On exit:
;   X contains the number of 512-byte blocks the cache holds
our_osbyte
    ; SFTODO: Need to use SWR as well, of course
    lda #osbyte_read_screen_address_for_mode
    jsr osbyte
    ; The following calculation will correctly discard any odd pages, so we
    ; don't need to be double-page aligned in order to make things work nicely.
    tya
    sec
    sbc #>low_cache_start
    lsr
    sta cache_entries

    ; The cache size doesn't change at runtime, it's always full with
    ; cache_entries entries, but it starts full of very old entries with the
    ; invalid block ID. This means those entries will be evicted in preference
    ; to any real entries and will never be returned in response to a cache
    ; lookup.
    ; SFTODO: We need to determine free memory (and SWR later), for now this will do.
    ldy cache_entries
our_osbyte_initialize_loop
    lda #0
    sta cache_timestamp - 1,y
    lda #$ff
    sta cache_id_low - 1,y
    sta cache_id_high - 1,y
    dey
    bne our_osbyte_initialize_loop

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
    dex
    rts

; OSWORD &E0 - host cache access
;
; On entry:
;   YX?0:     12 (send block length)
;   YX?1:     12 (receive block length)
;   YX!2:     address of 512-byte data block (high word must be 0)
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
; Z-machine address space starting at $ABCD00, but this code doesn't actually
; care.
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
    ; SFTODO: Potential to do iny instead of ldy #, but lack of assert in acme makes me a bit edgy about it
    ldy #our_osword_block_offered_timestamp_hint_offset
    lda (osword_block_ptr),y
    cmp #$ff
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
    ; SFTODO: FWIW in the benchmark this tick adjust case only occurs once
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
; SFTODO: ENSURE NOT PAGE CROSSING
wait_x
    dex        ; 2*7=14 cycles
    bne wait_x ; 3*6+2=20 cycles
    ldy #0     ; 2 cycles
    ; SFTODO: Kind of stating the obvious, but the tube loops obviously burn loads of CPU time, so even if it feels inefficient to be iterating over 255 cache entries once or twice per call to check timestamps or whatever, remember we have to do 512 iterations of this loop and/or the other similar tube loop, so that other code isn't negligible but is diluted. Plus of course we are saving a trip to disc.
    ; SFTODO: Ensure no page crossing in following loop
tube_read_loop
    lda tube_data
    ; We now need a 10 microsecond/20 cycle delay.
    sta (our_cache_ptr),y ; 6 cycles
    lda (our_cache_ptr),y ; 5 cycles (dummy, cache is page-aligned so not 6)
    lda our_cache_ptr,x   ; 4 cycles (dummy)
    iny                   ; 2 cycles
    bne tube_read_loop    ; 3 cycles if we branch
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
    ; SFTODO: Make sure this loop has no page crossing
find_cache_entry_by_id_loop
    cmp cache_id_low - 1,y
    beq possible_match
not_match
    dey
    bne find_cache_entry_by_id_loop
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

    ; Set our_cache_ptr to point to the block's data.
    jsr set_our_cache_ptr_to_index_y

    ; Set the result to say we were able to provide the requested block.
    ldy #our_osword_result_offset
    lda #0
    sta (osword_block_ptr),y

    ; Copy the requested block back to the caller.
    jsr claim_tube
    lda #2
    sta count
copy_requested_block_loop
    jsr set_yx_to_tube_transfer_block
    lda #tube_reason_256_byte_from_io
    jsr tube_entry
    ; We don't need an initial delay with this reason code.
    ; SFTODO: Ensure no page crossing in following loop
    ldy #0
tube_write_loop
    lda (our_cache_ptr),y    ; 5 cycles (dummy, cache is page-aligned so not 6)
    lda (our_cache_ptr),y    ; 5 cycles (dummy)
    lda (our_cache_ptr),y    ; 5 cycles
    sta tube_data
    ; We now need a 10 microsecond/20 cycle delay; the instructions in the loop
    ; before the store to tube_data also form part of this delay.
    iny                      ; 2 cycles
    bne tube_write_loop      ; 3 cycles if we branch
    jsr do_loop_tail_common
    bne copy_requested_block_loop
    jsr release_tube
    jsr reset_osword_block_data_offset

our_osword_done
    ; We don't need to preserve A, X or Y.
    rts

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
; SFTODO: THIS WILL PRESUMABLY HANDLE PAGING IN SWR ONCE WE SUPPORT IT - BUT WE'LL NEED TO TAKE CARE TO PAGE WHATEVER WAS IN BEFOREHAND BACK IN BEFORE WE RETURN FROM OSWORD
set_our_cache_ptr_to_index_y
    lda #0
    sta our_cache_ptr
    tya
    asl
    clc
    adc #>low_cache_start
    sta our_cache_ptr + 1
    rts

code_end

max_cache_entries = 255
free_block_index_none = max_cache_entries ; real values are 0-(max_cache_entries - 1)

old_userv = code_end ; 2 bytes

; We use cache_id_low - 1,y addressing in the hot find_cache_entry_by_id_loop,
; so it's good to have that page-aligned to avoid incurring an extra cycle
; penalty. The other tables are less critical, although cache_timestamp has a
; similar if not so hot use. Since max_cache_entries is 255 we can easily get
; the desired alignment by slotting some single-byte variables into the spare
; bytes at the start of each page without wasting any memory.
!if (old_userv + 2) & $100 <> 0 {
    cache_entries = ((old_userv + 2) & !$ff) + $100 ; 1 byte
} else {
    cache_entries = old_userv + 2 ; 1 byte
}
cache_id_low = cache_entries + 1 ; max_cache_entries bytes
free_block_index = cache_id_low + max_cache_entries ; 1 byte
cache_id_high = free_block_index + 1 ; max_cache_entries bytes
current_tick = cache_id_high + max_cache_entries ; 1 byte
cache_timestamp = current_tick + 1 ; max_cache_entries bytes
low_cache_start = cache_timestamp + max_cache_entries
!if low_cache_start & $ff <> 0 {
    !error "low_cache_start must be page-aligned"
}

relocate_target
    !byte 0
relocate_setup
    lda #osbyte_read_oshwm
    jsr osbyte
    sty relocate_target
    ; This must be the last thing in the executable.
    !source "acorn-relocate.asm"
