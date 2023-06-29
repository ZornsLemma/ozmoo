; Cache to use memory on the host in Acorn second processor builds.
;
; When Ozmoo is running on a second processor, we may still have a lot of main
; RAM, spare shadow RAM and sideways RAM unused in the host. This code
; implements a block cache using that RAM which Ozmoo's virtual memory subsystem
; can offer blocks to and retrieve blocks from before it resorts to reading
; blocks from disc.
;
; When optimising this code, note that the two loops to copy 512-byte blocks
; across the tube occupy the bulk of the CPU time. Iterating over up to 255
; cache entries to find old blocks/the requested block obviously doesn't take
; negligible time, but it's still fairly insignificant by comparison. It's good
; to be generally efficient but there's no need to push for every single cycle
; at the cost of legibility. Note also that it's important this code is compact
; because every page it's occupy is a page that isn't available for caching data
; in.

!source "acorn-shared-constants.asm"

userv = $200
osword_a = $ef
osword_x = $f0
osword_y = $f1
bbc_tube_data = $fee5
electron_tube_data = $fce5
tube_entry = $406
tube_reason_claim = $c0
tube_reason_release = $80
tube_reason_256_byte_to_io = 6
tube_reason_256_byte_from_io = 7
osbyte_read_oshwm = $83
opcode_jmp = $4c

zp_temp = $70 ; 3 bytes

; osword_[axy] will be corrupted by the OS when we make OSBYTE or OSWORD calls, so we have
; to copy the pointer to our OSWORD block in here.
osword_block_ptr = $73 ; 2 bytes

; The following don't need to be in zero page but we're not short of it and using zero page
; for these variables helps to shorten the code, maximising main RAM for caching.
!zone {
    tube_transfer_block = $75 ; 4 bytes
    shadow_ptr_high = $79 ; 1 byte
    free_block_index = $7a ; 1 byte
    current_tick = $7b ; 1 byte
    cache_entries = $7c ; 1 byte

    ; The relocation code uses $70-$7a inclusive so we must avoid using those
    ; addresses for the following variables, which are set before we execute by
    ; the loader and are used after we've relocated down.
    shadow_bounce_buffer_page = $7d ; 1 byte
    ; It feels a bit extravagant allocating two bytes of zero page for
    ; old_userv, but we need this to live somewhere it won't be overwritten if
    ; we're re-executed and we're not short of zero page really.
    old_userv = $7e ; 2 bytes
}

; SFTODO: Arbitrarily chosen magic number for tube claims. I don't know if there is
; some standard number allocated to the foreground application.
our_tube_reason_claim_id = $25 ; a six bit value
+assert (our_tube_reason_claim_id & %11000000) = 0

; When current_tick would wrap round to 0, we subtract timestamp_adjustment from
; all the existing timestamps (setting any which would become negative to 0). A
; small value of timestamp_adjustment preserves more useful range in the
; timestamps, but at the cost of having to do the adjustment more often.
timestamp_adjustment = $40 ; SFTODO: $80?

page_in_swr_bank_a_electron_size = 13

program_start
    jmp relocate_setup

    ; SFTODO: ON A RESTART OZMOO BINARY WILL PROBABLY INVOKE THIS OSBYTE, SHOULD WE MAKE IT PRESERVE ITS CACHE WHEN CALLED SECOND, THIRD, ETC TIME? THIS MIGHT HELP SPEED UP A RESTART AS THIS CACHE MAY BE USABLE FOR LOADING. OTOH THIS MAY BE AN EXTRA SOURCE OF OBSCURE BUGS ON A RESTART. - Thinking about this now, it still makes me fear obscure bugs, but *maybe* if on a second+ call, we set all the timestamps to be in ascending order of block ID (possibly not trivial, and we don't want to bloat this code - but we could right shift the block number - taking game size into account to decide how much to scale to not waste range - to squash it into the timestamp), that *might* allow the restart to benefit from any existing blocks in cache without blocks we could have used getting evicted unnecessarily - but I am far from confident about this. I am also not sure it's a big enough win to be worth it. We'd still have to load the "really popular" blocks from disc to populate the Ozmoo cache in the copro, because all the index data etc would be lost when it re-executes - that requires no extra work, but it means there is still going to be some disc access on restart of course. Testing this would be hard as you'd kind of need a "realistic, 'scrambled'" cache rather than the immediately post-restart cache. I suppose *maybe* doing a restart at the end of the benchmark would not be a terrible test.
    ; SFTODO: Thinking about that some more, I think there might still be a risk of the timestamps on the old stuff being newer than the hints and we'd lose the benefits of any preload optimisation. What would probably be safe easy and offer *some* benefit would be to preserve the cache but force all the timestamps to zero and set current_tick to 1 as we normally do on init. If we get a hit great, if we don't no great loss. Incidentally, don't forget we are not *re-executing* the cache executable, so there's no worries about a restart trampling over the cache data as we load high and relocate - we rerun the Ozmoo executable but not the cache executable.
; OSBYTE $88 - host cache initialisation
;
; On entry:
;   N/A
;
; On exit:
;   X contains the number of 512-byte blocks the cache holds
our_osbyte
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
    dex
    rts

our_userv
    cmp #$E0
    beq our_osword
    cmp #0
    beq our_osbyte ; *CODE/OSBYTE $88
    jmp (old_userv)

; As the code evolves, we may want/need to waste some space here to change the
; alignment of the following code so the time-critical loops don't have unwanted
; page crossings.
    !fill 0

; OSWORD &E0 - host cache access
;
; On entry:
;   YX?0:     12 (send block length)
;   YX?1:     12 (receive block length)
;   YX!2:     address of 512-byte data block
;   YX?6..7:  ID of 512-byte block currently held at YX!2 (offered to cache)
;             (ID $FFFF means no block is being offered to the cache.)
;             SFTODO: We could use 0 for this special case, Ozmoo will never
;             pass 0 as it will be in dynamic memory.
;   YX?8:     Timestamp hint for offered block, or $FF for no hint.
;   YX?9..10: ID of 512-byte block wanted at YX!2 (requested from cache)
;
; On exit:
;   YX?11:    0 ("no error") means data at YX!2 replaced with block YX?9..10
;             non-0 means block YX?9..10 is not in cache and data at YX!2 not
;             modified
;
; In practice a block ID of $ABCD will correspond to the 512-byte block of
; Z-machine address space starting at $ABCD00 << 1, but this code doesn't
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

    ; Set osword_block_ptr to point to our block; osword_[xy] will be corrupted
    ; by any OSBYTE/OSWORD calls we make.
    lda osword_x
    sta osword_block_ptr
    lda osword_y
    sta osword_block_ptr + 1

    lda romsel_copy
    pha

patch_if_no_cache_entries
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
    cmp #osword_cache_no_timestamp_hint
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
    inc current_tick ; can't wrap as (A == hint) != (osword_cache_no_timestamp_hint == $ff)
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
    ; (rather dated comment, may no longer be true)
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
    jsr set_tube_transfer_block_to_osword_data_address
    jsr claim_tube
    lda #2
    sta count
copy_offered_block_loop
    lda #tube_reason_256_byte_to_io
    ldx #<tube_transfer_block
    ldy #>tube_transfer_block
    jsr tube_entry
    ; We now need a 19 microsecond/38 cycle delay.
    ldx #7     ; 2 cycles
wait_x
    dex        ; 2*7=14 cycles
    bne wait_x ; 3*6+2=20 cycles
    +assert_no_page_crossing wait_x
    ldy #0     ; 2 cycles
tube_read_loop
lda_abs_tube_data
    ; We must not read from bbc_tube_data more often than once every 10
    ; microseconds/20 cycles.
    lda bbc_tube_data     ; 4 cycles
    sta (our_cache_ptr),y ; 6 cycles
!ifdef SIMPLE_DUMMY {
    lda (our_cache_ptr),y ; 5 cycles (dummy, cache is page-aligned so not 6)
} else {
    lsr our_cache_ptr     ; 5 cycles (dummy, our_cache_ptr is always 0 so harmless)
}
    iny                   ; 2 cycles
    bne tube_read_loop    ; 3 cycles if we branch
    +assert_no_page_crossing tube_read_loop
    ldy shadow_ptr_high
    beq not_shadow_bounce_in_tube_read_loop
    lda shadow_bounce_buffer_page
    ; SFTODO: Moving the following two instructions into a subroutine may save a
    ; byte.
    jsr shadow_ram_copy
    inc shadow_ptr_high
    dec our_cache_ptr + 1 ; counteract do_loop_tail_common incrementing this
not_shadow_bounce_in_tube_read_loop
    jsr do_loop_tail_common
    bne copy_offered_block_loop
    jsr undo_shadow_paging_if_necessary
    jsr release_tube
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
requested_block_not_found
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

    ; Copy the 512-byte block in our cache pointed to by our_cache_ptr back to
    ; the caller.
    jsr set_tube_transfer_block_to_osword_data_address
    jsr claim_tube
    lda #2
    sta count
copy_requested_block_loop
    lda shadow_ptr_high
    beq not_shadow_bounce_in_tube_write_loop
    ldy shadow_bounce_buffer_page
    sty our_cache_ptr + 1 ; counteract do_loop_tail_common incrementing this
    jsr shadow_ram_copy
    inc shadow_ptr_high
not_shadow_bounce_in_tube_write_loop
    lda #tube_reason_256_byte_from_io
    ldx #<tube_transfer_block
    ldy #>tube_transfer_block
    jsr tube_entry
    ; We don't need an initial delay with this reason code.
    ldy #0
tube_write_loop
    ; We must not write to bbc_tube_data more often than once every 10
    ; microseconds/20 cycles.
    lda (our_cache_ptr),y    ; 5 cycles (cache is page-aligned so not 6)
!ifdef SIMPLE_DUMMY {
    sta (our_cache_ptr),y    ; 6 cycles (dummy)
} else {
    ; SFTODO: Annoyingly this is one byte longer than the simple dummy. I can't
    ; find a two byte six cycle instruction other than "sta (our_cache_ptr),y"
    ; which we can use safely. If we knew X was 0 (which we don't, and it would
    ; take two bytes to load it) we could do things like "lda (old_userv,x)"
    ; (albeit before the real lda, of course, not right here) and we'd just
    ; harmlessly read the first byte of the old USERV handler, but with
    ; arbitrary X we could end up doing random reads from an I/O location.
    lsr+2 our_cache_ptr      ; 6 cycles (dummy, our_cache_ptr is always 0 so harmless)
}
sta_abs_tube_data
    sta bbc_tube_data        ; 4 cycles
    iny                      ; 2 cycles
    bne tube_write_loop      ; 3 cycles if we branch
    +assert_no_page_crossing tube_write_loop
    jsr do_loop_tail_common
    bne copy_requested_block_loop
    ; SFTODO: If this code needs squashing in the future, the following two jsrs
    ; are duplicated in two places - we could have an
    ; undo_shadow_paging_if_necessary_and_release_tube subroutine instead.
    jsr undo_shadow_paging_if_necessary
    jsr release_tube

our_osword_done
    ; We don't need to preserve A, X or Y.
    ; We need to leave the same sideways ROM paged in as when we were entered.
    pla
    ; fall through to page_in_swr_bank_a
page_in_swr_bank_a
    sta romsel_copy
    sta bbc_romsel
    rts
    ; Leave room for the larger Electron version of page_in_swr_bank_a to be copied
    ; over the BBC version.
    !fill page_in_swr_bank_a_electron_size - (* - page_in_swr_bank_a)
page_in_swr_bank_a_end

claim_tube
    lda #tube_reason_claim + our_tube_reason_claim_id
    jsr tube_entry
    bcc claim_tube
    rts

release_tube
    lda #tube_reason_release + our_tube_reason_claim_id
    jmp tube_entry

; Copy the four byte address of the offered/requested block in our OSWORD block to
; tube_transfer_block. This allows us to modify it without corrupting the OSWORD
; block which is returned to our caller.
set_tube_transfer_block_to_osword_data_address
    ldy #our_osword_data_offset+3
copy_osword_data_loop
    lda (osword_block_ptr),y
    sta tube_transfer_block-our_osword_data_offset,y
    dey
    +assert our_osword_data_offset > 0
    cpy #our_osword_data_offset
    bcs copy_osword_data_loop
    rts

do_loop_tail_common
    ; Bump the source and destination addresses by one page.
    inc our_cache_ptr + 1
    inc tube_transfer_block + 1
    dec count ; must be last
    rts

; Set our_cache_ptr to point to the block with index Y, paging in sideways or
; shadow RAM if appropriate.
set_our_cache_ptr_to_index_y
    lda #0
    sta our_cache_ptr ; SFTODO: always zero, so we could move this sta into discardable init
    sta shadow_ptr_high
    tya
    sec
    sbc main_ram_cache_entries
    bcs index_not_in_main_ram_cache
    tya
    asl
    clc
    adc #>main_ram_cache_start
    sta our_cache_ptr + 1
    rts
index_not_in_main_ram_cache
    ; A is a 0-based index with 0 indicating the first block of sideways RAM
    ; cache (if any, of course); A>=swr_cache_entries indicates the block is in
    ; shadow RAM.
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
    ; skip over the 1K of IBOS workspace at $8000 by changing swr_base. (Other
    ; machines pay a tiny price for this, as swr_base would be constant
    ; otherwise.)
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
anrts
    rts
index_in_shadow_cache
    ; Carry is already set
    sbc swr_cache_entries
    asl
    ; Carry is already clear
    adc #>shadow_start
    ldy shadow_bounce_buffer_page
    beq use_shadow_paging
    sta shadow_ptr_high
    sty our_cache_ptr + 1
    rts
use_shadow_paging
    sty zero_if_need_to_undo_shadow_paging
    sta our_cache_ptr + 1
    lda #1
jmp_shadow_paging_control1
    jmp $ffff ; patched

undo_shadow_paging_if_necessary
    ; If we paged in shadow RAM, we need to page main RAM back in. Unlike
    ; sideways RAM, where we page in on demand and always page the original bank
    ; back in before we return to the caller whether we actually changed it or
    ; not, shadow RAM paging is more expensive so we try to be smarter about it.
zero_if_need_to_undo_shadow_paging = *+1 ; SFTODO: could move into zero page
    lda #1 ; patched, although important it starts at 1
    bne anrts
    inc zero_if_need_to_undo_shadow_paging ; set back to 1
jmp_shadow_paging_control2
    jmp $ffff ; patched

code_end

max_cache_entries = 255
free_block_index_none = max_cache_entries ; real values are 0-(max_cache_entries - 1)

; SFTODO: If we need to squash the code further, these could move into zero page
; or into the spare bytes in the aligned arrays below; at the moment we don't
; need to worry about this as we fit comfortably in 512 bytes.
main_ram_cache_entries
    !byte 0
swr_cache_entries
    !byte 0
shadow_cache_entries
    !byte 0

; We're now going to allocate some page-aligned arrays. We don't want to bloat
; the executable itself with the empty data, so instead of using
; !align/!byte/!fill, we calculate the values as offsets from *.
;
; This data overlaps the following code, which is initialisation code and can be
; safely overwritten after it has been executed.
;
; To avoid page crossing we want to allocate each of the 255-byte arrays here
; inside its own page. We really want them to start one byte into the page so
; foo-1,y addressing doesn't cross a page boundary (particularly important for
; cache_id_low as it's used in the hot find_cache_entry_by_id loop).
;
; At the moment, the first byte of each page is just wasted, but we could
; potentially store single byte variables there.

+assert max_cache_entries = 255

!if (* & $ff) = 0 {
    aligned_data_start = *
} else {
    aligned_data_start = (* & $ff00) + $100
}
+assert (aligned_data_start & $ff) = 0
cache_id_low         = aligned_data_start + $001
cache_id_high        = aligned_data_start + $101
cache_timestamp      = aligned_data_start + $201
main_ram_cache_start = aligned_data_start + $300

; The relocation process can't relocate us upwards in memory, so by asserting
; that all builds (including the one at the high relocation address) fit below a
; mode 0 screen, we know our code won't be corrupted; the worst case is we have
; no free memory for cache. (It's tempting to make allowance for the possible
; bounce buffer here, but this is the wrong place for it. This assertion checks
; the high relocation address - the highest we will run at - is low enough for
; us to fit below the screen. The bounce buffer is allocated at OSHWM below our
; code, so we want as high a high relocation address as possible to maximise the
; chances of being able to allocate a page for a bounce buffer at OSHWM.)
+assert main_ram_cache_start <= $3000

; Discardable initialisation code.

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

initialize
    ; Install ourselves on USERV and save the old value at old_userv iff USERV
    ; doesn't already point to us; this avoids ending up with old_userv pointing
    ; to our own handler in a circle if we're re-executed. In practice nothing
    ; except our own OSBYTE/OSWORD are likely to go via USERV so we'd never get
    ; stuck in a circle anyway, but we might as well do it properly.
    lda #<our_userv
    ldx #>our_userv
    cmp userv
    bne userv_not_already_claimed
    cpx userv + 1
    beq userv_already_claimed
userv_not_already_claimed
    ; Install ourselves on USERV, saving any previous claimant so we can forward
    ; calls we're not handling.
    ldy userv
    sty old_userv
    ldy userv + 1
    sty old_userv + 1
    sta userv
    stx userv + 1
userv_already_claimed

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
    +copy_data_checked page_in_swr_bank_a_electron, page_in_swr_bank_a_electron_end, page_in_swr_bank_a, page_in_swr_bank_a_end
not_electron

    ; Calculate main_ram_cache_entries. We have RAM available between main_ram_cache_start
    ; and the screen.
    lda screen_mode_host
    ora #shadow_mode_bit
    tax
    lda #osbyte_read_screen_address_for_mode
    jsr osbyte
    ; The following calculation will correctly discard any odd pages, so we
    ; don't need to be double-page aligned in order to make things work nicely.
    tya
    sec
    sbc #>main_ram_cache_start
    lsr
    sta main_ram_cache_entries

    ; Calculate swr_cache_entries. Each 16K sideways RAM bank holds 32*512-byte
    ; blocks; the B+ and Integra-B private RAM are smaller and need special
    ; handling. It's easier to loop over the banks adding them up than to
    ; multiply the number of banks by 32 then adjust for private RAM, because it
    ; avoids needing 16-bit arithmetic; we can simply saturate at 255 if we
    ; overflow.
    lda #0
    sta swr_cache_entries
    ldy ram_bank_count
    beq no_swr
swr_loop
    ldx #32 ; size of normal 16K bank
    lda ram_bank_list-1,y
    cmp #64
    bcc bank_size_in_x ; this is a normal 16K bank
    ldx #(b_plus_private_ram_size / 512)
    cmp #128
    bcs bank_size_in_x ; branch if B+ private RAM
    ; This is the Integra-B private 12K.
    ldx #(integra_b_private_ram_size / 512)
    ; Set up RAMSEL so we can access the private 12K by setting b6 (PRVEN) of
    ; ROMSEL, much as we can access it by setting b7 on the B+.
    ; SFTODO: Copy and paste from core Ozmoo code - probably OK, but think.
    lda $37f
    ora #%00110000 ; set PRVS4 and PRVS8 to make all 12K visible
    sta $37f
    sta $fe34
bank_size_in_x
    txa
    clc
    adc swr_cache_entries
    bcc no_carry
    lda #255
no_carry
    sta swr_cache_entries
    dey
    bne swr_loop
no_swr

    ; Set cache_entries to the total number of cache entries, saturating at 255.
    clc
    lda main_ram_cache_entries
    adc swr_cache_entries
    bcs carry
    adc shadow_cache_entries
    bcc no_carry2
carry
    lda #255
no_carry2
    sta cache_entries

    ; If cache_entries < 2, we don't really have any usable cache. To avoid misbehaving,
    ; we set cache_entries to 1 (so our_osbyte will return 0) and we patch our_osword so
    ; it just returns immediately indicating block not found.
    cmp #2
    bcs cache_has_entries
    lda #1
    sta cache_entries
    lda #opcode_jmp
    sta patch_if_no_cache_entries
    lda #<requested_block_not_found
    sta patch_if_no_cache_entries + 1
    lda #>requested_block_not_found
    sta patch_if_no_cache_entries + 2
cache_has_entries

    ; We don't need to adjust swr_cache_entries or shadow_cache_entries to make
    ; the values actually sum to 255 if the previous addition saturated. Note
    ; that we do have at least as much of each type of cache as those values
    ; indicate, and set_our_cache_ptr_to_index_y will just not try to use more
    ; than 255 cache entries. SFTODONOW: Pretty sure this is true, but think about
    ; it fresh and do some testing.
    rts

; On an Electron with a Master RAM Board in shadow mode, the shadow RAM can't be
; turned off under software control and osbyte_read_screen_address_for_mode will
; always return $8000 even for modes without shadow_mode_bit set. This makes
; sense, but it's not much use to us when we're trying to determine how much
; shadow RAM is actually used in a mode. We use our own table of start addresses
; instead; we might as well do this on all platforms for consistency.
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

; Get ready to relocate down from the high address we load at to ~OSHWM.
relocate_setup
    ; Set Y (relocate_target) to OSHWM to start with.
    lda #osbyte_read_oshwm
    jsr osbyte

    ; Calculate shadow_cache_entries, the number of blocks we can store in spare
    ; shadow RAM (if any).
    ;
    ; We might need a bounce buffer to use spare shadow RAM - that is, a page of
    ; memory below the start of screen RAM at $3000 which we can use to copy
    ; data to/from spare shadow RAM so we can work with it. If we do, we
    ; allocate it at OSHWM and relocate ourselves a page higher than usual -
    ; this is convenient because it ensures the bounce buffer is as low as
    ; possible (without using memory below OSHWM).
    ;
    ; Y contains the relocation target, currently OSHWM, so if we want a bounce
    ; buffer we use page Y for it and bump Y to reserve the page.
    ;
    ; We need a bounce buffer if all of the following are true:
    ; - we have some spare shadow RAM (shadow mode 0 uses all shadow RAM itself)
    ; - we have a shadow driver to allow us to use spare shadow RAM
    ; - the shadow driver isn't capable of paging shadow RAM in and out (if it
    ;   is, we can just copy to/from shadow RAM directly)
    ;
    ; There's an additional condition which is always true in practice: we need
    ; OSHWM<=$2F00 so any bounce buffer we allocate is below screen RAM. Since
    ; this is discardable init code we do check for this and if it's not true we
    ; disable using spare shadow RAM, which obviously means we don't need a
    ; buffer.
    lda #0
    sta shadow_cache_entries
    sta shadow_bounce_buffer_page
    lda shadow_state
    cmp #shadow_state_first_driver
    bcc spare_shadow_init_done ; branch if no shadow driver
    ldx screen_mode_host
    sec
    lda screen_start_page_by_mode,x
    sbc #>shadow_start
    lsr ; convert pages to 512-byte blocks
    sta shadow_cache_entries
    beq spare_shadow_init_done ; branch if no spare shadow RAM (mode 0)
    lda shadow_paging_control_ptr+1
    bne init_shadow_paging ; branch if we can page shadow RAM in directly
    cpy #>shadow_start
    bcs no_room_for_bounce_buffer
    sty shadow_bounce_buffer_page
    iny
    jmp spare_shadow_init_done
no_room_for_bounce_buffer
    lda #0
    sta shadow_cache_entries
    jmp spare_shadow_init_done
init_shadow_paging
    ; We can page shadow RAM in/out directly, so patch up the places where we
    ; want to do that with the address of the relevant routine.
    sta jmp_shadow_paging_control1+2
    sta jmp_shadow_paging_control2+2
    lda shadow_paging_control_ptr
    sta jmp_shadow_paging_control1+1
    sta jmp_shadow_paging_control2+1
spare_shadow_init_done

    sty relocate_target

    ; This must be the last thing in the executable.
    !source "acorn-relocate.asm"

; A note on the SIMPLE_DUMMY !ifdefs: SIMPLE_DUMMY is not normally defined, but
; I wanted to leave this code around for future reference. It works perfectly
; and is reasonably obviously correct. However, by forcing both reads and writes
; of the same location in each tube copy loop, it means b-em's memory view just
; shows a yellow bar for the memory accessed by each loop. When SIMPLE_DUMMY is
; not defined we use less obvious/"riskier" code which is also (sadly) one byte
; longer to burn the extra cycles without touching the location we just did a
; real read/write from/to. This means that you can see red for write and green
; for read in b-em's memory view as the cache is accessed. This isn't really a
; big deal, but it is kind of cool. :-)

; SFTODO: It's a bit niche, but in theory on a B+ where we have the last 512
; bytes of private RAM available for the shadow driver, we *could* potentially
; put a copy of the tube read/write loops in there and use those copies when we
; want to read/write shadow RAM without having to copy a byte at a time between
; shadow RAM and the bounce buffer. (We'd need the current low memory copies of
; the tube read/write loops as well, because we'd have to select the appropriate
; one depending on whether the cache block is in main RAM or shadow RAM.)
