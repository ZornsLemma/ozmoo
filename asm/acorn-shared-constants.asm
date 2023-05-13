; Acorn constants shared by multiple Acorn executables; constants only relevant
; to the main Ozmoo executable are in constants.asm

; This file also documents the interaction between the different "utilities"
; that can be present (always in the host) and how they fit round each other in
; memory.
;
; The loader runs the utilities in the order they're listed here. (The build system
; puts them on the generated disc image in this order as well, to try to keep
; the drive head moving forwards to minimise seeking.) SFTODO: CHECK IT DOES PUT THEM ON IN THIS ORDER, ESP IF I TWEAK THE ORDER
;
; The following comments draw a distinction between "execution" - what memory is
; used/corrupted when we *RUN the code - and "runtime" - what memory is
; used/corrupted after the *RUN returns as the code installed (if any) is used
; during gameplay.
;
; SFTODO: MAY WANT TO TWEAK THESE COMMENTS TO IDENTIFY "EXECUTION ONE-OFF USE (CORRUPTS)" VS "RUNTIME MEM USE"
; - SHADDRV (acorn-shadow-driver.asm)
;   This checks for shadow RAM and installs an appropriate shadow driver if we
;   have one.
;   Execution corrupts: &900-&AFF, &70-&8F
;   Execution sets: shadow_state, shadow_driver_start-shadow_driver_end (including shadow_ram_copy, shadow_paging_control_ptr)
;   Runtime memory: shadow_driver_start-shadow_driver_end
;
; - FINDSWR (acorn-findswr.asm)
;   This checks for sideways RAM.
;   Execution corrupts: &900-&AFF, &70-&8F
;   Sets: swr_type, ram_bank_count, ram_bank_list
;   Runtime memory: N/A
;   SFTODO: RAM BANK COUNT AND RAM BANK LIST ARE USED DURING GAMEPLAY, SWR TYPE IS ONLY FOR LOADER
;
; - INSV (acorn-insv.asm)
;   This allows a mix of *FX4,0 and *FX4,1 cursor key behaviour for command
;   history-enabled builds.
;   Execution corrupts: &900-&AFF
;   Runtime memory: &900-&931
;   Runtime inputs: nominal_cursor_key_status
;
; - FASTSCR (acorn-scroll.asm)
;   This allows hardware scrolling of the bottom part of the screen while
;   leaving a runtime-controllable number of lines untouched at the top. We need
;   all of the runtime memory for code so there's no room for discardable
;   initialisation code there; we therefore load and run just below the mode 6
;   screen and copy the runtime part of the code to the final location as part
;   of the initialisation.
;   Execution corrupts: &5C00-&5FFF
;   Execution inputs: screen_mode_host, shadow_state, shadow_paging_control_ptr
;   Execution outputs: fast_scroll_status_host
;   Runtime memory: &932-&AFF
;   Runtime inputs: fast_scroll_lines_to_move
;   Note: The loader copies fast_scroll_status_host into fast_scroll_status in SFTODO: IT ACTUALLY COPIES TO use_custom_hw_scroll BUT I SHOULD RENAME THAT
;   the language processor, so it's available to the Ozmoo executable wherever
;   it's running.
;
; - CACHE2P (acorn-cache.asm)
;   This is a cache to allow use of spare main, sideways and shadow RAM on the
;   host when running Ozmoo on a second processor. This always runs as user code
;   within the tube host environment, so unlike all of the above executables it
;   has zero page at &70-&8F available and main RAM from OSHWM to HIMEM
;   available. (The above executables have to be able to co-exist with the main
;   Ozmoo executable on single processor systems.)
;   Execution corrupts: OSHWM-HIMEM, &70-8F
;   Execution inputs: screen_mode_host, shadow_state
;   Runtime memory: OSHWM-HIMEM, &70-&8F
;
; The Ozmoo executable itself uses some addresses associated with the above utilities: SFTODO SEMI DUPLICATING THE ABOVE
; - SHADDRV: shadow_ram_copy
; - FINDSWR: ram_bank_count, ram_bank_list
; - FASTSCR: fast_scroll_lines_to_move
; - INSV: nominal_cursor_key_status
; -
;
;

; For communicating values between the utilities themselves and the loader, we
; allocate some bytes at the end of user zero page. All of these are available
; for re-use while the game is running.
!ifdef ACORN_SHADOW_VMEM {
    shadow_state = $8b
    private_ram_in_use = $8c
}
!ifdef ACORN_HW_SCROLL_CUSTOM {
    WANT_HOST_SCREEN_MODE = 1
    fast_scroll_status_host = $8d
}
!ifdef ACORN_TUBE_CACHE {
    WANT_HOST_SCREEN_MODE = 1
}
!ifdef WANT_HOST_SCREEN_MODE {
    screen_mode_host = $8e
}
!ifdef ACORN_SWR {
    swr_type = $8f
}

; SFTODO: MOVE THIS?
; SFTODO: Use this in more places?
!macro assert .b {
    !if .b = 0 {
        !error "assertion failed"
    }
}

; We conditionally assemble a lot of these constants to try to avoid them being
; used by accident.

brkv = $202

buffer_keyboard = 0

shadow_mode_bit = 128

vdu_goto_xy = 31

osargs = $ffda
osbyte = $fff4
osbyte_read_host = 0
osbyte_read_screen_address_for_mode = $85
osbyte_issue_service_request = $8f

romsel_copy = $f4
bbc_romsel = $fe30
electron_romsel = $fe05

!ifdef ACORN_TURBO_SUPPORTED {
    ; We run the turbo test executable during boot if we're on a second
    ; processor, which sets is_turbo accordingly. is_turbo needs to be untouched
    ; by BASIC while the loader is running. On a second processor there's
    ; additional zero page from $90 (inclusive) to $ee (exclusive) available for
    ; the current language, but we know BASIC doesn't use that. We therefore
    ; appropriate the last byte of that extra zero page for the is_turbo flag
    ; where a) it will be safe b) it doesn't require any tricky special handling
    ; to skip over in acorn-ozmoo-constants.asm when allocating other variables
    ; to zero page. SFTODO: REVIEW THIS COMMENT LATER
    is_turbo = $ed
}

; We pack some miscellaneous data into the space after the shadow driver.
xxx_shadow_driver_end = $900 - 16
xxx_max_ram_bank_count = 9 ; 255*0.5K for VM plus 16K for dynamic memory

!ifdef ACORN_SHADOW_VMEM {
    shadow_start = $3000

    ; We steal the envelope buffers in page 8 for the shadow RAM driver. (It's
    ; tempting to steal the printer buffer at $880 as well, but the Integra-B
    ; seems to use that for its own purposes so we'll keep out of its way.)
    shadow_driver_start = $8c0
    shadow_paging_control_ptr = shadow_driver_start
    shadow_ram_copy = shadow_driver_start + 2
    shadow_driver_end = xxx_shadow_driver_end ; exclusive

    ; SFTODONOW: Decide if I prefer to reorder these for some cosmetic reason before I embed them in the loader
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
}

!ifdef ACORN_SWR {
    b_plus_private_ram_size = 12 * 1024 - 512 ; -512 to leave space for shadow copy code
    integra_b_private_ram_size = 12 * 1024 - 1024 ; -1024 to leave space for IBOS workspace

    ; SFTODO: Might want to move these to free up $9 and $a for vmem cache or something.
    max_ram_bank_count = xxx_max_ram_bank_count
    ram_bank_count = xxx_shadow_driver_end ; 1 byte
    ram_bank_list = xxx_shadow_driver_end + 1 ; max_ram_bank_count bytes
}

!ifdef USE_HISTORY {
    nominal_cursor_key_status = xxx_shadow_driver_end + 1 + xxx_max_ram_bank_count ; 1 byte

    insv_start = 0x900
    insv_resident_end = 0x932
}

!ifdef ACORN_HW_SCROLL_CUSTOM {
    fast_scroll_lines_to_move = xxx_shadow_driver_end + 1 + xxx_max_ram_bank_count + 1 ; 1 byte

    fast_scroll_start = 0x932
    fast_scroll_end = 0xb00
}

+assert xxx_shadow_driver_end + 1 + xxx_max_ram_bank_count + 1 < $900

!ifdef ACORN_TUBE_CACHE {
    osword_cache_op = $e0 ; USERV OSWORD
    osword_cache_no_timestamp_hint = $ff
}



; SFTODO: MOVE THIS? I PUT IT HERE INTENDING TO USE IT IN CACHE2P BUT DIDN'T, SO IT COULD TECHNICALLY MOVE BACK TO ACORN.ASM WHERE IT USED TO BE.
; Macro used to generate an OS error. (This needs acme >= 0.97.)
!macro os_error error_number, error_message {
    brk
    !byte error_number
    !text error_message, 0
}

; SFTODO: MOVE?
; SFTODO: Use in more places?
!macro inc16 addr {
    inc addr
    bne +
    inc addr+1
+
}

!macro assert_no_page_crossing .target {
    !if (>*) <> (>.target) {
        !error "Unacceptable page crossing"
    }
}

; SFTODO: DOCUMENT - INCL FROM_END BEING *EXCLUSIVE*
; SFTODO: USEFUL? USE IN MORE PLACES? GET RID? MOVE?
; SFTODO: EXPERIMENTAL
!macro copy_data from_start, from_end, to {
.size = from_end - from_start
    ldy #.size
-   lda from_start-1,y
    sta to-1,y
    dey
    bne -
}

; As copy_data, but additionally given the (exclusive) end of the destination
; and checks that the copy won't write past that point.
!macro copy_data_checked from_start, from_end, to_start, to_end {
    +assert (from_end - from_start) <= (to_end - to_start)
    +copy_data from_start, from_end, to_start
}
