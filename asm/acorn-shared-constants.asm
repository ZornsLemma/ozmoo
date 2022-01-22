; Acorn constants shared by multiple Acorn executables; constants only relevant
; to the main Ozmoo executable are in constants.asm

osargs = $ffda
osbyte = $fff4
osbyte_read_host = 0
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

!ifdef ACORN_SHADOW_VMEM {
    ; We steal the envelope buffers in page 8 for the shadow RAM driver. (It's
    ; tempting to steal the printer buffer at $880 as well, but the Integra-B
    ; seems to use that for its own purposes so we'll keep out of its way.)
    ; SFTODO: Delete following if not used; the idea is these will be 0 or
    ; point to routines to page shadow RAM in/out of main memory if possible,
    ; and CACHE2P will use them if available.
    ; page_in_shadow_ram_indirect = $8c0
    ; page_out_shadow_ram_indirect = $8c2
    shadow_ram_copy = $8c4
    ; SFTODO: We should check the code doesn't spill past
    ; shadow_ram_copy_max_end; this isn't really convenient when we're
    ; assembling it in the BASIC loader, but this may well change later.
    shadow_ram_copy_max_end = $900
}

; SFTODO: Might want to move these into page 4 resident variable space, to free up $9 and $a for vmem cache or something.
ram_bank_count = $904
ram_bank_list = $905

!ifdef USE_HISTORY {
    nominal_cursor_key_status = $a00
}

buffer_keyboard = 0

; SFTODO: MOVE THIS?
; SFTODO: Use this in more places?
!macro assert .b {
    !if .b = 0 {
        !error "assertion failed"
    }
}
