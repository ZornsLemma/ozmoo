; Acorn constants shared by multiple Acorn executables; constants only relevant
; to the main Ozmoo executable are in constants.asm

osargs = $ffda
osbyte = $fff4
osbyte_read_host = 0
osbyte_issue_service_request = $8f

romsel_copy = $f4
bbc_romsel = $fe30
electron_romsel = $fe05

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
    nominal_cursor_key_status = $840
}

buffer_keyboard = 0

; SFTODO: MOVE THIS?
!macro assert .b {
    !if .b = 0 {
        !error "assertion failed"
    }
}
