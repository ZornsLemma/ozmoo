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
    ; We steal the printer and envelope buffers in page 8 for the shadow RAM
    ; driver.
    ; page_in_shadow_ram_indirect = $880
    ; page_out_shadow_ram_indirect = $883
    ; SFTODO MAKE SURE THESE ARE ACTUALLY BOTH USED, OTHERWISE NO POINT HAVING THEM
    shadow_ram_copy = $886
    shadow_ram_copy_max_end = $900
}

; SFTODO: Might want to move these into page 4 resident variable space, to free up $9 and $a for vmem cache or something.
ram_bank_count = $904
ram_bank_list = $905

; SFTODO: MOVE THIS?
!macro assert .b {
    !if .b = 0 {
        !error "assertion failed"
    }
}
