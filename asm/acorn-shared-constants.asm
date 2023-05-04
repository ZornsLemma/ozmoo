; Acorn constants shared by multiple Acorn executables; constants only relevant
; to the main Ozmoo executable are in constants.asm

brkv = $202

shadow_mode_bit = 128

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

!ifdef ACORN_SHADOW_VMEM {
    shadow_start = $3000

    ; We steal the envelope buffers in page 8 for the shadow RAM driver. (It's
    ; tempting to steal the printer buffer at $880 as well, but the Integra-B
    ; seems to use that for its own purposes so we'll keep out of its way.)
    shadow_driver_start = $8c0
    shadow_paging_control_ptr = shadow_driver_start
    shadow_ram_copy = shadow_driver_start + 2
    shadow_driver_end = $900 ; exclusive

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

    ; We don't need zero page for all of these, but we have it free so with
    ; might as well use it to keep the code size down. The first two addresses
    ; are used to pass information to the BASIC loader.
    shadow_state = $70
    private_ram_in_use = $71
}

!ifdef ACORN_SWR {
b_plus_private_ram_size = 12 * 1024 - 512 ; -512 to leave space for shadow copy code
integra_b_private_ram_size = 12 * 1024 - 1024 ; -1024 to leave space for IBOS workspace
}

; SFTODO: Might want to move these to free up $9 and $a for vmem cache or something.
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

; SFTODO: MOVE THIS? I PUT IT HERE INTENDING TO USE IT IN CACHE2P BUT DIDN'T, SO IT COULD TECHNICALLY MOVE BACK TO ACORN.ASM WHERE IT USED TO BE.
; Macro used to generate an OS error.
; SFTODO: On one of my machines where acme is:
;     This is ACME, release 0.95.8 ("Fenchurch"), 8 Oct 2016
;       Platform independent version.
; macros cannot take string arguments and all the uses of this fail. Should I
; get rid of this (and any other, if there are any) macros which take string
; arguments, or just require a newer acme? (Right now I am not sure exactly
; which versions work, and acme seems to have a few forks floating around.) I
; noticed this myself, no one else has run into problems with this yet.
; (I tried updating to release 0.96.4 ("Fenchurch"), 1 Feb 2019, platform
; independent version but I still get the same error.)
; - OK, the machine which *does* build this OK has 0.97 ("Zem"), 31 Jan 2021,
; platform independent version.
!macro os_error error_number, error_message {
    brk
    !byte error_number
    !text error_message, 0
}
