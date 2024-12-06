; Which Z-machine to generate binary for
; (usually defined on the acme command line instead)
; Z6 will never be supported
;Z1 = 1
;Z2 = 1
;Z3 = 1
;Z4 = 1
;Z5 = 1
;Z7 = 1
;Z8 = 1

; SFTODO: It may be worth deleting lots of the Commodore code in this file, it is probably one of the more divergent bits of the Acorn port.
; OK, I have "started" this (i.e. I have deleted a bit of Commodore code where I was reshuffling something), but this is not "done".
!ifndef ACORN {
	!error "Non-Acorn code has been deleted in this file"
}

; Which machine to generate code for
!ifndef ACORN { ; SFTODO!?
; C64 is default target
!ifndef VMEM {
!ifndef SLOW {
	SLOW = 1
}
}

!ifndef FREE_SAVE_BLOCKS {
	FREE_SAVE_BLOCKS = 664  ; Default to the free space of an empty 1541 disk
}

!ifdef TARGET_X16 {
	TARGET_MEGA65_OR_X16 = 1
	NO_VMEM_CACHE = 1
	TARGET_ASSIGNED = 1
	COMPLEX_MEMORY = 1
	FAR_DYNMEM = 1
	SUPPORT_REU = 1
	SUPPORT_80COL = 1
	!ifndef SLOW {
		SLOW = 1
	}
;	SKIP_BUFFER = 1 ; This is for SLOW mode and non-VMEM mode, which we know we have
}
!ifdef TARGET_MEGA65 {
	TARGET_MEGA65_OR_X16 = 1
	TARGET_ASSIGNED = 1
	FAR_DYNMEM = 1
	COMPLEX_MEMORY = 1
	HAS_SID = 1
	SUPPORT_REU = 1
	SUPPORT_80COL = 1;
	!ifndef SLOW {
		SLOW = 1
	}
	SKIP_BUFFER = 1 ; This is for SLOW mode and non-VMEM mode, which we know we have
	!ifndef NOSCROLLBACK {
		SCROLLBACK = 1
	}
}
!ifdef TARGET_PLUS4 {
	NO_VMEM_CACHE = 1
	TARGET_PLUS4_OR_C128 = 1
	TARGET_ASSIGNED = 1
	COMPLEX_MEMORY = 1
	VMEM_END_PAGE = $fc
	SUPPORT_REU = 0
	!ifndef SLOW {
		SLOW = 1
	}
	!ifndef NOSCROLLBACK {
		SCROLLBACK = 1
		; SCROLLBACK_RAM_PAGES may be set by make.rb;
		; Must be an even number, where 6 * 4 <= SCROLLBACK_RAM_PAGES <= 11 * 4
		!ifndef SCROLLBACK_RAM_PAGES {
			SCROLLBACK_RAM_PAGES = 6 * 4
		}
	}
}
!ifdef TARGET_C64 {
	TARGET_ASSIGNED = 1
}
!ifdef TARGET_C128 {
	NO_VMEM_CACHE = 1
	TARGET_PLUS4_OR_C128 = 1
	TARGET_ASSIGNED = 1
	FAR_DYNMEM = 1
	COMPLEX_MEMORY = 1
	VMEM_END_PAGE = $fc
	HAS_SID = 1
	SUPPORT_80COL = 1;
	SUPPORT_REU = 1
;	REUBOOST = 1
	!ifndef SLOW {
		SLOW = 1
	}
	!ifndef NOSCROLLBACK {
		SCROLLBACK = 1
		; SCROLLBACK_RAM_PAGES may be set by make.rb;
		; Must be an even number, where 6 * 4 <= SCROLLBACK_RAM_PAGES <= 11 * 4
	}
}
} else {
	; SFTODO: This may not be the best way of doing this, but I want to get things working and tested before (much later) maybe trying to make the Acorn port fit a bit more cleanly
	TARGET_ASSIGNED = 1
	SUPPORT_REU = 0 ; SFTODO: Can I get rid of need for this definition?
}

!ifndef TARGET_ASSIGNED {
	; No target given. C64 is the default target
	TARGET_C64 = 1
}

!ifdef TARGET_C64 {
	HAS_SID = 1
	!ifdef SLOW {
		!ifndef VMEM {
			SKIP_BUFFER = 1
		}
	}
	!ifndef NOSCROLLBACK {
		SCROLLBACK = 1
	}
}

!ifndef ACORN { ; SFTODO!?
!ifdef VMEM {
	!ifndef SUPPORT_REU {
		SUPPORT_REU = 1
	}
} else {
	!ifndef SUPPORT_REU {
		SUPPORT_REU = 0
	}
	; SFTODO: SCROLLBACK/NOSCROLLBACK?
}


!ifndef VMEM_END_PAGE {
	VMEM_END_PAGE = $00 ; Last page of accessible RAM for VMEM, plus 1.
}

!ifndef ACORN {
!ifdef TARGET_PLUS4 {
	cache_pages = 0
} else {
	!ifdef CACHE_PAGES {
		cache_pages = CACHE_PAGES ; Note, this is not final. One page may be added. vmem_cache_count will hold final # of pages.
	} else {
		cache_pages = 4 ; Note, this is not final. One page may be added. vmem_cache_count will hold final # of pages.
	}
}
}
}

!ifdef SCROLLBACK_RAM_PAGES {
	SCROLLBACK_RAM_START_PAGE = (VMEM_END_PAGE - SCROLLBACK_RAM_PAGES) & $ff ; VMEM_END_PAGE is 0 for C64, hence & $ff
}

!ifndef TERPNO {
	TERPNO = 8
}

!ifdef Z1 {
	ZMACHINEVERSION = 1
}
!ifdef Z2 {
	ZMACHINEVERSION = 2
}
!ifdef Z3 {
	ZMACHINEVERSION = 3
	Z3PLUS = 1
}
!ifdef Z4 {
	ZMACHINEVERSION = 4
	Z3PLUS = 1
	Z4PLUS = 1
}
!ifdef Z5 {
	ZMACHINEVERSION = 5
	Z3PLUS = 1
	Z4PLUS = 1
	Z5PLUS = 1
}
!ifdef Z7 {
	ZMACHINEVERSION = 7
	Z3PLUS = 1
	Z4PLUS = 1
	Z5PLUS = 1
	Z7PLUS = 1
}
!ifdef Z8 {
	ZMACHINEVERSION = 8
	Z3PLUS = 1
	Z4PLUS = 1
	Z5PLUS = 1
	Z7PLUS = 1
}

!ifdef TRACE {
	z_trace_size = 256
} else {
	z_trace_size = 0
}

!ifdef STACK_PAGES {
	stack_size = STACK_PAGES * $100;
} else {
	stack_size = $0400;
}

!ifndef BGCOL {
	BGCOL = 9
}
!ifndef FGCOL {
	FGCOL = 2
}
!ifndef INPUTCOL {
	INPUTCOL = FGCOL
}

!ifndef BGCOLDM {
	BGCOLDM = 2
}
!ifndef FGCOLDM {
	FGCOLDM = 4
}
!ifndef INPUTCOLDM {
	INPUTCOLDM = FGCOLDM
}

!ifndef Z5PLUS {
	!if (INPUTCOL != FGCOL) OR (INPUTCOLDM != FGCOLDM) {
		USE_INPUTCOL = 1
	}
}


; Border color: 0 = as background, 1 = as foreground, 2-9: specified Z-code colour. Default: as background

!ifndef BORDERCOL {
	!ifdef Z5PLUS {
		BORDERCOL = 0
	} else {
		BORDERCOL = BGCOL
	}
}
!ifndef BORDERCOLDM {
	!ifdef Z5PLUS {
		BORDERCOLDM = 0
	} else {
		BORDERCOLDM = BGCOLDM
	}
}
; For z3 and z4, change border colour magic values 0 and 1 to actual bgcol or fgcol, for shorter code
!ifndef Z5PLUS {
	!if BORDERCOL = 0 {
		BORDERCOL_FINAL = BGCOL
	}
	!if BORDERCOL = 1 {
		BORDERCOL_FINAL = FGCOL
	}
	!if BORDERCOLDM = 0 {
		BORDERCOLDM_FINAL = BGCOLDM
	}
	!if BORDERCOLDM = 1 {
		BORDERCOLDM_FINAL = FGCOLDM
	}
}
!ifndef BORDERCOL_FINAL {
	BORDERCOL_FINAL = BORDERCOL
}
!ifndef BORDERCOLDM_FINAL {
	BORDERCOLDM_FINAL = BORDERCOLDM
}
!if BORDERCOL_FINAL = 0 {
	BORDER_MAY_FOLLOW_BG = 1
} else {
	!if BORDERCOLDM_FINAL = 0 {
		BORDER_MAY_FOLLOW_BG = 1
	}
}
!if BORDERCOL_FINAL = 1 {
	BORDER_MAY_FOLLOW_FG = 1
} else {
!if BORDERCOLDM_FINAL = 1 {
	BORDER_MAY_FOLLOW_FG = 1
}
}

!ifndef STATCOL {
	STATCOL = FGCOL
}
!ifndef STATCOLDM {
	STATCOLDM = FGCOLDM
}

!ifndef CURSORCOL {
	CURSORCOL = 1 ; Follow FGCOL
}
!ifndef CURSORCOLDM {
	CURSORCOLDM = 1 ; Follow FGCOL
}

!ifndef CURSORCHAR {
	CURSORCHAR = 224
}

!ifndef SPLASHWAIT {
	SPLASHWAIT = 15
}

!ifdef Z5PLUS {
	COLOURFUL_LOWER_WIN = 1
}
!ifdef USE_INPUTCOL {
	!ifndef COLOURFUL_LOWER_WIN {
		COLOURFUL_LOWER_WIN = 1
	}
}
;!if CURSORCOL > 1 {
;	!ifndef COLOURFUL_LOWER_WIN {
;		COLOURFUL_LOWER_WIN = 1
;	}
;}
;!if CURSORCOLDM > 1 {
;	!ifndef COLOURFUL_LOWER_WIN {
;		COLOURFUL_LOWER_WIN = 1
;	}
;}

; To improve readability of code and avoid double-nesting so we can test for
; ACORN_SWR and !ACORN_SWR_SMALL_DYNMEM in a single !ifdef, we define
; ACORN_SWR_BIG_DYNMEM internally - the build script should never set this. SFTODO: COMMENT IS OUTDATED WITH INTRODUCTION OF MEDIUM
!ifdef ACORN_SWR {
!ifndef ACORN_SWR_SMALL_DYNMEM {
!ifndef ACORN_SWR_MEDIUM_DYNMEM {
ACORN_SWR_BIG_DYNMEM = 1
ACORN_SWR_MEDIUM_OR_BIG_DYNMEM = 1
} else {
ACORN_SWR_MEDIUM_OR_BIG_DYNMEM = 1
}
}
}

; The screen hole mostly "just works" for the small dynamic memory model;
; because the hole always comes *after* all dynamic memory, only read-only VM
; block access needs to take it into account. Define an extra constant to allow
; for easy testing of the combination of the big dynamic memory model and screen
; hole where more extensive screen hole support is required. SFTODO: COMMENT IS OUTDATED WITH INTRODUCTION OF MEDIUM
!ifdef ACORN_SCREEN_HOLE {
!ifdef ACORN_SWR_BIG_DYNMEM {
    ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE = 1
}
}
; We have some code to handle 12K private RAM on an Integra-B or B+; this is only
; needed in sidewayS RAM+shadow RAM builds.
!ifdef ACORN_SWR {
!ifndef ACORN_SCREEN_HOLE {
ACORN_PRIVATE_RAM_SUPPORTED = 1
}
}

; For simplicity make-acorn.py just indicates whether we would like to use absolute globals, and this code
; defines ACORN_ABSOLUTE_GLOBALS if we want to and can use them.
!ifdef ACORN_PREFER_ABSOLUTE_GLOBALS {
	!ifndef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
		ACORN_ABSOLUTE_GLOBALS = 1
	}
}

;  * = $0801 ; This must now be set on command line: --setpc $0801

!ifdef TARGET_X16 {
; Basic: 1 SYS2061
!byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00
}

program_start

!ifdef TARGET_C128 {
	lda #%00001110 ; 48K RAM0 (0-$c000)
	sta $ff00
}
initial_jmp
!ifndef ACORN_RELOCATABLE {
	jmp .initialize
} else {
    jmp relocate
}

!ifdef VMEM {
	RESTART_SUPPORTED = 1
} else {
	!ifdef TARGET_MEGA65 {
		RESTART_SUPPORTED = 1
	} 
	!ifdef TARGET_X16 {
		RESTART_SUPPORTED = 1
	} 
}

; =========================================== Highbytes of jump table

z_jump_high_arr

; 0OP

z_opcount_0op_jump_high_arr
	!byte >z_ins_rtrue - 1
	!byte >(z_ins_rfalse - 1)
	!byte >(z_ins_print - 1)
	!byte >(z_ins_print_ret - 1)
	!byte >(z_ins_nop - 1)
!ifndef Z5PLUS {
	!byte >(z_ins_save - 1)
	!byte >(z_ins_restore - 1)
} else {
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
}
!ifdef RESTART_SUPPORTED {
	!byte >(z_ins_restart - 1)
} else {
	!byte >(z_ins_not_supported - 1)
}
	!byte >(z_ins_ret_popped - 1)
!ifndef Z5PLUS {
	!byte >(stack_pull - 1) ; z_ins_pop
} else {
	!byte >(z_ins_catch - 1)
}
	!byte >(z_ins_quit - 1)
	!byte >(z_ins_new_line - 1)
!ifndef Z4PLUS {
	!byte >(z_ins_show_status - 1)
} else {
	!byte >(z_ins_nop - 1) ; should be nop according to show_status/spec 1.0
}
	!byte >(make_branch_true - 1) ; z_ins_verify
!ifdef Z5PLUS {
	!byte >(z_not_implemented - 1)
	!byte >(make_branch_true - 1) ; z_ins_piracy
} else {
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
}

; 1OP

z_opcount_1op_jump_high_arr
	!byte >(z_ins_jz - 1)
	!byte >(z_ins_get_sibling - 1)
	!byte >(z_ins_get_child - 1)
	!byte >(z_ins_get_parent - 1)
	!byte >(z_ins_get_prop_len - 1)
	!byte >(z_ins_inc - 1)
	!byte >(z_ins_dec - 1)
	!byte >(z_ins_print_addr - 1)
	!byte >(z_ins_call_xs - 1)
	!byte >(z_ins_remove_obj - 1)
	!byte >(z_ins_print_obj - 1)
	!byte >(z_ins_ret - 1)
	!byte >(z_ins_jump - 1)
	!byte >(z_ins_print_paddr - 1)
	!byte >(z_ins_load - 1)
!ifndef Z5PLUS {
	!byte >(z_ins_not - 1)
} else {
	!byte >(z_ins_call_xn - 1)
}

; 2OP

z_opcount_2op_jump_high_arr
	!byte >(z_not_implemented - 1)
	!byte >(z_ins_je - 1)
	!byte >(z_ins_jl - 1)
	!byte >(z_ins_jg - 1)
	!byte >(z_ins_dec_chk - 1)
	!byte >(z_ins_inc_chk - 1)
	!byte >(z_ins_jin - 1)
	!byte >(z_ins_test - 1)
	!byte >(z_ins_or - 1)
	!byte >(z_ins_and - 1)
	!byte >(z_ins_test_attr - 1)
	!byte >(z_ins_set_attr - 1)
	!byte >(z_ins_clear_attr - 1)
	!byte >(z_ins_store - 1)
	!byte >(z_ins_insert_obj - 1)
	!byte >(z_ins_loadw_and_storew - 1)
	!byte >(z_ins_loadb - 1)
	!byte >(z_ins_get_prop - 1)
	!byte >(z_ins_get_prop_addr - 1)
	!byte >(z_ins_get_next_prop - 1)
	!byte >(z_ins_add - 1)
	!byte >(z_ins_sub - 1)
	!byte >(z_ins_mul - 1)
	!byte >(z_ins_div - 1)
	!byte >(z_ins_mod - 1)
!ifndef Z4PLUS {
	!byte >(z_not_implemented - 1)
} else {
	!byte >(z_ins_call_xs - 1)
}
!ifndef Z5PLUS {
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
} else {
	!byte >(z_ins_call_xn - 1)
	!byte >(z_ins_set_colour - 1)
	!byte >(z_ins_throw - 1)
}
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)

; VAR	

z_opcount_var_jump_high_arr
	!byte >(z_ins_call_xs - 1)
	!byte >(z_ins_loadw_and_storew - 1)
	!byte >(z_ins_storeb - 1)
	!byte >(z_ins_put_prop - 1)
	!byte >(z_ins_read - 1)
	!byte >(z_ins_print_char - 1)
	!byte >(z_ins_print_num - 1)
	!byte >(z_ins_random - 1)
	!byte >(z_ins_push - 1)
	!byte >(z_ins_pull - 1)
	!byte >(z_ins_split_window - 1)
	!byte >(z_ins_set_window - 1)
!ifdef Z4PLUS {
	!byte >(z_ins_call_xs - 1)
	!byte >(z_ins_erase_window - 1)
	!byte >(z_ins_erase_line - 1)
	!byte >(z_ins_set_cursor - 1)
	!byte >(z_ins_get_cursor - 1)
	!byte >(z_ins_set_text_style - 1)
	!byte >(z_ins_buffer_mode - 1)
} else {
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
}
	!byte >(z_ins_output_stream - 1)
	!byte >(z_ins_not_supported - 1)
	!byte >(z_ins_sound_effect - 1)
!ifdef Z4PLUS {
	!byte >(z_ins_read_char - 1)
	!byte >(z_ins_scan_table - 1)
} else {
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
}
!ifdef Z5PLUS {
	!byte >(z_ins_not - 1)
	!byte >(z_ins_call_xn - 1)
	!byte >(z_ins_call_xn - 1)
	!byte >(z_ins_tokenise_text - 1)
	!byte >(z_ins_encode_text - 1)
	!byte >(z_ins_copy_table - 1)
	!byte >(z_ins_print_table - 1)
	!byte >(z_ins_check_arg_count - 1)
} else {
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
}

; EXT

z_opcount_ext_jump_high_arr
!ifdef Z5PLUS {
	!byte >(z_ins_save - 1)
	!byte >(z_ins_restore - 1)
	!byte >(z_ins_log_shift - 1)
	!byte >(z_ins_art_shift - 1)
	!byte >(z_ins_set_font - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_not_implemented - 1)
	!byte >(z_ins_save_undo - 1)
	!byte >(z_ins_restore_undo - 1)
	!byte >(z_ins_print_unicode - 1)
	!byte >(z_ins_check_unicode - 1)
	!byte >(z_ins_set_true_colour - 1)
}


; =========================================== Lowbytes of jump table
	
z_jump_low_arr

; 0OP

z_opcount_0op_jump_low_arr
	!byte <(z_ins_rtrue - 1)
	!byte <(z_ins_rfalse - 1)
	!byte <(z_ins_print - 1)
	!byte <(z_ins_print_ret - 1)
	!byte <(z_ins_nop - 1)
!ifndef Z5PLUS {
	!byte <(z_ins_save - 1)
	!byte <(z_ins_restore - 1)
} else {
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
}
!ifdef RESTART_SUPPORTED {
	!byte <(z_ins_restart - 1)
} else {
	!byte <(z_ins_not_supported - 1)
}
	!byte <(z_ins_ret_popped - 1)
!ifndef Z5PLUS {
	!byte <(stack_pull - 1) ; z_ins_pop
} else {
	!byte <(z_ins_catch - 1)
}
	!byte <(z_ins_quit - 1)
	!byte <(z_ins_new_line - 1)
!ifndef Z4PLUS {
	!byte <(z_ins_show_status - 1)
} else {
	!byte <(z_ins_nop - 1) ; should be nop according to show_status/spec 1.0
}
	!byte <(make_branch_true - 1) ; z_ins_verify
!ifdef Z5PLUS {
	!byte <(z_not_implemented - 1)
	!byte <(make_branch_true - 1) ; z_ins_piracy
} else {
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
}

; 1OP

z_opcount_1op_jump_low_arr
	!byte <(z_ins_jz - 1)
	!byte <(z_ins_get_sibling - 1)
	!byte <(z_ins_get_child - 1)
	!byte <(z_ins_get_parent - 1)
	!byte <(z_ins_get_prop_len - 1)
	!byte <(z_ins_inc - 1)
	!byte <(z_ins_dec - 1)
	!byte <(z_ins_print_addr - 1)
	!byte <(z_ins_call_xs - 1)
	!byte <(z_ins_remove_obj - 1)
	!byte <(z_ins_print_obj - 1)
	!byte <(z_ins_ret - 1)
	!byte <(z_ins_jump - 1)
	!byte <(z_ins_print_paddr - 1)
	!byte <(z_ins_load - 1)
!ifndef Z5PLUS {
	!byte <(z_ins_not - 1)
} else {
	!byte <(z_ins_call_xn - 1)
}
	
; 2OP

z_opcount_2op_jump_low_arr
	!byte <(z_not_implemented - 1)
	!byte <(z_ins_je - 1)
	!byte <(z_ins_jl - 1)
	!byte <(z_ins_jg - 1)
	!byte <(z_ins_dec_chk - 1)
	!byte <(z_ins_inc_chk - 1)
	!byte <(z_ins_jin - 1)
	!byte <(z_ins_test - 1)
	!byte <(z_ins_or - 1)
	!byte <(z_ins_and - 1)
	!byte <(z_ins_test_attr - 1)
	!byte <(z_ins_set_attr - 1)
	!byte <(z_ins_clear_attr - 1)
	!byte <(z_ins_store - 1)
	!byte <(z_ins_insert_obj - 1)
	!byte <(z_ins_loadw_and_storew - 1)
	!byte <(z_ins_loadb - 1)
	!byte <(z_ins_get_prop - 1)
	!byte <(z_ins_get_prop_addr - 1)
	!byte <(z_ins_get_next_prop - 1)
	!byte <(z_ins_add - 1)
	!byte <(z_ins_sub - 1)
	!byte <(z_ins_mul - 1)
	!byte <(z_ins_div - 1)
	!byte <(z_ins_mod - 1)
!ifndef Z4PLUS {
	!byte <(z_not_implemented - 1)
} else {
	!byte <(z_ins_call_xs - 1)
}
!ifndef Z5PLUS {
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
} else {
	!byte <(z_ins_call_xn - 1)
	!byte <(z_ins_set_colour - 1)
	!byte <(z_ins_throw - 1)
}
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)

; VAR	

z_opcount_var_jump_low_arr
	!byte <(z_ins_call_xs - 1)
	!byte <(z_ins_loadw_and_storew - 1)
	!byte <(z_ins_storeb - 1)
	!byte <(z_ins_put_prop - 1)
	!byte <(z_ins_read - 1)
	!byte <(z_ins_print_char - 1)
	!byte <(z_ins_print_num - 1)
	!byte <(z_ins_random - 1)
	!byte <(z_ins_push - 1)
	!byte <(z_ins_pull - 1)
	!byte <(z_ins_split_window - 1)
	!byte <(z_ins_set_window - 1)
!ifdef Z4PLUS {
	!byte <(z_ins_call_xs - 1)
	!byte <(z_ins_erase_window - 1)
	!byte <(z_ins_erase_line - 1)
	!byte <(z_ins_set_cursor - 1)
	!byte <(z_ins_get_cursor - 1)
	!byte <(z_ins_set_text_style - 1)
	!byte <(z_ins_buffer_mode - 1)
} else {
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
}
	!byte <(z_ins_output_stream - 1)
	!byte <(z_ins_not_supported - 1)
	!byte <(z_ins_sound_effect - 1)
!ifdef Z4PLUS {
	!byte <(z_ins_read_char - 1)
	!byte <(z_ins_scan_table - 1)
} else {
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
}
!ifdef Z5PLUS {
	!byte <(z_ins_not - 1)
	!byte <(z_ins_call_xn - 1)
	!byte <(z_ins_call_xn - 1)
	!byte <(z_ins_tokenise_text - 1)
	!byte <(z_ins_encode_text - 1)
	!byte <(z_ins_copy_table - 1)
	!byte <(z_ins_print_table - 1)
	!byte <(z_ins_check_arg_count - 1)
} else {
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
}

; EXT

z_opcount_ext_jump_low_arr
!ifdef Z5PLUS {
	!byte <(z_ins_save - 1)
	!byte <(z_ins_restore - 1)
	!byte <(z_ins_log_shift - 1)
	!byte <(z_ins_art_shift - 1)
	!byte <(z_ins_set_font - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_not_implemented - 1)
	!byte <(z_ins_save_undo - 1)
	!byte <(z_ins_restore_undo - 1)
	!byte <(z_ins_print_unicode - 1)
	!byte <(z_ins_check_unicode - 1)
	!byte <(z_ins_set_true_colour - 1)
}

z_number_of_ext_opcodes_implemented = * - z_opcount_ext_jump_low_arr

z_number_of_opcodes_implemented = * - z_jump_low_arr

!ifdef TARGET_C128 {
!source "constants-c128.asm"

!ifdef CUSTOM_FONT {
!ifdef UNDO {
.setup_copy_font
	ldy #>$1800
	lda reu_bank_for_undo
	ldx #$fc
	clc
	jsr store_reu_transfer_params
	lda #8
    sta reu_translen + 1
	rts

; Font is actually at $1800, where it can't be copied to bank 1, since dynmem starts at $1600
; so we copy it to $0800 instead
c128_copy_font_to_bank_1
	jsr .setup_copy_font
	lda #%10110000;  C64 -> REU with immediate execution
	sta reu_command
	; Wait until raster is in border
-	bit $d011
	bpl -
	; Make REU see RAM bank 1
	lda $d506
	ora #%01000000 ; Bit 6: 0 means bank 0, bit 7 is unused
	sta $d506
	jsr .setup_copy_font
	ldy #>$0800
	sty reu_c64base + 1
	lda #%10110001;  REU -> c64 with immediate execution
	sta reu_command
	; Restore REU to see RAM bank 0
	lda $d506
	and #%00111111 ; Bit 6: 0 means bank 0, bit 7 is unused
	sta $d506
	rts	
}
}

c128_reset_to_basic
	; this needs to be at the start of the program since
	; I need to bank back the normal memory and the latter
	; part of Ozmoo will be under the BASIC ROM.
	lda #0
	sta $ff00
	lda #$01
	sta $2b
	lda #$10
	sta $2c
	jmp basic_reset

; Adding support for 2MHz in the border
; https://sites.google.com/site/h2obsession/CBM/C128/2mhz-border

allow_2mhz_in_40_col !byte 1
use_2mhz_in_80_col !byte 0 ; Initial value should always be 0

use_2mhz_in_80_col_in_game_value = 1 ; This value is used after setup

!ifndef SMOOTHSCROLL {
;phase 2 of 2MHz speed-up = change CPU to 2MHz
;and set raster IRQ for top-of-screen less 1 raster
;and do normal KERNAL routines of IRQ
c128_border_phase2
	lda #1
	ldx allow_2mhz_in_40_col
	stx reg_2mhz	;CPU = 2MHz
	sta $d019	;clear VIC raster IRQ
	lda #<c128_border_phase1    ;set top-of-screen (phase 1)
	ldx #>c128_border_phase1
	sta $0314        ;as new IRQ vector
	stx $0315
	lda $d011
	and #$7f	;high raster bit = 0
	sta $d011
	lda #48+3-1	;low raster bits (default + Y_Scroll - 1 early raster = 50)
	sta $d012
	cli		;allow sprite/pen IRQs
	jsr $c22c	;flash VIC cursor, etc.
	jmp $fa6b	;update Jiffy Clock, control Cassette, handle SOUND/PLAY/MOVSPR
				;and return from IRQ

;phase 1 of 2MHz speed-up = change CPU back to 1MHz
;and set raster IRQ for bottom-of-screen
;NOTE the CPU is in BANK 15 (the VIC will soon start top of visible screen)
c128_border_phase1
	lda #<c128_border_phase2    ;set bottom-of-screen (phase 2)
	ldx #>c128_border_phase2
	sta $0314        ;as new IRQ vector
	stx $0315
	lda $d011
	and #$7f	;high raster bit = 0
	sta $d011
	lda #251	;low raster bits (1 raster beyond visible screen)
	sta $d012
	lda #1
	sta $d019	;clear VIC raster IRQ
	lsr		; A = 0
	sta reg_2mhz	;CPU = 1MHz
	jmp $ff33	;return from IRQ
}

} else ifdef TARGET_X16 {
!source "constants-x16.asm"
} else {
!ifdef ACORN {
!source "acorn-shared-constants.asm"
!source "acorn-ozmoo-constants.asm"
} else {
!source "constants.asm"
}
}
!source "constants-header.asm"

; global variables
; filelength !byte 0, 0, 0
; c64_model !byte 0 ; 1=NTSC/6567R56A, 2=NTSC/6567R8, 3=PAL/6569
; SFTODO: Looks like upstream supports scrollback with a REU; could we support this on Acorn?

; SF: This is upstream code but moved so I can put it in a macro and inline it
; in a different place on Acorn.
; SFTODO: This is now so short can we just get rid of it as a macro?
!macro prepare_static_high_memory_inline {
	lda #$ff
	sta zp_pc_h
	sta zp_pc_l
}

!if SUPPORT_REU = 1 {
statmem_reu_banks !byte 0
}


; include other assembly files
!ifdef ACORN {
	!source "acorn.asm"
	!source "acorn-swr.asm" ; SFTODO: rename this file? screen hole is not exactly swr (although it kind of is, because screen hole is trivial if we have no swr)
	!source "acorn-utilities.asm"
}
!ifdef SMOOTHSCROLL {
!source "smoothscroll.asm"
}
!source "utilities.asm"
!ifdef SCROLLBACK {
!source "scrollback.asm"
}
!source "screenkernal.asm"
!source "screen.asm"
!source "streams.asm" ; Must come before "text.asm"
!source "memory.asm"
!ifndef ACORN {
	!source "disk.asm"
	; SFTODO: Does it make sense to try to support sound on Acorn?
;!ifdef SOUND {
!source "sound.asm"
;}
} else {
	!source "acorn-disk.asm"
}
;!ifdef VMEM {
	!if SUPPORT_REU = 1 {
	!source "reu.asm"
	}
;}
!source "stack.asm"
;##!ifdef VMEM {
!source "vmem.asm"
;##}
!source "zmachine.asm"
!source "zaddress.asm"
!source "text.asm"
!source "dictionary.asm"
!source "objecttable.asm"


!ifndef ACORN {
!ifdef NO_VMEM_CACHE {
	!if SPLASHWAIT > 0 {
		!source "splashscreen.asm"
	}
}
}

!ifdef Z7 {
calc_z7_offsets
	ldy #header_string_offset
	jsr read_header_word
	sta string_offset + 1
	stx string_offset + 2
	ldy #header_routine_offset
	jsr read_header_word
	stx routine_offset + 2

	ldx #3
-	asl string_offset + 2
	rol string_offset + 1
	rol string_offset
	asl routine_offset + 2
	rol
	rol routine_offset
	dex
	bne -
	sta routine_offset + 1
	rts
}

!ifdef Z4PLUS {
!ifdef TARGET_C128 {
update_screen_width_in_header
	lda s_screen_width
	ldy #header_screen_width_chars
	+write_header_byte
!ifdef Z5PLUS {
	ldy #header_screen_width_units
	tax
	lda #0
	jsr write_header_word
}
	rts
} else ifdef TARGET_X16 {
update_screen_width_in_header
	lda s_screen_width
	ldy #header_screen_width_chars
	+write_header_byte
!ifdef Z5PLUS {
	ldy #header_screen_width_units
	tax
	lda #0
	jsr write_header_word
}
	lda s_screen_height
	ldy #header_screen_height_lines
	+write_header_byte
!ifdef Z5PLUS {
	ldy #header_screen_height_units
	tax
	lda #0
	jsr write_header_word
}
	rts
}
} ; Z4PLUS


!ifdef TARGET_X16 {
x16_backup_basic_zp
	ldx #last_basic_zp_address - first_basic_zp_address + 1
-	lda first_basic_zp_address - 1,x
	sta basic_zp_backup_area - 1,x
	dex
	bne -
	rts

x16_restore_basic_zp
	ldx #last_basic_zp_address - first_basic_zp_address + 1
-	lda basic_zp_backup_area - 1,x
	sta first_basic_zp_address - 1,x
	dex
	bne -
	rts

basic_zp_backup_area
!fill last_basic_zp_address - first_basic_zp_address + 1, 0
}

!ifdef TARGET_C128 {

c128_setup_mmu
	lda #5 ; 4 KB common RAM at bottom only
	sta c128_mmu_ram_cfg
	ldx #2
-	lda c128_mmu_values,x
	sta c128_mmu_pcra,x
	dex
	bpl -

	ldx #copy_page_c128_src_end - copy_page_c128_src
-	lda copy_page_c128_src - 1,x
	sta copy_page_c128 - 1,x
	dex
	bne -
	rts

!ifdef UNDO_RAM {
; Setup a RAM buffer for Undo
undo_ram_setup
	bit reu_bank_for_undo
	bpl + ; An REU bank has already been assigned
	; Check if undo fits in bank 1
	jsr calc_dynmem_size
	clc
	lda #(>stack_size) + 1
	adc nonstored_pages
	bcs + ; Dynmem + stack + ZP > 64 KB, so no way to fit in RAM buffer
	sta object_temp
	adc nonstored_pages
	; adc story_start + header_static_mem
	; bcs +
	; ldy story_start + header_static_mem + 1
	; beq +++
	; clc
	; adc #1
	bcs +
+++	adc #>story_start_far_ram
	bcs +
	cmp #VMEM_END_PAGE
	bcs +
	lda #VMEM_END_PAGE
	sec
	sbc object_temp
	sta ram_undo_page
	; Decrease vmem size
	lda object_temp
	lsr ; Divide by 2 to get # of VMEM blocks
	bcc ++
	adc #0 ; Adds one since C=1 (for when undo size is e.g. 20.5 VMEM blocks)
++	sta vmap_entries_reserved
	; lda vmap_max_entries
	; sec
	; sbc object_temp + 1
	; sta vmap_max_entries
	ldy #$7f
	sty reu_bank_for_undo ; Special value $7f means undo uses RAM buffer
+
	rts
}

c128_move_dynmem_and_calc_vmem
	; Copy dynmem to bank 1
	lda #>story_start
	sta zp_temp
	lda #>story_start_far_ram
	sta zp_temp + 1
	lda nonstored_pages
	sta zp_temp + 2
-	lda zp_temp
	ldy #>(vmem_cache_start + $200)
	ldx #0
	jsr copy_page_c128
	lda #>(vmem_cache_start + $200)
	ldy zp_temp + 1
	ldx #1
	jsr copy_page_c128
	inc zp_temp
	inc zp_temp + 1
	dec zp_temp + 2
	bne -

	; Copy any preloaded statmem pages down in memory, now that dynmem has moved
	lda #>story_start
	sta zp_temp + 1 ; First destination page
	clc
	adc nonstored_pages
	sta zp_temp ; First source page

-	lda zp_temp
	cmp #VMEM_END_PAGE
	bcs .done_vmem_move
	ldy zp_temp + 1
	ldx #0
	jsr copy_page_c128
	inc zp_temp
	inc zp_temp + 1
	bne - ; Always branch

.done_vmem_move
	; Add free RAM in bank 1 as vmem memory

	lda #>story_start
	sta vmap_first_ram_page

	; Remember above which index in vmem the blocks are in bank 1
!ifdef SCROLLBACK_RAM_PAGES {
	lda #SCROLLBACK_RAM_START_PAGE
} else {
	lda #VMEM_END_PAGE
}
	sec
	sbc #>story_start
	lsr ; Convert from 256-byte pages to 512-byte vmem blocks
	sta first_vmap_entry_in_bank_1

	; Remember the first page used for vmem in bank 1
	lda #>story_start_far_ram
	adc nonstored_pages ; Carry is already clear
	sta vmap_first_ram_page_in_bank_1

	; Calculate how many vmem pages we can fit in bank 1
	lda #VMEM_END_PAGE
	sec
	sbc vmap_first_ram_page_in_bank_1
	lsr ; Convert from 256-byte pages to 512-byte vmem blocks
!ifdef UNDO_RAM {
	sec
	sbc vmap_entries_reserved
	clc
}
	; Now A holds the # of vmem blocks we can fit in bank 1

	adc first_vmap_entry_in_bank_1 ; Add the # of vmem blocks in bank 0
	cmp #vmap_max_size
	bcc +
	lda #vmap_max_size
+	sta vmap_max_entries
	rts
}

!ifndef ACORN {
!ifdef VMEM {
!ifndef NOSECTORPRELOAD {
.progress_suggested !byte 6

load_suggested_pages
; Load all suggested pages which have not been pre-loaded

; Print progress bar
	lda #13
	jsr s_printchar
	lda vmap_used_entries
	sec
	sbc vmap_blocks_preloaded
	tax
-	cpx #6
	bcc .start_loading
	lda #47
	jsr s_printchar
	txa
	sec
	sbc #6
	tax
	bne -
.start_loading
	lda vmap_blocks_preloaded ; First index which has not been loaded
	cmp vmap_used_entries ; Total # of indexes in the list
	bcs +
	sta vmap_index
	tax
	jsr load_blocks_from_index
	dec .progress_suggested
	bne ++
	lda #20
	jsr s_printchar
	lda #6
	sta .progress_suggested
++	inc vmap_blocks_preloaded
	bne .start_loading ; Always branch
+
	ldx vmap_used_entries
	cpx vmap_max_entries
	bcc +
	dex
+	

!ifdef TRACE_VM {
	jsr print_vm_map
}
	rts
} ; ifndef NOSECTORPRELOAD
} ; ifdef VMEM

} ; ifndef ACORN

!ifndef ACORN {
!ifdef UNDO {
print_no_undo
	; ldy #header_flags_2 + 1
	; jsr read_header_word
	; and #(255 - 16) ; no undo
	; ldy #header_flags_2 + 1
	; +write_header_byte
	lda #>.no_undo_msg
	ldx #<.no_undo_msg
	jsr printstring_raw
	jmp wait_a_sec
.no_undo_msg
	!pet "Undo not available",13,13,0
}

NEED_CALC_DYNMEM = 1


!ifndef VMEM {
!ifndef TARGET_MEGA65_OR_X16 {
SIMPLE_NON_VMEM = 1
}
}

; !ifdef TARGET_MEGA65 {
; NEED_CALC_DYNMEM = 1
; }
; !ifdef VMEM {
; NEED_CALC_DYNMEM = 1
; }

!ifdef NEED_CALC_DYNMEM {
calc_dynmem_size
	; Store the size of dynmem AND (if VMEM is enabled)
	; check how many z-machine memory blocks (256 bytes each) are not stored in raw disk sectors
!ifdef TARGET_C128 {
	; Special case because we need to read a header word from dynmem before dynmem
	; has been moved to its final location.
	lda story_start + header_static_mem
	ldx story_start + header_static_mem + 1
} else {
	; Target is not C128
	ldy #header_static_mem
	jsr read_header_word
}
	stx dynmem_size
	sta dynmem_size + 1

!ifndef SIMPLE_NON_VMEM {	
;!ifdef TARGET_MEGA65 {
	tay
	cpx #0
	beq .maybe_inc_nonstored_pages
	iny ; Add one page if statmem doesn't start on a new page ($xx00)
.maybe_inc_nonstored_pages
	tya
!ifdef TARGET_MEGA65 {
	and #%00000001 ; keep index into kB chunk
} else ifdef TARGET_X16 {
	and #%00000001 ; keep index into kB chunk
} else {
	and #vmem_indiv_block_mask ; keep index into kB chunk
}
	beq .store_nonstored_pages
	iny
.store_nonstored_pages
	sty nonstored_pages
;}
}
	rts
}
} ; not ACORN

	
program_end

; SF: It can be helpful for testing paged RAM builds to burn some non-paged RAM.
!ifdef WASTE_BYTES {
    !fill WASTE_BYTES
}

!ifdef USE_HISTORY {
high_history_start
}

	!align 255, 0, 0

!if z_trace_size > 0 {
!ifdef USE_HISTORY {
; SF: If z_trace_size > 0, we may miss out on a chance to make high_history use
; a larger space between the end of z_trace_page and stack_start, but that's not
; really a big deal and it isn't worth complicating the logic for.
high_history_end
}
z_trace_page
	!fill z_trace_size, 0
}

!ifdef VMEM {
    ; The stack needs to be page-aligned. If we're using vmem, data_start needs
    ; to be at a 512-byte boundary. We don't want a gap between the stack and
    ; data_start because it will increase the size of saved games and mean
    ; they're only compatible with builds of Ozmoo with the same alignment
    ; padding, so we put the gap here.
    ; SFTODODATA-ISH MAYBE SORT OF USABLE TO SQUEEZE SOMETHING IN
    !if (stack_size & $100) = 0 {
        ; Stack is an even number of pages, so this must be a 512-byte boundary.
        !align 511, 0, 0
    } else {
        ; Stack is an odd number of pages, so this must not be a 512-byte boundary.
        !align 511, 256, 0
    }
}

!if z_trace_size = 0 {
!ifdef USE_HISTORY {
high_history_end
}
}

; On Acorn, we always have at least USE_HISTORY bytes allocated at
; low_history_start, so we never deliberately allocate any memory for history
; here. However, if alignment requirements happen to give us space for a larger
; history buffer here anyway, we use it. (Of course, it's possible that if we'd
; only known we were going to use a high history buffer, we could have allocated
; other stuff in low memory - but there's not really much we can do, as this is
; a bit recursive.)
!ifdef USE_HISTORY {
	!if (* - high_history_start) > (low_history_end - low_history_start) {
		history_start = high_history_start
		history_end = *
	} else {
		history_start = low_history_start
		history_end = low_history_end
	}

    !if history_end - history_start < 255 {
        history_size = history_end - history_start
    } else {
        history_size = 255  ; max size of history buffer
    }
	!if history_size < USE_HISTORY {
		!error "Not enough space allocated for history"
	}

    history_lastpos = history_size -1 ; last pos (size of history buffer - 1)
}

stack_start

!ifdef TARGET_X16 {
!ifdef CUSTOM_FONT {
fontfile_name !pet "[font]"
fontfile_name_len = * - fontfile_name

font_read_error
	lda #ERROR_FLOPPY_READ_ERROR
	jmp fatalerror
}
}

deletable_screen_init_1
	; start text output from bottom of the screen

!ifdef TARGET_X16 {
!ifdef CUSTOM_FONT {
	lda #fontfile_name_len
	ldx #<fontfile_name
	ldy #>fontfile_name
	jsr kernal_setnam ; call SETNAM
	lda #2      ; file number 2
	ldx #8
	ldy #2      ; secondary address
	jsr kernal_setlfs ; call SETLFS
	lda #3 ; Load into VRAM, bank 1
	ldx #$00
	ldy #$f0
	jsr kernal_load
	php
	jsr close_io
	plp
	bcs font_read_error
}
}

!ifndef Z4PLUS {
	!ifdef TARGET_C128 {
		bit COLS_40_80
		bpl .width40
		; 80 col
		lda #52
		sta sl_score_pos
		lda #66
		sta sl_moves_pos
		lda #60
		sta sl_time_pos
.width40
		; Default values are correct, nothing to do here.
	} else {
        !ifdef ACORN {
            !ifdef SUPPORT_80COL {
                lda s_screen_width
                cmp #80
                bne .width40
                ; 80 col
!ifdef WANT_SCORE_GAME {
                lda #54
                sta sl_score_pos
                lda #67
                sta sl_moves_pos
}
!ifdef WANT_TIME_GAME {
                lda #64
                sta sl_time_pos
}
.width40
                ; Default values are correct, nothing to do here.
            }
        }
    }
}
	+init_screen_model
	rts

z_init
!zone z_init {

!ifdef DEBUG {
!ifdef PREOPT {
	jsr print_following_string
	!text "*** vmem optimization mode ***",13,13,0
}
}


!ifdef Z4PLUS { ; SF: I added this, maybe suggest to upstream
	lda #0
	jsr set_z_exe_mode ; 0 = Normal
}
	
!ifdef TRACE {
	; Setup trace
	lda #0
	sta z_trace_index
	tay
-	sta z_trace_page,y
	iny
	bne -
}

	; Calculate Z7 string offset and routine offset
!ifdef Z7 {
	jsr calc_z7_offsets
}
	
	; Modify header to tell game about terp capabilities
!ifndef Z4PLUS {
	ldy #header_flags_1
	jsr read_header_word
	and #(255 - 16 - 64) ; Statusline IS available, variable-pitch font is not default
	ora #32 ; Split screen available
	+write_header_byte
!ifdef SOUND {
	jsr init_sound
}
} else {
!ifdef Z4 {
	ldy #header_flags_1
	jsr read_header_word
	and #(255 - 4 - 8) ; bold font, italic font not available
	ora #(16 + 128) ; Fixed-space style, timed input available
	+write_header_byte
} else { ; Z5PLUS
	ldy #header_flags_1
	jsr read_header_word
    !ifndef ACORN {
        COLOUR = 1
    } else {
        COLOUR = 0
    }
	and #(255 - 4 - 8 - (1 - COLOUR)) ; bold font, italic font not available
	ora #(COLOUR + 16 + 128) ; Colours, Fixed-space style, timed input available
	+write_header_byte

; check_undo
	ldy #header_flags_2 + 1
	jsr read_header_word
	and #(255 - 8 - 32) ; pictures and mouse never available
!ifndef ACORN {
!ifdef UNDO {
	bit reu_bank_for_undo
	bpl .undo_is_available
}
	; Tell game UNDO isn't supported
	and #(255 - 16) ; no undo
.undo_is_available
} else {
	; SF: For the moment, on Acorn we know at build time if undo is supported or not.
	!ifndef UNDO {
		; Tell game UNDO isn't supported
		and #(255 - 16) ; no undo
	}
}
	pha
!ifdef SOUND {
	; check if the game wants to play sounds, and if we can support this
	pla
	pha
	and #$80
	beq + ; Game doesn't want to play sounds
	jsr init_sound
	bcc +
	; No sound files found, so tell game sound isn't supported
	pla
	and #(255 - 128)  ; no sound effect
	pha
+
    pla
	ldy #header_flags_2 + 1
	+write_header_byte
} else {
    pla
	and #(255 - 128)  ; no sound effect
	ldy #header_flags_2 + 1
	+write_header_byte
}
}
}
!ifdef Z4PLUS {
	lda #TERPNO ; Interpreter number (8 = C64)
	ldy #header_interpreter_number 
	+write_header_byte
	lda #(64 + 14) ; "N" = release 14
	ldy #header_interpreter_version  ; Interpreter version. Usually ASCII code for a capital letter
	+write_header_byte
	+lda_screen_height
	ldy #header_screen_height_lines
	+write_header_byte
!ifdef Z5PLUS {
	ldy #header_screen_height_units
	tax
	lda #0
	jsr write_header_word
}
; SFTODO: I wonder if this C128-specific code offers me a hint; I *suspect* C128 is the only upstream platform with a *variable* screen width, so I may need to follow C128-style code path for Acorn port in this respect. Just a thought, middle of initial merge right now so not investigated.
!ifdef TARGET_C128 {
	jsr update_screen_width_in_header
} else ifdef TARGET_X16 {
	jsr update_screen_width_in_header
} else {
	+lda_screen_width
	ldy #header_screen_width_chars
	+write_header_byte
!ifdef Z5PLUS {
	ldy #header_screen_width_units
	tax
	lda #0
	jsr write_header_word
}
} ; End not TARGET_C128
} ; End Z4PLUS
	lda #0 ; major standard revision number which this terp complies to
	tax    ; minor standard revision number which this terp complies to
	ldy #header_standard_revision_number
	jsr write_header_word

!ifdef Z5PLUS {
	lda #1
	ldy #header_font_width_units
	+write_header_byte
	ldy #header_font_height_units
	+write_header_byte
	; TODO: Store default background and foreground colour in 2c, 2d (or comply to game's wish?)
	
	; Copy alphabet pointer from header, or default
	ldy #header_alphabet_table
	jsr read_header_word
	cmp #0
	bne .custom_alphabet
	cpx #0
	beq .store_alphabet_pointer
.custom_alphabet
	jsr set_z_address
	ldy #0
-	jsr read_next_byte
	sta z_alphabet_table,y
	iny
	cpy #26*3
	bcc -
.store_alphabet_pointer
}
;	ldx #<default_alphabet
;	ldy #>default_alphabet
;.store_alphabet_pointer
;	stx alphabet_table
;	sty alphabet_table + 1
	
	; Copy z_pc from header
	ldy #header_initial_pc
	jsr read_header_word
	pha
	txa
	tay
	pla
	tax
	lda #0
!ifndef VMEM {
	sta z_pc
}
	jsr set_z_pc
;	jsr get_page_at_z_pc NOT NEEDED - DONE AT THE END OF set_z_pc

	; Setup globals pointer
	ldy #header_globals
	jsr read_header_word
	tay
	txa
	clc
!ifndef ACORN_ABSOLUTE_GLOBALS {
!ifdef FAR_DYNMEM  {
	adc #<(story_start_far_ram - 32)
	sta z_low_global_vars_ptr
	sta z_high_global_vars_ptr
	tya
	adc #>(story_start_far_ram - 32)
} else {
	adc #<(story_start - 32)
	sta z_low_global_vars_ptr
	sta z_high_global_vars_ptr
	tya
	adc #>(story_start - 32)
}
	sta z_low_global_vars_ptr + 1
	adc #1
	sta z_high_global_vars_ptr + 1
}

!ifndef ACORN {
!ifdef HAS_SID {
	jsr init_sid
}
!ifdef TARGET_PLUS4 {
	lda #0
	sta ted_volume
}
}

	
!ifdef BENCHMARK {
	lda #$ff
	ldx #$80
	ldy #1
	jmp z_rnd_init
} else {
	jmp z_rnd_init_random
}
}

!ifndef ACORN {
	!error "Lots of non-ACORN code has been removed here"
}

!ifdef VMEM {

prepare_static_high_memory
; SFTODO: We're very close to being able to just remove prepare_static_high_memory on Acorn, including the jsr to it
!ifdef PREOPT {
    ; vmap_used_entries can't be 0. SFTODO: I think? Note that upstream commit d62112e (which I have merged; at worst it only slightly bloats PREOPT builds) fixed a bug where vmap_used_entries
    ; was 0, so it *may* be that this would now be acceptable and we'd avoid wasting the zeroth entry in vmap for PREOPT
    ; builds. However, PREOPT obviously worked on upstream before this fix, so it may be that's not directly related and
    ; having vmap_used_entries be 0 could still cause problems elsewhere. For now I am going to leave this along but at
    ; some point it's probably worth re-examining all the code and seeing if we can set vmap_used_entries to 0 safely here.

    lda #1
    sta vmap_used_entries
} else {
    ; +acorn_deletable_init_inline will initialise vmap_used_entries. SFTODO: DOESN'T EXIST NOW, TWEAK COMMENT
}
!ifdef TRACE_VM {
	jsr print_vm_map
}
	rts

} ; End of VMEM

.initialize_continued
	jsr deletable_init ; this will load game data at story_start
	jsr parse_object_table
!ifndef Z5PLUS {
	; Setup default dictionary
	jsr parse_default_dictionary
}

!ifdef Z5PLUS {
	; set up terminating characters
	jsr parse_terminating_characters
}

	; jsr streams_init - on Acorn this is done from acorn-init-preload.asm

	; jsr stack_init - this is done at the start of deletable_screen_init_2 on Acorn

	jsr deletable_screen_init_2

	jsr z_init

!ifdef SCROLLBACK {
	lda scrollback_supported
	sta scrollback_enabled
}

	; On Acorn we don't use z_exe_mode_exit, so z_execute can't return. We
	; therefore jmp to it; this saves two bytes of stack which just might be
	; helpful and has no real downside.
	jmp z_execute

!ifdef ACORN {
	!source "acorn-init-stack.asm"
}

end_of_routines_in_stack_space

; Optionally waste the rest of the space in the stack so any code which isn't
; protected by our build-time check on end_of_routines_in_stack_space will be
; forced into memory where it gets overwritten as the game data loads.
!ifdef ACORN_DEBUG_PRELOAD_CODE {
	!fill (stack_start + stack_size) - end_of_routines_in_stack_space, 0
}

; SFTODO: It would probably be cleaner to move .initialize into acorn-init-preload.asm
; SF: This code has been moved within this file compared to the Commodore
; version. This allows us to save approximately 21 bytes by making the first
; part of .initialize discardable initialization code.
.initialize
!ifdef ACORN_RELOCATABLE {
initialize
}
    ; Reset the stack pointer; setjmp relies on this.
    ldx #$ff
    txs
!ifdef TESTSCREEN {
	jmp testscreen
}

	jsr deletable_init_start
;	jsr init_screen_colours
	jsr deletable_screen_init_1
	jmp .initialize_continued

    ; It's fine for code to spill over past story_start *as long as it's going
    ; to be executed before it gets overwritten*. We don't have any preload data
    ; attached, unlike the C64, so this doesn't cause problems.
	!source "acorn-init-preload.asm"
!ifdef ACORN_RELOCATABLE {
    ; This must be the last thing in the executable.
	ACORN_RELOCATE_WITH_DOUBLE_PAGE_ALIGNMENT = 1
    !source "acorn-relocate.asm"
}
end_of_routines

	!if (end_of_routines_in_stack_space - stack_start) > stack_size {
		!error "Routines in stack space have overflowed stack by ", end_of_routines_in_stack_space - stack_start - stack_size, " bytes"
	}
	data_start = stack_start + stack_size
	!ifdef VMEM {
		!if (data_start & 0x1ff) != 0 {
			!error "data_start must be at a 512-byte boundary"
		}
	}
	!ifndef ACORN_SWR_MEDIUM_DYNMEM {
		story_start = data_start
	} else {
		story_start = $8000
		vmem_start = data_start
	}

	!align 255, 0, 0
scratch_overlapping_game_start


; SFTODO: MODE_7_STATUS and MODE_7_INPUT should probably have ACORN_ prefix.

; SFTODO: Don't forget the transient command workspace at &A8 is available for short-term use.

; SFTODO: Possibly "too slow" and there may be other issues, but JGH's "portable ROM paging" trick (https://stardot.org.uk/forums/viewtopic.php?p=345669&sid=53743ef3b22a3ea1ccfd5e32b8cd1ddf#p345669) just might make it more practical to share an executable between Electron and BBC. In any event, if this is otherwise attractive, don't write it off without doing some timing - Ozmoo does page a lot, but it is moderately optimised and it would be best not to assume. (Although there is some overlap here with the idea of having a "page_in_rom_a" subroutine which the loader sets up or which the initialisation code patches to do the right thing for the current host. And JGH's trick corrupts a lot of registers, which is not ideal for Ozmoo I suspect. Still, it would probably be worth replacing STA &F4:STA &FE30 (and similarly for other registers) with JSR page_in_rom_a where page_in_rom_a is STA &F4:STA &FE30:RTS and seeing how that affects performance. There *might* be a worthwhile saving in code size which would help pay for any slowdown. Also remember there's another TODO knocking around somewhere about using the standard "BBC" sequence inline but dynamically patching it to JSR on systems needing a different and longer (thus not patchable in place) paging sequence.)

; SFTODO: It may be possible to support multiple different SWR paging methods without too much overhead *if* we can distinguish "read only" SWR paging from "(potentially) read/write" SWR paging. I am thinking more Watford/Solidisk SWR rather than BBC-vs-Electron here. Obviously read only paging *has* to work in the standard way for all SWR, otherwise the OS couldn't page it in correctly. *If* we can easily distinguish potential read/write paging *and* that's relatively uncommon, the overhead of using a subroutine (which we could patch in discardable init/via a "SWR driver" a bit like shadow driver) to do paging when we need read/write might not be particularly noticeable. (It might be significant though.)

; In acorn-disk.asm we truncate this sum to 16 bits to avoid build errors, but
; if the truncation actually has any effect the game is broken. We can't
; generate this error at that point because story_start isn't defined there on
; the first pass, so we have to do it here.
!ifdef ACORN_SAVE_RESTORE_OSFILE {
!if story_start + ACORN_DYNAMIC_SIZE_BYTES > $ffff {
    !error "GameWontFit: dynamic memory overflows address space"
}
}
