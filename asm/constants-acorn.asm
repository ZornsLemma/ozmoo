; SFTODONOW: It's not good to have acorn-constants.asm and constants-acorn.asm, but let's see how this experiment goes before tidying that up.

; SFTODONOW: Since this is experimental, *all* SFTODOs should be reviewed to see if they are urgent or not

; Determine vmap_max_size; this code used to live in vmem.asm but it's better to
; put it here so we can use the value of vmap_max_size when allocating low
; memory.

; The Acorn port takes advantage of knowing the game size at build time to avoid
; wasting memory on a vmap_max_size larger than the game will ever need.
!ifndef ACORN_SWR {
    !ifndef ACORN_TUBE_CACHE {
        !ifdef ACORN_TURBO_SUPPORTED {
            ; A turbo second processor has enough RAM to hold 255 512-byte blocks.
            max_vmap_max_size = 255
        } else { ; !ACORN_TURBO_SUPPORTED
            max_vmap_max_size = (flat_ramtop - story_start) / 512
        }
    } else { ; ACORN_TUBE_CACHE
        ; The host cache is initialised using "extra" entries in the vmap.
        max_vmap_max_size = 255
        !ifndef ACORN_TURBO_SUPPORTED {
            ; During execution (after the initial preload of the host cache),
            ; vmap_max_entries only covers the second processor's own 64K, so we
            ; don't need large vmap support.
            ACORN_SMALL_RUNTIME_VMAP = 1
        }
    }
} else { ; ACORN_SWR
    ; We might have enough main+sideways RAM to hold 255 512-byte blocks.
    max_vmap_max_size = 255
}

; Set vmap_max_size = min(max_vmap_max_size, ACORN_VMEM_BLOCKS) - there's no
; point allocating space for more vmem blocks than the game can ever use.
!if max_vmap_max_size < ACORN_VMEM_BLOCKS {
    vmap_max_size = max_vmap_max_size
} else {
    vmap_max_size = ACORN_VMEM_BLOCKS
}

; Finished setting vmap_max_size.

zp_constant_ptr = $00
low_constant_ptr = $400
high_constant_ptr = *

* = $02

!macro allocate_zp n {
	* = * + n
}

z_opcode	+allocate_zp 1
mempointer	+allocate_zp 2

; --- ZERO PAGE --
; available zero page variables (pseudo registers)
;z_opcode              = $02
;mempointer            = $03 ; 2 bytes
mem_temp              = $05 ; 2 bytes
z_extended_opcode	  = $07

mempointer_y          = $08 ; 1 byte
z_opcode_number       = $09
zp_pc_h               = $0a
zp_pc_l               = $0b
z_opcode_opcount      = $0c ; 0 = 0OP, 1=1OP, 2=2OP, 3=VAR
z_operand_count		  = $0d
zword				  = $0e ; 6 bytes

zp_mempos             = $14 ; 2 bytes

z_operand_value_high_arr = $16 ; !byte 0, 0, 0, 0, 0, 0, 0, 0
z_operand_value_low_arr = $1e ;  !byte 0, 0, 0, 0, 0, 0, 0, 0

;
; NOTE: This entire block of variables, except last byte of z_pc_mempointer
; and z_pc_mempointer_is_unsafe is included in the save/restore files
; and _have_ to be stored in a contiguous block of zero page addresses
;
	z_local_vars_ptr      = $26 ; 2 bytes
	z_local_var_count	  = $28
	stack_pushed_bytes	  = $29 ; !byte 0, 0
	stack_ptr             = $2b ; 2 bytes
	stack_top_value 	  = $2d ; 2 bytes !byte 0, 0
	stack_has_top_value   = $2f ; !byte 0
	; SF: z_pc is big-endian, z_pc_mempointer is little-endian
	z_pc				  = $30 ; 3 bytes (last byte shared with z_pc_mempointer)
	z_pc_mempointer		  = $32 ; 2 bytes (first byte shared with z_pc)
	zp_save_start = z_local_vars_ptr
	zp_bytes_to_save = z_pc + 3 - z_local_vars_ptr
;
; End of contiguous zero page block
;
;

!ifdef PREOPT {
HAVE_VMAP_USED_ENTRIES = 1
}

zchar_triplet_cnt	  = $35
packed_text			  = $36 ; 2 bytes
alphabet_offset		  = $38
escape_char			  = $39
escape_char_counter	  = $3a
abbreviation_command  = $40

parse_array           = $41 ; 2 bytes
string_array          = $43 ; 2 bytes
;terminators_ptr       = $45 ; 2 bytes

z_address			  = $45 ; 3 bytes
z_address_temp		  = $48

object_tree_ptr       = $49 ; 2 bytes
object_num			  = $4b ; 2 bytes
object_temp			  = $4d ; 2 bytes

; SF: On the Acorn port, the vmap starts off full unless we are doing a PREOPT
; build, so vmap_used_entries == vmap_max_entries at all times. We use
; vmap_max_entries everywhere and don't allocate a byte anywhere for
; vmap_used_entries if it isn't used.
vmap_max_entries	  = $4f

z_low_global_vars_ptr	  = $50 ; 2 bytes
z_high_global_vars_ptr	  = $52 ; 2 bytes
z_exe_mode	  		  = $55

stack_tmp			  = $56; ! 5 bytes
default_properties_ptr = $5b ; 2 bytes
zchars				  = $5d ; 3 bytes

; SF: I experimented with increasing vmap_quick_index_length to see if it helps
; on "big" systems with the maximum supported 144K of sideways RAM. Upping it
; to 12 knocked 0.9% off the run time for the benchmark; given the scarcity of
; zero page on Acorn non-tube builds, I don't think this is worth having.
; SFTODO: Could/should we use a larger vmap_quick_index_length on a second
; processor build? This might be particularly valuable on "big" games with a
; larger working set which might benefit from the turbo variant. Be careful
; though, it's possible that increasing this too much may harm performance,
; though my gut feeling is that even if it doesn't help much it won't hurt in
; practice. Still, it would be one way to use the extra ZP on a second processor.
; SFTODO: It might be worth making vmap_quick_index_length variable at runtime
; based on how much RAM we actually have. (It is used in few enough places the
; initialisation code could patch the code up.)
vmap_quick_index_match= $60
vmap_next_quick_index = $61
vmap_quick_index	  = $62 ; Must follow vmap_next_quick_index!
vmap_quick_index_length = 6 ; Says how many bytes vmap_quick_index_uses

z_temp				  = $68 ; 12 bytes

; SFTODO: I should look at tidying up the ACORN conditional stuff in this file
; to try to "match" the Commodore platforms later (although this may not be
; trivial, because those platforms probably share some common details which
; the Acorn port doesn't), but let's not worry about that for now.

; SFTODO: Is it OK to use 162 bytes of the stack like this? In practice we
; certainly seem to get away with it, and my brief experiments when I
; implemented setjmp suggest Ozmoo won't ever get near 64 bytes of stack use,
; but (particularly if we have some otherwise wasted space floating around due
; to page alignment issues) it might be worth relocating at least one of these
; buffers.
; SFTODO: I think I can and probably should move these buffers to the *top* of
; the stack and initialise the stack pointer to start just below them when we
; intialise. This will burn about three bytes of totally discardable init code,
; so it's virtually free, and it then means we are morally in the clear - Ozmoo
; then uses "quite a lot" of stack (but it is the foreground application) and if
; a filing system or interrupt handler goes bananas and uses loads of stack it
; will cause a wrap (its fault!) rather than trampling over our data not 
; protected by being above the stack pointer (our fault). It is worth noting
; this is a temporary buffer used only during printing, so there shouldn't be
; any filing system calls, and an interrupt handler is unlikely to go crazy with
; stack use, but even so, making this change would be slightly better I think.
; (It *might* also remove the need for relocating these when using ACORN_OSRDCH,
; though it's probably safest not to let the stack get "too full" if we can
; help it.)
!ifndef ACORN_OSRDCH {
; SFTODO: Not specifically related to print_buffer etc, but do I need to tweak anything because SCREEN_WIDTH is variable at runtime on some Acorn builds? I don't know if it's variable at runtime on any Commodore platforms.
print_buffer		  = $100 ; SCREEN_WIDTH + 1 bytes
print_buffer2		  = $151 ; SCREEN_WIDTH + 1 bytes
}

; SFTODO: It might be worth having a constants-acorn.asm (note there is a constants-c128.asm - is that *in addition* to this, or a complete alternative to this?). Not necessarily just for stuff in the following block.

; Acorn OS and hardware constants
brkv = $202
wrchv = $20e
keyv = $228
error_ptr = $fd
osfind = $ffce
osgbpb = $ffd1
osbput = $ffd4
osfile = $ffdd
osrdch = $ffe0
osasci = $ffe3
osnewl = $ffe7
oswrch = $ffee
osword = $fff1
osbyte = $fff4
oscli = $fff7
vdu_back = 8
vdu_down = 10
vdu_cls = 12
vdu_cr = 13
vdu_set_text_colour = 17
vdu_redefine_colour = 19
vdu_disable = 21
vdu_set_mode = 22
vdu_miscellaneous = 23
vdu_reset_text_window = 26
vdu_escape = 27
vdu_define_text_window = 28
vdu_home = 30
vdu_goto_xy = 31
osfile_save = $00
osfile_load = $ff
osfile_read_catalogue_information = $05
osfind_close = $00
osfind_open_input = $40
osfind_open_output = $80
osgbpb_write_ignoring_ptr = $02
osgbpb_read_using_ptr = $03
osgbpb_read_ignoring_ptr = $04
osword_input_line = 0
osword_read_clock = 1
osword_write_host = 6
osword_sound = 7
osword_floppy_op = $7f
!ifdef ACORN_TUBE_CACHE {
osword_cache_op = $e0 ; USERV OSWORD
osword_cache_no_timestamp_hint = $ff
}
osbyte_set_cursor_editing = $04
osbyte_wait_for_vsync = $13
osbyte_flush_buffer = $15
osbyte_read_vdu_status = $75
osbyte_reflect_keyboard_status = $76
osbyte_acknowledge_escape = $7e
osbyte_read_key = $81
osbyte_read_oshwm = $83
osbyte_read_screen_address_for_mode = $85
osbyte_read_cursor_position = $86
osbyte_read_screen_mode = $87 ; SFTODO: RENAME TO INDICATE CHAR RETURNING FEATURE?
!ifdef ACORN_TUBE_CACHE {
osbyte_initialise_cache = $88 ; USERV *CODE/OSBYTE
}
osbyte_enter_language = $8e
osbyte_read_vdu_variable = $a0
osbyte_rw_function_key_status = $e1
osbyte_rw_shift_function_key_status = $e2
osbyte_rw_escape_key = $e5
osbyte_read_language = $fc
vdu_variable_text_window_bottom = $09
vdu_variable_text_window_top = $0b
vdu_status_cursor_editing = 1<<6
cr = 13
del = 127
mode_7_text_colour_base = 128
mode_7_graphics_colour_base = 144
err_not_found = $d6
ctrl_key_adjust = 64
max_screen_width = 80
!ifdef ACORN_SWR {
flat_ramtop = $8000
swr_ramtop = $c000
} else {
flat_ramtop = $f800
}
!ifdef ACORN_SHADOW_VMEM {
shadow_start = $3000
}

; Acorn memory allocations
; SFTODO: It might be worth reordering/reallocating these so the order is a
; bit more logical.

zp_temp               = $75 ; 5 bytes
; SF: cursor_{row,column} are used to hold the cursor positions for the two
; on-screen windows. They mainly come into play via save_cursor/restore_cursor;
; the active cursor position is zp_screen{row,column} and that's all that
; matters most of the time.
cursor_row            = $7a ; 2 bytes
cursor_column         = $7c ; 2 bytes
!if 0 { ; SFTODO: These zp locations can be re-used now; I suspect (but obviously can test) there's no real value to using zp for text output related things
;screen_width          = $54 ; 1 byte ; SFTODO: I have re-used this one already
;screen_height         = $89 ; 1 byte ; SFTODO: I have re-used this one already
;screen_height_minus_1 = $8a ; 1 byte ; SFTODO: I have re-used this one already
}
mempointer_ram_bank = $8a ; 1 byte SFTODO: have experimentally moved this into zp since I had this space free, it's not necessarily that worthwhile

vmem_temp			  = $00 ; 2 bytes
!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
; These zero-page addresses are only used very briefly to store temporary values
; in. We use some of the transient command workspace at $a8; this is safe as
; there will be no service calls of any kind while the values stored here are
; live, and it's less likely to create subtle bugs than re-using other Ozmoo
; zero page.
screen_hole_zp_ptr    = $a8 ; 2 bytes
screen_hole_tmp       = $aa ; 1 byte
; SFTODO: This address is *probably* less performance critical, but since we now
; have plenty of zp available thanks to using the transient command workspace,
; there's no point wasting code/cycles on a non-zp address. It may be better to
; rename this screen_hole_tmp2 or screen_hole_tmp+1 or something later.
screen_hole_tmp_slow  = $ab ; 1 byte
}
!ifdef MODE_7_INPUT {
; This overlaps screen_hole_zp_ptr but that's fine; this is transient workspace
; and can't be relied on to hold values for long anyway.
mode_7_input_tmp = $a8 ; 1 byte
}
!ifndef ACORN_SWR {
!ifdef USE_HISTORY {
; This overlaps the above uses of transient command workspace, but that's fine - the
; whole point is we cannot rely on it to hold values except in the short term. (On
; a second processor this is actually our zero page, but we're treating it as if it's
; short-term only just as it is on the host.)
osbyte_set_cursor_editing_tmp = $a8 ; 5 bytes
}
}
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
dynmem_ram_bank       = $89; 1 byte
}
; alphabet_table		  = $7e ; 2 bytes SFTODO: This is no longer in ZP on Commodore, this means I have two bytes of zp free - at some point I will need to tidy up the ZP allocation anyway - I have now used $7e FWIW, $7f has also now been used but it's only experimental

window_start_row	  = $80; 4 bytes

current_window		  = $74 ; !byte 0

is_buffered_window	  = $7e;  !byte 1

; Screen kernal stuff. Must be kept together or update s_init in screenkernal.
s_ignore_next_linebreak = $84 ; 3 bytes
s_reverse 			  = $87 ; !byte 0
s_os_reverse          = $88 ; !byte 0

s_cursors_inconsistent = $34 ; !byte 0

max_chars_on_line	  = $8b; !byte 0
buffer_index		  = $8c ; !byte 0
last_break_char_buffer_pos = $8d ; !byte 0

zp_screencolumn       = $8e ; current cursor column
zp_screenrow          = $8f ; current cursor row

stack = $100

* = low_constant_ptr

!set pending_extra_skip = 0

!macro allocate_fixed n {
	* = * + n
}

!macro skip_fixed_low_allocation addr, n {
	; The logic below to increment pending_extra_skip will only work if calls to
	; this macro are for ascending values of addr, so enforce that.
	!if addr < ascending_check {
		!error "Low allocation checks not in ascending order"
	}
	!set ascending_check = addr

	; At this point, the label for the n bytes has already been allocated, so if
	; the allocation overlaps addr that's not acceptable.
	!if (n != 0) and ((addr >= *) and (addr < (* + n))) {
		!error "Low allocation collision at ", *, " with ", addr
	}

	; If this allocation takes us up to addr, add an extra skip to avoid a
	; collision. SFTODO: This will only avoid a collision if the next allocation
	; is a single byte; we will *detect* such a collision via the code above,
	; and I think we can eliminate it happening by ensuring we always do single
	; byte allocations in the region which has fixed allocations.
	!if (* + n + pending_extra_skip) == addr {
		!set pending_extra_skip = pending_extra_skip + 1
	}
}

!macro allocate_low n {
	!set ascending_check = 0
	!ifdef MODE_7_INPUT {
		+skip_fixed_low_allocation input_colour, n
	}
	!ifdef ACORN_RELOCATABLE {
		+skip_fixed_low_allocation relocate_target, n
	}

	* = * + n + pending_extra_skip
	!set pending_extra_skip = 0
	;!warn "XXX", *
}

; $0400-$046B hold the BASIC resident integer variables. We use some of these
; addresses to pass information from the loader to the Ozmoo executable. Note
; that we don't read/write (e.g.) B% in the BASIC loader directly; the point is
; just that the loader can write values into the addresses which happen to
; correspond to these variables and as long as the loader doesn't use (e.g.) B%
; for anything else we know the values will not be corrupted. We can't do this
; with most of language workspace as BASIC is using it for its own purposes
; while the loader is running.
resident_integer_b = $408
resident_integer_o = $43c ; SFTODO: NOT USED, WE SHOULD PROB ASSERT <= THIS AT SOME POINT
resident_integer_x = $460

; We need to avoid allocating loader-modified addresses with @% and A% as those
; have special meanings in BASIC. To keep the macro complexity down, we just
; assign some addresses which are always needed there.

* = low_constant_ptr
num_rows		+allocate_fixed 1
memory_buffer	+allocate_fixed 7 ; larger on C64, but this is all we use
!if * < resident_integer_b {
	!error "Not enough low allocations to reach B%"
}

; Now allocate loader-written addresses.

; game_data_filename/restart_command have filename_size bytes allocated; we only
; need one or the other in any particular build.
filename_size = 47
game_data_filename_or_restart_command +allocate_fixed filename_size
!if * >= resident_integer_x {
	!error "game_data_filename_or_restart_command not within resident integer space"
}

screen_mode	+allocate_fixed 1
; fg_colour, bg_colour and (if MODE_7_INPUT is defined) input_colour must be
; adjacent and in this order.
fg_colour	+allocate_fixed 1
bg_colour	+allocate_fixed 1

; The next few loader-written values aren't used by all executables; we need them to
; have fixed addresses across the executables which do use them so the loader can
; write to them without making things difficult, so we always skip the space for them
; even if they're not used. allocate_low will re-use the wasted space when allocating.

first_optional_low_address = *

!ifdef MODE_7_INPUT {
input_colour
	!if bg_colour + 1 != input_colour {
		!error "bg_colour and input_colour must be adjacent"
	}
}
	+allocate_fixed 1
!ifdef ACORN_RELOCATABLE {
relocate_target
ozmoo_relocate_target = relocate_target ; SFTODO!?
}
	+allocate_fixed 1

; O% is the first resident integer variable after A% which has a special meaning
; to BASIC and which the loader can't entirely avoid, so if possible (and it is,
; at the moment) we want to avoid spilling into it. SFTODO: If this gets
; problematic, we could probably fairly easily make sure we do all our assembly
; ASAP in the loader before we poke any values into page 4 to pass them to the
; Ozmoo executable.
!if * > resident_integer_o {
	!error "Loader-written values have spilled into O%", *
}

* = first_optional_low_address
+allocate_low 0

; The following allocations are only used by the Ozmoo executable itself, so we
; no longer care about working around BASIC's use of the language workspace.

s_stored_x		+allocate_low 1
s_stored_y		+allocate_low 1

!ifdef TRACE {
z_trace_index	+allocate_low 1
}
!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
sideways_ram_hole_start	+allocate_low 1
sideways_ram_hole_vmem_blocks = 2 ; always 1024 bytes if we have a hole
}
!ifdef HAVE_VMAP_USED_ENTRIES {
; This is used only in PREOPT builds where performance isn't critical so we
; don't waste a byte of zero page on it.
vmap_used_entries	+allocate_low 1
}
!ifdef ACORN_HW_SCROLL {
use_hw_scroll 	+allocate_low 1
}
!ifdef ACORN_TURBO_SUPPORTED {
is_turbo	+allocate_low 1 ; SFTODO: RENAME turbo_flag?
}
cursor_status	+allocate_low 1
!ifdef ACORN_SHADOW_VMEM {
; We use _mem suffixes on these variables to avoid accidental confusion with the
; Commodore values, which are assembly-time constants.
vmem_cache_count_mem	+allocate_low 1
vmem_cache_start_mem	+allocate_low 1
vmem_blocks_in_sideways_ram	+allocate_low 1
vmem_cache_cnt	+allocate_low 1
; We add one in the next line because PAGE alignment may add an extra cache page
; on top of the recommended number of pages.
vmem_cache_page_index
	+allocate_low ACORN_RECOMMENDED_SHADOW_CACHE_PAGES + 1
vmem_cache_page_index_end
}
!ifdef ACORN_SWR {
b_plus_private_ram_size = 12 * 1024 - 512 ; -512 to leave space for shadow copy code
integra_b_private_ram_size = 12 * 1024 - 1024 ; -1024 to leave space for IBOS workspace
; SFTODO: There's a gap here in page 4 now we've stopped storing RAM bank list there; move things up. - this includes $41c which used to be mempointer_ram_bank
vmem_blocks_in_main_ram	+allocate_low 1
vmem_blocks_stolen_in_first_bank	+allocate_low 1
z_pc_mempointer_ram_bank = $7f ; 1 byte SFTODO EXPERIMENTAL ZP $41f ; 1 byte SFTODO: might benefit from zp? yes, bigdynmem builds do use this in fairly hot path (and it's also part of macros so it might shrink code size) - savings from zp not going to be huge, but not absolutely negligible either
jmp_buf_ram_bank 	+allocate_low 1
}

; SFTODO: Not too happy with this, but it will do for now - I do need to tidy all this up at some point
!ifdef MODE_7_INPUT {
input_colour_code_or_0	+allocate_low 1
}

; SFTODO: THESE MEMORY ALLOCATIONS ARE MESSY
!ifdef ACORN_SCREEN_HOLE {
acorn_screen_hole_start_page	+allocate_low 1
acorn_screen_hole_start_page_minus_one = $54
acorn_screen_hole_pages	+allocate_low 1; SFTODO: PROB NOT GOING TO BENEFIT FROM ZP BUT MAYBE TRY IT
acorn_screen_hole_pages_minus_one +allocate_low 1 ; SFTODO: PROB NOT GOING TO BENEFIT FROM ZP BUT MAYBE TRY IT
}

game_disc_crc	+allocate_low 2
initial_clock	+allocate_low 5

jmp_buf_size = 32 ; SFTODO: this could possibly be squeezed a bit lower if necessary
jmp_buf	+allocate_low jmp_buf_size

; vmap_z_l is at $501 because we use "vmap_z_l-1,x" addressing in a hot loop and
; we want to avoid any page-crossing penalty.
vmap_z_l = $501

; SFTODONOW: I SUSPECT I WANT TO BE FANCIER ABOUT HISTORY SO I CAN SQUEEZE SOME OTHER STUFF INTO PAGE 4 DEPENDING ON MIN HISTORY SIZE WE WILL TOLERATE
!ifdef USE_HISTORY {
low_history_end = vmap_z_l
!if * >= low_history_end {
	!error "No space for low_history"
}
low_history_start
	+allocate_low low_history_end - *
}

; The progress_indicator_* variables can re-use the space at
; z_operand_value_high_arr; they're only used during the initial loading when
; the Z-machine has not been set up.
; SFTODONOW: Move these assignments into the progress indicator code?
progress_indicator_blocks_per_step = z_operand_value_high_arr ; 2 bytes
progress_indicator_blocks_until_next_step = z_operand_value_high_arr + 2 ; 2 bytes

scratch_page = $600
!ifdef ACORN_SWR {
scratch_double_page = scratch_page
} else {
; Second processor builds load at $700, so this page isn't wasted.
}

!ifdef ACORN_TURBO_SUPPORTED {
; SFTODO: It might be possible to use an entire 64K bank for dynmem on a turbo copro,
; instead of using what's left of bank 0 after the OS and Ozmoo binary take their share.
turbo_control = $fef0
turbo_bank_base = $301
mempointer_turbo_bank = turbo_bank_base + mempointer
z_pc_mempointer_turbo_bank = turbo_bank_base + z_pc_mempointer
}
!ifdef TRACE_SETJMP {
; This address is owned by Econet but this is debug-only code.
setjmp_min_s = $90
}

!ifndef ACORN_SWR {
; On a second processor zero page is available up to but not including $ee. SFTODO?
}

* = high_constant_ptr
