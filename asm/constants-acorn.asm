; Acorn version of Commodore constants.asm.

; Note that this may allocate some constant storage at *, so make sure it's
; only sourced where this is acceptable. SFTODO!

; SFTODONOW: It's not good to have acorn-constants.asm and constants-acorn.asm, but let's see how this experiment goes before tidying that up.

; SFTODONOW: Since this is experimental, *all* SFTODOs should be reviewed to see if they are urgent or not

; Determine vmap_max_size; this code used to live in vmem.asm but it's better to
; put it here so we can use the value of vmap_max_size when allocating low
; memory.
!ifdef VMEM {

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

}
; Finished setting vmap_max_size.

high_constant_ptr = *

; Zero page allocations

zp_start = $00
!ifndef ACORN_SWR {
	zp_end = $ee
} else {
	zp_end = $90
}

* = zp_start

!macro allocate_zp n {
	!if n < 1 {
		!error "Bad allocate_zp size"
	}
	* = * + n
	!if * > zp_end {
		!error "Out of zero page"
	}
}

z_opcode	+allocate_zp 1
mempointer	+allocate_zp 2
mem_temp	+allocate_zp 2
z_extended_opcode	+allocate_zp 1

mempointer_y +allocate_zp 1
z_opcode_number	+allocate_zp 1
zp_pc_h +allocate_zp 1
zp_pc_l	+allocate_zp 1
z_opcode_opcount	+allocate_zp 1; 0 = 0OP, 1=1OP, 2=2OP, 3=VAR
z_operand_count	+allocate_zp 1
zword	+allocate_zp 6

zp_mempos	+allocate_zp 2

z_operand_value_high_arr	+allocate_zp 8
z_operand_value_low_arr	+allocate_zp 8

;
; NOTE: This entire block of variables, except last byte of z_pc_mempointer
; and z_pc_mempointer_is_unsafe is included in the save/restore files
; and _have_ to be stored in a contiguous block of zero page addresses
;SFTODO: z_pc_mempointer_is_unsafe doesn't exist?!
;
z_local_vars_ptr	+allocate_zp 2
z_local_var_count	+allocate_zp 1
stack_pushed_bytes	+allocate_zp 2
stack_ptr	+allocate_zp 2
stack_top_value	+allocate_zp 2
stack_has_top_value	+allocate_zp 1
; SF: z_pc is big-endian, z_pc_mempointer is little-endian
z_pc ; 3 bytes (last byte shared with z_pc_mempointer)
	+allocate_zp 2
z_pc_mempointer ; 2 bytes (first byte shared with z_pc)
	+allocate_zp 2
zp_save_start = z_local_vars_ptr
zp_bytes_to_save = z_pc + 3 - z_local_vars_ptr
;
; End of contiguous zero page block
;
;

; SFTODO: MOVE THIS (EVEN IF ONLY WITHIN THIS FILE) NOW?
!ifdef PREOPT {
HAVE_VMAP_USED_ENTRIES = 1
}

zchar_triplet_cnt	+allocate_zp 1
packed_text	+allocate_zp 2
alphabet_offset	+allocate_zp 1
escape_char	+allocate_zp 1
escape_char_counter	+allocate_zp 1
abbreviation_command +allocate_zp 1

parse_array	+allocate_zp 2
string_array	+allocate_zp 2

z_address	+allocate_zp 3
z_address_temp	+allocate_zp 1

object_tree_ptr	+allocate_zp 2
object_num	+allocate_zp 2
object_temp	+allocate_zp 2

; SF: On the Acorn port, the vmap starts off full unless we are doing a PREOPT
; build, so vmap_used_entries == vmap_max_entries at all times. We use
; vmap_max_entries everywhere and don't allocate a byte anywhere for
; vmap_used_entries if it isn't used.
vmap_max_entries	+allocate_zp 1

z_low_global_vars_ptr	+allocate_zp 2
z_high_global_vars_ptr	+allocate_zp 2
z_exe_mode	+allocate_zp 1

stack_tmp	+allocate_zp 5
default_properties_ptr	+allocate_zp 2
zchars	+allocate_zp 3

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
vmap_quick_index_match	+allocate_zp 1
vmap_next_quick_index	+allocate_zp 1
vmap_quick_index_length = 6 ; Says how many bytes vmap_quick_index_uses
vmap_quick_index +allocate_zp vmap_quick_index_length ; Must follow vmap_next_quick_index!

z_temp	+allocate_zp 12

zp_temp	+allocate_zp 5
; SF: cursor_{row,column} are used to hold the cursor positions for the two
; on-screen windows. They mainly come into play via save_cursor/restore_cursor;
; the active cursor position is zp_screen{row,column} and that's all that
; matters most of the time.
cursor_row	+allocate_zp 2
cursor_column	+allocate_zp 2
mempointer_ram_bank	+allocate_zp 1 ; SFTODO: have experimentally moved this into zp since I had this space free, it's not necessarily that worthwhile

vmem_temp	+allocate_zp 2
!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
dynmem_ram_bank	+allocate_zp 1
}

window_start_row	+allocate_zp 4

current_window	+allocate_zp 1

is_buffered_window	+allocate_zp 1

; Screen kernal stuff. Must be kept together or update s_init in screenkernal.
s_ignore_next_linebreak	+allocate_zp 3
s_reverse	+allocate_zp 1
s_os_reverse	+allocate_zp 1

s_cursors_inconsistent	+allocate_zp 1

max_chars_on_line	+allocate_zp 1
buffer_index	+allocate_zp 1
last_break_char_buffer_pos	+allocate_zp 1

zp_screencolumn	+allocate_zp 1 ; current cursor column
zp_screenrow	+allocate_zp 1 ; current cursor row

!ifdef ACORN_SWR {
z_pc_mempointer_ram_bank +allocate_zp 1 ; 1 byte SFTODO EXPERIMENTAL ZP $41f ; 1 byte SFTODO: might benefit from zp? yes, bigdynmem builds do use this in fairly hot path (and it's also part of macros so it might shrink code size) - savings from zp not going to be huge, but not absolutely negligible either
}

!ifdef ACORN_SCREEN_HOLE {
acorn_screen_hole_start_page_minus_one +allocate_zp 1
}

; "Transient" zero page allocations. On non-second processor builds, these use the
; OS transient command zero page at $a8-$af inclusive - these addresses cannot be
; trusted to retain their values across * commands and (being paranoid) any service
; call, but they can be used for very short term storage. On second processor builds,
; we just allocate some of the available zero page for this.
; SFTODO: There's no advantage to a small transient_zp_size on non-tube; on tube we could make it smaller depending on MODE_7_INPUT and USE_HISTORY.
transient_zp_size = 5 ; bytes of transient zero page needed
!if transient_zp_size > 8 {
	!error "transient_zp_size is too large"
}
!ifndef ACORN_SWR {
transient_zp	+allocate_zp transient_zp_size
} else {
transient_zp = $a8
}

!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
; These zero-page addresses are only used very briefly to store temporary values
; in. We use some of the transient command workspace at $a8; this is safe as
; there will be no service calls of any kind while the values stored here are
; live, and it's less likely to create subtle bugs than re-using other Ozmoo
; zero page.
; SFTODONOW: NEW HANDLING FOR TRANSIENT ALLOC? AT VERY LEAST MOVE TIL AFTER OTHER ZP ALLOCS?
screen_hole_zp_ptr    = transient_zp ; 2 bytes
screen_hole_tmp       = transient_zp + 2 ; 1 byte
; SFTODO: This address is *probably* less performance critical, but since we now
; have plenty of zp available thanks to using the transient command workspace,
; there's no point wasting code/cycles on a non-zp address. It may be better to
; rename this screen_hole_tmp2 or screen_hole_tmp+1 or something later.
screen_hole_tmp_slow  = transient_zp + 3 ; 1 byte
}
!ifdef MODE_7_INPUT {
; This overlaps screen_hole_zp_ptr but that's fine; this is transient workspace
; and can't be relied on to hold values for long anyway.
mode_7_input_tmp = transient_zp ; 1 byte
}
!ifndef ACORN_SWR {
!ifdef USE_HISTORY {
; This overlaps the above uses of transient command workspace, but that's fine - the
; whole point is we cannot rely on it to hold values except in the short term. (On
; a second processor this is actually our zero page, but we're treating it as if it's
; short-term only just as it is on the host.)
osbyte_set_cursor_editing_tmp = transient_zp ; 5 bytes
}
}

first_spare_zp_address = *

; SFTODONOW: For debuggability, make all non-!ifdef ZP allocations come before all !ifdef-ed ones. (I can't really do this with $400 because of the resident integer variable constraints, but I can in zp.)

; SFTODONOW: With the new allocation it seems there are about 9 bytes of zp free even on non-tube. If I do smart allocation these should get used for *something*, but it may be worth looking around for things which seem like particularly promising candidates for being promoted to zp.


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

stack = $100

!set pending_extra_skip = 0

!macro allocate_fixed n {
	!if n < 1 {
		!error "Bad allocate_fixed size"
	}
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
	!if n < 0 {
		!error "Bad allocate_low size"
	}

	!if (* <= zp_end) and ((* + n) > zp_end) {
		; The label for the n bytes has already been allocated, so if the
		; allocation won't fit in the remaining zero page it's too late to do
		; anything but fail.
		!error "Low allocation would bit split across zp and low memory"
	} else { ; SFTODO: MAKE ELSE CODE ONE LEVEL HIGHER, SINCE IF CASE ERRORS?
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

		; Transition from spare zero page to low memory; this will avoid split
		; errors above if we are only allocating single bytes.
		!if * == zp_end {
			* = first_optional_low_address
			+allocate_low 0
		}
	}
}

; SFTODO: Maybe move these macros lower to nearer their first use?
; SFTODO: It might be possible to make pre_allocate/post_allocate able to
; allocate from low and high memory simultaneously, rather than switching
; permanently to high memory the first time we don't have enough low memory. But
; I will try to avoid adding that extra (albeit possibly relatively minimal)
; complexity right now while all this code is new.

!macro pre_allocate n {
	!if n < 1 {
		!error "Bad pre_allocate size"
	}
	!set pre_allocate_size = n

	!if (* <= zp_end) and ((* + n) > zp_end) {
		; SFTODO: Using first_non_fixed_low_address might waste a byte or two if we didn't actually use all the fixed addresses, but in practice this isn't a huge problem as if we're in this case anyway we're already ahead of the game.
		* = first_non_fixed_low_address
		zero_start = *
	}

	!if (* + n) >= low_memory_upper_bound {
		zero_end = *
		* = high_constant_ptr
		!set low_memory_upper_bound = $ffff
	}
}

!macro post_allocate n {
	!if n != pre_allocate_size {
		!error "Bad post_allocate size"
	}

	!if * >= low_memory_upper_bound {
		; It's important this is zero-filled because acorn_deletable_init_inline
		; will only zero this memory if it's allocated in low memory.
		!fill n, 0
	} else {
		* = * + n
	}
}

!if 0 { ;SFTODO: DELETE?
; We don't know the true value of low_memory_upper_bound until later.
!set low_memory_upper_bound = $ffff
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
resident_integer_o = $43c
resident_integer_x = $460

; We need to avoid allocating loader-modified addresses with @% and A% as those
; have special meanings in BASIC. To keep the macro complexity down, we just
; assign some variables which are needed by all executables there.

* = $400
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

; The next few loader-written values aren't used by all executables; we need
; them to have fixed addresses across the executables which do use them so the
; loader can write to them without making things difficult, so we always make
; space for them even if they're not used. allocate_low will re-use the wasted
; space when it allocates.

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

; SFTODO: Using this address is a bit of a hack, but we need to avoid trampling over the absolutely fixed addresses when we're on a second processor and get a lot of stuff into zero page. (Poor comment, needs tweaking once I have a clearer mental picture of whole thing...)
first_non_fixed_low_address = *

; O% is the first resident integer variable after A% which has a special meaning
; to BASIC and which the loader can't entirely avoid, so if possible (and it is,
; at the moment) we want to avoid spilling into it. SFTODO: If this gets
; problematic, we could probably fairly easily make sure we do all our assembly
; ASAP in the loader before we poke any values into page 4 to pass them to the
; Ozmoo executable.
!if * > resident_integer_o {
	!error "Loader-written values have spilled into O%"
}

* = first_spare_zp_address

; The following allocations are only used by the Ozmoo executable itself, so we
; no longer care about working around BASIC's use of the language workspace. We
; prefer to do single byte allocations first, as not needing to deal well with
; multi-byte allocations simplifies allocate_low.

s_stored_x		+allocate_low 1
s_stored_y		+allocate_low 1
cursor_status	+allocate_low 1

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

!ifdef ACORN_SWR {
b_plus_private_ram_size = 12 * 1024 - 512 ; -512 to leave space for shadow copy code
integra_b_private_ram_size = 12 * 1024 - 1024 ; -1024 to leave space for IBOS workspace
; SFTODO: There's a gap here in page 4 now we've stopped storing RAM bank list there; move things up. - this includes $41c which used to be mempointer_ram_bank
vmem_blocks_in_main_ram	+allocate_low 1
vmem_blocks_stolen_in_first_bank	+allocate_low 1
jmp_buf_ram_bank 	+allocate_low 1
}

!ifdef MODE_7_INPUT {
input_colour_code_or_0	+allocate_low 1
}

; SFTODO: THESE MEMORY ALLOCATIONS ARE MESSY
!ifdef ACORN_SCREEN_HOLE {
acorn_screen_hole_start_page	+allocate_low 1
acorn_screen_hole_pages	+allocate_low 1; SFTODO: PROB NOT GOING TO BENEFIT FROM ZP BUT MAYBE TRY IT
acorn_screen_hole_pages_minus_one +allocate_low 1 ; SFTODO: PROB NOT GOING TO BENEFIT FROM ZP BUT MAYBE TRY IT
}

!ifdef ACORN_SHADOW_VMEM {
; We use _mem suffixes on these variables to avoid accidental confusion with the
; Commodore values, which are assembly-time constants.
vmem_cache_count_mem	+allocate_low 1
vmem_cache_start_mem	+allocate_low 1
vmem_blocks_in_sideways_ram	+allocate_low 1
; vmem_cache_cnt and vmem_cache_page_index must be adjacent in memory.
vmem_cache_cnt ; 1 byte
vmem_cache_page_index = vmem_cache_cnt + 1
; The next line adds 1 byte for vmem_cache_cnt and another 1 byte because PAGE alignment may causes us to use one more shadow cache page than recommended (because that page would be pasted otherwise).
	+allocate_low 1 + ACORN_RECOMMENDED_SHADOW_CACHE_PAGES + 1
vmem_cache_page_index_end
}

!ifdef TRACE_SETJMP {
setjmp_min_s +allocate_low 1
}

game_disc_crc	+allocate_low 2
initial_clock	+allocate_low 5

jmp_buf_size = 32 ; SFTODO: this could possibly be squeezed a bit lower if necessary
jmp_buf	+allocate_low jmp_buf_size

; SFTODONOW: THIS ASSUMES VMEM, BUT LET'S NOT WORRY ABOUT THAT JUST YET....
; We use "vmap-z_l-1,x" addressing in a hot loop and we want to avoid any
; page-crossing penalty. We allocate vmap_z_l as high as possible in page 5 so
; we have the largest possible contiguous space for the allocations we started
; in page 4 to spill over into page 5.
vmap_z_l = $600 - vmap_max_size
!if vmap_z_l < $501 {
	!error "vmap_z_l is too low"
}

!set low_memory_upper_bound = vmap_z_l
!ifdef USE_HISTORY {
	; If we have enough space in low memory for the history, don't allow the
	; following allocations to steal it. If we don't have enough space anyway,
	; allow the following allocations to use everything up to vmap_z_l and we
	; will allocate space for the history in the executable. SFTODONOW: TEST THIS, NEED TO REMEMBER TO SET START=END FOR HIST LOW VARS
	!if * < (low_memory_upper_bound - USE_HISTORY) {
		!set low_memory_upper_bound = low_memory_upper_bound - USE_HISTORY
	}
}
!if * >= low_memory_upper_bound {
	!error "Out of low memory"
}

; We now allocate data which would otherwise live in the executable in low
; memory if possible. acorn_deletable_init_inline will zero-initialise anythnig
; which does end up in low memory automatically and non-zero values will patched
; up.
; SFTODO: The repetition of the {pre,post}_allocate macros is annoying.

; SFTODO: RENAME zero_start/end TO AVOID CONFUSION WITH ZERO *PAGE*? THE ZERO MEANS "IS CLEARED ON STARTUP"
!if * >= $400 {
zero_start
}

; SF: I've reordered these streams_* variables so the smallest ones come first
; in an attempt to minimise wasted space. I don't believe the code relies on
; them being in any particular order.
	+pre_allocate 1
streams_stack_items
	+post_allocate 1
	+pre_allocate 2
streams_buffering
	+post_allocate 2
	+pre_allocate 4
streams_current_entry
	+post_allocate 4
	+pre_allocate 4
streams_output_selected
	+post_allocate 4
	+pre_allocate 60
streams_stack
	+post_allocate 60

!ifdef ACORN_HW_SCROLL {
; It's very unlikely both of these will ever fit, but it doesn't hurt to try.
	+pre_allocate max_screen_width
top_line_buffer
	+post_allocate max_screen_width
	+pre_allocate max_screen_width
top_line_buffer_reverse
	+post_allocate max_screen_width
}

; If we couldn't fit everything in low memory, pre_allocate will have set zero_end
; appropriately. If everything did fit in low memory, we need to define zero_end.
!ifndef zero_end {
	!if * >= vmap_z_l {
		!error "zero_end not defined but * >= vmap_z_l"
	}
zero_end
}

!ifdef USE_HISTORY {
low_history_end = vmap_z_l
!if * >= low_history_end {
low_history_start = low_history_end
} else {
low_history_start
}
}

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

!ifndef ACORN_SWR {
; On a second processor zero page is available up to but not including $ee. SFTODO?
}

!if * < high_constant_ptr {
	* = high_constant_ptr
}

; SFTODONOW: Do smart ZP allocation, and fill spare ZP (which we have in spades on tube) with stuff that would otherwise go into low memory - this is another good reason for allocation 1 byte things first, then getting larger
