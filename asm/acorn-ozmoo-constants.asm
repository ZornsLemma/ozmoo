; Acorn version of Commodore constants.asm.

; This file refers to "low" and "high" memory:
; - low memory is language workspace at $400-$7ff, mainly pages 4 and 5
; - high memory is part of the main executable from program_start upwards
;
; Memory is allocated in zero page, low memory and high memory to take advantage
; of the space available on the specific configuration this executable is being
; built for. Allocation in high memory takes place at the current assembly
; pointer (*) when this file is sourced, so this must be sourced somewhere this
; is acceptable.

; Pages 6 and 7 are allocated to scratch space and (on a second processor) code.
scratch_page = $600
!ifdef ACORN_SWR {
scratch_double_page = scratch_page
} else {
; Second processor builds load at $700, so this page isn't wasted.
}

!set high_alloc_ptr = *

; SFTODONOW: As this is new code, it's probably worth reviewing it (particularly the macros) fresh

; === Acorn OS and hardware constants
;
; These could be moved into acorn-shared-constants.asm, but these aren't needed
; by anything except the main Ozmoo executable.

stack = $100
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
shadow_mode_bit = 128
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

; === Virtual memory configuration

!ifdef VMEM {

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

min_vmem_blocks = 2

; We only need to allocate space for vmap_used_entries in PREOPT builds.
!ifdef PREOPT {
	HAVE_VMAP_USED_ENTRIES = 1
}

; We use "vmap-z_l-1,x" addressing in a hot loop and we want to avoid any
; page-crossing penalty. We allocate vmap_z_l as high as possible in page 5 so
; we have the largest possible contiguous space for the allocations in page 4 to
; spill over into page 5.
vmap_z_l = scratch_page - vmap_max_size
!if vmap_z_l < $501 {
	!error "vmap_z_l is too low"
}

}

; === Determine upper boundary of low memory

; SFTODO: Not too happy with name "low_end_vmap"
!ifdef VMEM {
	low_end_vmap = vmap_z_l
} else {
	low_end_vmap = scratch_page
}

!ifndef USE_HISTORY {
	low_end = low_end_vmap
} else {
	low_end = low_end_vmap - USE_HISTORY
	low_history_end = low_end_vmap
}

; === Determine available zero page

zp_start = $00
!ifndef ACORN_SWR {
	zp_end = $ee
} else {
	zp_end = $90
}

; === Fixed allocations
;
; These addresses are written to by the loader and read by the Ozmoo executable;
; in order to avoid needing complex logic in the loader, these need to be at the
; same fixed address in all executables they are relevant to (but in executables
; which don't need them, the address can be re-used for something else). These
; all live in memory allocated to BASIC resident integer variables which the
; loader does not use, allowing the loader to write to them without BASIC
; overwriting them.

; In order to avoid complicating the allocation macros further, we manually
; assign some internal variables (not written to by the loader) to the page 4
; memory below resident_integer_b.

low_start = $400
num_rows = $400 ; 1 byte
memory_buffer = $401 ; 7 bytes - larger on C64, but this is all we use

resident_integer_b = $408
; SFTODO: DELETE? resident_integer_o = $43c
resident_integer_x = $460

* = resident_integer_b
low_fixed_gap_start = *

; game_data_filename/restart_command have filename_size bytes allocated; we only
; need one or the other in any particular build.
filename_size = 47
game_data_filename_or_restart_command
	* = * + filename_size

screen_mode
	* = * + 1

; fg_colour, bg_colour and (if MODE_7_INPUT is defined) input_colour must be
; adjacent and in this order.
fg_colour
	* = * + 1
bg_colour
	* = * + 1

; Start of optional loader-written variables. To avoid needing to complicate the
; allocation macros further, we just manually assign other (internal) variables
; to unused space.

!ifdef MODE_7_INPUT {
input_colour
	!if bg_colour + 1 != input_colour {
		!error "bg_colour and input_colour must be adjacent"
	}
} else {
maxwords
}
	* = * + 1

!ifdef ACORN_RELOCATABLE {
relocate_target
ozmoo_relocate_target = relocate_target ; SFTODO!?
} else {
wordoffset
}
	* = * + 1

zero_start
low_fixed_gap_end = *

!if * >= resident_integer_x {
	!error "Fixed allocations have overflowed resident integer space"
}

; === Allocation macros and associated initialisation
; SFTODO: MIX OF "ALLOC" AND "ALLOCATION" IN MACROS/VARIABLES

!set zp_alloc_ptr = zp_start
!set low_alloc_ptr = low_fixed_gap_end

!macro set_alloc_end {
	!if * < zp_end {
		!set alloc_end = zp_end
	} else {
		!if * < low_end {
			!set alloc_end = low_end
		} else {
			!set alloc_end = $ffff
		}
	}
}

!macro set_alloc_star addr {
	* = addr
	+set_alloc_end ; SFTODO: inline this macro if this is only use of it?
}

!macro save_alloc_star {
	!if * <= zp_end {
		!set zp_alloc_ptr = *
	} else {
		!if * <= low_end {
			!set low_alloc_ptr = *
		} else {
			!set high_alloc_ptr = *
		}
	}
}

; Algorithm for detecting overapping intervals taken from
; https://scicomp.stackexchange.com/questions/26258/the-easiest-way-to-find-intersection-of-two-intervals,
; but tweaked to work with half-open intervals.

!macro skip_fixed_alloc fixed_start, fixed_size, current_size {
	.fixed_end = fixed_start + fixed_size
	.current_end = * + current_size
	; acme's "not" is a bitwise operation and doesn't do what we want here...
	!if (* >= .fixed_end) or (fixed_start >= .current_end) {
	} else {
		!if * != fixed_start {
			!warn "Wasting ", fixed_start - *, " bytes before fixed allocation at ", fixed_start
		}
		* = .fixed_end
		+pre_allocate current_size
	}
}

!macro check_fixed_alloc fixed_start, fixed_size {
	.fixed_end = fixed_start + fixed_size
	.current_end = * + current_size
	; acme's "not" is a bitwise operation and doesn't do what we want here...
	!if (* >= .fixed_end) or (fixed_start >= .current_end) {
	} else {
		!error "Fixed allocation would be overwritten"
	}
}

!macro pre_allocate n {
	!if n < 1 {
		!error "Invalid n"
	}

	+save_alloc_star
	!if (zp_alloc_ptr + n) <= zp_end {
		+set_alloc_star zp_alloc_ptr
	} else {
		!if (low_alloc_ptr + n) <= low_end {
			+set_alloc_star low_alloc_ptr
		} else {
			+set_alloc_star high_alloc_ptr
		}
	}

!ifdef ACORN_TURBO_SUPPORTED {
	+skip_fixed_alloc zp_temp_turbo_flag, 1, n
}

	!set pre_allocation = n
}

!macro allocate n {
	!if n < 1 {
		!error "Invalid n"
	}

	!if check_pre_allocation and (n > 1) and (n != pre_allocation) {
		!error "Missing/incorrect pre-allocation"
	}

	; At this point, the label for the memory we're about to allocate has
	; already been set, so it's too late to fix anything. We should have already
	; arranged any necessary fix-ups, but let's at least check here so we can
	; generate an error.
	!if (* + n) > alloc_end {
		!error "No room for allocation of ", n, " bytes at ", *, " (alloc_end = ", alloc_end, ")"
	}

!ifdef ACORN_TURBO_SUPPORTED {
	+check_fixed_alloc zp_temp_turbo_flag, 1
}

	!if * < low_end {
		* = * + n
	} else {
		!fill n, 0
	}

	; By "pre-allocating" space for one byte here, we avoid the need to
	; explicitly call pre_allocate for single byte allocations; this is harmless
	; if we're actually going to perform a multi-byte allocation next.
	+pre_allocate 1
}

; === Non-fixed allocations

; These allocations don't have fixed addresses (although in practice many of
; them are predictable across builds) as they're only used internally by an
; Ozmoo executable and the assembler obviously knows where they are.

; === Non-fixed allocations, part 1: Guaranteed zero page allocations
;
; These allocations are guaranteed to be allocated contiguously in zero page; we
; verify this afterwards. We put the non-conditionally-assembled things first,
; so zero page addresses are more consistent between different builds - this
; makes debugging a little less confusing.

!set check_pre_allocation = 0
+set_alloc_star zp_start
+pre_allocate 1

z_opcode	+allocate 1
mempointer	+allocate 2
mem_temp	+allocate 2
z_extended_opcode	+allocate 1

mempointer_y +allocate 1
z_opcode_number	+allocate 1
zp_pc_h +allocate 1
zp_pc_l	+allocate 1
z_opcode_opcount	+allocate 1; 0 = 0OP, 1=1OP, 2=2OP, 3=VAR
z_operand_count	+allocate 1
zword	+allocate 6

zp_mempos	+allocate 2

z_operand_value_high_arr	+allocate 8
z_operand_value_low_arr	+allocate 8

;
; NOTE: This entire block of variables, except last byte of z_pc_mempointer is
; included in the save/restore files and _have_ to be stored in a contiguous
; block of zero page addresses
;
z_local_vars_ptr	+allocate 2
z_local_var_count	+allocate 1
stack_pushed_bytes	+allocate 2
stack_ptr	+allocate 2
stack_top_value	+allocate 2
stack_has_top_value	+allocate 1
; SF: z_pc is big-endian, z_pc_mempointer is little-endian
z_pc ; 3 bytes (last byte shared with z_pc_mempointer)
	+allocate 2
z_pc_mempointer ; 2 bytes (first byte shared with z_pc)
	+allocate 2
zp_save_start = z_local_vars_ptr
zp_bytes_to_save = z_pc + 3 - z_local_vars_ptr
;
; End of contiguous zero page block
;
;

zchar_triplet_cnt	+allocate 1
packed_text	+allocate 2
alphabet_offset	+allocate 1
escape_char	+allocate 1
escape_char_counter	+allocate 1
abbreviation_command +allocate 1

parse_array	+allocate 2
string_array	+allocate 2

z_address	+allocate 3
z_address_temp	+allocate 1

object_tree_ptr	+allocate 2
object_num	+allocate 2
object_temp	+allocate 2

; SF: On the Acorn port, the vmap starts off full unless we are doing a PREOPT
; build, so vmap_used_entries == vmap_max_entries at all times. We use
; vmap_max_entries everywhere and don't allocate a byte anywhere for
; vmap_used_entries if it isn't used.
vmap_max_entries	+allocate 1

z_low_global_vars_ptr	+allocate 2
z_high_global_vars_ptr	+allocate 2
z_exe_mode	+allocate 1

stack_tmp	+allocate 5
default_properties_ptr	+allocate 2
zchars	+allocate 3

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
vmap_quick_index_match	+allocate 1
vmap_next_quick_index	+allocate 1
vmap_quick_index_length = 6 ; Says how many bytes vmap_quick_index_uses
vmap_quick_index +allocate vmap_quick_index_length ; Must follow vmap_next_quick_index!

z_temp	+allocate 12

zp_temp	+allocate 5
; SF: cursor_{row,column} are used to hold the cursor positions for the two
; on-screen windows. They mainly come into play via save_cursor/restore_cursor;
; the active cursor position is zp_screen{row,column} and that's all that
; matters most of the time.
cursor_row	+allocate 2
cursor_column	+allocate 2
mempointer_ram_bank	+allocate 1 ; SFTODO: have experimentally moved this into zp since I had this space free, it's not necessarily that worthwhile

vmem_temp	+allocate 2

window_start_row	+allocate 4

current_window	+allocate 1

is_buffered_window	+allocate 1

; Screen kernal stuff. Must be kept together or update s_init in screenkernal.
s_ignore_next_linebreak	+allocate 3
s_reverse	+allocate 1
s_os_reverse	+allocate 1

s_cursors_inconsistent	+allocate 1

max_chars_on_line	+allocate 1
buffer_index	+allocate 1
last_break_char_buffer_pos	+allocate 1

zp_screencolumn	+allocate 1 ; current cursor column
zp_screenrow	+allocate 1 ; current cursor row

; "Transient" zero page allocations. On non-second processor builds, these use the
; OS transient command zero page at $a8-$af inclusive - these addresses cannot be
; trusted to retain their values across * commands or (being paranoid) any service
; call, but they can be used for very short term storage. On second processor builds,
; we just allocate some of the available zero page for this.
; SFTODO: There's no advantage to a small transient_zp_size on non-tube; on tube we could make it smaller (but would need to check code to see exactly how much smaller and be careful) SFTODONOW
transient_zp_size = 5 ; bytes of transient zero page needed
; SFTODO: Note at the moment code in acorn.asm uses transient_zp directly and assumes size >=2
!if transient_zp_size > 8 {
	!error "transient_zp_size is too large"
}
!ifndef ACORN_SWR {
				+pre_allocate transient_zp_size
transient_zp	+allocate transient_zp_size
} else {
transient_zp = $a8
}

!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
dynmem_ram_bank	+allocate 1
}

!ifdef ACORN_SWR {
z_pc_mempointer_ram_bank +allocate 1
}

!ifdef ACORN_SCREEN_HOLE {
acorn_screen_hole_start_page_minus_one +allocate 1
}

!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
; These zero-page addresses are only used very briefly to store temporary values
; in. We use some of the transient command workspace at $a8; this is safe as
; there will be no service calls of any kind while the values stored here are
; live, and it's less likely to create subtle bugs than re-using other Ozmoo
; zero page.
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

; Confirm that up to this point we've been allocating in zero page; this means
; we know adjacent allocations were actually adjacent in memory and therefore we
; didn't need to jump through any extra hoops to ensure that.
!if low_alloc_ptr > low_fixed_gap_end {
	!error "Unexpected non-zero page allocations"
}
!set check_pre_allocation = 1

; === Non-fixed allocations, part 2: Flexible allocations
;
; The following allocations may end up in zero page, low memory or high memory.
; The allocation macros will pack them into the available memory and adjacent
; allocations in the source code may not be adjacent in memory; if this is
; important, a single block must be allocated and divided up afterwards.

; SFTODO: Reordering these to affect what happens to get into zp may have an
; impact on performance, either directly or via reducing code size

s_stored_x +allocate 1
s_stored_y +allocate 1

!ifdef VMEM {
nonstored_pages	+allocate 1
vmap_index +allocate 1 ; current vmap index matching the z pointer
vmem_offset_in_block +allocate 1 ; 256 byte offset in 512 byte block (0-1)
}

readblocks_numblocks +allocate 1
					+pre_allocate 2
readblocks_currentblock	+allocate 2
!ifndef ACORN_ADFS {
!ifndef ACORN_DSD {
	+pre_allocate 2
readblocks_base         +allocate 2
} else {
readblocks_base         +allocate 1
}
}

num_default_properties +allocate 1
	+pre_allocate 2
objects_start_ptr      +allocate 2

	+pre_allocate 2
parent_num +allocate 2
	+pre_allocate 2
child_num +allocate 2
	+pre_allocate 2
sibling_num +allocate 2	  ; won't be used at the same time
dest_num = sibling_num      ; won't be used at the same time

bitmask_index +allocate 1
attribute_index +allocate 1

property_number +allocate 1
property_length +allocate 1

	+pre_allocate 2
multiplier
divisor
	+allocate 2
	+pre_allocate 2
multiplicand
dividend
division_result
	+allocate 2
	+pre_allocate 4
product
remainder
	+allocate 4

last_char_index	+allocate 1
parse_array_index +allocate 1

cursor_status	+allocate 1

!ifdef TRACE {
z_trace_index	+allocate 1
}

!ifdef ACORN_PRIVATE_RAM_SUPPORTED {
sideways_ram_hole_start	+allocate 1
; We can use 255 to indicate "no sideways RAM hole", because vmap_max_size<=255
; and therefore virtual memory block indexes are in the range 0-254 inclusive.
sideways_ram_hole_start_none = 255
sideways_ram_hole_vmem_blocks = 2 ; always 1024 bytes if we have a hole
sideways_ram_hole_vmap_max_size = 254 ; see convert_index_x_to_ram_bank_and_address
}

!ifdef HAVE_VMAP_USED_ENTRIES {
; This is used only in PREOPT builds where performance isn't critical so we
; don't waste a byte of zero page on it.
vmap_used_entries	+allocate 1
}

!ifdef ACORN_HW_SCROLL {
use_hw_scroll 	+allocate 1
}

!ifdef ACORN_TURBO_SUPPORTED {
; is_turbo is a bit annoying. Because of the way we test for the presence of a
; turbo second processor, we end up with the result at zp_temp_turbo_flag in
; BASIC's user-allocated zero page. We need to avoid this code allocating
; anything over the top of it, hence the special cases for it in the allocation
; macros.
is_turbo = zp_temp_turbo_flag ; 1 byte SFTODO: RENAME turbo_flag?
}

!ifdef ACORN_SWR {
b_plus_private_ram_size = 12 * 1024 - 512 ; -512 to leave space for shadow copy code
integra_b_private_ram_size = 12 * 1024 - 1024 ; -1024 to leave space for IBOS workspace
vmem_blocks_in_main_ram	+allocate 1
vmem_blocks_stolen_in_first_bank	+allocate 1
jmp_buf_ram_bank 	+allocate 1
}

!ifdef MODE_7_INPUT {
input_colour_code_or_0	+allocate 1
}

!ifdef ACORN_SCREEN_HOLE {
acorn_screen_hole_start_page	+allocate 1
acorn_screen_hole_pages	+allocate 1; SFTODO: PROB NOT GOING TO BENEFIT FROM ZP BUT MAYBE TRY IT
acorn_screen_hole_pages_minus_one +allocate 1 ; SFTODO: PROB NOT GOING TO BENEFIT FROM ZP BUT MAYBE TRY IT
}

!ifdef ACORN_SHADOW_VMEM {
; We use _mem suffixes on these variables to avoid accidental confusion with the
; Commodore values, which are assembly-time constants.
vmem_cache_count_mem	+allocate 1
vmem_cache_start_mem	+allocate 1
vmem_blocks_in_sideways_ram	+allocate 1
; vmem_cache_cnt and vmem_cache_page_index must be adjacent in memory. SFTODO: I
; am not sure that's true on Acorn; perhaps check and loosen this requirement,
; though in reality it's unlikely to make any difference.
; The next line adds 1 byte for vmem_cache_cnt and another 1 byte because PAGE
; alignment may causes us to use one more shadow cache page than recommended
; (because that page would be pasted otherwise).
	+pre_allocate 1 + ACORN_RECOMMENDED_SHADOW_CACHE_PAGES + 1
vmem_cache_cnt ; 1 byte
	+allocate 1 + ACORN_RECOMMENDED_SHADOW_CACHE_PAGES + 1
vmem_cache_page_index = vmem_cache_cnt + 1
vmem_cache_page_index_end
}

!ifdef TRACE_SETJMP {
setjmp_min_s +allocate 1
}

!ifdef USE_HISTORY {
history_current +allocate 1  ; the current entry (when selecting with up/down)
history_first +allocate 1    ; offset to the first (oldest) entry
history_last +allocate 1     ; offset to the end of the last (newest) entry
history_disabled +allocate 1 ; 0 means disabled, otherwise enabled
}

+pre_allocate 2
read_parse_buffer +allocate 2
+pre_allocate 2
read_text_cursor +allocate 2
read_text_column +allocate 1
read_text_char_limit +allocate 1
read_text_operand_count +allocate 1
!ifdef Z4PLUS {
+pre_allocate 2
read_text_time +allocate 2 ; update interval in 1/10 seconds
+pre_allocate 3
read_text_time_jiffy +allocate 3 ; update interval in jiffys
+pre_allocate 3
read_text_jiffy +allocate 3  ; current time
+pre_allocate 2
read_text_routine +allocate 2 ; called with .read_text_time intervals
}
!ifdef Z5PLUS {
read_text_return_value +allocate 1 ; return value
}

!ifdef MODE_7_INPUT {
maxwords   +allocate 1
}
numwords   +allocate 1
!ifdef ACORN_RELOCATABLE {
wordoffset +allocate 1
}
textend    +allocate 1
wordstart  +allocate 1
wordend    +allocate 1
ignore_unknown_words +allocate 1

current_zchar +allocate 1

				+pre_allocate 2
game_disc_crc	+allocate 2
				+pre_allocate 5
initial_clock	+allocate 5

jmp_buf_size = 32 ; SFTODO: this could possibly be squeezed a bit lower if necessary
	  	+pre_allocate jmp_buf_size
jmp_buf	+allocate jmp_buf_size

; SFTODO: RENAME zero_start/end TO AVOID CONFUSION WITH ZERO *PAGE*? THE ZERO MEANS "IS CLEARED ON STARTUP"

	+pre_allocate 4
streams_current_entry
	+allocate 4
	+pre_allocate 60
streams_stack
	+allocate 60
	+pre_allocate 1
streams_stack_items
	+allocate 1
	+pre_allocate 2
streams_buffering
	+allocate 2
	+pre_allocate 4
streams_output_selected
	+allocate 4

!ifdef ACORN_HW_SCROLL {
	+pre_allocate max_screen_width
top_line_buffer
	+allocate max_screen_width
	+pre_allocate max_screen_width
top_line_buffer_reverse
	+allocate max_screen_width
}

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

; If we have a history buffer, it's cleared explicitly, not via the
; zero_start-zero_end clear operation - this is necessary in general because it
; might be located in the executable just below data_start.
zero_end = low_alloc_ptr

!ifdef USE_HISTORY {
	; We should - by choice of low_end earlier - have at least USE_HISTORY bytes
	; for low history, but we may have more if we weren't able to allocate right
	; up to low_end.
	low_history_start = low_alloc_ptr
	!if low_history_end - low_history_start < USE_HISTORY {
		!error "Low history buffer too small"
	}
}

!ifdef ACORN_TURBO_SUPPORTED {
; SFTODO: It might be possible to use an entire 64K bank for dynmem on a turbo copro,
; instead of using what's left of bank 0 after the OS and Ozmoo binary take their share.
turbo_control = $fef0
turbo_bank_base = $301
mempointer_turbo_bank = turbo_bank_base + mempointer
z_pc_mempointer_turbo_bank = turbo_bank_base + z_pc_mempointer
}

; Further assembly continues in high (executable) memory.
* = high_alloc_ptr

; SFTODO: Indentation in this file is a bit inconsistent, especially the pre_allocate lines
