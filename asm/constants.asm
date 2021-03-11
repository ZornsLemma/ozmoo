; C128 is now in a separate constants-c128 instead
;

!ifdef TARGET_C64 {
basic_reset           = $a000
SCREEN_HEIGHT         = 25
SCREEN_WIDTH          = 40
SCREEN_ADDRESS        = $0400
COLOUR_ADDRESS        = $d800
COLOUR_ADDRESS_DIFF   = COLOUR_ADDRESS - SCREEN_ADDRESS
num_rows 			  = $a6 ; !byte 0
CURRENT_DEVICE        = $ba
ti_variable           = $a0; 3 bytes
keyboard_buff_len     = $c6
keyboard_buff         = $277

use_reu				  = $9b
window_start_row	  = $9c; 4 bytes


; Screen kernal stuff. Must be kept together or update s_init in screenkernal.
s_ignore_next_linebreak = $b0 ; 3 bytes
s_reverse 			  = $b3 ; !byte 0

zp_temp               = $fb ; 5 bytes
savefile_zp_pointer   = $c1 ; 2 bytes
first_banked_memory_page = $d0 ; Normally $d0 (meaning $d000-$ffff needs banking for read/write access) 
reu_filled            = $0255 ; 4 bytes
vmap_buffer_start     = $0334
vmap_buffer_end       = $0400 ; Last byte + 1. Should not be more than vmap_buffer_start + 512
}

!ifdef TARGET_PLUS4 {
basic_reset           = $8000
SCREEN_HEIGHT         = 25
SCREEN_WIDTH          = 40
SCREEN_ADDRESS        = $0c00
COLOUR_ADDRESS        = $0800
COLOUR_ADDRESS_DIFF   = $10000 + COLOUR_ADDRESS - SCREEN_ADDRESS
CURRENT_DEVICE        = $ae
ti_variable           = $a3; 3 bytes
keyboard_buff_len     = $ef
keyboard_buff         = $527


zp_temp               = $3b ; 5 bytes
;use_reu				  = $87
window_start_row	  = $88; 4 bytes


num_rows 			  = $b7 ; !byte 0

; Screen kernal stuff. Must be kept together or update s_init in screenkernal.
s_ignore_next_linebreak = $b8 ; 3 bytes
s_reverse 			  = $bb ; !byte 0

savefile_zp_pointer   = $c1 ; 2 bytes
; first_banked_memory_page = $fc ; Normally $fc (meaning $fc00-$ffff needs banking, but that area can't be used anyway) 

fkey_string_lengths = $55f
fkey_string_area = $567

vmap_buffer_start     = $0332
vmap_buffer_end       = $03f2 ; Last byte + 1. Should not be more than vmap_buffer_start + 510
;vmap_buffer_start     = $0333
;vmap_buffer_end       = $0437 ; Last byte + 1. Should not be more than vmap_buffer_start + 510

ted_voice_2_low       = $ff0f
ted_voice_2_high      = $ff10
ted_volume            = $ff11

}

!ifdef TARGET_MEGA65 {
basic_reset           = $a000 ; the mega65 version is always run in C64 mode
SCREEN_HEIGHT         = 25
SCREEN_WIDTH          = 80
!ifdef CUSTOM_FONT {
SCREEN_ADDRESS        = $1000
} else {
SCREEN_ADDRESS        = $0800
}
COLOUR_ADDRESS        = $d800
COLOUR_ADDRESS_DIFF   = COLOUR_ADDRESS - SCREEN_ADDRESS
CURRENT_DEVICE        = $ba
ti_variable           = $a0; 3 bytes
num_rows 			  = $a6 ; !byte 0
keyboard_buff_len     = $c6
keyboard_buff         = $277

use_reu				  = $9b
window_start_row	  = $9c; 4 bytes

; Screen kernal stuff. Must be kept together or update s_init in screenkernal.
s_ignore_next_linebreak = $b0 ; 3 bytes
s_reverse 			  = $b3 ; !byte 0

zp_temp               = $fb ; 5 bytes
savefile_zp_pointer   = $c1 ; 2 bytes
first_banked_memory_page = $d0 ; Normally $d0 (meaning $d000-$ffff needs banking for read/write access) 
reu_filled            = $0255 ; 4 bytes
vmap_buffer_start     = $0334
vmap_buffer_end       = $0400 ; Last byte + 1. Should not be more than vmap_buffer_start + 512

}

; --- ZERO PAGE --
; BASIC not much used, so many positions free to use
; memory bank control
!ifndef ACORN {
zero_datadirection    = $00
zero_processorports   = $01
}
; available zero page variables (pseudo registers)
z_opcode              = $02
mempointer            = $03 ; 2 bytes
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

!ifndef ACORN {
vmap_max_entries	  = $34
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

vmap_used_entries	  = $4f

z_low_global_vars_ptr	  = $50 ; 2 bytes
z_high_global_vars_ptr	  = $52 ; 2 bytes
!ifndef ACORN {
z_trace_index		  = $54
}
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
!ifndef ACORN {

s_colour 			  = $74 ; !byte 1 ; white as default

vmem_temp			  = $92 ; 2 bytes
; alphabet_table		  = $96 ; 2 bytes

current_window		  = $a7 ; !byte 0

is_buffered_window	  = $ab;  !byte 1


s_stored_x			  = $b4 ; !byte 0
s_stored_y			  = $b5 ; !byte 0
s_current_screenpos_row = $b6 ; !byte $ff

max_chars_on_line	  = $bd; !byte 0
buffer_index		  = $be ; !byte 0
last_break_char_buffer_pos = $bf ; !byte 0

zp_cursorswitch       = $cc
zp_screenline         = $d1 ; 2 bytes current line (pointer to screen memory)
zp_screencolumn       = $d3 ; 1 byte current cursor column
zp_screenrow          = $d6 ; 1 byte current cursor row
zp_colourline         = $f3 ; 2 bytes current line (pointer to colour memory)
cursor_row			  = $f7 ; 2 bytes
cursor_column		  = $f9 ; 2 bytes
}

!ifndef ACORN {
print_buffer		  = $100 ; SCREEN_WIDTH + 1 bytes
print_buffer2         = $200 ; SCREEN_WIDTH + 1 bytes
} else {
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
}

!ifndef ACORN {
memory_buffer         =	$02a7
memory_buffer_length  = 89

!ifdef TARGET_PLUS4 {
charset_switchable 	  = $547
} else {
charset_switchable 	  = $291
}

; --- I/O registers ---
!ifdef TARGET_PLUS4 {
; TED reference here:
; http://mclauchlan.site.net.au/scott/C=Hacking/C-Hacking12/gfx.html
reg_screen_bitmap_mode = $ff12
reg_screen_char_mode  = $ff13
reg_bordercolour      = $ff19
reg_backgroundcolour  = $ff15 
}
!ifdef TARGET_MEGA65 {
reg_screen_char_mode  = $d018 
reg_bordercolour      = $d020
reg_backgroundcolour  = $d021 
}
!ifdef TARGET_C64 {
reg_screen_char_mode  = $d018 
reg_bordercolour      = $d020
reg_backgroundcolour  = $d021 
}

; --- Kernel routines ---
!ifdef TARGET_C64 {
kernal_reset          = $fce2 ; cold reset of the C64
kernal_delay_1ms      = $eeb3 ; delay 1 ms
}
!ifdef TARGET_PLUS4 {
kernal_reset          = $fff6 ; cold reset of the PLUS4
kernal_delay_1ms      = $e2dc ; delay 1 ms
}
!ifdef TARGET_MEGA65 {
kernal_reset          = $e4b8 ; Reset back to C65 mode
kernal_delay_1ms      = $eeb3 ; delay 1 ms
}
kernal_setlfs         = $ffba ; set file parameters
kernal_setnam         = $ffbd ; set file name
kernal_open           = $ffc0 ; open a file
kernal_close          = $ffc3 ; close a file
kernal_chkin          = $ffc6 ; define file as default input
kernal_clrchn         = $ffcc ; close default input/output files
kernal_readchar       = $ffcf ; read byte from default input into a
;use streams_print_output instead of kernal_printchar
;($ffd2 only allowed for input/output in screen.asm and text.asm)
kernal_printchar      = $ffd2 ; write char in a
kernal_load           = $ffd5 ; load file
kernal_save           = $ffd8 ; save file
kernal_readtime       = $ffde ; get time of day in a/x/y
kernal_getchar        = $ffe4 ; get a character
}

; SFTODO: It might be worth having a constants-acorn.asm (note there is a constants-c128.asm - is that *in addition* to this, or a complete alternative to this?). Not necessarily just for stuff in the following block.
!ifdef ACORN {

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
vdu_down = 10
vdu_cls = 12
vdu_cr = 13
vdu_set_text_colour = 17
vdu_redefine_colour = 19
vdu_disable = 21
vdu_set_mode = 22
vdu_miscellaneous = 23
vdu_reset_text_window = 26
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
osbyte_read_screen_mode = $87
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
buffer_keyboard = 0
max_screen_width = 80
!ifdef ACORN_SWR {
flat_ramtop = $8000
swr_ramtop = $c000
} else {
flat_ramtop = $f800
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
screen_height_minus_1 = $8a ; 1 byte
}

vmem_temp			  = $00 ; 2 bytes
!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
; SF: I don't really like re-using zero page addresses like this, but I'm
; fairly sure these are safe.
; SFTODO: Might be worth adding a debug macro to conditionally assemble
; these to use Econet zero page workspace.
screen_hole_zp_ptr    = vmem_temp ; 2 bytes
screen_hole_tmp       = z_address_temp ; 1 byte
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

; $0400-$046B hold the BASIC resident integer variables. We use some of these
; addresses to pass information from the loader to the Ozmoo executable.
z_trace_index = $400 ; !byte 0
s_stored_x = $401 ; !byte 0
s_stored_y = $402 ; !byte 0
!if 0 { ; SFTODO: These can be re-used now
screen_width_minus_1 = $403 ; !byte 0
screen_width_plus_1 = $404 ; !byte 0
}
game_disc_crc = $405 ; 2 bytes
num_rows = $407 ; !byte 0
!ifdef ACORN_RELOCATABLE {
relocate_target = $408 ; !byte 0, low byte of B%
ozmoo_relocate_target = relocate_target ; SFTODO!?
}
; fg_colour and bg_colour must be adjacent and in this order
fg_colour = $409 ; !byte 0
bg_colour = $40a ; !byte 0
screen_mode = $40b ; !byte 0, high byte of B%
!ifdef VMEM {
vmap_max_entries = $40c ; !byte 0
}
!ifdef ACORN_HW_SCROLL {
use_hw_scroll = $40d ; !byte 0
}
!ifdef ACORN_TURBO_SUPPORTED {
is_turbo = $40e ; !byte 0 SFTODO: RENAME turbo_flag?
}
cursor_status = $40f ; !byte 0
!ifdef ACORN_SHADOW_VMEM {
; We call this vmem_cache_cnt_mem because the Commodore vmem_cache_cnt is a
; constant, not an address containing a value. This way we avoid accidentally
; mixing them up.
vmem_cache_cnt_mem = $410; ! byte 0
}
!ifdef ACORN_SWR {
; SFTODO: There's a gap here in page 4 now we've stopped storing RAM bank list there; move things up.
mempointer_ram_bank = $41c ; 1 byte SFTODO: might benefit from zp? looking at profiles it's really not that hot on big or small dynmem model
vmem_blocks_in_main_ram = $41d ; 1 byte
vmem_blocks_stolen_in_first_bank = $41e ; 1 byte
z_pc_mempointer_ram_bank = $7f ; 1 byte SFTODO EXPERIMENTAL ZP $41f ; 1 byte SFTODO: might benefit from zp? yes, bigdynmem builds do use this in fairly hot path (and it's also part of macros so it might shrink code size) - savings from zp not going to be huge, but not absolutely negligible either
; SFTODO: 2 bytes at $420 currently wasted, shuffle up SFTODO TEMP REUSED THESE NOW
jmp_buf_ram_bank = $422 ; 1 byte
}
initial_clock = $423 ; 5 bytes
memory_buffer = $428 ; 7 bytes (larger on C64, but this is all we use)
; The following two strings have filename_size bytes allocated. We're not short
; on storage in low memory in general, but as the BASIC loader needs to write
; these strings we must use memory which won't clash with BASIC's own use, so
; this has to fit inside the resident integer variable workspace and that is
; relatively scarce. We could probably increase filename_size but it might mean
; some reshuffling of other data which happens to live in the resident integer
; variable workspace but doesn't need to.
filename_size = 49 ; this takes us from inside K% to end of W%
game_data_filename_or_restart_command = $42f
jmp_buf = $42f+filename_size ; "up to" 257 bytes - in reality 64 bytes is probably enough
; The progress_indicator_* variables can re-use the space at
; z_operand_value_high-arr; they're only used during the initial loading when
; the Z-machine has not been set up.
progress_indicator_blocks_per_chunk = z_operand_value_high_arr ; 2 bytes
progress_indicator_blocks_left_in_chunk = z_operand_value_high_arr + 2 ; 2 bytes
; SFTODO: The remaining space in page 4 is wasted on an over-large jmp_buf.

!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
; SFTODO: I'm fairly sure this isn't performance critical, but it might be worth
; doing a comparison with it in zero page at some point just to be safe.
screen_hole_tmp_slow = $500 ; 1 byte
}
vmap_z_l = $501 ; not $500, because we use "vmap_z_l - 1,x" addressing in a hot loop
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

; SFTODO: THESE MEMORY ALLOCATIONS ARE MESSY
!ifdef ACORN_SCREEN_HOLE {
acorn_screen_hole_start_page = $41f ; $9e ; SFTODO TEMP EXPERIMENTAL $41f ; SFTODO TEMP ADDRESS, EXPERIMENTAL - THIS DOESN'T REALLY NEED TO BE IN ZP (BUT MAYBE DO COMPARATIVE MEASUREMENTS) - OK, I REALLY DON'T THINK THIS BENEFITS SIGNIFICANTLY FROM ZP
acorn_screen_hole_start_page_minus_one = $54
acorn_screen_hole_pages = $420 ; SFTODO: PROB NOT GOING TO BENEFIT FROM ZP BUT MAYBE TRY IT
acorn_screen_hole_pages_minus_one = $421 ; SFTODO: PROB NOT GOING TO BENEFIT FROM ZP BUT MAYBE TRY IT
}

}
