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

z_opcode_number       = $09
zp_pc_h               = $0a
zp_pc_l               = $0b
z_opcode_opcount      = $0c ; 0 = 0OP, 1=1OP, 2=2OP, 3=VAR
z_operand_count		  = $0d
zword				  = $0e ; 6 bytes

zp_mempos             = $14 ; 2 bytes

z_operand_value_high_arr = $16 ; !byte 0, 0, 0, 0, 0, 0, 0, 0
z_operand_value_low_arr = $1e ;  !byte 0, 0, 0, 0, 0, 0, 0, 0

; NOTE: This entire block, except last byte of z_pc_mempointer and z_pc_mempointer_is_unsafe is saved!
z_local_vars_ptr      = $26 ; 2 bytes
z_local_var_count	  = $28
stack_pushed_bytes	  = $29 ; !byte 0, 0
stack_ptr             = $2b ; 2 bytes
stack_top_value 	  = $2d ; 2 bytes !byte 0, 0
stack_has_top_value   = $2f ; !byte 0
; SF: z_pc is big-endian, z_pc_mempointer is little-endian
z_pc				  = $30 ; 3 bytes (last byte shared with z_pc_mempointer)
z_pc_mempointer		  = $32 ; 2 bytes (first byte shared with z_pc)
; z_pc_mempointer_is_unsafe = $34

zp_save_start = z_local_vars_ptr
zp_bytes_to_save = z_pc + 3 - z_local_vars_ptr


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

vmap_quick_index_match= $60
vmap_next_quick_index = $61
vmap_quick_index	  = $62 ; Must follow vmap_next_quick_index!
; SFTODO: It might be worth upping vmap_quick_index_length for SWR, although
; don't assume this will be beneficial, and as it lives in scarce ZP there is
; a real cost to increasing it. (As an experiment only, I could steal some ZP
; belonging to e.g. Econet just as an easy way to test different sizes.)
vmap_quick_index_length = 6 ; Says how many bytes vmap_quick_index_uses

z_temp				  = $68 ; 12 bytes

!ifndef ACORN {

s_colour 			  = $74 ; !byte 1 ; white as default

vmem_temp			  = $92 ; 2 bytes
alphabet_table		  = $96 ; 2 bytes

use_reu				  = $9b

window_start_row	  = $9c; 4 bytes

num_rows			  = $a6 ; !byte 0
current_window		  = $a7 ; !byte 0

is_buffered_window	  = $ab;  !byte 1

; Screen kernal stuff. Must be kept together or update s_init in screenkernal.
s_ignore_next_linebreak = $b0 ; 3 bytes
s_reverse 			  = $b3 ; !byte 0

s_stored_x			  = $b4 ; !byte 0
s_stored_y			  = $b5 ; !byte 0
s_current_screenpos_row = $b6 ; !byte $ff

max_chars_on_line	  = $bd; !byte 0
buffer_index		  = $be ; !byte 0
last_break_char_buffer_pos = $bf ; !byte 0


zp_cursorswitch       = $cc
zp_screenline         = $d1 ; 2 bytes current line (pointer to screen memory)
zp_screencolumn       = $d3 ; current cursor column
zp_screenrow          = $d6 ; current cursor row
zp_colourline         = $f3 ; 2 bytes current line (pointer to colour memory)
cursor_row			  = $f7 ; 2 bytes
cursor_column		  = $f9 ; 2 bytes
zp_temp               = $fb ; 5 bytes
}

!ifndef ACORN {
print_buffer		  = $100 ; 41 bytes
print_buffer2             = $129 ; 41 bytes
} else {
; SFTODO: Is it OK to use 162 bytes of the stack like this? In practice we
; certainly seem to get away with it, and my brief experiments when I
; implemented setjmp suggest Ozmoo won't ever get near 64 bytes of stack use,
; but (particularly if we have some otherwise wasted space floating around due
; to page alignment issues) it might be worth relocating at least one of these
; buffers.
print_buffer		  = $100 ; 81 bytes SF: OK? THIS IS OBV STACK ON C64 TOO SO IT'S PROB FINE BUT CHECK HOW IT'S USED
print_buffer2		  = $151 ; 81 bytes SF: OK? THIS IS OBV STACK ON C64 TOO SO IT'S PROB FINE BUT CHECK HOW IT'S USED
}

!ifndef ACORN {
memory_buffer         =	$02a7
memory_buffer_length  = 89

first_banked_memory_page = $d0 ; Normally $d0 (meaning $d000-$ffff needs banking for read/write access) 

charset_switchable 	  = $291

datasette_buffer_start= $0334 ; Actually starts at 33c, but the eight bytes before that are unused
datasette_buffer_end  = $03fb

; --- BASIC rom routines ---
;basic_printstring     = $ab1e ; write string in a/y (LO </HI >)
;basic_printinteger    = $bdcd ; write integer value in a/x

; --- I/O registers ---
reg_screen_char_mode  = $d018 
reg_bordercolour      = $d020
reg_backgroundcolour  = $d021 

; --- Kernel routines ---
kernal_delay_1ms      = $eeb3 ; delay 1 ms
kernal_setcursor      = $e50c ; set cursor to x/y (row/column)
kernal_reset          = $fce2 ; cold reset of the C64
kernal_scnkey         = $ff9f ; scan the keyboard
kernal_setlfs         = $ffba ; set file parameters
kernal_setnam         = $ffbd ; set file name
kernal_open           = $ffc0 ; open a file
kernal_close          = $ffc3 ; close a file
kernal_chkin          = $ffc6 ; define file as default input
kernal_chkout         = $ffc9 ; define file as default output
kernal_clrchn         = $ffcc ; close default input/output files
kernal_readchar       = $ffcf ; read byte from default input into a
;use streams_print_output instead of kernal_printchar
;($ffd2 only allowed for input/output in screen.asm and text.asm)
kernal_printchar      = $ffd2 ; write char in a
kernal_load           = $ffd5 ; load file
kernal_save           = $ffd8 ; save file
kernal_readtime       = $ffde ; get time of day in a/x/y
kernal_getchar        = $ffe4 ; get a character
kernal_plot           = $fff0 ; set (c=1)/get (c=0) cursor: x=row, y=column
}


; story file header constants
header_version = $0
header_flags_1 = $1
header_high_mem = $4
header_initial_pc = $6
header_dictionary = $8
header_object_table = $a
header_globals = $c
header_static_mem = $e
header_flags_2 = $10
header_serial = $12
header_abbreviations = $18
header_filelength = $1a
header_checksum = $1c
header_interpreter_number = $1e
header_interpreter_version = $1f
header_screen_height_lines = $20
header_screen_width_chars = $21
header_screen_width_units = $22
header_screen_height_units = $24
header_font_width_units = $26
header_font_height_units = $27
!ifndef ACORN {
header_default_bg_colour = $2c
header_default_fg_colour = $2d
}
header_terminating_chars_table = $2e
header_standard_revision_number = $32
header_alphabet_table = $34
header_header_extension_table = $36

!ifdef ACORN {

; Acorn OS constants
brkv = $202
error_ptr = $fd
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
vdu_set_text_colour = 17
vdu_redefine_colour = 19
vdu_reset_text_window = 26
vdu_define_text_window = 28
vdu_home = 30
vdu_goto_xy = 31
osfile_save = $00
osfile_load = $ff
osword_input_line = 0
osword_read_clock = 1
osword_sound = 7
osword_floppy_op = $7f
osbyte_set_cursor_editing = $04
osbyte_flush_buffer = $15
osbyte_reflect_keyboard_status = $76
osbyte_acknowledge_escape = $7e
osbyte_read_key = $81
osbyte_read_cursor_position = $86
osbyte_read_screen_mode = $87
osbyte_enter_language = $8e
osbyte_read_vdu_variable = $a0
osbyte_rw_escape_key = $e5
osbyte_read_language = $fc
vdu_variable_text_window_bottom = $09
vdu_variable_text_window_top = $0b
cr = 13
del = 127
mode_7_text_colour_base = 128
ctrl_key_adjust = 64
buffer_keyboard = 0
max_screen_width = 80

default_mode_7_status_colour = 6
default_mode_6_fg_colour = 7
default_mode_6_bg_colour = 4

; Acorn memory allocations
; SFTODO: It might be worth reordering/reallocating these so the order is a
; bit more logical.

zp_temp               = $75 ; 5 bytes
cursor_row            = $7a ; 2 bytes
cursor_column         = $7c ; 2 bytes
screen_width          = $54 ; 1 byte
screen_height         = $89 ; 1 byte
screen_height_minus_1 = $8a ; 1 byte

vmem_temp			  = $00 ; 2 bytes
alphabet_table		  = $7e ; 2 bytes

window_start_row	  = $80; 4 bytes

current_window		  = $74 ; !byte 0

is_buffered_window	  = $08;  !byte 1

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

!ifndef ACORN_SWR {
scratch_page = $400
} else {
; SFTODO: In this build we're currently wasting most of $400-$4FF, but this
; minimises rejigging.
scratch_page = $600
scratch_double_page = $600
ram_bank_count = $400
ram_bank_list = $401 ; SFTODO: size? potentially up to 9 banks???
ramsel = $fe30
ramsel_copy = $f4
}
; SF: cursor_{row,column} are used to hold the cursor positions for the two
; on-screen windows. They mainly come into play via save_cursor/restore_cursor;
; the active cursor position is zp_screen{row,column} and that's all that
; matters most of the time.
z_trace_index = $500 ; 1 byte
s_stored_x = $501 ; !byte 0
s_stored_y = $502 ; !byte 0
screen_width_minus_1 = $503 ; 1 byte
screen_width_plus_1 = $504 ; 1 byte
memory_buffer = $505 ; 7 bytes (larger on C64, but this is all we use)
initial_clock = $50c ; 5 bytes
game_disc_crc = $511 ; 2 bytes
num_rows = $513 ; !byte 0
vmap_max_entries = $514 ; !byte 0
!ifdef ACORN_HW_SCROLL {
    use_hw_scroll = $515 ; !byte 0
}
screen_mode = $516 ; !byte 0
; fg_colour and bg_colour must be adjacent and in this order
fg_colour = $517 ; !byte 0
bg_colour = $518 ; !byte 0
cursor_status = $519; !byte 0
jmp_buf = $51a ; "up to" 257 bytes - in reality 64 bytes is probably enough
; SFTODO: vmap_z_[hl] can probably live in $400-800, if I populate them in the
; discardable init code in this binary rather than pre-calculating them and
; patching them into the binary. I won't touch this until I decide about SWR
; and pre-opt, as this may well influence my decision.

; SFTODO: On a SWR version we'd need to read HIMEM from the OS, but we can get
; away with this while we only support second processor.
!ifdef ACORN_SWR {
; SFTODO: I need to handle non-shadow mode 7, but this will keep things
; simple while I get started.
ramtop = $8000
} else {
ramtop = $f800
}

!ifdef ACORN_SWR {
; SFTODO: Mega hacky, but this constant will allow me to identify hack locations
; SFTODO: DELETE ram_bank = 4
hack_ram_bank = 15
}

}
