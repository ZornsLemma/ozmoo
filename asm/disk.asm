; SFTODO: I half wonder if disk.asm should be pure C64 code and we should
; just have a disk-acorn.asm for Acorn stuff
!ifndef ACORN {
first_unavailable_save_slot_charcode	!byte 0
current_disks !byte $ff, $ff, $ff, $ff
boot_device !byte 0
ask_for_save_device !byte $ff
}

; SFTODO: readblock could be part of discardable init code on a non-VMEM build;
; maybe make it a macro so ozmoo.asm can put it in discardable init or not as
; appropriate? (If we support restart on non-VMEM builds for Acorn, we'd maybe
; need readblock to do the check for having the game disc in the drive.)
!ifdef ACORN {
    WANT_READBLOCK = 1
} else {
    !ifdef VMEM {
        WANT_READBLOCK = 1
    } else {
        WANT_READBLOCK = 0
    }
}

!if WANT_READBLOCK = 0 {
disk_info
	!byte 0, 0, 1  ; Interleave, save slots, # of disks
	!byte 8, 8, 0, 0, 0, 130, 131, 0 
} else {
!ifndef ACORN {
device_map !byte 0,0,0,0
}

; SFTODODATA 6
nonstored_blocks		!byte 0
readblocks_numblocks	!byte 0 
readblocks_currentblock	!byte 0,0 ; 257 = ff 1
!ifndef ACORN {
readblocks_currentblock_adjusted	!byte 0,0 ; 257 = ff 1
readblocks_mempos		!byte 0,0 ; $2000 = 00 20
readblocks_base         !byte 0,0
} else { ; ACORN
!ifndef ACORN_ADFS {
readblocks_base         !byte 0
!ifndef ACORN_DSD {
                        !byte 0
}
}
}
!ifndef ACORN {
disk_info
!ifdef Z3 {
	!fill 71
}
!ifdef Z4 {
	!fill 94
}
!ifdef Z5 {
	!fill 94
}
!ifdef Z8 {
	!fill 120
}
}

readblocks
    ; read <n> blocks (each 256 bytes) from disc to memory
    ; set values in readblocks_* before calling this function
    ; register: a,x,y
!ifdef TRACE_FLOPPY {
    jsr newline
    jsr print_following_string
!ifndef ACORN {
    !pet "readblocks (n,zp,c64) ",0
} else {
    !text "readblocks (n,zp,c64) ",0
}
    lda readblocks_numblocks
    jsr printa
    jsr comma
    lda readblocks_currentblock + 1
    jsr print_byte_as_hex
    lda readblocks_currentblock
    jsr print_byte_as_hex
    jsr comma
    lda readblocks_mempos + 1
    jsr print_byte_as_hex
    lda readblocks_mempos 
    jsr print_byte_as_hex
    jsr newline
}
!ifndef ACORN {
-   jsr readblock ; read block
    inc readblocks_mempos + 1   ; update mempos,block for next iteration
    inc readblocks_currentblock
    bne +
    inc readblocks_currentblock + 1
+   dec readblocks_numblocks        ; loop
    bne -
    rts

.readblock_from_reu
	ldx readblocks_currentblock_adjusted
	ldy readblocks_currentblock_adjusted + 1
	inx
	bne +
	iny
+	tya
	ldy readblocks_mempos + 1 ; Assuming lowbyte is always 0 (which it should be)
	jmp copy_page_from_reu

readblock
    ; read 1 block from floppy
    ; $mempos (contains address to store in) [in]
    ; set values in readblocks_* before calling this function
    ; register a,x,y

!ifdef TRACE_FLOPPY {
	jsr print_following_string
	!pet "Readblock: ",0
	lda readblocks_currentblock + 1
	jsr print_byte_as_hex
	lda readblocks_currentblock
	jsr print_byte_as_hex
}

	lda readblocks_currentblock
	sec
	sbc nonstored_blocks
	sta readblocks_currentblock_adjusted
	sta .blocks_to_go
	lda readblocks_currentblock + 1
	sbc #0
	sta readblocks_currentblock_adjusted + 1
	sta .blocks_to_go + 1

	; Check if game has been cached to REU
	bit use_reu
	bvs .readblock_from_reu

    ; convert block to track/sector
	
	lda disk_info + 2 ; Number of disks
	ldx #0 ; Memory index
	ldy #0 ; Disk id
.check_next_disk	
	txa
	clc
	adc disk_info + 3,x
	sta .next_disk_index ; x-value where next disk starts
	; Check if the block we are looking for is on this disk
	lda readblocks_currentblock_adjusted
	sec
	sbc disk_info + 6,x
	sta .blocks_to_go_tmp + 1
	lda readblocks_currentblock_adjusted + 1
	sbc disk_info + 5,x
	sta .blocks_to_go_tmp
	bcc .right_disk_found ; Found the right disk!
	; This is not the right disk. Save # of blocks to go into next disk.
	lda .blocks_to_go_tmp
	sta .blocks_to_go
	lda .blocks_to_go_tmp + 1
	sta .blocks_to_go + 1
	jmp .next_disk ; Not the right disk, keep looking!
; Found the right disk
.right_disk_found
	lda disk_info + 4,x
	sta .device
	lda disk_info + 7,x
	sta .disk_tracks ; # of tracks which have entries
	lda #1
	sta .track
.check_track
	lda disk_info + 8,x
	beq .next_track
	and #%00111111
	sta .sector
	lda .blocks_to_go + 1
	sec
	sbc .sector
	sta .blocks_to_go_tmp + 1
	lda .blocks_to_go
	sbc #0
	sta .blocks_to_go_tmp
	bcc .right_track_found ; Found the right track
	sta .blocks_to_go
	lda .blocks_to_go_tmp + 1
	sta .blocks_to_go + 1
.next_track
	inx
	inc .track
	dec .disk_tracks
	bne .check_track
!ifndef UNSAFE {
; Broken config
	lda #ERROR_CONFIG ; Config info must be incorrect if we get here
	jmp fatalerror
}
.next_disk
	ldx .next_disk_index
	iny
!ifdef UNSAFE {
	jmp .check_next_disk
} else {
	cpy disk_info + 2 ; # of disks
	bcs +
	jmp .check_next_disk
+	lda #ERROR_OUT_OF_MEMORY ; Meaning request for Z-machine memory > EOF. Bad message? 
	jmp fatalerror
}

.right_track_found
	; Add sectors not used at beginning of track
	; .blocks_to_go + 1: logical sector#
	; disk_info + 8,x: # of sectors skipped / 2 (2 bits), # of sectors used (6 bits)
	sty .temp_y
!ifdef TRACE_FLOPPY {
	jsr arrow
	lda .track
	jsr print_byte_as_hex
	jsr comma
	lda .blocks_to_go + 1
	jsr print_byte_as_hex
}
	lda disk_info + 8,x
	lsr
	lsr
	lsr
	lsr
	lsr	
	and #%00000110; a now holds # of sectors at start of track not in use
	sta .skip_sectors
; Initialize track map. Write 0 for sectors not yet used, $ff for sectors used 
	lda disk_info + 8,x
	and #%00111111
	clc
	adc .skip_sectors
	sta .sector_count
	tay
	dey
	lda #0
-	cpy .skip_sectors
	bcs +
	lda #$ff
+	sta .track_map,y
	dey
	bpl -
;	Find right sector.
;		1. Start at 0
;		2. Find next free sector
;		3. Decrease blocks to go. If < 0, we are done
;		4. Mark sector as used.
;		5. Add interleave, go back to 2	
; 1
	lda #0
; 2
-	tay
	lda .track_map,y
	beq +
	iny
	tya
	cpy .sector_count
	bcc -
	lda #0
	beq - ; Always branch
; 3
+	dec .blocks_to_go + 1
	bmi +
; 4
	lda #$ff
	sta .track_map,y
; 5
	tya
	clc
	adc disk_info ; #SECTOR_INTERLEAVE
.check_sector_range	
	cmp .sector_count
	bcc -
	sbc .sector_count ; c is already set
	bcs .check_sector_range ; Always branch
+	sty .sector
!ifdef TRACE_FLOPPY {
	jsr comma
	tya
	jsr print_byte_as_hex
}
; Restore old value of y
	ldy .temp_y
	jmp .have_set_device_track_sector

.track_map 		!fill 40 ; Holds a map of the sectors in a single track
.sector_count 	!byte 0
.skip_sectors 	!byte 0
.temp_y 		!byte 0


    ; convert track/sector to ascii and update drive command
read_track_sector
	; input: a: track, x: sector, y: device#, Word at readblocks_mempos holds storage address
	sta .track
	stx .sector
	sty .device
.have_set_device_track_sector
	lda .track
    jsr conv2dec
    stx .uname_track
    sta .uname_track + 1
    lda .sector
    jsr conv2dec
    stx .uname_sector
    sta .uname_sector + 1

!ifdef TRACE_FLOPPY_VERBOSE {
    jsr space
    jsr dollar
    lda readblocks_mempos + 1
    jsr print_byte_as_hex
    lda readblocks_mempos 
    jsr print_byte_as_hex
    jsr comma
    ldx readblocks_currentblock
    jsr printx
    ;jsr comma
    ;lda #<.uname
    ;ldy #>.uname
    ;jsr printstring
    jsr newline
}
    ; open the channel file
    lda #cname_len
    ldx #<.cname
    ldy #>.cname
    jsr kernal_setnam ; call SETNAM

    lda #$02      ; file number 2
    ldx .device
	tay      ; secondary address 2
    jsr kernal_setlfs ; call SETLFS

    jsr kernal_open     ; call OPEN

    bcs .error    ; if carry set, the file could not be opened

    ; open the command channel

    lda #uname_len
    ldx #<.uname
    ldy #>.uname
    jsr kernal_setnam ; call SETNAM
    lda #$0F      ; file number 15
    ldx .device
    tay      ; secondary address 15
    jsr kernal_setlfs ; call SETLFS

    jsr kernal_open ; call OPEN (open command channel and send U1 command)
    bcs .error    ; if carry set, the file could not be opened

    ; check drive error channel here to test for
    ; FILE NOT FOUND error etc.

    ldx #$02      ; filenumber 2
    jsr kernal_chkin ; call CHKIN (file 2 now used as input)

    lda readblocks_mempos
    sta zp_mempos
    lda readblocks_mempos+1
    sta zp_mempos + 1

    ldy #$00
-   jsr kernal_readchar ; call CHRIN (get a byte from file)
    sta (zp_mempos),Y   ; write byte to memory
    iny
    bne -         ; next byte, end when 256 bytes are read
	jmp close_io
.error
    ; accumulator contains BASIC error code
    ; most likely errors:
    ; A = $05 (DEVICE NOT PRESENT)
    jsr close_io    ; even if OPEN failed, the file has to be closed
    lda #ERROR_FLOPPY_READ_ERROR
    jsr fatalerror
.cname !text "#"
cname_len = * - .cname

.uname !text "U1 2 0 "
.uname_track !text "18 "
.uname_sector !text "00"
    !byte 0 ; end of string, so we can print debug messages

uname_len = * - .uname
.track  !byte 0
.sector !byte 0
.device !byte 0
.blocks_to_go !byte 0, 0
.blocks_to_go_tmp !byte 0, 0
.next_disk_index	!byte 0
.disk_tracks	!byte 0
} else { ; ACORN
!ifdef ACORN_SWR {
    ; Our caller is responsible for paging in a suitable RAM bank and setting
    ; readblocks_mempos to the physical address within that bank. All we do
    ; here to accommodate sideways RAM is use a bounce buffer when loading to
    ; it.
    lda readblocks_mempos + 1
    pha
    bpl +
    lda #>scratch_double_page
    sta readblocks_mempos + 1
    sta .copy_lda_abs_y + 2
+
}

    ; We prefix errors with two newlines, so even if we're part way through a
    ; line there will always be at least one blank line above the error message.
    ; We use s_printchar for output here, as this will be mixed in with the
    ; output from the game which was running when this disc read was required.
    ldx #2
    ldy #error_print_s_printchar
    jsr setjmp
    beq .no_error
!ifdef ACORN_ADFS {
    jsr close_osgbpb_block_handle
}
    jsr error_print_following_string
    ; SFTODO: It's a bit of a luxury item, but it might be nice to allow * to
    ; enter * commands here - if you're on ADFS and you accidentally stick a
    ; DFS floppy in, you can get stuck in an unrecoverable "Bad FS map" retry
    ; loop, and if you had a * prompt you could do a *MOUNT and (I think) fix
    ; things. In reality no one is really likely to get caught by this, but
    ; all else being equal it might be nice to allow a way out.
    !text 13, "Press SPACE to retry...", 0
    jsr wait_for_space
    lda #13
    jsr s_printchar
    lda #13
    jsr s_printchar
.no_error

!ifndef ACORN_ADFS {
    ; This code is based on the BASIC FNdisk() function here:
    ; http://beebwiki.mdfs.net/OSWORD_%267F#Coding

    ; Convert (256 byte) block number within game into drive, track and sector.
.drive_bits = 24+8
.track = division_result
.sector = remainder
.drive = remainder + 1
!ifndef ACORN_DSD {
    clc
    lda readblocks_currentblock
    adc readblocks_base
    sta dividend
    lda readblocks_currentblock + 1
    adc readblocks_base + 1
    sta dividend + 1
    lda #10
    sta divisor
    lda #.drive_bits + 0
    sta .drive
    lda #0
    sta divisor + 1
    jsr divide16

    lda division_result
    sta .track
    lda remainder
    sta .sector
} else {
    clc
    lda readblocks_currentblock
    sta dividend
    lda readblocks_currentblock + 1
    sta dividend + 1
    lda #20
    sta divisor
    lda #0
    sta divisor + 1
    jsr divide16

    lda division_result
    clc
    adc readblocks_base
    sta .track
    lda remainder ; cylinder sector, 0-9 on drive 0, 10-19 on drive 2
    ldx #0
    cmp #10
    bcc +
    ; sec - carry is already set
    sbc #10
    ldx #2
+   sta .sector
    txa
    ora #.drive_bits
    sta .drive
}

.retry
    lda .drive
    sta .osword_7f_block_drive
    lda .track
    sta .osword_7f_block_track
    lda .sector
    sta .osword_7f_block_sector
    ; We know the number of blocks is going to be <= vmem_block_pagecount, so
    ; there's no need to loop round in case it's too many to read. (Note also
    ; that you can't read across a track boundary using OSWORD $7F, but we align
    ; the data file to avoid needing to do this.)
    lda readblocks_numblocks
    ora #$20
    sta .osword_7f_block_sector_size_and_count

!ifdef FAKE_READ_ERRORS {
    ; Test code to fake intermittent read failures
    jsr kernal_readtime
    cmp #25
    bcs +
    brk
    !byte 'A' ; error code, use a printable character to confirm it isn't printed
    !text "Fake read error"
    !byte 0
+
}

    lda #osword_floppy_op
    ldx #<.osword_7f_block
    ldy #>.osword_7f_block
    jsr osword
    lda .osword_7f_block_result
    beq .read_ok
    cmp #$10
    beq .retry
    brk
    !byte 0
    !text "Disc read error"
    !byte 0
.read_ok
} else { ; ACORN_ADFS
    lda osgbpb_block_handle
    bne .file_is_open
    lda #osfind_open_input
    ldx #<game_data_filename
    ldy #>game_data_filename
    jsr osfind
    bne +
    brk
cant_open_data_error
    !byte 0
    !text "Can't open DATA"
    !byte 0
+   sta osgbpb_block_handle

.file_is_open
    ; We could just use osgbpb_block_data_address for readblocks_mempos,
    ; but OSGBPB would then increment it automatically - this is sort of a good
    ; thing, but it creates small variations in behaviour compared to DFS which
    ; then require tweaks elsewhere to compensate.
    ;
    ; If the game isn't an exact multiple of 512 bytes long, reads of the last
    ; 512-byte block will return fewer bytes, but that's fine - there's no need
    ; to pad the game data on disc.
    lda readblocks_mempos
    sta osgbpb_block_data_address + 0
    lda readblocks_mempos + 1
    sta osgbpb_block_data_address + 1
    lda #0
    sta osgbpb_block_transfer_length + 0
    sta osgbpb_block_pointer + 0
    lda readblocks_numblocks
    sta osgbpb_block_transfer_length + 1
    lda readblocks_currentblock
    sta osgbpb_block_pointer + 1
    lda readblocks_currentblock + 1
    sta osgbpb_block_pointer + 2
    lda #osgbpb_read_using_ptr
    ldx #<osgbpb_block
    ldy #>osgbpb_block
    jsr osgbpb
}

    jsr set_default_error_handler

!ifdef ACORN_SWR {
    pla
    bpl +
    sta readblocks_mempos + 1
    sta .copy_sta_abs_y + 2
    ldx #1
    ldy #0
-   
.copy_lda_abs_y
    lda scratch_double_page,y
.copy_sta_abs_y
    sta $ff00,y ; patched
    iny
    bne -
    inc .copy_sta_abs_y + 2
    inc .copy_lda_abs_y + 2
    dex
    bpl -
+
}

    ; Now we know the operation has succeeded and there won't be a retry,
    ; increment the disc and memory positions.
    clc
    lda readblocks_mempos + 1
    adc readblocks_numblocks
    sta readblocks_mempos + 1
    clc
    lda readblocks_currentblock
    adc readblocks_numblocks
    sta readblocks_currentblock
    bcc +
    inc readblocks_currentblock + 1
.readblocks_rts
+   rts

!ifndef ACORN_ADFS {
.osword_7f_block
.osword_7f_block_drive
     !byte 0   ; drive
readblocks_mempos
     !word 0   ; low order word of address
     !word 0   ; high order word of address
     !byte 3   ; number of parameters
     !byte $53 ; read data
.osword_7f_block_track
     !byte 0   ; track
.osword_7f_block_sector
     !byte 0   ; sector
.osword_7f_block_sector_size_and_count
     !byte 0   ; sector size and count
.osword_7f_block_result
     !byte 0   ; result
} else { ; ACORN_ADFS
readblocks_mempos
    !word 0
}

wait_for_space
    lda #osbyte_flush_buffer
    ldx #buffer_keyboard
    jsr osbyte
-   jsr osrdch
    cmp #' '
    bne -
    rts
}
} ; End of !ifdef VMEM

!ifndef ACORN {
close_io
    lda #$0F      ; filenumber 15
    jsr kernal_close ; call CLOSE

    lda #$02      ; filenumber 2
    jsr kernal_close ; call CLOSE

    jmp kernal_clrchn ; call CLRCHN
}

!ifndef ACORN {
!zone disk_messages {
prepare_for_disk_msgs
	rts

print_insert_disk_msg
; Parameters: y: memory index to start of info for disk in disk_info
	sty .save_y
	; ldx .print_row
	; ldy #2
	; jsr set_cursor
	lda #>insert_msg_1
	ldx #<insert_msg_1
	jsr printstring_raw
	ldy .save_y
; Print disk name
	lda disk_info + 7,y ; Number of tracks
	clc
	adc .save_y
	tay
-	lda disk_info + 8,y
	beq .disk_name_done
	bmi .special_string
	jsr printchar_raw
	iny
	bne - ; Always branch
.special_string
	and #%00000111
	tax
	lda .special_string_low,x
	sta .save_x
	lda .special_string_high,x
	ldx .save_x
	jsr printstring_raw
	iny
	bne - ; Always branch
.disk_name_done
	lda #>insert_msg_2
	ldx #<insert_msg_2
	jsr printstring_raw
	ldy .save_y
	lda disk_info + 4,y
	tax
	cmp #10
	bcc +
	lda #$31
	jsr printchar_raw
	txa
	sec
	sbc #10
+	clc
	adc #$30
	jsr printchar_raw
	lda #>insert_msg_3
	ldx #<insert_msg_3
	jsr printstring_raw
	;jsr kernal_readchar ; this shows the standard kernal prompt (not good)
!IF 0 { ; SF
-	jsr kernal_getchar
} else {
-    LDA #'X'
    JSR $FFEE
    JMP SFHANG
}
    beq -
	; lda .print_row
	; clc
	; adc #3
	; sta .print_row
	ldy .save_y
	rts
.save_x	!byte 0
.save_y	!byte 0
.print_row	!byte 14
;.device_no	!byte 0
.special_string_128
	!pet "Boot ",0
.special_string_129
	!pet "Story ",0
.special_string_130
	!pet "Save ",0
.special_string_131
	!pet "disk ",0
.special_string_low		!byte <.special_string_128, <.special_string_129, <.special_string_130, <.special_string_131
.special_string_high	!byte >.special_string_128, >.special_string_129, >.special_string_130, >.special_string_131


insert_msg_1
!pet 13,"  Please insert ",0
insert_msg_2
!pet 13,"  in drive ",0
insert_msg_3
!pet " [ENTER] ",0
}
}


!ifdef VMEM {
WANT_RESTART = 1
} else {
!ifdef ACORN {
WANT_RESTART = 1
}
}
!ifdef WANT_RESTART {
z_ins_restart
!ifndef ACORN {
	; Find right device# for boot disk

	ldx disk_info + 3
	lda disk_info + 4,x
	cmp #10
	bcc +
	inc .restart_code_string + 12
	sec
	sbc #10
+	ora #$30
	sta .restart_code_string + 13
	
	; Check if disk is in drive
	lda disk_info + 4,x
	tay
	txa
	cmp current_disks - 8,y
	beq +
	jsr print_insert_disk_msg
+

	; Copy restart code
	ldx #.restart_code_end - .restart_code_begin
-	lda .restart_code_begin - 1,x
	sta .restart_code_address - 1,x
	dex
	bne -

	; Setup	key sequence
	ldx #0
-	lda .restart_keys,x
	beq +
	sta 631,x
	inx
	bne - ; Always branch
+	stx 198
	jsr clear_screen_raw
	; lda #147
	; jsr $ffd2
	lda #z_exe_mode_exit
	sta z_exe_mode
	rts
.restart_keys
;	!pet "lO",34,":*",34,",08:",131,0
	!pet "sY3e4",13,0

.restart_code_address = 30000

.restart_code_begin
.restart_code_string_final_pos = .restart_code_string - .restart_code_begin + .restart_code_address
	ldx #0
-	lda .restart_code_string_final_pos,x
	beq +
	jsr $ffd2
	inx
	bne -
	; Setup	key sequence
+	lda #131
	sta 631
	lda #1
	sta 198
	rts
		
.restart_code_string
	!pet 147,17,17,"    ",34,":*",34,",08",19,0
; .restart_code_keys
	; !pet 131,0
.restart_code_end

} else { ; ACORN
!ifdef ACORN_NO_SHADOW {
    jsr undo_mode_7_3c00
    ; Turn the cursor off during the restart; switching to proper mode 7 will
    ; have re-enabled it without updating cursor_status, so we just call
    ; init_cursor_control and that will turn it off again.
    jsr init_cursor_control
}

    ; SFTODO: We should probably check the game disc is in the drive here,
    ; although really the user shouldn't have taken it out anyway. Is this that
    ; much different than them doing so during normal gameplay? readblocks will
    ; (of course) happily read junk from whatever random disc is in the drive.

    ; Since we discarded our initialisation code on startup, we have to
    ; re-execute the Ozmoo binary from disc to restart.
!ifndef ACORN_ADFS {
    ; On DFS, the loader puts the restart command at this address; this saves a
    ; tiny bit of space in the binary and (more importantly) means the binary
    ; doesn't need to know at build time whether it's on drive 0 or drive 2.
    ldx #<restart_command
    ldy #>restart_command
} else {
    ; Build up the restart command at scratch_page by copying game_data_filename
    ; and replacing the last component by the executable leafname.
.last_dot = zp_temp
    lda #'/'
    sta scratch_page
    ldx #0
-   inx
    lda game_data_filename - 1,x
    sta scratch_page,x
    cmp #'.'
    bne +
    stx .last_dot
+   cmp #13
    bne -
    ldx .last_dot
    ldy #255
-   inx
    iny
    lda .executable_leafname,y
    sta scratch_page,x
    cmp #13
    bne -
    ldx #<scratch_page
    ldy #>scratch_page
}
    jmp oscli

!ifdef ACORN_ADFS {
.executable_leafname
!ifndef ACORN_SWR {
    !text "OZMOO2P", 13
} else {
    !ifndef ACORN_ELECTRON {
        !ifndef ACORN_NO_SHADOW {
            !text "OZMOOSH", 13
        } else {
            !text "OZMOOB", 13
        }
    } else { ; ACORN_ELECTRON
        !text "OZMOOE", 13
    }
}
}
}
}

z_ins_restore
!ifdef Z3 {
	jsr restore_game
	beq +
	jmp make_branch_true
+	jmp make_branch_false
}
!ifdef Z4 {
	jsr restore_game
	beq +
	inx
+	jmp z_store_result
}
!ifdef Z5PLUS {
	jsr restore_game
	beq +
	inx
+	jmp z_store_result
}

z_ins_save
!ifdef Z3 {
	jsr save_game
	beq +
	jmp make_branch_true
+	jmp make_branch_false
}
!ifdef Z4 {
	jsr save_game
	jmp z_store_result
}
!ifdef Z5PLUS {
	jsr save_game
	jmp z_store_result
}

!zone save_restore {
!ifndef ACORN {
.inputlen !byte 0
.filename !pet "!0" ; 0 is changed to slot number
.inputstring !fill 15 ; filename max 16 chars (fileprefix + 14)
.input_alphanum
    ; read a string with only alphanumeric characters into .inputstring
    ; return: x = number of characters read
    ;         .inputstring: null terminated string read (max 20 characters)
    ; modifies a,x,y
	jsr turn_on_cursor
    lda #0
    sta .inputlen
-   jsr kernal_getchar
    cmp #$14 ; delete
    bne +
    ldx .inputlen
    beq -
    dec .inputlen
	pha
	jsr turn_off_cursor
	pla
    jsr s_printchar
	jsr turn_on_cursor
    jmp -
+   cmp #$0d ; enter
    beq .input_done
	cmp #$20
	beq .char_is_ok
    sec
    sbc #$30
    cmp #$5B-$30
    bcs -
    sbc #$09 ;actually -$0a because C=0
    cmp #$41-$3a
    bcc -
    adc #$39 ;actually +$3a because C=1
.char_is_ok
    ldx .inputlen
    cpx #14
    bcs -
    sta .inputstring,x
    inc .inputlen
    jsr s_printchar
	jsr update_cursor
    jmp -
.input_done
	pha
	jsr turn_off_cursor
	pla
	jsr s_printchar ; return
    ldx .inputlen
    lda #0
    sta .inputstring,x
	rts

.error
    ; accumulator contains BASIC error code
    ; most likely errors:
    ; A = $05 (DEVICE NOT PRESENT)
	sta zp_temp + 1 ; Store error code for printing
    jsr close_io    ; even if OPEN failed, the file has to be closed
	lda #>.disk_error_msg
	ldx #<.disk_error_msg
	jsr printstring_raw
	; Add code to print error code!
    lda #0
    rts
	
list_save_files
	lda #13
	jsr s_printchar
	ldx	first_unavailable_save_slot_charcode
	dex
	stx .saveslot_msg + 9
	ldx disk_info + 1 ; # of save slots
	lda #0
-	sta .occupied_slots - 1,x
	dex
	bne -
	; Remember address of row where first entry is printed
	lda zp_screenline
	sta .base_screen_pos
	lda zp_screenline + 1
	sta .base_screen_pos + 1

    ; open the channel file
    lda #1
    ldx #<.dirname
    ldy #>.dirname
    jsr kernal_setnam ; call SETNAM

    lda #2      ; file number 2
    ldx disk_info + 4 ; Device# for save disk
+   ldy #0      ; secondary address 2
    jsr kernal_setlfs ; call SETLFS

    jsr kernal_open     ; call OPEN
    bcs .error    ; if carry set, the file could not be opened

    ldx #2      ; filenumber 2
    jsr kernal_chkin ; call CHKIN (file 2 now used as input)

	; Skip load address and disk title
	ldy #32
-	jsr kernal_readchar
	dey
	bne -

.read_next_line	
	lda #0
	sta zp_temp + 1
	; Read row pointer
	jsr kernal_readchar
	sta zp_temp
	jsr kernal_readchar
	ora zp_temp
	beq .end_of_dir

	jsr kernal_readchar
	jsr kernal_readchar
-	jsr kernal_readchar
	cmp #0
	beq .read_next_line
	cmp #$22 ; Charcode for "
	bne -
	jsr kernal_readchar
	cmp #$21 ; charcode for !
	bne .not_a_save_file
	jsr kernal_readchar
	cmp #$30 ; charcode for 0
	bcc .not_a_save_file
	cmp first_unavailable_save_slot_charcode
;	cmp #$3a ; (charcode for 9) + 1
	bcs .not_a_save_file
	tax
	lda .occupied_slots - $30,x
	bne .not_a_save_file ; Since there is another save file with the same number, we ignore this file.
	txa
	sta .occupied_slots - $30,x
	jsr printchar_raw
	lda #58
	jsr printchar_raw
	lda #32
	jsr printchar_raw
	dec zp_temp + 1
	
-	jsr kernal_readchar
.not_a_save_file	
	cmp #$22 ; Charcode for "
	beq .end_of_name
	bit zp_temp + 1
	bpl - ; Skip printing if not a save file
	jsr printchar_raw
	bne - ; Always branch
.end_of_name
-	jsr kernal_readchar
	cmp #0 ; EOL
	bne -
	bit zp_temp + 1
	bpl .read_next_line ; Skip printing if not a save file
	lda #13
	jsr printchar_raw
	bne .read_next_line
	
.end_of_dir
	jsr close_io

	; Fill in blanks
	ldx #0
-	lda .occupied_slots,x
	bne +
	txa
	ora #$30
	jsr printchar_raw
	lda #58
	jsr printchar_raw
	lda #13
	jsr printchar_raw
+	inx
	cpx disk_info + 1 ; # of save slots
	bcc -
	; Sort list
	ldx #1
	stx .sort_item
-	jsr .insertion_sort_item
	inc .sort_item
	ldx .sort_item
	cpx disk_info + 1; # of save slots
	bcc -
	
	lda #1 ; Signal success
    rts

.insertion_sort_item
	; Parameters: x, .sort_item: item (1-9)
	stx .current_item
--	jsr .calc_screen_address
	stx zp_temp + 2
	sta zp_temp + 3
	ldx .current_item
	dex
	jsr .calc_screen_address
	stx zp_temp
	sta zp_temp + 1
	ldy #0
	lda (zp_temp + 2),y
	cmp (zp_temp),y
	bcs .done_sort
	; Swap items
	ldy #17
-	lda (zp_temp),y
	pha
	lda (zp_temp + 2),y
	sta (zp_temp),y
	pla
	sta (zp_temp + 2),y
	dey
	bpl -
	dec .current_item
	ldx .current_item
	bne --
.done_sort
	rts
.calc_screen_address
	lda .base_screen_pos
	ldy .base_screen_pos + 1
	stx .counter
	clc
-	dec .counter
	bmi +
	adc #40
	tax
	tya
	adc #0
	tay
	txa
	bcc - ; Always branch
+	tax
	tya
	rts
.dirname
	!pet "$"
.occupied_slots
	!fill 10,0
.disk_error_msg
	!pet 13,"Disk error #",0
.sort_item
	!byte 0
.current_item
	!byte 0
.counter
	!byte 0
.base_screen_pos
	!byte 0,0
.insert_save_disk
	ldx disk_info + 4 ; Device# for save disk
	lda current_disks - 8,x
	sta .last_disk
	beq .dont_print_insert_save_disk ; Save disk is already in drive.
	jsr prepare_for_disk_msgs
	ldy #0
	jsr print_insert_disk_msg
	ldx disk_info + 4 ; Device# for save disk
	lda #0
	sta current_disks - 8,x
	beq .insert_done
.dont_print_insert_save_disk	
	ldx #0
	ldy #5
-	jsr kernal_delay_1ms
	dex
	bne -
	dey
	bne -
.insert_done
    ldx #0
!ifdef Z5PLUS {
	jmp erase_window
} else {
    jsr erase_window
	ldx window_start_row + 1 ; First line in lower window
	ldy #0
	jmp set_cursor
}	
	

.insert_story_disk
	ldy .last_disk
	beq + ; Save disk was in drive before, no need to change
	bmi + ; The drive was empty before, no need to change disk now
	jsr print_insert_disk_msg
	tya
	ldx disk_info + 4 ; Device# for save disk
	sta current_disks - 8,x
+	ldx #0
    jmp erase_window

maybe_ask_for_save_device
	lda ask_for_save_device
	beq .dont_ask
	lda #0
	sta ask_for_save_device
.ask_again
	lda #>.save_device_msg ; high
	ldx #<.save_device_msg ; low
	jsr printstring_raw
	jsr .input_alphanum
	cpx #0
	beq .dont_ask
	cpx #3
	bcs .ask_again
	; One or two digits
	cpx #1
	bne .two_digits
	lda .inputstring
	and #1
	ora #8
	bne .store_device ; Always jump
.two_digits
	lda .inputstring + 1
	and #1
	ora #10
.store_device
	sta disk_info + 4
.dont_ask
	rts
	
restore_game
	jsr maybe_ask_for_save_device

    jsr .insert_save_disk

	; List files on disk
	jsr list_save_files
	beq .restore_failed

	; Pick a slot#
	lda #>.saveslot_msg_restore ; high
	ldx #<.saveslot_msg_restore ; low
	jsr printstring_raw
	lda #>.saveslot_msg ; high
	ldx #<.saveslot_msg ; low
	jsr printstring_raw
	jsr .input_alphanum
	cpx #1
	bne .restore_failed
	lda .inputstring
	cmp first_unavailable_save_slot_charcode
	bpl .restore_failed ; not a number (0-9)
	tax
	lda .occupied_slots - $30,x
	beq .restore_failed ; If the slot is unoccupied, fail.
	sta .restore_filename + 1

	; Print "Restoring..."
	lda #>.restore_msg
	ldx #<.restore_msg
	jsr printstring_raw

	jsr .swap_pointers_for_save
	
	; Perform restore
	jsr do_restore
    bcs .restore_failed    ; if carry set, a file error has happened

	; Swap in z_pc and stack_ptr
	jsr .swap_pointers_for_save
	lda use_reu
	bmi +
    jsr .insert_story_disk
+	jsr get_page_at_z_pc
	lda #0
	ldx #1
	rts
.restore_failed
	lda use_reu
	bmi +
    jsr .insert_story_disk
	; Return failed status
+	lda #0
	tax
	rts

save_game

	jsr maybe_ask_for_save_device

    jsr .insert_save_disk

	; List files on disk
	jsr list_save_files
	beq .restore_failed

	; Pick a slot#
	lda #>.saveslot_msg_save ; high
	ldx #<.saveslot_msg_save ; low
	jsr printstring_raw
	lda #>.saveslot_msg ; high
	ldx #<.saveslot_msg ; low
	jsr printstring_raw
	jsr .input_alphanum
	cpx #1
	bne .restore_failed
	lda .inputstring
	cmp first_unavailable_save_slot_charcode
	bpl .restore_failed ; not a number (0-9)
	sta .filename + 1
	sta .erase_cmd + 3
	
	; Enter a name
	lda #>.savename_msg ; high
	ldx #<.savename_msg ; low
	jsr printstring_raw
	jsr .input_alphanum
	cpx #0
	beq .restore_failed
	
	; Print "Saving..."
	lda #>.save_msg
	ldx #<.save_msg
	jsr printstring_raw

	; Erase old file, if any
    lda #5
    ldx #<.erase_cmd
    ldy #>.erase_cmd
    jsr kernal_setnam
    lda #$0f      ; file number 15
    ldx disk_info + 4 ; Device# for save disk
	ldy #$0f      ; secondary address 15
    jsr kernal_setlfs
    jsr kernal_open ; open command channel and send delete command)
    bcs .restore_failed  ; if carry set, the file could not be opened
    lda #$0f      ; filenumber 15
    jsr kernal_close
	
	; Swap in z_pc and stack_ptr
	jsr .swap_pointers_for_save
	
	; Perform save
	jsr do_save
    bcs .restore_failed    ; if carry set, a save error has happened

	; Swap out z_pc and stack_ptr
	jsr .swap_pointers_for_save

 	lda use_reu
	bmi +
	jsr .insert_story_disk
+	lda #0
	ldx #1
	rts

do_restore
    lda #3
    ldx #<.restore_filename
    ldy #>.restore_filename
    jsr kernal_setnam
    lda #1      ; file number
    ldx disk_info + 4 ; Device# for save disk
	ldy #1      ; not $01 means: load to address stored in file
    jsr kernal_setlfs
    lda #$00      ; $00 means: load to memory (not verify)
    jsr kernal_load
    php ; store c flag so error can be checked by calling routine
    lda #1 
    jsr kernal_close
    plp ; restore c flag
    rts

do_save
    lda .inputlen
    clc
    adc #2 ; add 2 bytes for prefix
    ldx #<.filename
    ldy #>.filename
    jsr kernal_setnam
    lda #1      ; file# 1
    ldx disk_info + 4 ; Device# for save disk
	ldy #1
    jsr kernal_setlfs
    lda #<(stack_start - zp_bytes_to_save)
    sta $c1
    lda #>(stack_start - zp_bytes_to_save)
    sta $c2
    ldx story_start + header_static_mem + 1
    lda story_start + header_static_mem
    clc
    adc #>story_start
    tay
    lda #$c1      ; start address located in $C1/$C2
    jsr kernal_save
    php ; store c flag so error can be checked by calling routine
    lda #1 
    jsr kernal_close
    plp ; restore c flag
    rts
.last_disk	!byte 0
.saveslot !byte 0
.saveslot_msg_save	!pet 13,"Save to",0 ; Will be modified to say highest available slot #
.saveslot_msg_restore	!pet 13,"Restore from",0 ; Will be modified to say highest available slot #
.saveslot_msg	!pet " slot (0-9, RETURN=cancel): ",0 ; Will be modified to say highest available slot #
.savename_msg	!pet "Comment (RETURN=cancel): ",0
.save_msg	!pet 13,"Saving...",13,0
.restore_msg	!pet 13,"Restoring...",13,0
.save_device_msg !pet 13,"Device# (8-11, RETURN=default): ",0
.restore_filename !pet "!0*" ; 0 will be changed to selected slot
.erase_cmd !pet "s:!0*" ; 0 will be changed to selected slot
}

.swap_pointers_for_save
	ldx #zp_bytes_to_save - 1
-	lda zp_save_start,x
	ldy stack_start - zp_bytes_to_save,x
	sta stack_start - zp_bytes_to_save,x
	sty zp_save_start,x
	dex
	bpl -
.swap_pointers_for_save_rts
	rts

!ifdef ACORN {

; SFTODO: This is a bit too pessimistic - since we only save precisely the
; number of bytes in static memory, even if nonstored_blocks has overflowed into
; sideways RAM due to rounding up to a 512-byte boundary, we can still use
; OSFILE if the true dynamic data fits into main RAM. At the price of some
; extra complexity and "trial" builds in the build script, we could have it
; set ACORN_SAVE_RESTORE_OSFIND instead of tying it to ACORN_SWR_BIG_DYNMEM
; like this.
; SFTODO: It would be safer to have to set a flag to get the OSFILE versions,
; so we default to the safe OSFIND versions.
!ifdef ACORN_SWR_BIG_DYNMEM {
ACORN_SAVE_RESTORE_OSFIND = 1
}
!ifdef ACORN_SAVE_RESTORE_OSFIND {
    .save_op = osfind_open_output
    .load_op = osfind_open_input
} else {
    .save_op = osfile_save
    .load_op = osfile_load
}

.filename_buffer = scratch_page
.osfile_check_buffer = scratch_page + 0x100 - 0x12
.filename_buffer_size = (.osfile_check_buffer - .filename_buffer) - 1
; Returns with Z set iff user wants to abort the save/restore.
; SFTODO: Should we save the current filesystem number before user enters any
; * commands and reselect that filesystem afterwards before we access any game
; data?
.get_filename
    ; We use raw OS text output here, because * commands will do and their output
    ; will mix with ours.
    ; Start off with the OS text cursor where it should be.
    jsr s_cursor_to_screenrowcolumn
    ; Set up a text window so the raw OS text output only scrolls what it should.
    clc
    jsr s_pre_scroll
    ; Print the initial descriptive prompt
    jsr set_os_normal_video
    lda #>.filename_msg
    ldx #<.filename_msg
    jsr printstring_os
    ; Loop round printing a brief prompt, reading input and acting accordingly.
.get_filename_loop
.filename_prompt_loads
    lda #$ff ; high byte
    ldx #$ff ; low byte
    jsr printstring_os
    ; Read a string from the keyboard. We don't use OSWORD 0 because it allows
    ; the user to type arbitrary control codes and have them sent through to
    ; OSWRCH.
    jsr turn_on_cursor
    ldx #0
.input_loop
    jsr osrdch
    ; We don't call check_user_interface_controls here. We could do, but:
    ; - It corrupts X. This would be easy to work around.
    ; - In mode 7, the fact we have a text window in effect will interfere with
    ;   changing the colour of the status bar. This could be worked around, but
    ;   it is a big faff and quite a lot of code for very little real benefit.
    cmp #cr
    beq .input_loop_done
    cmp #32
    bcc .input_loop
    cmp #del
    beq .delete
    cpx #.filename_buffer_size - 1
    beq .input_loop
    jsr oswrch
    sta .filename_buffer,x
    inx
    bne .input_loop ; Always branch
.delete
    cpx #0
    beq .input_loop
    jsr oswrch
    dex
    jmp .input_loop
.input_loop_done
    sta .filename_buffer,x
    jsr osnewl
    jsr turn_off_cursor
    ; Skip leading spaces, if any.
    ldy #$ff
-   iny
    lda .filename_buffer,y
    cmp #' '
    beq - 
    ; Check for empty string or * command.
    cmp #13
    beq .swap_pointers_for_save_rts
    cmp #'*'
    beq .oscli
    ; It's a filename. We could just return it and let the filing system do what
    ; it likes with it. However, a) I seem to have a tendency to type "SAVE FOO"
    ; at the save prompt b) DFS silently truncates filenames at a space, and
    ; these two things together mean I end up saving as SAVE instead of FOO as
    ; I intended. Since spaces aren't allowed in filenames, we check for this.
-   iny
    lda .filename_buffer,y
    cmp #' '
    beq .space_in_filename
    cmp #13
    bne -
    tax ; clear Z
    rts
.space_in_filename
    lda #>.space_in_filename_msg
    ldx #<.space_in_filename_msg
    jsr printstring_os
    jmp .get_filename_loop
.oscli
    ldx #1
    ldy #error_print_osasci
    jsr setjmp
    bne .oscli_error
.no_oscli_error
    ; We re-enable Escape generating errors while we're executing the command,
    ; this way the user can terminate a long-running command.
    ldx #0
    jsr do_osbyte_rw_escape_key
    ldx #<.filename_buffer
    ldy #>.filename_buffer
    jsr oscli
    jmp .oscli_done
.oscli_error
    jsr osnewl
.oscli_done
    ldx #1
    jsr do_osbyte_rw_escape_key
    ; There might be an Escape pending which the * command didn't acknowledge,
    ; so we do it ourselves. (To see this is necessary, do *ROMS on OS 3.20 and
    ; press Escape while the output is being produced.)
    lda #osbyte_acknowledge_escape
    jsr osbyte
    jsr set_default_error_handler
    jmp .get_filename_loop
.filename_msg
    ; This message is tweaked to work nicely in 40 or 80 column mode without
    ; needing word wrapping code.
    !text "Please enter a filename or * command or just press RETURN to carry on playing.", 13
    ; SFTODO: We could omit the following message (don't forget all builds would
    ; need the 0!) on a non-VMEM build, where you never need to keep the game disc
    ; in. Arguably it's clearer to say it, and it's harmless except for using
    ; a few bytes of memory. (But as I've said elsewhere, you do need the game
    ; disc in for RESTART when I support this on non-VMEM.)
    !text "You can safely remove the game disc now.", 13, 0
.save_prompt
    !text "save>", 0
.restore_prompt
    !text "restore>", 0
.space_in_filename_msg
    !text "Sorry, no spaces allowed in filenames.", 13, 0

!ifdef VMEM {
    ; This uses s_printchar for output; we've reverted to the normal Ozmoo
    ; output mechanism by the time this is called, and readblocks might use it
    ; to print error messages if we get disc errors during the read, so we need
    ; to be consistent with that.
.get_game_disc_back
    ; We check to see if the game disc is inserted by reading block 0 of the
    ; game file into scratch memory and comparing its CRC it with the one we
    ; calculated on startup. Although save games could include this data,
    ; thanks to the small number of zp bytes included at the beginning of a
    ; save, no sector on as save game disc will have a copy of block 0. It's
    ; also likely something in that block has been modified between startup and
    ; saving the game. Of course it's possible to contrive situations where this
    ; check will allow a non-game disc to be left in the drive, but this is
    ; pretty good and it avoids the need for spurious prompts in the most common
    ; case where the user didn't take the disc out at all.
.retry
    lda #0
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
    lda #1
    sta readblocks_numblocks
    lda #<scratch_page
    sta readblocks_mempos
    lda #>scratch_page
    sta readblocks_mempos + 1
    jsr readblocks
    lda #0
    ldx #<scratch_page
    ldy #>scratch_page
    jsr calculate_crc
    cpx game_disc_crc
    bne .crc_bad
    cpy game_disc_crc + 1
    beq .crc_ok
.crc_bad
!ifdef ACORN_ADFS {
    jsr close_osgbpb_block_handle
}
    lda #>.reinsert_prompt
    ldx #<.reinsert_prompt
    jsr printstring_raw
    jsr wait_for_space
    jmp .retry
.crc_ok
    rts
.reinsert_prompt
    ; This message is tweaked to work nicely in 40 or 80 column mode without
    ; needing word wrapping code.
    !text 13, "Please put the game disc in drive 0 and press SPACE...", 13, 0
}

.io_restore_output
    ; We're about to return control to our caller, so we need to prepare for
    ; a return to Ozmoo-mediated output.
    ; Pick up the current OS cursor position and use it as the position of the
    ; internal Ozmoo cursor used by s_printchar.
    jsr s_screenrowcolumn_from_cursor
    ; Reset the text window; this moves the OS cursor but it doesn't matter as
    ; Ozmoo is now managing the cursor position again.
    lda #vdu_reset_text_window
    jmp oswrch



restore_game
    lda #>.restore_prompt
    sta .filename_prompt_loads + 1
    lda #<.restore_prompt
    sta .filename_prompt_loads + 3
    lda #.load_op
    jsr .save_restore_game
    ; As described in section 8.4 of the Z-machine standards document, we need
    ; to update the header in case the screen dimensions have changed compared
    ; to when this game was saved.
    +lda_screen_height
	sta story_start + header_screen_height_lines
!ifdef Z5PLUS {
	sta story_start + header_screen_height_units + 1
}
    +lda_screen_width
	sta story_start + header_screen_width_chars
!ifdef Z5PLUS {
	sta story_start + header_screen_width_units + 1
}
.restore_game_rts
    rts
    
save_game
    lda #>.save_prompt
    sta .filename_prompt_loads + 1
    lda #<.save_prompt
    sta .filename_prompt_loads + 3
    lda #.save_op
    ; fall through to .save_restore_game
.save_restore_game
.osfile_or_osfind_op = zp_temp ; 1 byte
.result = zp_temp + 1 ; 1 byte, 0 for failure, 1 for success
!ifdef ACORN_SAVE_RESTORE_OSFIND {
    .start_ptr = zp_temp + 2 ; 2 bytes
!ifdef ACORN_ELECTRON {
    .bytes_to_read = zp_temp + 4 ; 2 bytes
}
}
    sta .osfile_or_osfind_op

!ifdef ACORN_ADFS {
    jsr close_osgbpb_block_handle
}

    ; We default the result to failure and only set it to success after actually
    ; loading/saving something. If the user aborts the operation, that counts
    ; as failure. See discussion at
    ; https://stardot.org.uk/forums/viewtopic.php?f=2&t=19975&p=281210#p281204
    ; and in the light of that, note that if this subroutine returned 1
    ; (success) after a restore was aborted by the user, we wouldn't comply with
    ; the observation in the Z-machine standard (under "restore") saying
    ; "If the restore fails, 0 is returned, but once again this necessarily
    ; happens since otherwise control is already elsewhere.", because we would
    ; return 2 (z_ins_restore bumps 1->2) but control would not be elsewhere.
    ; Finally, note that the Commodore 64 code treats aborting as failure.
    lda #0
    sta .result
    jsr .get_filename
    beq .save_restore_game_cleanup_partial ; user aborted 

	; Swap in z_pc and stack_ptr
    ; We need to normalise z_local_vars_ptr and stack_ptr first so saves are
    ; compatible between different builds.
    lda z_local_vars_ptr + 1
    sec
    sbc #>stack_start
    sta z_local_vars_ptr + 1
    lda stack_ptr + 1
    sec
    sbc #>stack_start
    sta stack_ptr + 1
	jsr .swap_pointers_for_save

	; Perform save or load
    ldx #1
    ldy #error_print_osasci
    jsr setjmp
    beq .no_osfile_error
!ifdef ACORN_SAVE_RESTORE_OSFIND {
    jsr close_osgbpb_block_handle
}
    ; If this is a load and a disc error occurred partway through, the game is
    ; probably in an inconsistent or otherwise corrupt state. There really isn't
    ; much we can do - we don't have enough RAM to take a copy of the game
    ; before the load so we can restore it if the load fails. SFTODO: While it
    ; would break savegame compatibility with Commodore port - unless the change
    ; was pushed upstream - we could calculate a CRC over the save and put it
    ; in an additional two bytes at the start. We could then re-calculate the
    ; CRC after a load and if it doesn't match we at least know the load failed
    ; in a dangerous way and can hang/print a very loud warning. (Most errors
    ; are harmless, e.g. "file not found", so we need the CRC to distinguish
    ; harmless and dangerous errors.) This would work well, but it would add
    ; extra code (less free RAM => more disc access) for an unlikely case. (I
    ; don't think the savegame compatibility is a big issue; even if this ever
    ; turns out to be useful, it's easy enough to chop two bytes off the front
    ; of an Acorn file before transferring to Commodore or to calculate a CRC
    ; for a Commodore savegame before loading onto Acorn.) I do already have CRC
    ; code now for the disc swap, so this wouldn't cost all that much extra.
    ; (I'm not super keen to do this, but savegame compatibility with the C64
    ; is a red herring, since those savegames will include some absolute C64
    ; addresses in the zp bytes and would need a trivial-ish fixup *anyway*,
    ; they would not "just work".)
    jsr osnewl
    jmp .save_restore_game_cleanup_full
.no_osfile_error
    ; If it's a save, check if we're going to overwrite an existing file and
    ; ask the user if that's OK.
    lda .osfile_or_osfind_op
    cmp #.save_op
    bne .not_save
    jsr .check_for_overwrite
    bne .save_restore_game_cleanup_full
.not_save
    ; The OSFILE block is updated after the call, so we have to populate these
    ; values via code every time.
!ifndef ACORN_SAVE_RESTORE_OSFIND {
    lda #<.filename_buffer
    sta .osfile_save_load_block_filename_ptr
    lda #>.filename_buffer
    sta .osfile_save_load_block_filename_ptr + 1
    .start_ptr = .osfile_save_load_block_start_address
}
    lda #<(stack_start - zp_bytes_to_save)
    sta .start_ptr
    lda #>(stack_start - zp_bytes_to_save)
    sta .start_ptr + 1
!ifndef ACORN_SAVE_RESTORE_OSFIND {
    lda story_start + header_static_mem + 1
    sta .osfile_save_load_block_end_address
    lda story_start + header_static_mem
    clc
    adc #>story_start
    sta .osfile_save_load_block_end_address + 1
} else {
!ifndef ACORN_ELECTRON {
    lda story_start + header_static_mem + 1
    clc
    adc #<(stack_size + zp_bytes_to_save)
    sta .osgbpb_save_length
    lda story_start + header_static_mem
    adc #>(stack_size + zp_bytes_to_save)
    sta .osgbpb_save_length + 1
} else {
    ; On the Electron build, .osfile_pseudo_emulation sets .osgbpb_save_length.
}
}
    lda .osfile_or_osfind_op
!ifndef ACORN_SAVE_RESTORE_OSFIND {
    ldx #<.osfile_save_load_block
    ldy #>.osfile_save_load_block
    jsr osfile
} else {
    jsr .osfile_pseudo_emulation
}
    lda #1
    sta .result

.save_restore_game_cleanup_full
	; Swap out z_pc and stack_ptr
	jsr .swap_pointers_for_save
    ; We need to un-normalise z_local_vars_ptr and stack_ptr now.
    lda z_local_vars_ptr + 1
    clc
    adc #>stack_start
    sta z_local_vars_ptr + 1
    lda stack_ptr + 1
    clc
    adc #>stack_start
    sta stack_ptr + 1
    jsr set_default_error_handler
    ; If we just restored we have updated z_pc and need to take that into
    ; account. This is unnecessary but harmless if we just saved.
	jsr get_page_at_z_pc
.save_restore_game_cleanup_partial
 	jsr .io_restore_output
    ; SFTODO: We should probably do get_game_disc_back even for non-VMEM, now
    ; we always support RESTART.
!ifdef VMEM {
    jsr .get_game_disc_back
}
    lda #0
    ldx .result ; must be last as caller will check Z flag
	rts

; On exit, beq will branch if it's OK to save the file.
.check_for_overwrite
    ; We don't use .osfile_save_load_block here (even if it's in this build)
    ; because we don't want to trample over the load address in there.
    lda #<.filename_buffer
    sta .osfile_check_buffer
    lda #>.filename_buffer
    sta .osfile_check_buffer + 1
    lda #osfile_read_catalogue_information
    ldx #<.osfile_check_buffer
    ldy #>.osfile_check_buffer
    jsr osfile
    tax
    +make_acorn_screen_hole_jmp
    beq .file_doesnt_exist
    lda #>.overwrite_msg
    ldx #<.overwrite_msg
    jsr printstring_os
    jsr turn_on_cursor
    lda #osbyte_flush_buffer
    ldx #buffer_keyboard
    jsr osbyte
-   jsr osrdch
    cmp #'Y'
    beq +
    cmp #'y'
    beq +
    cmp #'N'
    beq +
    cmp #'n'
    bne -
+   and #!$20 ; force upper case
    jsr oswrch
    pha
    jsr turn_off_cursor
    jsr osnewl
    pla
    cmp #'Y'
.file_doesnt_exist
    rts
.overwrite_msg
    !text "File exists - overwrite it? (Y/N) ", 0

!ifndef ACORN_SAVE_RESTORE_OSFIND {
!ifdef ACORN_ELECTRON {
    !error "ACORN_ELECTRON is only compatible with ACORN_SAVE_RESTORE_OSFIND"
}

.osfile_save_load_block
.osfile_save_load_block_filename_ptr
    !word 0 ; filename
    !word stack_start - zp_bytes_to_save ; load address low
    !word 0 ; load address high
    !word 0 ; exec address low: 0 => use specified load address (on load)
    !word 0 ; exec address high
.osfile_save_load_block_start_address
    !word 0 ; start address low
    !word 0 ; start address high
.osfile_save_load_block_end_address
    !word 0 ; end address low
    !word 0 ; end address high
} else { ; ACORN_SAVE_RESTORE_OSFIND
.osgbpb_wrapper
    ; These values in the OSGBPB block keep getting updated, so we have to
    ; set them every time.
    ldx #<scratch_page
    stx osgbpb_block_data_address
    ldx #>scratch_page
    stx osgbpb_block_data_address + 1
    ldx #<osgbpb_block
    ldy #>osgbpb_block
    jmp osgbpb

; This code pseudo-emulates OSFILE using OSFIND+OSGBPB, using a bounce buffer so
; it can handle data located in sideways RAM. A contains the OSFIND operation
; code on entry.
.osfile_pseudo_emulation
.chunk_size = 256 ; for documentation; we hard-code assumptions about this too
    ; Open the file
    ldx #<.filename_buffer
    ldy #>.filename_buffer
    jsr osfind
    cmp #0
    bne +
    brk
    !byte err_not_found
    !text "Not found"
    !byte 0
+   sta osgbpb_block_handle

    ; Is this a load or save?
    lda .osfile_or_osfind_op
    cmp #osfind_open_input
    beq .osfile_pseudo_emulation_load

    ; It's a save.
!ifdef ACORN_ELECTRON {
    ; On this build the "zero page"+stack area isn't contiguous with dynamic
    ; memory, so we need to do two separate invocations of
    ; .osfile_pseudo_emulation_save_internal.
    lda #<(zp_bytes_to_save + stack_size)
    sta .osgbpb_save_length
    lda #>(zp_bytes_to_save + stack_size)
    sta .osgbpb_save_length + 1
    jsr .osfile_pseudo_emulation_save_internal
    lda #<story_start
    sta .start_ptr
    lda #>story_start
    sta .start_ptr + 1
    lda story_start + header_static_mem + 1
    sta .osgbpb_save_length
    lda story_start + header_static_mem
    sta .osgbpb_save_length + 1
    jsr .osfile_pseudo_emulation_save_internal
    jmp close_osgbpb_block_handle

.osfile_pseudo_emulation_save_internal
}

.osgbpb_save_length = osfile_emulation_workspace
.osgbpb_save_loop
    ; Save the smaller of .chunk_size and .osgbpb_save_length bytes. If
    ; .osgbpb_save_length is 0, we're done.
    ldx #<.chunk_size
    ldy #>.chunk_size
    lda .osgbpb_save_length + 1
    bne +
    tay
    ldx .osgbpb_save_length
    beq .osgbpb_save_done
+   stx osgbpb_block_transfer_length
    sty osgbpb_block_transfer_length + 1

    ; Subtract the number of bytes to save from .osgbpb_save_length.
    sec
    lda .osgbpb_save_length
    sbc osgbpb_block_transfer_length
    sta .osgbpb_save_length
    lda .osgbpb_save_length + 1
    sbc osgbpb_block_transfer_length + 1
    sta .osgbpb_save_length + 1

    ; Copy the data into the bounce buffer.
    ldy #0
-   lda (.start_ptr),y
    sta scratch_page,y
    iny
    cpy osgbpb_block_transfer_length
    bne -

    ; Advance .start_ptr.
    clc
    lda .start_ptr
    adc osgbpb_block_transfer_length
    sta .start_ptr
    lda .start_ptr + 1
    adc osgbpb_block_transfer_length + 1
    sta .start_ptr + 1

    ; Write the bounce buffer out.
    lda #osgbpb_write_ignoring_ptr
    jsr .osgbpb_wrapper

    ; Loop round until we're done.
    jmp .osgbpb_save_loop

.osgbpb_save_done
!ifndef ACORN_ELECTRON {
    jmp close_osgbpb_block_handle
} else {
    rts
}

    ; It's a load.
.osfile_pseudo_emulation_load
!ifdef ACORN_ELECTRON {
    ; On this build the "zero page"+stack area isn't contiguous with dynamic
    ; memory, so we need to do two separate invocations of
    ; .osfile_pseudo_emulation_load_internal.
    lda #<(zp_bytes_to_save + stack_size)
    sta .bytes_to_read
    lda #>(zp_bytes_to_save + stack_size)
    sta .bytes_to_read + 1
    jsr .osfile_pseudo_emulation_load_internal
    lda #<story_start
    sta .start_ptr
    lda #>story_start
    sta .start_ptr + 1
    lda #$ff
    sta .bytes_to_read
    sta .bytes_to_read + 1
    jsr .osfile_pseudo_emulation_load_internal
    jmp close_osgbpb_block_handle

.osfile_pseudo_emulation_load_internal
}

.bytes_read = osfile_emulation_workspace
.osgbpb_load_loop
    ; Read some data into the bounce buffer and work out how much we read; if we
    ; read nothing, it's EOF and we're done. On the Electron we read a maximum
    ; of .bytes_to_read bytes; on other builds we just read until EOF.
!ifndef ACORN_ELECTRON {
    ldx #<.chunk_size
    stx osgbpb_block_transfer_length
    ldx #>.chunk_size
    stx osgbpb_block_transfer_length + 1
} else {
    ldx #<.chunk_size
    ldy #>.chunk_size
    lda .bytes_to_read + 1
    bne .read_full_chunk
    tay
    ldx .bytes_to_read
.read_full_chunk
    stx osgbpb_block_transfer_length
    stx .lda_imm_chunk_size_low + 1
    sty osgbpb_block_transfer_length + 1
    sty .lda_imm_chunk_size_high + 1
}
    lda #osgbpb_read_ignoring_ptr
    jsr .osgbpb_wrapper
    sec
!ifdef ACORN_ELECTRON {
.lda_imm_chunk_size_low
}
    lda #<.chunk_size
    sbc osgbpb_block_transfer_length
    sta .bytes_read
!ifdef ACORN_ELECTRON {
.lda_imm_chunk_size_high
}
    lda #>.chunk_size
    sbc osgbpb_block_transfer_length + 1
    sta .bytes_read + 1
    ora .bytes_read
    beq .osgbpb_load_done

    ; Copy the data out of the bounce buffer.
    ldy #0
-   lda scratch_page,y
    sta (.start_ptr),y
    iny
    cpy .bytes_read
    bne -

    ; If we read less than .chunk_size bytes, we've hit EOF and we're done.
    lda .bytes_read + 1
    beq .osgbpb_load_done

    ; SFTODO: Both of the following chunks of code (electron and general) could
    ; be simplified, because we know we read .chunk_size == 0x100 bytes - we
    ; don't need to do a general subtraction or addition.

!ifdef ACORN_ELECTRON {
    ; Decrement .bytes_to_read
    sec
    lda .bytes_to_read
    sbc .bytes_read
    sta .bytes_to_read
    lda .bytes_to_read + 1
    sbc .bytes_read + 1
    sta .bytes_to_read + 1
}

    ; Advance .start_ptr
    clc
    lda .start_ptr
    adc .bytes_read
    sta .start_ptr
    lda .start_ptr + 1
    adc .bytes_read + 1
    sta .start_ptr + 1

    ; Loop round until we're done.
    bne .osgbpb_load_loop ; Always branch

.osgbpb_load_done
!ifndef ACORN_ELECTRON {
    ; fall through to close_osgbpb_block_handle
} else {
    rts
}
}

!ifdef ACORN_ADFS {
    ACORN_WANT_OSGBPB_BLOCK = 1
} else {
    !ifdef ACORN_SAVE_RESTORE_OSFIND {
        ACORN_WANT_OSGBPB_BLOCK = 1
    }
}
!ifdef ACORN_WANT_OSGBPB_BLOCK {
; Close .osgbpb_block_handle iff it's non-0. If it's non-0 we set it to 0
; before doing anything, so we don't get into an infinite loop if the close
; operation fails.
close_osgbpb_block_handle
    ldy osgbpb_block_handle
    beq +
    lda #osfind_close ; 0
    sta osgbpb_block_handle
    jmp osfind
+   rts ; SFTODO: Can we move this and share a nearby rts?

; This block may be shared by both ADFS readblocks and OSFIND+OSGBPB save/
; restore. This isn't a problem, because we don't *want* to keep the game data
; file open across a save/restore anyway as the disc may be removed/dismounted.
osgbpb_block
osgbpb_block_handle
    !byte 0
osgbpb_block_data_address
    !word 0 ; low word
    !word 0 ; high word
osgbpb_block_transfer_length
    !word 0 ; low word
    !word 0 ; high word
osgbpb_block_pointer
    !word 0 ; pointer low word
    !word 0 ; pointer high word
}
}
}
