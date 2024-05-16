; SFTODO: JUST CHECK WHY THE ELECTRON VERSION CORRUPTS THE LOADER SCREEN - IS THE GAME LOADING UNNCESSARILY HIGH? - I THINK A BIG PART OF THIS IS THE LZSA EXE COMPRESSION, WHICH MAKES THE IN-MEMORY FOOTPRINT OF THE BINARY LARGER
; Acorn version of disk.asm. There are a few brief bits of code here which are
; duplicates of code in disk.asm, but on the whole things are so different it
; seems better to keep this completely separate and accept the duplication.

; SFTODO: readblock could be part of discardable init code on a non-VMEM build;
; maybe make it a macro so ozmoo.asm can put it in discardable init or not as
; appropriate? (If we support restart on non-VMEM builds for Acorn, we'd maybe
; need readblock to do the check for having the game disc in the drive.)

readblocks
    ; read <n> blocks (each 256 bytes) from disc to memory
    ; set values in readblocks_* before calling this function
    ; register: a,x,y
!ifdef TRACE_FLOPPY {
    jsr newline
    jsr print_following_string
    !text "readblocks (n,zp,c64) ",0
    lda readblocks_numblocks
    jsr printa
    jsr comma
    lda readblocks_currentblock + 1
    jsr print_byte_as_hex
    lda readblocks_currentblock
    jsr print_byte_as_hex
    jsr comma
    ; SFTODO: FOR TURBO COPRO ARGUABLY WE SHOULD PRINT readblocks_mempos + 2
    lda readblocks_mempos + 1
    jsr print_byte_as_hex
    lda readblocks_mempos 
    jsr print_byte_as_hex
    jsr newline
}
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
    sta undo_copy_lda_abs_y + 2
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
    !text cr, "Press SPACE to retry...", 0
    jsr wait_for_space
    lda #cr
    jsr s_printchar
    lda #cr
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
    cmp #50
    bcs +
    ; The error code doesn't matter, but we use a printable one here to confirm it
    ; isn't being printed by accident. (We wouldn't notice if it were 0.)
    +os_error 'A', "Fake read error"
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
!if 1 { ; SFTODO EXPERIMENTAL - 24 bytes (plus " &xx"=4 in error message)
    pha
    lsr
    lsr
    lsr
    lsr
    ldx #(-2 & $ff)
-   and #$0f
    cmp #10
    bcc +
    adc #'A'-10-1-'0'
+   adc #'0'
    sta .disc_read_error_number-(-2 & $ff),x
    pla
    inx
    bne -
}
    brk
    !byte 0
    !text "Disc error &"
.disc_read_error_number
    ; SFTODO: The " &xx" bit should only be included if the above code is
    !text "xx"
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
    +os_error 0, "Can't open DATA"
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
!ifdef ACORN_TURBO_SUPPORTED {
    lda readblocks_mempos + 2
    sta osgbpb_block_data_address + 2
}
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
    sta undo_copy_sta_abs_y + 2
    ldx #1
    ldy #0
-   
undo_copy_lda_abs_y
    lda scratch_double_page,y
undo_copy_sta_abs_y
    sta $ff00,y ; patched
    iny
    bne -
    inc undo_copy_sta_abs_y + 2
    inc undo_copy_lda_abs_y + 2
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
!ifdef ACORN_TURBO_SUPPORTED {
    !byte 0
}
}

wait_for_space
    lda #osbyte_flush_buffer
    ldx #buffer_keyboard
    jsr osbyte
-   jsr osrdch
    cmp #' '
    bne -
    rts

RESTART_SUPPORTED = 1 ; SFTODO: move into ozmoo.asm?
z_ins_restart

    ; SFTODO: We should probably check the game disc is in the drive here,
    ; although really the user shouldn't have taken it out anyway. Is this that
    ; much different than them doing so during normal gameplay? readblocks will
    ; (of course) happily read junk from whatever random disc is in the drive.

    ; Since we discarded our initialisation code on startup, we have to
    ; re-execute the Ozmoo binary from disc to restart.

    ; The re-executed binary will display a loading progress bar, so put the
    ; OS cursor in the right place ready for it.
    jsr s_cursor_to_screenrowcolumn

!ifndef ACORN_ADFS {
    ; On DFS, the loader puts the restart command at this address; this saves a
    ; tiny bit of space in the binary and (more importantly) means the binary
    ; doesn't need to know at build time whether it's on drive 0 or drive 2.
restart_command = game_data_filename_or_restart_command
    ldx #<restart_command
    ldy #>restart_command
} else {
game_data_filename = game_data_filename_or_restart_command
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
+   cmp #cr
    bne -
    ldx .last_dot
    ldy #255
-   inx
    iny
    lda .executable_leafname,y
    sta scratch_page,x
    cmp #cr
    bne -
    ldx #<scratch_page
    ldy #>scratch_page
}
    jmp oscli

!ifdef ACORN_ADFS {
.executable_leafname
!ifndef ACORN_SWR {
    !text "OZMOO2P", cr
} else {
    !ifndef ACORN_ELECTRON_SWR {
        !ifndef ACORN_NO_SHADOW {
            !text "OZMOOSH", cr
        } else {
            !text "OZMOOB", cr
        }
    } else { ; ACORN_ELECTRON_SWR
        !text "OZMOOE", cr
    }
}
}

z_ins_restore
!ifndef Z4PLUS {
	jsr restore_game
	beq +
	jmp make_branch_true
+	jmp make_branch_false
}
!ifdef Z4PLUS {
	jsr restore_game
	beq +
	inx
+	jmp z_store_result
}

z_ins_save
!ifndef Z4PLUS {
	jsr save_game
	beq +
	jmp make_branch_true
+	jmp make_branch_false
}
!ifdef Z4PLUS {
	jsr save_game
	jmp z_store_result
}

!zone save_restore {
; It's faster and simpler to save/restore using OSFILE but it can't access
; sideways RAM, so we can only use if it's we're not using ACORN_SWR_MEDIUM_OR_BIG_DYNMEM.
; SFTODO: This is bit too pessimistic - since we only save precisely the number
; of bytes in dynamic memory as specified by the game's header, even if
; nonstored_pages has overflowed into sideways RAM due to rounding up of some
; kind, we can still use OSFILE if the actual dynamic memory fits into main RAM.
; The build system already passes this in as ACORN_DYNAMIC_SIZE_BYTES, so this
; isn't even hard. (Or is it? What would the condition be here? !if
; !ACORN_SWR_BIG_DYNMEM or (story_start + ACORN_DYNAMIC_SIZE_BYTES <= $8000) {
; ACORN_SAVE_RESTORE_OSFILE = 1 }), and I think things might get circular
; because story_start will move depending on the result of this test. Maybe I'm
; getting confused, but I think the build system would have to be responsible
; for doing two trial builds.)
; SFTODO: Comment a bit outdated now we have medium model
!ifndef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
ACORN_SAVE_RESTORE_OSFILE = 1
}

; We don't need this code if we're saving piecemeal; it works out better to just
; save the zero page addresses directly.
!ifdef ACORN_SAVE_RESTORE_OSFILE {
.swap_pointers_for_save
	ldx #zp_bytes_to_save - 1
-	lda zp_save_start,x
	ldy stack_start - zp_bytes_to_save,x
	sta stack_start - zp_bytes_to_save,x
	sty zp_save_start,x
	dex
	bpl -
	rts
}

!ifdef ACORN_SAVE_RESTORE_OSFILE {
.save_op = osfile_save
.load_op = osfile_load
} else {
.save_op = osfind_open_output
.load_op = osfind_open_input
}

.filename_buffer = scratch_page
.osfile_check_buffer = scratch_page + 0x100 - 0x12
.filename_buffer_size = (.osfile_check_buffer - .filename_buffer) - 1
; On exit beq will branch iff user wants to abort the save/restore.
; SFTODO: Should we save the current filesystem number before user enters any
; * commands and reselect that filesystem afterwards before we access any game
; data?
.get_filename
    ; We use raw OS text output here, because * commands will do and their output
    ; will mix with ours.
    ; Start off with the OS text cursor where it should be.
    jsr s_cursor_to_screenrowcolumn
    ; Set up a text window so the raw OS text output only scrolls what it should.
    jsr s_pre_scroll_leave_cursor_in_place
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
    cmp #' '
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
    cmp #cr
    beq .get_filename_rts
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
    cmp #cr
    bne -
    tax ; clear Z
.get_filename_rts
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
    ; SFTODNOW: I think there's a long-standing glitch here where if you're
    ; on an Integra-B and the private RAM bank is in use (i.e. the machine doesn't
    ; have so much SWR we don't bother with it), you can hang the machine by
    ; pressing Escape during the execution of the * command because PRVS4/8 are
    ; set (all the time Ozmoo is executing) and when the acknowledge escape
    ; OSBYTE causes the OS to flush buffers, IBOS sets b6 of ROMSEL *without*
    ; clearing PRVS4/8 itself.
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
    !text "Please enter a filename or * command or just press RETURN to carry on playing.", cr
    ; SFTODO: We could omit the following message (don't forget all builds would
    ; need the 0!) on a non-VMEM build, where you never need to keep the game disc
    ; in. Arguably it's clearer to say it, and it's harmless except for using
    ; a few bytes of memory. (But as I've said elsewhere, you do need the game
    ; disc in for RESTART when I support this on non-VMEM.)
    !text "You can safely remove the game disc now.", cr, 0
.save_prompt
    !text "save>", 0
.restore_prompt
    !text "restore>", 0
.space_in_filename_msg
    !text "Sorry, no spaces allowed in filenames.", cr, 0

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
    ; save, no sector on a save game disc will have a copy of block 0. It's
    ; also likely something in that block has been modified between startup and
    ; saving the game. Of course it's possible to contrive situations where this
    ; check will allow a non-game disc to be left in the drive, but this is
    ; pretty good and it avoids the need for spurious prompts in the most common
    ; case where the user didn't take the disc out at all.
.retry
    lda #0
    sta readblocks_currentblock
    sta readblocks_currentblock + 1
!ifdef ACORN_TURBO_SUPPORTED {
    sta readblocks_mempos + 2
}
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
    !text cr, "Please put the game disc in drive 0 and press SPACE...", cr, 0
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
    ; to when this game was saved. (HHGTTG SG, at least, still seems to generate
    ; the status bar using the "original" width, but I can reproduce this is
    ; frotz so I guess it's a small bug in the game itself.)
    ; SFTODO: Not just here specifically, I can't help feeling read/write_header_*
    ; could be optimised (for space; I don't think they're performance critical)
    ; on Acorn. If nothing else, on non-medium-SWR builds, they boil down to
    ; trivial reads/writes of main RAM.
    php
    pha
    +lda_screen_height
    ldy #header_screen_height_lines
    jsr write_header_byte
!ifdef Z5PLUS {
    ldy #header_screen_height_units + 1
    jsr write_header_byte
}
    +lda_screen_width
    ldy #header_screen_width_chars
    jsr write_header_byte
!ifdef Z5PLUS {
    ldy #header_screen_width_units + 1
    jsr write_header_byte
}
    pla
    plp
.restore_game_rts
    rts

.save_restore_game_cleanup_partial_indirect
    jmp .save_restore_game_cleanup_partial ; SFTODO CAN I AVOID THIS?
    
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
!ifndef ACORN_SAVE_RESTORE_OSFILE {
    ; SFTODO: Rename these variables?? They're not that bad, just a thought.
    .osgbpb_data_ptr = zp_temp + 2 ; 2 bytes
    .osgbpb_length = zp_temp + 4 ; 2 bytes
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
    beq .save_restore_game_cleanup_partial_indirect ; user aborted

!ifdef ACORN_SWR_MEDIUM_OR_BIG_DYNMEM {
    ; Now we know we won't be doing a partial cleanup, we can page in the SWR
    ; bank containing dynamic memory, so the load/save can see it. We don't need
    ; to explicitly undo this; calling get_page_at_z_pc with zp_pc_h set to $ff
    ; below will do it for us.
    ; SFTODO: This could use a "slow" version of the paging macro, but it would be the
    ; only such user with this ram bank so probably no point?
    +acorn_page_in_bank_using_a dynmem_ram_bank
}

    ; We need to normalise z_local_vars_ptr and stack_ptr first before saving so
    ; saves are compatible between different builds.
    lda z_local_vars_ptr + 1
    sec
    sbc #>stack_start
    sta z_local_vars_ptr + 1
    lda stack_ptr + 1
    sec
    sbc #>stack_start
    sta stack_ptr + 1
!ifdef ACORN_SAVE_RESTORE_OSFILE {
	; Swap in z_pc and stack_ptr
	jsr .swap_pointers_for_save
}

	; Perform save or load
    ldx #1
    ldy #error_print_osasci
    jsr setjmp
    beq .no_osfile_error
!ifndef ACORN_SAVE_RESTORE_OSFILE {
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
    bne .no_overwrite_check
    jsr .check_for_overwrite
    bne .save_restore_game_cleanup_full
.no_overwrite_check
!ifdef ACORN_SAVE_RESTORE_OSFILE {
    ; The OSFILE block is updated after the call, so we have to reset it via code
    ; every time.
    ldx #.osfile_save_load_block_length - 1
-   lda .osfile_save_load_block_master,x
    sta .osfile_save_load_block,x
    dex
    bpl -
    lda .osfile_or_osfind_op
    ldx #<.osfile_save_load_block
    ldy #>.osfile_save_load_block
    jsr osfile
} else {
    jsr .open_file ; SFTODO: I THINK THIS IS ONLY CALLER, SO INLINE THIS IF SO
    ; We save/load the handful of bytes at zp_save_start as a separate
    ; operation; this saves us having to use .swap_pointers_for_save and makes
    ; it easy to skip over the screen hole (if we have one) because the second
    ; save/load operation is working with page-aligned data starting at
    ; stack_start.
    lda #zp_save_start
    sta .osgbpb_data_ptr
    lda #zp_bytes_to_save
    sta .osgbpb_length
    lda #0
    sta .osgbpb_data_ptr + 1
    sta .osgbpb_length + 1
    jsr .osgbpb_save_or_load
    lda #<stack_start
    sta .osgbpb_data_ptr
    lda #>stack_start
    sta .osgbpb_data_ptr + 1
!ifndef ACORN_SWR_MEDIUM_DYNMEM {
    ; Save the stack and dynamic memory in one go, as they're adjacent.
.non_zp_length = stack_size + ACORN_DYNAMIC_SIZE_BYTES
    lda #<.non_zp_length
    sta .osgbpb_length
    lda #>.non_zp_length
    sta .osgbpb_length + 1
    jsr .osgbpb_save_or_load
} else {
    ; Save the stack.
    lda #<stack_size
    sta .osgbpb_length
    lda #>stack_size
    sta .osgbpb_length + 1
    jsr .osgbpb_save_or_load
    ; Save the dynamic memory.
    lda #<story_start
    sta .osgbpb_data_ptr
    lda #>story_start
    sta .osgbpb_data_ptr + 1
    lda #<ACORN_DYNAMIC_SIZE_BYTES
    sta .osgbpb_length
    lda #>ACORN_DYNAMIC_SIZE_BYTES
    sta .osgbpb_length + 1
    jsr .osgbpb_save_or_load
}
    jsr close_osgbpb_block_handle
}
    lda #1
    sta .result

.save_restore_game_cleanup_full
!ifdef ACORN_SAVE_RESTORE_OSFILE {
	; Swap out z_pc and stack_ptr
	jsr .swap_pointers_for_save
}
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
!ifdef ACORN_SWR {
    ; We set zp_pc_h to an invalid value to avoid any risk of relying on an
    ; outdated value in mempointer_ram_bank. SFTODO: Do I mean z_pc_mempointer_ram_bank really?
    lda #$ff
    sta zp_pc_h
}
!ifdef ACORN_TURBO_SUPPORTED {
    ; We set zp_pc_h to an invalid value to avoid any risk of relying on an
    ; outdated value in mempointer_turbo_bank. SFTODO: Do I mean z_pc_mempointer_turbo_bank really?
    lda #$ff
    sta zp_pc_h
}
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

; On exit, beq will branch iff it's OK to save the file.
.check_for_overwrite
    lda #<.filename_buffer
    sta .osfile_check_buffer
    lda #>.filename_buffer
    sta .osfile_check_buffer + 1
    lda #osfile_read_catalogue_information
    ldx #<.osfile_check_buffer
    ldy #>.osfile_check_buffer
    jsr osfile
    tax
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

!ifdef ACORN_SAVE_RESTORE_OSFILE {
.osfile_save_load_block_master
    !word .filename_buffer ; filename
    !word stack_start - zp_bytes_to_save ; load address low
    !word 0 ; load address high
    !word 0 ; exec address low: 0 => use specified load address (on load)
    !word 0 ; exec address high
    !word stack_start - zp_bytes_to_save ; start address low
    !word 0 ; start address high
    !word (story_start + ACORN_DYNAMIC_SIZE_BYTES) & $ffff ; end address low
    !word 0 ; end address high
.osfile_save_load_block_master_end
.osfile_save_load_block_length = .osfile_save_load_block_master_end - .osfile_save_load_block_master
.osfile_save_load_block
    !fill .osfile_save_load_block_length
} else { ; !ACORN_SAVE_RESTORE_OSFILE
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

.open_file
    ; Open the file
    lda .osfile_or_osfind_op
    ldx #<.filename_buffer
    ldy #>.filename_buffer
    jsr osfind
    cmp #0
    bne +
    +os_error err_not_found, "Not found"
+   sta osgbpb_block_handle
.open_file_rts
    rts


.chunk_size = 256 ; for documentation; we hard-code assumptions about this too

; Use OSGBPB to to save or load (specified by an OSFIND operation code in A on
; entry) .osgbpb_length bytes at address .osgbpb_data_ptr. A bounce buffer is
; used so data can be located in sideways RAM, which the caller should have
; ensured is paged in appropriately. Any screen hole is automatically skipped,
; provided (as will be the case) .osgbpb_data_ptr is page-aligned.
.osgbpb_save_or_load
    lda .osfile_or_osfind_op
    cmp #osfind_open_input
    beq .osgbpb_load

.osgbpb_save_loop
    ; Save the smaller of .chunk_size and .osgbpb_length bytes. If
    ; .osgbpb_length is 0, we're done.
    ldx #<.chunk_size
    ldy #>.chunk_size
    lda .osgbpb_length + 1
    bne +
    tay
    ldx .osgbpb_length
    beq .open_file_rts
+   stx osgbpb_block_transfer_length
    sty osgbpb_block_transfer_length + 1

    ; Subtract the number of bytes to save from .osgbpb_length.
    sec
    lda .osgbpb_length
    sbc osgbpb_block_transfer_length
    sta .osgbpb_length
    lda .osgbpb_length + 1
    sbc osgbpb_block_transfer_length + 1
    sta .osgbpb_length + 1

    ; Copy the data into the bounce buffer.
    ldy #0
-   lda (.osgbpb_data_ptr),y
    sta scratch_page,y
    iny
    cpy osgbpb_block_transfer_length
    bne -

    ; Advance .osgbpb_data_ptr.
    clc
    lda .osgbpb_data_ptr
    adc osgbpb_block_transfer_length
    sta .osgbpb_data_ptr
    lda .osgbpb_data_ptr + 1
    adc osgbpb_block_transfer_length + 1
!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
    ; Note that we check for equality here, because we're looping round and we
    ; only want to apply the adjustment when we first hit the screen hole.
    cmp acorn_screen_hole_start_page
    bne +
    adc acorn_screen_hole_pages_minus_one ; -1 as carry is set
+
}
    sta .osgbpb_data_ptr + 1

    ; Write the bounce buffer out.
    lda #osgbpb_write_ignoring_ptr
    jsr .osgbpb_wrapper

    ; Loop round until we're done.
    jmp .osgbpb_save_loop

.osgbpb_load
.bytes_read = memory_buffer ; 2 bytes SFTODO: can we find some zp for this?
.osgbpb_load_loop
    ; Load the smaller of .chunk_size and .osgbpb_length bytes.
    ldx #<.chunk_size
    ldy #>.chunk_size
    lda .osgbpb_length + 1
    bne .read_full_chunk
    tay
    ldx .osgbpb_length
.read_full_chunk
    stx osgbpb_block_transfer_length
    ; SFTODO: Do we have some zp handy so we could avoid self-modifying code without making the code longer?
    stx .lda_imm_this_chunk_size_low + 1
    sty osgbpb_block_transfer_length + 1
    sty .lda_imm_this_chunk_size_high + 1
    lda #osgbpb_read_ignoring_ptr
    jsr .osgbpb_wrapper
    ; Set .bytes_read to the number of bytes read; if it's zero, this is EOF and
    ; we're done.
    sec
.lda_imm_this_chunk_size_low
    lda #<.chunk_size
    sbc osgbpb_block_transfer_length
    sta .bytes_read
.lda_imm_this_chunk_size_high
    lda #>.chunk_size
    sbc osgbpb_block_transfer_length + 1
    sta .bytes_read + 1
    ora .bytes_read
    beq .close_osgbpb_block_handle_rts

    ; Copy the data out of the bounce buffer.
    ldy #0
-   lda scratch_page,y
    sta (.osgbpb_data_ptr),y
    iny
    cpy .bytes_read
    bne -

    ; If we read fewer than .chunk_size bytes, we're done - this is either EOF or
    ; we requested fewer than .chunk_size bytes in the first place.
    lda .bytes_read + 1
    beq .close_osgbpb_block_handle_rts

    ; Decrement .osgbpb_length by .chunk_size.
    dec .osgbpb_length + 1

    ; Advance .osgbpb_data_ptr by .chunk_size.
    inc .osgbpb_data_ptr + 1
!ifdef ACORN_SWR_BIG_DYNMEM_AND_SCREEN_HOLE {
    ; Note that we check for equality here, because we're looping round and we
    ; only want to apply the adjustment when we first hit the screen hole.
    lda .osgbpb_data_ptr + 1
    cmp acorn_screen_hole_start_page
    bne +
    adc acorn_screen_hole_pages_minus_one ; -1 as carry is set
    sta .osgbpb_data_ptr + 1
+   ; Z flag is clear however we get here
}

    ; Loop round until we're done.
    bne .osgbpb_load_loop ; Always branch
}

!ifdef ACORN_ADFS {
    ACORN_WANT_OSGBPB_BLOCK = 1
} else {
    !ifndef ACORN_SAVE_RESTORE_OSFILE {
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
.close_osgbpb_block_handle_rts
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

!ifndef UNDO {
!ifdef Z5PLUS {
z_ins_save_undo
z_ins_restore_undo
    ; Return -1 to indicate that this is not supported.
    ; SF: I asked Fredrik and we need this to be safe even if we've indicated
    ; via the header that undo isn't supported.
    ldx #$ff
    txa
    jmp z_store_result
}
} else {

undo_state_available !byte 0

; we provide basic undo support for z3 as well through a hot key
; so the basic undo routines need to be available for all versions

!ifdef ACORN_SWR {
    !error "This undo implementation only works for tube builds."
}

!macro do_save_undo_inline {
    lda #>undo_buffer_start
    sta undo_copy_sta_abs_y + 2

    ; Copy the stack and dynamic memory into the undo buffer.
    lda #>stack_start
    sta undo_copy_lda_abs_y + 2
    ldx #(>stack_size) + ACORN_INITIAL_NONSTORED_PAGES
    jsr undo_copy_x_pages

    ; Now copy the zp_bytes_to_save bytes at zp_save_start into the undo buffer.
    ; We use X for this loop as there are zp,x but no zp,y addressing modes.
    ldx #zp_bytes_to_save - 1
-   lda zp_save_start,x
    sta zp_undo_buffer_start,x
    dex
    bpl -

    ldx #1
	stx undo_state_available
}

!macro do_restore_undo_inline {
    lda #>undo_buffer_start
    sta undo_copy_lda_abs_y + 2

    ; Restore the stack and dynamic memory from the undo buffer.
    lda #>stack_start
    sta undo_copy_sta_abs_y + 2
    ldx #(>stack_size) + ACORN_INITIAL_NONSTORED_PAGES
    jsr undo_copy_x_pages

    ; Now restore the zp_bytes_to_save bytes at zp_save_start from the undo buffer.
    ; We use X for this loop as there are zp,x but no zp,y addressing modes.
    ldx #zp_bytes_to_save - 1
-   lda zp_undo_buffer_start,x
    sta zp_save_start,x
    dex
    bpl -

	jsr get_page_at_z_pc

    ldx #0
	stx undo_state_available
}

    ; SFTODO: Near-identical copy code appears in a few places in Acorn Ozmoo,
    ; although I believe this is the only copy in the tube executable. We could
    ; possibly factor out the commonality at the source level using a macro, but
    ; given the "caller" is expected to modify the operands of two of the
    ; instructions it's probably more confusing than necessary to do that.
undo_copy_x_pages
    ldy #0
-
undo_copy_lda_abs_y
    lda $ff00,y ; patched at runtime
undo_copy_sta_abs_y
    sta $ff00,y ; patched at runtime
    iny
    bne -
    inc undo_copy_lda_abs_y + 2
    inc undo_copy_sta_abs_y + 2
    dex
    bne -
    rts

!ifdef Z5PLUS {
    ; the "undo" assembler instruction is only available in Z5+
z_ins_save_undo
    +do_save_undo_inline
	; Return 2 if just restored, -1 if not supported, 1 if saved, 0 if fail
	lda #0
  	jmp z_store_result

z_ins_restore_undo
	ldx undo_state_available
	beq +
    +do_restore_undo_inline
    ; Return 0 if failed
    ldx #2
+   lda #0
    jmp z_store_result
}
} ; ifdef UNDO

} ; zone save_restore
