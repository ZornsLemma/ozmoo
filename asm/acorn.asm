; Acorn-specific code factored out into its own file for readability.
; SFTODO: Maybe rename this file to acorn-something.asm now we've pulled some code out into other acorn-*.asm files

; General note on terminology for the Acorn port: Upstream code overloads the
; word "block" to refer to both 256-byte and 512-byte blocks. I am not changing
; any upstream variable names, but in Acorn code I will try to consistently
; avoid "block" and use the terms"page" (always 256 bytes) or "dpage" (always
; 512 bytes) instead.
; SFTODO: I don't really like the pseudo-word "dpage", but it will do, and it
; would probably by easy to search and replace to change this later.
; SFTODO: Update this comment to reflect the fact I seem to want to talk about "vmem blocks" to mean 512-byte dpages in some places; I think this is OK, but it's misleading to say that I will try to always use dpage when it's not true.
; SFTODO: I HAVEN'T PUT THIS INTO EFFECT YET

; A note on Acorn memory models - this affects code in many places, but I have
; to write this somewhere. SFTODO: This is perhaps not best file any more
;
; The second processor build (ifndef ACORN_SWR) has a simple flat memory model
; with user RAM from $0400-$f7ff inclusive. It's rather like the C64 but without
; even the complication of paging the kernal ROM in and out.
; SFTODO: SAY SOMETHING ABOUT ACORN_TURBO_SUPPORTED
;
; The sideways RAM build (ifdef ACORN_SWR) is a bit more involved. The hardware
; situation here is that we have main RAM (not paged) from $0000-$7fff
; inclusive, with user RAM starting at OSHWM/PAGE, which varies between machines
; but will typically be in the range $e00-$1f00. Some builds hard-code a certain
; address, others use acorn-relocate.asm to accommodate this variation. We also
; have up to 16 different 16K banks of "sideways" RAM paged in at $8000-$bfff
; inclusive. (The BBC series and Electron have different ways to control paging;
; see the acorn_page_in_bank_* macros.) The OS is not paged and lives
; permanently at $c000-$ffff inclusive. The loader will have located any
; available sideways RAM banks, verified there's at least one and put the count
; and a list of bank numbers at ram_bank_{count,list} for us. SFTODO: MINOR QUIBBLE - LOADER WILL HAVE VERIFIED WE HAVE ENOUGH SWR BANKS, THERE MAY BE NONE IF WE CAN GET BY WITHOUT ANY
;
; SFTODO: This is outdated now I'm reworking the bigdyn model, needs rewriting.
; Also the memory hole stuff alters how Electron and B-no-shadow work.
;
; Acorn Ozmoo uses two slightly different sideways RAM models. Both of them
; allow static/high memory to be spread over approximately 9 sideways RAM banks
; (indexed in 512-byte chunks with indexes from 0-254, with chunk 0 starting
; at story_start+nonstored_pages). The standard Ozmoo mempointer (data) and 
; z_pc_mempointer (Z-machine PC) pointers are extended to each have an associated
; RAM bank (mempointer_ram_bank and z_pc_mempointer_ram_bank respectively). (If
; the relevant byte of Z-machine memory lives in main RAM, the bank number is
; irrelevant as main RAM is not affected by paging.)
;
; The "big dynamic RAM" model (ifndef ACORN_SWR_SMALL_DYNMEM) allows the game's
; dynamic memory (which starts in main RAM at story_start, as on any Ozmoo build)
; to be larger than main RAM and overflow into the first 16K sideways RAM bank.
; The first 16K sideways RAM bank therefore has to be paged in by default, so
; that miscellaneous Ozmoo code which might try to access dynamic memory can do
; so without any trouble. In this model, accesses to memory via read_next_byte
; and read_next_byte_at_z_pc temporarily page in the relevant bank to read the
; byte and then page the first 16K sideways RAM bank back in. (As an
; optimisation, read_next_byte_at_z_pc_unsafe* and friends are used during
; instruction decoding to avoid flipping back and forth excessively while
; reading a multi-byte instruction. This is possible because only a very limited
; set of cases can cause accesses to dynamic memory during instruction decoding.)
;
; The "small dynamic RAM" model (ifdef ACORN_SWR_SMALL_DYNMEM) requires the game's
; dynamic memory to fit in main RAM. Since dynamic memory can then be accessed
; regardless of the currently paged in bank, Ozmoo instead keeps the bank
; containing the Z-machine's PC paged in by default, temporarily paging it out
; only when reading a data byte.
;
; On a second processor or BBC series machine with shadow RAM, screen RAM is
; separate from user RAM and doesn't get in the way. On a BBC B with no shadow
; RAM, we use a trick (see ACORN_NO_SHADOW) to relocate the 1K screen RAM to
; $3c00, leave a gap in the Ozmoo binary to accommodate that and we can then
; mostly forget about screen RAM. Dynamic memory starts at story_start just
; after the Ozmoo stack (as on the C64) and is followed (with suitable paging
; for ACORN_SWR) directly by the virtual memory cache.
;
; On the Electron shadow RAM is rare and we can't use the ACORN_NO_SHADOW trick
; to get the screen memory (8K, from $6000-$8000) out of the way. Ozmoo really
; wants dynamic memory to be contiguous, and there isn't really enough RAM free
; between the Ozmoo stack and the screen memory to run all the games we'd like
; to. We therefore compromise by forcing the use of the big dynamic RAM model
; and making dynamic RAM start at $8000 instead of following the Ozmoo stack.
; This limits us to 16K of dynamic memory, which isn't too bad (and is more than
; we'd have free below screen RAM). We use the main RAM between the Ozmoo stack
; and the screen RAM as additional virtual memory cache so it isn't wasted. An
; Electron is therefore about 7K worse off than a BBC B with the same amount of
; sideways RAM as a result of its larger screen memory, in addition to not
; supporting games needing more than 16K of dynamic memory. The Electron save/
; restore code has to be slightly different because the data we need to save/
; restore is no longer contiguous in memory.

; Control a few small debug assertions and similar.
; SFTODO: Make this controllable via the build script?
; SFTODONOW: Should probably do some testing with these on - and make sure I turn them off after! It *might* be sensible to leave them on for now and then disable them when promoting alpha to beta. It is all a bit of a mess right now really - since I (and probably everyone else) don't use --debug-assert, that is never turned on, but the hard-coded ones below are on by default right now.

; ACORN_DEBUG_ASSERT causes additional code to be included which *verifies but
; does not change state*.

; ACORN_DEBUG_INTRUSIVE causes additional code to be included which *does* change
; state, but in ways that should be harmless; this helps to verify the belief
; that they are indeed harmless. SFTODONOW: Expose via make-acorn.py?
ACORN_DEBUG_INTRUSIVE = 1

; SFTODO: COMMENT?
DEBUG_BIG_DYNMEM = 1 ; SFTODO: RENAME ACORN_DEBUG_BIG_DYNMEM?
; SFTODONOW: I should probably have this *on* during pre-release testing
; SFTODONOW: Need to think carefully about what debug flags should be on by default in a "proper" release
; SFTODONOW: Should I have some kind of single --debug switch in make-acorn.py which turns on multiple things?

; Macro used to catch cases where a supposedly unreachable execution path is
; taken. This is intended for use in discardable init code where we're not too
; space-conscious and so the check can be left in permanently. SFTODO: In some
; cases, maybe it would be better just to rewrite the code to avoid even a
; theoretical possibility of this macro being executed.
!macro assert_discardable_unreached {
    +os_error 0, "Unreachable"
}

; Macro used to catch cases where a supposedly unreachable execution path is
; taken. This is intended for use in space-conscious code and is a no-op unless
; ACORN_DEBUG_ASSERT is defined.
!ifndef ACORN_DEBUG_ASSERT {
!macro assert_unreached {
}
} else {
assert_unreached_sub
    +assert_discardable_unreached

!macro assert_unreached {
    jsr assert_unreached_sub
}
}

; Macros used to detect "impossible" values of the carry flag. They are intended
; for use in space-conscious code and are no-ops unless ACORN_DEBUG_ASSERT is
; defined.
; SFTODO: Use this in more places?
!ifndef ACORN_DEBUG_ASSERT {
!macro assert_carry_set {
}

!macro assert_carry_clear {
}
} else {
assert_carry_set_sub
    bcs assert_carry_sub_rts
    +os_error 0, "C clear"
assert_carry_sub_rts
    rts

assert_carry_clear_sub
    bcc assert_carry_sub_rts
    +os_error 0, "C set"

!macro assert_carry_set {
    jsr assert_carry_set_sub
}

!macro assert_carry_clear {
    jsr assert_carry_clear_sub
}
}


; SFTODO: Once things settle down and I don't have immediate plans to add new features, it would be good to look for opportunities to shrink the code - particularly on builds for smaller machines - as squeezing out an extra one or two dpages of vmem cache in main RAM might make all the difference. Due to alignment it doesn't always need that much, a few bytes may tip it over the next alignment boundary.

; SFTODO: It's maybe a bit hacky, but perhaps we could offer an option to use pages 9 and &A on a B/B+ and those plus pages &B and &C on a Master as vmem cache if the user wants.

; SFTODO: We could perhaps avoid a proliferation of versions because of ROM paging by writing the code as "JSR page_in_bank_a:NOP:NOP:..." etc (wrapped in macros of course) and have those subroutines peek the return address from the stack and patch the JSR up to do the correct paging for the current platform directly. Of course the code size might vary, so the most likely use of this would be a) a third party model B SWR system which required no more code than standard paging b) making a BBC+Electron joint executable which penalises the Electron by making it JSR to subroutines to do paging but does direct paging on BBC. (The original subroutines would patch the JSR to a JSR to the Electron-specific subroutine on the Electron or the direct hardware code on the BBC.) Maybe a worthwhile idea in some other cases too.

; SFTODO: Perhaps do some timings to see how much of an impact replacing direct SWR paging code with a JSR to that same code has. It's probably significant, but it may be that some of the optimisations in Ozmoo over time mean this actually isn't a huge performance overhead, which would help make executables more shareable across different machines.

; SFTODO: Perhaps given the problems with auto-detecting restart commad and the save directory, we could have --nfs-install-dir and/or --nfs-save-dir options or something which hard-code these values in the loader or something? Don't just give up on auto-detection if it can be made to work though.

; SFTODO: In b-em, the benchmark seems to work on ReCo 65816 *except* the timestamps at start and end are wrong/odd. Might be worth having a quick look into this.

; SFTODO: In b-em, the bencnhmark won't work on Dossy 65816. Might be worth having a quick look into this.
