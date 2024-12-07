## 14.40 (alpha 58)

* Merge upstream changes. The main significant change for the Acorn port is the introduction of a "default Unicode map" which will convert non-ASCII characters used by games to best-effort ASCII equivalents. This is disabled by default on Acorn, as it costs a little bit of extra code/data, but can be enabled using the new build option --default-unicode-map.

* The BUILD file added to the generated disc image now contains the operating system and Python versions used for the build. I don't think this should be a significant privacy concern, but if you are worried about this you can use the --no-build-file option to disable generation of the BUILD file. Please also let me know so we can discuss your concerns; I'd rather remove this information than have people avoid doing builds with the other useful support information in.

* Medium and big dynamic memory builds now contain special-case code for accessing local variables. We know these are on the stack, so we can avoid paging the sideways RAM bank (if any) containing dynamic memory in and out. (This optimisation was previously applied only to big dynamic memory builds with a screen hole.)

* We use absolute,y addressing instead of (zp),y addressing for accessing global variables when possible, which it usually is - only big dynamic memory builds where there is a screen hole and the global variables live above the screen hole are currently unable to support this. For big dynamic memory builds, we also avoid paging in the sideways RAM bank if we know the global variables are in main RAM. This is implemented using a combination of conditional assembly and dynamic binary patching at runtime. Where we can know at build time that an executable will use absolute addressing, this frees up four bytes of zero page and shrinks the runtime code. The new --no-absolute-globals and --no-runtime-absolute-globals allow this to be disabled if necessary, although this is mainly for debugging and support purposes - barring bugs, there should be no downside to these changes.

* We do some related optimisations when updating local or global variables (using z_set_variable_reference_to_value) to avoid paging in a dynamic memory bank unnecessarily. Again, this uses a combination of conditional assembly and dynamic binary patching at runtime. This costs an extra 14 bytes of runtime code on medium or big dynamic memory builds but offers a small performance increase on medium builds and should make a bigger (albeit not huge) difference on big dynamic memory builds. For big builds running on machines where the global variables happen to end up in main RAM the performance improvement will be greater, as we can avoid ever having to page in the dynamic memory bank here.

* As an aside, the previous two changes mean that if you have any control over where in dynamic memory the global variables are located for a game you're writing, for maximum performance on Acorn Ozmoo you should put them at as low an address in dynamic memory as you can. This will increase the chances of these optimisations kicking in.

* Removed a couple of unused variables and tweaked the use of zero page to increase the chances of some variables that are referenced by a lot of instructions being allocated there. This shrinks the runtime code and as a bonus gives a small performance increase as well.

* Moved stack_init into discardable init code, shrinking the runtime code by about 20 bytes.

* Move existing benchmark into benchmarks.json, change make-acorn.py to build using a benchmark from this file and add a new benchmark (a walkthrough of LordVaderUK's Moonbase Escape, courtesy of EdwardianDuck). The existing --benchmark option still works and tries to automatically pick the benchmark to use, if this goes wrong the new --named-benchmark and --list-benchmarks options allow the benchmark to be specified explicitly on the command line.

* Re-use the space allocated to the fast hardware scrolling machine code for the slow hardware scrolling buffer. This shrinks the Ozmoo main RAM use by 160 bytes on builds which support slow hardware scrolling (which is the default).

* In builds which support both slow and fast hardware scrolling (which is the default), don't include slow hardware scrolling support in the executable for the BBC B with no shadow RAM, saving 98 bytes. This machine configuration always supports fast hardware scrolling so the slow hardware scrolling code is redundant.

* Micro-optimised the z_exe_mode code to save a few bytes on Z4+ games. It is completely omitted for Z1-3 games, saving around 26 bytes.

* For Z3 games, select either the score or time game code at build time rather than runtime, avoiding the need to include both in the executable. This shrinks the code by around 133 bytes for a score game.

* Move streams_init to discardable init code, shrinking the runtime code by 25 bytes.

* Some initialisation code has been shuffled around, shrinking the runtime code by 9 bytes. Some code which should have always been located in the Z-machine stack during initialisation (and always was, in practice) has now been moved so it is explicitly located there.

* Omit fatalerror and printinteger code for builds which don't define DEBUG or CHECK_ERRORS. Benchmark builds always define DEBUG, but normal builds don't define either by default and this shrinks the runtime code by 124 bytes.

* Miscellaneous other micro-optimisations to save a few bytes/cycles. I haven't counted but I suspect cumulatively these save at least 30 bytes.

* Improve the detection of *FX111 being used by older versions of Watford DFS to return the current drive instead of controlling shadow RAM. Thanks to Jonathan Graham Harston for advice.

* Remove unused debug pause subroutine. This will slightly (and somewhat artifically) improve the benchmark performance if it frees up enough memory to make an extra 512 byte block of RAM available at runtime.

* Rework the horrible old runtime code for deciding which (if any) screen modes a game can run in given the available RAM. It's possible there will still be bugs in this but I think it is a lot clearer now and it should be more obvious what's happening. (I haven't had the fortitude to really dig into the code history, but I suspect this was so complex partly because of the way the code evolved. In the early days where you didn't get a choice of screen mode if you had no shadow RAM the loader didn't have to do much, but trying to bolt support for variable screen modes on to the existing logic made it complex.)

* Fix a bug in the fast scroll code which affected modes 3 and 6. The fact that the screen display doesn't completely fill the allocated screen memory was not being handled correctly and a supposedly-blank new line at the bottom of the screen could sometimes have a fragment of old text on it. This showed up in Varicella by typing a simple (invalid) "r" command at the first prompt in mode 3, but presumably could affect any game. The code has been rewritten and is probably slightly cleaner as a result of being forced to think again about how it's supposed to work.

* Run the shadow driver installation code at &2C00 instead of &900. It was already a tight fit at &900 and sooner or later another shadow RAM type will be added and even more space will be needed. It now runs from !BOOT; we can't run it from the loader as there is likely to be BASIC code at &2C00, and we can't run it just below mode 6 HIMEM because the Aries/Watford hardware probing code will page the &3000-&8000 region in and out briefly, crashing the code if it runs from there. (Ask me how I know...)

## 14.22 (alpha 57)

* Fix corner cases with games that only have one block of non-dynamic memory. These were shown up as a side-effect of Fredrik's new "catch" test.

* Make --trace-vm work on Acorn again (more-or-less).

## 14.22 (alpha 56)

* Fix save and restore for Z1 and Z2 games - the conditional assembly just omitted the relevant code at z_ins_{save,restore}. Thanks to Ken Lowe for reporting this!

## 14.22 (alpha 55)

* make-acorn.py incorrectly refused to allow --x-for-examine on Z5+ games, even if it itself turned it on.

* --x-for-examine could cause build failures with an out of range branch on Z5+ games.

## 14.22 (alpha 54)

* Tweak !BOOT file to avoid leaving CHAIN "LOADER" showing. I suspect I wrote it the old way out of paranoia that third-party shadow RAM boards would cause problems, but I can't see why so let's at least try it.

* The ReCo6502 (at least as emulated in b-em) uses &EA-&ED (at least) when copying a language over the tube, so the *BASIC command in the !BOOT file corrupts the result of running TURBO and the loader then gives an error about an invalid turbo flag. Work around this by having TURBO store the result in main RAM as well and then have !BOOT copy it back from there into &ED afterwards so everything works as normal. (We test this byte a lot and having it in zero page is helpful.)

* The Acorn kernal_readtime implementation didn't work on a 65816 with initial_clock in zero page (as tends to happen on tube builds) because abs,X addressing doesn't wrap around to zero page if abs+X>&FFFF. This is true at least in b-em, and as far as I can tell this is how real hardware behaves. Force the use of zp,X addressing where relevant to avoid this.
