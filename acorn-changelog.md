## 14.22 (alpha 58)

* TODO

* TODO: CHECK THE BELOW ACTUALLY DID GET INCLUDED

* Medium and big dynamic memory builds now contain special-case code for accessing local variables. We know these are on the stack, so we can avoid paging the sideways RAM bank (if any) containing dynamic memory in and out. (This optimisation was previously applied only to big dynamic memory builds with a screen hole.)

* We use absolute,y addressing instead of (zp),y addressing for accessing global variables when possible, which it usually is - only big dynamic memory builds where there is a screen hole and the global variables live above the screen hole are currently unable to support this. For big dynamic memory builds, we also avoid paging in the sideways RAM bank if we know the global variables are in main RAM. This is implemented using a combination of conditional assembly and dynamic binary patching at runtime. Where we can know at build time that an executable will use absolute addressing, this frees up four bytes of zero page and shrinks the runtime code. The new --no-absolute-globals and --no-runtime-absolute-globals allow this to be disabled if necessary, although this is mainly for debugging and support purposes - barring bugs, there should be no downside to these changes.

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
