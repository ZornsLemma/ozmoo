## 14.22 (alph 57)

* TODO

## 14.22 (alpha 56)

* Fix save and restore for Z1 and Z2 games - the conditional assembly just omitted the relevant code at z_ins_{save,restore}. Thanks to Ken Lowe for reporting this!

## 14.22 (alpha 55)

* make-acorn.py incorrectly refused to allow --x-for-examine on Z5+ games, even if it itself turned it on.

* --x-for-examine could cause build failures with an out of range branch on Z5+ games.

## 14.22 (alpha 54)

* Tweak !BOOT file to avoid leaving CHAIN "LOADER" showing. I suspect I wrote it the old way out of paranoia that third-party shadow RAM boards would cause problems, but I can't see why so let's at least try it.

* The ReCo6502 (at least as emulated in b-em) uses &EA-&ED (at least) when copying a language over the tube, so the *BASIC command in the !BOOT file corrupts the result of running TURBO and the loader then gives an error about an invalid turbo flag. Work around this by having TURBO store the result in main RAM as well and then have !BOOT copy it back from there into &ED afterwards so everything works as normal. (We test this byte a lot and having it in zero page is helpful.)

* The Acorn kernal_readtime implementation didn't work on a 65816 with initial_clock in zero page (as tends to happen on tube builds) because abs,X addressing doesn't wrap around to zero page if abs+X>&FFFF. This is true at least in b-em, and as far as I can tell this is how real hardware behaves. Force the use of zp,X addressing where relevant to avoid this.
