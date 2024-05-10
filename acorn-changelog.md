## 14.22 (alpha 54)

* Tweak !BOOT file to avoid leaving CHAIN "LOADER" showing. I suspect I wrote it the old way out of paranoia that third-party shadow RAM boards would cause problems, but I can't see why so let's at least try it.

* The ReCo6502 (at least as emulated in b-em) uses &EA-&ED (at least) when copying a language over the tube, so the *BASIC command in the !BOOT file corrupts the result of running TURBO and the loader then gives an error about an invalid turbo flag. Work around this by having TURBO store the result in main RAM as well and then have !BOOT copy it back from there into &ED afterwards so everything works as normal. (We test this byte a lot and having it in zero page is helpful.)

* The Acorn kernal_readtime implementation with initial_clock in zero page (as tends to happen on tube builds) didn't work on a 65816 because abs,X addressing doesn't wrap around to zero page if abs+X>&FFFF. This is true at least in b-em, and as far as I can tell this is how real hardware behaves. Force the use of zp,X addressing where relevant to avoid this.
