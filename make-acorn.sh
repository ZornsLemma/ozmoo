#!/bin/bash
set -e
mkdir -p temp
cd asm
# SFTODO: The next line hard-codes Z3, the Commodore build system automatically sets the right Zn flag. All I really need to do is invoke these commands from within make-acorn.py and get rid of this file, but I want to keep things simple for now.
acme --setpc "\$800" -DACORN=1 -DCACHE_PAGES=4 -DSTACK_PAGES=4 -DZ3=1 -DCONF_TRK=1 --cpu 6502 --format plain -DBORDERCOL=0 -DSMALLBLOCK=1 -DVMEM=1 -DALLRAM=1 -DSPLASHWAIT=0 -l "../temp/acme_labels.txt" -r "../temp/acme_report.txt" --outfile "../temp/ozmoo" ozmoo.asm
cd ..
beebasm -i templates/base.beebasm -do temp/base.ssd -opt 3 2>&1 | grep -v "no SAVE command"
python make-acorn.py "$@"
