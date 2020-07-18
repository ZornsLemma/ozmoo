#!/bin/bash
set -e
mkdir -p temp
cd asm
acme --setpc 2049 -DACORN=1 -DCACHE_PAGES=4 -DSTACK_PAGES=4 -DZ3=1 -DCONF_TRK=1 --cpu 6502 --format plain -DBORDERCOL=0 -DSMALLBLOCK=1 -DALLRAM=1 -DSPLASHWAIT=0 -l "temp/acme_labels.txt" -r "temp/acme_report.txt" --outfile "temp/ozmoo" ozmoo.asm
cd ..
cat temp/ozmoo examples/dejavu.z3 > temp/ozmoo-with-data
beebasm -v -i z.beebasm -do z.ssd -opt 3
