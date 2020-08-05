REM SFTODO: This file is just a temporary holding place for some experiments.
REM SFTODO TEMP HACK
DIM code% 256
wrchv=&20E
FOR opt%=0 TO 3 STEP 3
P%=code%
[OPT opt%
LDA wrchv:STA call_old_wrchv+1
LDA wrchv+1:STA call_old_wrchv+2
LDA #our_oswrch MOD 256:STA wrchv
LDA #our_oswrch DIV 256:STA wrchv+1
RTS
.our_oswrch
.call_old_wrchv
JSR &FFFF
PHA
LDA #14:STA &FE00
LDA &34B:SEC:SBC #&1C:STA &FE01
LDA #15:STA &FE00
LDA &34A:STA &FE01
PLA
RTS
]
NEXT
CALL code%
MODE 7
PRINT "Hello"
?&3C00=ASC("F")
?&3C01=ASC("o")
?&3C02=ASC("o")
FOR I%=&3C03 TO &3FFF
?I%=65
NEXT
PROCr(12, 32)
REM 32 OR 36 OR 48 OR 52 OR 96 OR 100 OR 112 OR 116 OR 160 OR 164 OR STOPPED AS PROB JUST +128
PROCr(13,0)
REM
VDU 28,0,24,39,0
REM I think D8/D9 will be set by OS using the other variables
REM ?&D8=0:REM low byte, top scan line (necessary?)
REM ?&D9=&3C:REM high byte, top scan line (necessary?)
?&34E=&3C:REM high byte of bottom of screen memory
?&350=0:REM low byte display start address for 6845
REM ?&351=32:REM high byte display start address for 6845
?&351=&3C
?&34A=0:REM low byte text cursor address for 6845
REM?&34B=32:REM high byte text cursor address for 6845
REM?&34B=&3C:REM high byte text cursor address for 6845
REM ?&34B=&74
REM ?&351=&74 - this makes the cursor appear in right place but not actual text
REM PROCr(14,32) - this makes cursor appear, which is promising, until OS overwrites it
REM C940 in OS 1.2 copies word 350->34A - then CA06 on will use C4A with the -&74 EOR &20
REM hack (as mentioned in AUG) to set cursor position. So I have a feeling we *either*
REM have the cursor in the right position, or we have the text in the right position.
REMIF GET
PRINT:REM force OS to pick up new settings
END
IF GET
REMPROCr(12, (&7C-&74) EOR &20)
FOR I%=0 TO 255
PROCr(12, I%)
PROCr(13, 0)
*FX21
IF GET
NEXT
END
DEF PROCr(r,v)
VDU 23,0,r,v,0,0,0,0,0,0
ENDPROC
REM SFTODO END TEMP HACK
