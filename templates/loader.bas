IF PAGE<>&800 THEN PRINT "Sorry, second processor only for now!":END
MODE 7
PRINT "Which screen mode do you want to play"'"in, 0, 3, 4, 6 or 7? ";
*FX21
REPEAT
M$=GET$
UNTIL INSTR("03467",M$)<>0
M%=VAL(M$)
MODE M%
VDU 23,1,0;0;0;0;
VDU 19,0,4,0,0,0
REM Z3 games may put a teletext control character at top left before
REM loading the game data, so put one there ourselves to avoid a
REM visual glitch.
IF M%=7 THEN PRINT CHR$(134);
PRINT "Loading, please wait...";
*DIR S
*/$.OZMOO
