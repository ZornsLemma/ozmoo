IF PAGE<>&800 THEN PRINT "Sorry, second processor only for now!":END
MODE 7
PRINT "Which screen mode do you want to play"'"in, 6 or 7? ";
*FX21
REPEAT
M$=GET$
UNTIL M$="6" OR M$="7"
MODE VAL(M$)
VDU 23,1,0;0;0;0;
VDU 19,0,4,0,0,0
PRINT "Loading, please wait...";
*DIR S
*/$.OZMOO
