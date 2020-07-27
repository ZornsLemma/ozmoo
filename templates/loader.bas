IF PAGE<>&800 THEN PRINT "Sorry, second processor only for now!":END
MODE 6
VDU 23;8202;0;0;0;
VDU 19,0,4,0,0,0
PRINT "Loading, please wait...";
*RUN OZMOO
