/*
I have a set of number with different digits. I want to make all of them 6
digits, if they are not, adding zero to the right? Like this,
Original data set
x
345
4567
777777
23567
.
.
.
Desired result look like this,
x
345000
456700
777777
235670
.
.
.
Thanks very much! Will send baozi for the help.
*/

data x;
   input x $6.;
   y=translate(x, '0', ' ');
datalines;
345
4567
777777
23567
;
run;