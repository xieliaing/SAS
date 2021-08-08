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

/************************/

/*
Original data be like:
var1 var2 var3 var4
2 4 6 7
4 9 7 6
5 2 1 1

如何得到一个新的variable， 它的值是var1-var4中有最大值的那个variable的名字。
结果应该是:

newvar
var4
var2
var1


谢谢
*/
data _xxx;
   input var1 var2 var3 var4;
cards;
2 4 6 7
4 9 7 6
5 2 1 1
7 3 7 3
;
run;

proc transpose data=_xxx out=_xxx2;
run;

proc means data=_xxx2 noprint;
     var col1-col4;
     output out=_xxx3(keep=v1-v4)
            maxid(col1(_name_)  col2(_name_)  col3(_name_) col4(_NAME_))= v1-v4/autoname;
run;

proc transpose data=_xxx3 out=_xxx3t;
     var v1-v4;
run;
data _xxx;
    merge _xxx _xxx3t(keep=COL1);
    rename col1=varname;
run;