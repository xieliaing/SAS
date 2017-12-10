data matrix;
     input X1-X5;
datalines;
1 2 4 5 6
7 8 9 0 1
2 3 4 5 6
3 4 5 6 7
7 8 9 0 2
2 4 6 8 0
;
run;

data seed;
     input X1-X5;
datalines;
0 0 0 0 0
;
run;

options nosource;
proc export data=matrix  outfile='c:\matrix.csv'  dbms=csv replace; run;
options source;

proc fastclus data=matrix  seed=seed      out=norm(keep=DISTANCE)
              maxiter=0    maxclusters=1  noprint  ;
     var x1-x5;
run;

/* 
In output file NORM, variable DISTANCE is the square root of Frobenius norm. If LEAST=P option is specified, then p-norm is calculated. In PROC FASTCLUS, you can specify p in the range of  [1, \inf].

Now what you got is vector norm for each row, taking the sum of squares of DISTANCE, you obtain the Frobenius norm of the data matrix, which can be easily obtained through PROC MEANS on a data view: 
*/
data normv/ view=normv;
     set norm(keep=DISTANCE);
     DISTANCE2=DISTANCE**2;
     drop DISTANCE;
run;
proc means data=normv noprint;
     var DISTANCE2;
     output  out=matrixnorm  sum(DISTANCE2)=Frobenius_sqr;
run;