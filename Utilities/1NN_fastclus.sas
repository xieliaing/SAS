data fix;
input x y;
datalines;
1 3
2 4
3 5
8 0.2
15 1
;
run;
data have;
input x y;
datalines;
1.2 6
0.3 4
10 1.2
7 1
2.9 4
;
run;
data fix;
   set fix;
   CLUSTER=_n_;
run;

%let dsid=%sysfunc(open(fix));
%let ntotal=%sysfunc(attrn(&dsid, NOBS));
%let dsid=%sysfunc(close(&dsid));
proc fastclus data=have out=have2
              seed=fix  maxclusters=&ntotal 
              noprint maxiter=0 ;
     var x y;
run;

/* BE SURE TO MERGE BACK TO SEE WHICH SEEDS WERE MATCHED */
proc sort data=fix; by cluster; run;
proc sort data=have2; by cluster; run;
data have2;
     merge have2   fix(rename=(x=x2  y=y2));
     by cluster;
run;