%macro Quasi_Separation(dsn, target, sep_var);
/*
  used the data-driven method of Greenacre 1988.
   The levels (rows) are hierarchically clustered based on the reduction in the 
   chi-squared test of association between the categorical variable and the target. 
*/
proc means data=&dsn;
   class &sep_var;
   var &target;
   output out=_level mean=prop;
run;

ods output clusterhistory=_cluster;

proc cluster data=_level method=ward outtree=_tree;
    freq _freq_;
	var prop;
	id &sep_var;
run;

ods listing;

proc freq data=&dsn;
   table &target * &sep_var /chisq;
   output out=_chi(keep=_pchi_) chisq;
run;

data _null;
%global opt_cluster;
   retain min_chi num_cluster 0;
   if _n_=1 then set _chi;

   set _cluster;
 
   chi_sqr=_pchi_*rsquared;
   df=numberofclusters-1;
   if chi_sqr=0 then
      logvalue=0;
   else
      logvalue=logsdf('CHISQ', chi_sqr, df);

   if (min_chi=.) then do
        min_chi=logvalue;
		num_cluster=numberofclusters;
   end;
   else
        if (min_chi >= logvalue) then do;
		   min_chi=logvalue;
		   num_cluster=numberofclusters;
		end;
		else do;
           min_chi=min_chi;
		   num_cluster=num_cluster;
		end;

   call symput('opt_cluster', num_cluster);
run;
%put &opt_cluster;
%mend quasi_separation;

%quasi_separation(crssamp.general, majorder, edu);

%put &opt_cluster;

proc tree data=_tree;
run;
