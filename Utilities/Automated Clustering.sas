
%macro fc(Profile,
          corr_dsn,
		  S_dsn,
          out_dsn,
		  proportion,
		  cumulative
          );
%if &Profile=0 %then %do;
    %let esiid_sample=esiid_sample;
	%let R1R2=0;
%end;
%else %if &profile=1 %then %do;
    %let esiid_sample=esiid_sample_R1;
	%let R1R2=1;
%end;
%else %if &profile=2 %then %do;
    %let esiid_sample=esiid_sample_R2;
	%let R1R2=2;
%end;
%else %do;
    %goto exit;
	%put Profile value should be 0, 1 or 2;
%end;

%if &proportion>1 %then %let proportion=0.005;
%if &cumulative>1 %then %let cumulative=1;
%else %if &cumulative<-1 %then %let cumulative=0;

%let nmax=6;
data _null_;
     set &S_dsn  end=eof;
	 if Proportion<&proportion or eof then do;
	    call symput('Cutoff1', compress(Number));
		stop;
	 end;
run;

data _null_;
     set &S_dsn end=eof;
	 if Cumulative>=&cumulative or eof then do;
	    call symput('Cutoff2', compress(Number));
		stop;
	 end;
run;
%let minCut=%sysfunc(min(&Cutoff1, &CutOff2));
%let maxCut=%sysfunc(max(&CutOff1, &CutOff2));
%put Start=&minCut  End=&maxCut;  

%do CutOff=&minCut %to &maxCut;

proc sql noprint outobs=&Cutoff;

     select name into :_vars separated by ' '
	 from   sashelp.vcolumn
	 where  memname=%upcase("&corr_dsn") & memtype="DATA" & 
	        substr(name, 1, 4)="corr"
	 ;
quit;

%do i=2 %to &nmax;
    %do j=1 %to 4;
/* choose the best initialization centers (min Distortion)*/
    data _null_;
         r=round(ranuni(0)*1000000);
	     call symput('seed', r);
    run;

    proc fastclus data=&corr_dsn  out=_temp_ maxiter=100 maxclus=&i 
                  random=&seed replace=random  outseed=seed_&j
                  noprint;
         var  &_vars;
    run;
    proc means data=_temp_  sum noprint;
         var Distance;
	     output out=_temp_dist sum(Distance)=Distortion;
    run;
    %if &j=1 %then %do;
    data _init_choose;
        set _temp_dist;
	    Iter=&j; Seed=&seed; 
    run;
    %end;
    %else %do;
    data _temp_dist;
         set _temp_dist;
	     Iter=&j; Seed=&seed; 
    run;
    proc append base=_init_choose data=_temp_dist; run;
    %end;

    %end; /* end selection loop of initial seeds */

data _null_;
     set _init_choose;
	 put Iter=  Distortion=  Seed=;
run;
proc sort data=_init_choose; by Distortion; run;
data _null_;
     set _init_choose;
	 if _n_=1 then call symput('j', compress(Iter));
	 stop;
run;
/* use best selected seed for clustering */
ods select none;
proc fastclus data=&corr_dsn   out=clus_Ns maxiter=100 maxclus=&i  
              seed=seed_&j     mean=clus_Ns_mean  outstat=clus_Ns_stat
              dist ;
     var  &_vars;
run;
ods select all;

proc means data=clus_Ns  sum noprint;
     var Distance;
	 output out=_dist_  sum(Distance)=Distortion;
run;

data _null_;
     set _dist_;	 
     BIC=Distortion + 1.0*&CutOff * &i *log(_FREQ_);
	 call symput('Distortion', Distortion);	
	 call symput('BIC', BIC);	 
	 stop;
run;
%put ClusterNumber=&i  BIC=&BIC  Distortion=&Distortion Dimension=&CutOff ;
%put *****************************************************************************;

%if &i=2 %then %do;
data _hist;
    Iter=&i;  BIC=&BIC;  Distortion=&Distortion; Dimension=&CutOff; seed=&seed;
run;
%end;
%else %do;
data _null_;
     set _hist nobs=ntotal  point=ntotal;
	 call symput('last_BIC', BIC);
	 stop;
run;

data _hist_temp;
    Iter=&i;  BIC=&BIC;  Distortion=&Distortion; Dimension=&CutOff; seed=&seed;
run;
proc append base=_hist  data=_hist_temp; run;
%end;

%end; /* End selection loop for number of clusters*/
%if &CutOff=&minCut %then %do;
    data _hist_all;  set _hist;  run;
%end;
%else %do;
    proc append data=_hist base=_hist_all; run;
%end;

proc sort data=_hist; by BIC; run;
data _null_;
     set _hist ;
	 call symput('best_cluster', iter);
	 stop;
run;
%put Number of Feature: &CutOff , Optimal Cluster Number = &best_cluster;
%put *******************************************************************************;

%end; /* CutOff Loop */

proc sort data=_hist_all; by dimension BIC; run;
data _hist_selected;
     set _hist_all; by dimension BIC;
	 if first.dimension;
run;
proc freq data=_hist_selected noprint;
     table iter/out=_hist_final(drop=PERCENT);
run;
proc sort data=_hist_final; by descending COUNT; run;
data _null_;
     set _hist_final;
	 if _n_=1 then do;
	    call symput('best_cluster', iter);
	    stop;
	 end;
run;

%put Final Selected Number of Clusters: &best_cluster;;
%put ************************************************;
ods select none;
proc fastclus data=&corr_dsn  seed=seed_&j
              out=&out_dsn
              maxiter=100  maxclus=&best_cluster  dist 
              mean=&out_dsn._mean_R&R1R2   outstat=&out_dsn._stat_R&R1R2   
              ;
     var &_vars;
run;
ods select all;

proc means data=&out_dsn noprint;
     class CLUSTER;
	 var &_vars;
	 output out=&out_dsn._meanR&R1R2.(where=(_stat_='MEAN' & _type_=1));
run;
data &out_dsn._meanR&R1R2;
     set &out_dsn._meanR&R1R2;
	 if corr1>0 then alpha=atan(corr2/corr1);
	 else alpha=atan(corr2/corr1)-constant('PI');
run;
proc sort data=&out_dsn._meanR&R1R2; by corr1; run;
data &out_dsn._meanR&R1R2;
     set &out_dsn._meanR&R1R2  end=eof;
	 newClus=_n_;
	 if eof then call symput('ntotal', _n_);
run;
data &out_dsn;
     set &out_dsn;
	 array _C{&ntotal} _temporary_;
	 if _n_=1 then do;	    
	    do until (eof1);
           set &out_dsn._meanR&R1R2(keep=newClus CLUSTER rename=(CLUSTER=oldClus))  end=eof1;
           _C[oldClus]=newClus;
		end;
	 end;
	 CLUSTER=_C[CLUSTER];
	 drop newClus;
run;	    
%exit:
%mend;

/*  TYPICAL USAGE

options nonotes;
%let  corr_dsn=a_filtered_corr;
%let  S_dsn=_S2_New;
%let  out_dsn=Clus_N0;
%let  proportion=0.001;
%let  cumulative=0.8;
%fc(0, &corr_dsn, &S_dsn, &out_dsn, &proportion, &cumulative);
options notes;
*/
