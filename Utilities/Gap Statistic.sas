/* Gap Statistics of Hastie et al. */

data test;
     length ID 5;
     array _X{50} X1-X50;
	 do ID=1 to 4000;
	    beta=mod(ID, 5)+1;
		do _j=1 to 50;
		   _X[_j]=rannor(6898776)+beta*3;
		end;
		output;
		keep ID X:;
	 end;
run;


%macro GAPstat(dsn, ID_var, dsn_GAP, max_K, max_rep, samplemethod=1, seed=147852369);
%local dsn  K B dsid nobs ID dsn_out  features features2   num_features ;

%let dsn=test;
%let B=&max_rep;
%let dsid=%sysfunc(open(test));
%let nobs=%sysfunc(attrn(&dsid, nobs));
%let dsid=%sysfunc(close(&dsid));
%put &nobs;

%let ID=&ID_var;
%let dsn_out=_clus_out;
%let ref_dist=_ref_dist;


proc contents data=&dsn out=allvars(keep=name varnum) noprint; run;
proc sort data=allvars; by varnum; run;
proc sql  noprint;
     select name into :features separated by ' '
	 from   allvars
	 where  name^='ID'
	 ;
     select count(distinct name) into :num_features 
	 from   allvars
	 where  name^='ID'
	 ;
quit;


%if &samplemethod eq 1 %then %do;
proc means data=&dsn noprint;
     var &features;
	 output out=_lim(where=(_STAT_ in ('MIN', 'MAX')));
run;
%end;
%else %if &samplemethod eq 2 %then %do;
proc princomp data=&dsn   noint cov noprint
                     outstat=_V(where=(_TYPE_='USCORE'));
	   var &features;
run;
data _V_score/view=_V_score;
       set _V;
	   _TYPE_='PARMS';
	   
run;
proc score data=&dsn  score=_V_score type=parms  out=_Z;
       var &features;
run;
proc means data=_Z  noprint;
       var &features;
	   output   out=_lim(where=(_STAT_ in ('MAX', 'MIN')));
run;
%end;
%else %do;
         %put Only value=1 (uniformly sampling) and 2 (SVD sampling) allowed.;
         %goto exit;
%end;

data &ref_dist;
     array _stat{2, &num_features} _temporary_;
	 array _F{&num_features}  &features;
	 
     do until (eof);
		   set _lim end=eof;
		   if _STAT_='MIN' then _row=1;
		   else _row=2;
		   do _k=1 to &num_features; 
              if _row=1 then _stat[_row, _k]=_F[_k];
			  else _stat[_row, _k]=_F[_k]-_stat[_row-1, _k];
		   end;
	end;
	 
     
	 do _B=1 to &B;
	    do &ID=1 to &nobs;
		   do _k=1 to &num_features;
		      _F[_k]=ranuni(0)*(_stat[2, _k]) + _stat[1, _k];
		   end;		  
		   keep _B &ID &features;
		   output;
		end;
	end;
run;

%if &samplemethod eq 2 %then %do;
proc transpose data=_V  out=_Vt;  run;
proc sql noprint;
       select  name into :features2 separated by ' ", "'
	   from   allvars
	   where  upcase(name)^=("&ID")
	   ;
quit;
data _Vt_score/view=_Vt_Score;     
	   set _Vt; 
	   set allvars(where=(name in ("&features2")));
       retain  _TYPE_ "PARMS";	
	   _LABEL_=_NAME_;
	   _NAME_=name;
	   drop name;
run;
proc score data=&ref_dist   score=_Vt_score   type=parms
                out=&ref_dist.(keep=_B  &ID &features);
	   var &features;
run;
%end;

%do K=1 %to &max_K;
    proc fastclus data=&dsn  maxclusters=&K  
              out=&dsn_out.(keep=&ID Distance CLUSTER) 
			  outstat=_stat_&K.(where=(_TYPE_='WITHIN_STD'))
              noprint  least=2 ;
         var &features;
    run;

    proc fastclus data=&ref_dist maxclusters=&K  
              out=&dsn_out._&K.(keep=&ID _B Distance CLUSTER)  
              noprint;
         by _B;
         var &features;
    run;

    proc means data=&dsn_out  sum noprint;     
         var Distance;
	     output out=_dist_  sum(Distance)=Distortion;
    run;
    proc means data=&dsn_out._&K  sum noprint;
         by _B;
         var Distance;
	     output out=_dist_&K  sum(Distance)=Distortion;
    run;
    data _dist_&K(drop=mean _B:) _logmean_&K.(keep=mean);
         array _D{&B} _B1-_B&B;
	     set _dist_&K end=eof;
	     logD=log(Distortion);
	     _D[_n_]=logD;
	     output _dist_&k;
	     if eof then do;	    
            mean=mean(of _D[*]);
		    output  _logmean_&K;		
	     end;
   run;

   proc means data=_dist_&K  noprint;
        var logD;
	    output out=_s_&K.(where=(_STAT_='STD'));
   run;

   data Gap_&K;
        set _dist_; set _logmean_&K ; set _s_&K(keep=logD);
        GAP=mean-log(Distortion);
        s=logD*sqrt(1+1/&B);
	    keep GAP s;
   run;
%end;

data &dsn_GAP;
     set %do k=1 %to &max_K;
	        Gap_&k
		 %end;;
run;
%exit:
%mend;



%let dsn=test;
%let ID=ID;
%let dsn_GAP=GAP;
%let max_K=10;
%let max_rep=100;
%GAPStat(&dsn, &ID, &dsn_GAP, &max_K, &max_rep);

