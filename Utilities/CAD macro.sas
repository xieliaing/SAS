
/* Get CMH general association measurement and Statistics for discrete variables 
   Usually CMH GA approximates Chisq measurments but in case of small count, it has
   more power
*/
%macro dct_association(dsn_in, dsn_out, stats_out, dsn_vars, target);
%let blank=%str( );
%let ds_seq1=&blank;
%let ds_seq2=&blank;
data _null_;
     set &dsn_vars  end=eof;
     call symput(compress('var_'||_n_), var_name); 
	 call symput(compress('id_'||_n_), id);
	 if eof then call symput('num_vars', _n_);
run;
proc freq data=&dsn_in  noprint;
     %do j=1 %to &num_vars; 
	     tables &&var_&j * &target /missing out=_d_&j;
     %end;
run;

%do j=1 %to &num_vars;
    proc freq data=_d_&j  noprint;
	     weight count;
	     tables &&var_&j * &target /  cmh;
		 output  out=_stat_&j  cmhga;
	run;
	data _stat_&j;
	     set _stat_&j;
         var_name = "&&var_&j";
	run;
	data _d_&j;
	    retain freq_1  freq_all  0;
	    set _d_&j  end=eof;
		length value $16.;
		if &target=0 then freq_all=count;
		else freq_all+count;
		freq_1=count;
		value=&&var_&j;
		var_name = "&&var_&j";
		if &target=1 then do;
          keep var_name value  freq_1  freq_all;
		  output;
		end;
	run; 
	%let ds_seq1=&ds_seq1  _d_&j;
	%let ds_seq2=&ds_seq2  _stat_&j;	
%end;
data &dsn_out;     set &ds_seq1;   run;
data &stats_out;   set &ds_seq2;   run;
proc sort data=&dsn_out; by var_name; run;
proc sort data=&stats_out; by p_cmhga; run;
proc datasets library=work nolist;
     delete  _d_:  _stat_:;
quit;
%mend;


/********************************************************************/
%macro cts_association(dsn_in, dsn_out, stats_out, dsn_vars, target);
%let blank=%str( );
%let ds_seq1=&blank;
%let ds_seq2=&blank;
%let dsn_vars=&dsn_vars;
%let dsn_in=&dsn_in;
%let data_out=&dsn_in;
data _null_;
     set &dsn_vars  end=eof;
     call symput(compress("var_"||_n_), var_name); 
	 call symput(compress("id_"||_n_), trim(id));
	 if eof then call symput("num_vars", _n_);	 
run;
%cts2dct(&dsn_vars,  dsn_in=&dsn_in, dsn_out=&data_out);

/* Begin Analysis using Cochran-Armitage Trend Test and Pearson Chisq */
proc freq data=&data_out  noprint;
     %do j=1 %to &num_vars; 
	     tables &&var_&j * &target /missing out=_d_&j;
     %end;
run;
%do j=1 %to &num_vars;
    proc freq data=_d_&j  order=formatted noprint;
	     weight count;
	     tables &&var_&j * &target /  chisq  trend;
		 output  out=_stat_&j  pchi  trend;
	run;
	data _stat_&j;
	     set _stat_&j;
         var_name = "&&var_&j";
		 X2L=_pchi_-(_trend_)**2;
		 DF_X2L=df_pchi-df_trend;
		 P_X2L=1-probchi(X2L, DF_X2L);
		 label X2L="Chisq(L)"  
		       P_X2L="P-value for Chisq(L)"
			   DF_X2L="DF of Chisq(L)"
	     ;
	run;	
	data _d_&j;	   
	    set _d_&j  end=eof;
		length value 8;
		value=put(&&var_&j, fmt_&&id_&j...);
		var_name = "&&var_&j";		
        keep var_name value  &target  count;
		output;
	run; 
	%let ds_seq1=&ds_seq1  _d_&j;
	%let ds_seq2=&ds_seq2  _stat_&j;	
%end;
data &dsn_out;     set &ds_seq1;   run;
data &stats_out;   set &ds_seq2;   run;
proc sort data=&dsn_out; by var_name  value; run;
data &dsn_out;
     set &dsn_out;
     by  var_name value;
     retain freq_all 0;
     if first.value then freq_all=count;
     if first.value AND last.value then do;
        freq_1=0; rate=0;
        output;
     end;
     else if last.value then do;
          freq_1=count;
          freq_all=freq_all+freq_1;
          rate=freq_1/freq_all;
          output;
     end;
     keep var_name value  freq_all freq_1 rate;
run;
proc sort data=&stats_out; by var_name; run;
proc datasets library=work  nolist;
     delete _stat_:  _d_:  _temp:;
quit;
%mend;




/* Model Diagnostics using Cook's D 
   Input data must be in format of contingency table 
*/
%macro inflgenmod(
     data=_last_,    /* Name of input data set                  */
     resp=,          /* Name of criterion variable              */
     model=,         /* Model specification                     */
     class=,         /* Names of class variables                */
     dist=,          /* Error distribution                      */
     link=,          /* Link function                           */
	 offset=,        /* Offset variable(s)                      */
     mopt=,          /* other model options (e.g., NOINT)       */
     freq=,          /* Freq variable                           */
     weight=,        /* Observation weight variable (zeros)     */
     id=,            /* Name of observation ID variable (char)  */
	 out=cookd,      /* Name of output data set                 */
     obstats=,       /* For a model already fitted              */
     parmest=       /*  "      "      "      "                 */
     /*infl=%str(difchi > 4 or hat > &hcrit or &bubble > 1)*/
	 );


	%*-- Reset required global options;
	%if &sysver >= 7 %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;

%let abort=0;
%let dist=%upcase(&dist);

%if %length(&model) = 0 %then %do;
    %put ERROR: List of model terms (MODEL=) is empty.;
    %let abort=1;
    %goto done;
    %end;

%if %length(&resp) = 0 %then %do;
    %put ERROR: No response (RESP=) has been specified.;
    %let abort=1;
    %goto done;
    %end;

%if %length(&dist) = 0 %then %do;
    %put WARNING: No distribution (DIST=) has been specified.;
    %put WARNING: GENMOD will use DIST=NORMAL.;
    %end;


%if &sysver < 6.12 %then %do;
   %if %upcase(&dist)=BINOMIAL %then %do;
      %if %length(%scan(&resp,2,/))=0 %then %do;
         %put ERROR: Response must be specified as RESP=events/trials for DIST=BINOMIAL;
         %let abort=1;
         %goto done;
         %end;
      %if %length(&link)=0 %then %let link=logit;
      %end;
   %end;

%if %length(&obstats)=0 or %length(&parmest)=0 %then %do;
ods select none;
proc genmod data=&data  order=formatted  rorder=formatted;
  class &class;
        %if %length(&freq)>0 %then %do;  freq &freq; %end;
        %if %length(&weight)>0 %then %do;  scwgt &weight; %end;
  model &resp = &model /
        %if %length(&dist)>0 %then %do;  dist=&dist %end;
        %if %length(&link)>0 %then %do;  link=&link %end;
		%if %length(&offset)>0 %then %do; offset=&offset  %end;
        %if %length(&mopt)>0 %then %do;  %str(&mopt) %end;
        obstats residuals;
	%if &sysver<7 %then %do;
  	    make 'obstats'  out=_obstat_ noprint;
  	    make 'parmest'  out=_parms_ noprint;
	%end;
	%else %do;		
	 	ods listing exclude ObStats;		
		ods output ObStats=_obstat_;
		ods output ParameterEstimates=_parms_;
		*proc print data=_parms_;
	 %end;
  run;
  ods select all;

%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
%let obstats=_obstat_;
%let parmest=_parms_;
%end;

options nonotes;
%let parms=0;
data _null_;
   set &parmest end=eof;
   parms + df;
   if eof then do;
      call symput('parms', left(put(parms,5.)));
      end;
run;
%*put parms=&parms;
%if &parms=0 %then %let abort=1; %if &abort %then %goto DONE;

data &out;
  /* GENMOD seems to make all class variables character */
  /* keep only the GENMOD computed variables */
  merge &data 
  		&obstats(keep=pred--reslik)
		end=eof;
  drop hcrit obs;
  obs=_N_;
  label hat = 'Leverage (H value)'
        cookd = "Cook's Distance"
        difchi = 'Change in Pearson ChiSquare'
        difdev = 'Change in Deviance'
        reschi = 'Pearson residual'
        resdev = 'Deviance residual'
        streschi = 'Adjusted Pearson residual'
        stresdev = 'Adjusted Deviance residual'
        seres = 'Residual Std. Error'
        pred = 'Fitted value';

  /* hat is the leverage */
  hat = Std*Hesswgt*Std;
  if hat<1 then do;
     cookd = hat*Streschi**2/((&parms)*(1-hat));
     seres = sqrt(1-hat);
     end;

  difchi = streschi**2;
  difdev = stresdev**2;

  if eof then do;
     hcrit =  &parms / obs;
     call symput('hcrit', put(hcrit,4.3));
  end;
run;
%put HAT CRITERIA = &hcrit;

data &out;
     set &out;
	 infl=(DIFCHI > 4 OR HAT > &hcrit OR cookd > 1);
run;
   

%done:
    %if &abort %then %put ERROR: The INFLGLIM macro ended abnormally.;
	%*-- Restore global options;
	%if &sysver >= 7 %then %do;
		options &o1 &o2;
	%end;
	%else %do;
	   options notes;
	%end;
%mend;



/********************************************************************/
/* categorize continuous variables in order to conduct simplified Model Diagnose 
   maximum=10 deciles. If distinct values<10 then #of deciles=#of distinct values
*/
%macro cts2dct(dsn_vars,        /* data contains the cts variables' names */
               dsn_in=_last_,   /* raw data */
               dsn_out=_last_,  /* data applied format */               
               bin_num=10       /* number of equal-sized bins */
               );
%local NotesStatus  blank   ds_seq1  ds_seq2  num_vars  i  j n;
%let blank=%str( );
%let ds_seq1=&blank;
%let ds_seq2=&blank;
%if &sysver>=7 %then %do;   
    %let NotesStatus=%sysfunc(getoption(notes));
	options nonotes;
%end;
%else options nonotes;

%if %length(&dsn_vars)=0 %then %do;
    %let errorcode=1;
    %goto done;
%end;
%if %length(&bin_num)=0 | &bin_num LE 1 %then %do;
    %let errorcode=3;
	%goto done;
%end;

data _null_;
     set &dsn_vars  end=eof;
     call symput(compress("var_"||_n_), var_name); 
	 call symput(compress("id_"||_n_), trim(id));
	 if eof then call symput("num_vars", _n_);	 
run;
%let i=1; %put &num_vars;

%if &num_vars=0 %then %do;
    %let errorcode=2;
    %goto done;
%end;

%local pctls  last_end  interval;
%let interval=%sysevalf((100-0)/&bin_num);
%let pctls=; 
%let last_end=0;
%do i=1 %to %eval(&bin_num-1);
    %let last_end=%sysevalf(&last_end+&interval);
    %let pctls=&pctls  &last_end;
%end;
%let pctls=0 &pctls 100;
proc univariate data=&dsn_in  noprint;
     var  %do i=1 %to &num_vars;
			   &&var_&i
		  %end;;	 
	 output out=_pctl  pctlpts=&pctls 
	                   pctlpre=%do i=1 %to &num_vars;
	                               P&i._
							   %end;; 
run;
data _pctl_t;       
      set _pctl;
	  %do i=1 %to &num_vars;
	     var_name="&&var_&i";
		 %let n=1;
		 %do  %while (%scan(&pctls, &n, %str( )) NE %str( ));
		      %let p=%scan(&pctls, &n, %str( ));
                   P&p = P&i._&p; 
			  %let n=%eval(&n+1);
		 %end;
		 var_name="&&var_&i";  id = compress(&i||'A');
		 keep var_name  P: id;
		 output;
	  %end;
run;
%do i=1 %to &num_vars;
    data _temp_&i;
	     array Pctl{0:&bin_num}  _temporary_;
	     if _n_=1 then do;		    
			do j=&i to &i;
			   set _pctl_t  point=j;
			   Pctl[0]=P0; 
			   /*
			   Pctl[1]=P10; Pctl[2]=P20; Pctl[3]=P30; Pctl[4]=P40; Pctl[5]=P50;
               Pctl[6]=P60; Pctl[7]=P70; Pctl[8]=P80; Pctl[9]=P90; 
			   */
			   %do n=1 %to %eval(&bin_num-1);
			       %let p=%scan(&pctls, %eval(&n+1), &blank);
			       Pctl[&n] = P&p;
			   %end;
               Pctl[&bin_num]=P100;
			end;			
		 end;
		 j=1;
		 do while (j<=&bin_num);
		    fmtname=compress("fmt_&&id_&i");	type="n";		
            start=Pctl[j-1]; 
            if j=1 then do; 
               end=Pctl[j]; label=(start+end)/2; 
			   j+1;
			end;
			else  do until(start < end);
			         end=Pctl[j];  label=(start+end)/2; 
					 put j=  end=;
			         j=j+1;
			      end;
			keep fmtname  start end  label;
			output;			
		 end;
		 hlo='O'; label=.;
		 keep fmtname  start end  label  hlo;
		 output;
		 stop;
	run;
	proc format cntlin=_temp_&i; run;
%end;
/*
 Apply format */
data &dsn_out;
     set &dsn_in;
	 format  %do i=1 %to  &num_vars;
	             &&var_&i  fmt_&&id_&i...
			 %end;;
run;

%let errorcode=0;
%done:
    %if errorcode=1 %then
        %put ERROR: Dataset containing names of continuous variables is not specified.;
	%else %if errorcode=2 %then
	    %put ERROR: Dataset containing names of continuous variables is empty.;
	%else %if errorcode=3 %then
	    %put ERROR: Specified BIN number is less than 2;

	/* clear temporary datasets */
	proc datasets library=work  nolist;
	     delete _pctl:  _temp_:;
	quit;
	/* restore system options */
	%if &sysver>=7 %then options  &NotesStatus;
	%else  options notes;
%mend;

%macro stackL2W(dsn_in, dsn_out, covar, target);
       data &dsn_out;
	     array _a{*}  freq_1  freq_all  pct;
	     set &dsn_in;  by &&var_&i;
		 var_name="&&covar";
		 value=&covar;
		 if first.&&covar then do; 
            freq_all=count; pct=percent; 
         end;
		 if first.&&covar AND last.&&covar then do;
            freq_1=0;  keep var_name  value   freq_1  freq_all  pct;
			output;
		 end;
		 else if last.&&covar then do;
		    freq_1=count; freq_all+count; pct+percent;
			keep var_name  value  freq_1  freq_all  pct;
			output;
		 end;
		run;
%mend;

%macro stackW2L(dsn_in, dsn_out, covar, freq_1, freq_all, target);
       data &dsn_out;
	      set &dsn_in;
		  &&covar=value;  &target=0; count=&freq_all-&freq_1;
		  output;
		  if &freq_1=0 then do;
		     &&covar=value;  &target=1; count=0;
			 output;
		  end;
		  else do;
		     &&covar=value;  &target=1; count=&freq_1;
			 output;
		  end;
		  keep  &&covar   &target  count;
%mend;

%macro dct_association2(dsn_in, dsn_out, dsn_var, target, threshold=5);
%let blank=%str( );
data _null_;
     set &dsn_vars  end=eof;
     call symput(compress("var_"||_n_), var_name); 
	 call symput(compress("id_"||_n_), trim(id));
	 if eof then call symput("num_vars", _n_);	 
run;
%let i=1; %put &num_vars;
proc freq data=&dsn_in  noprint;
     %do i=1 %to &num_vars;
         tables   &&var_&i*&target /missing out=work._dct_&i;
	 %end;
run;
%do i=1 %to &num_vars;
     proc sort data=work._dct_&i; by &&var_&i  &target; run;
 	 %stackL2W(work._dct_&i, work._dct2_&i, &&var_&i, Apps_a);
 
     proc sort data=work._dct2_&i; by pct; run;
	 data work._dct3_&i;
	      set work._dct2_&i;
		  retain _last_val_  ; length  _last_val_  $36.;
		  retain _pct_  _t1_  _t0_  0;
		  if missing(value) then value="**";
		  if pct>&threshold|_pct_>&threshold then do;		     
		     if missing(_last_val_) then _last_val_=value;
			 else  _last_val_=compress(value||'/'||_last_val_);
             keep  var_name  _last_val_  freq_1  freq_all;	
             rename _last_val_=value; 
             output;
			 _pct_=0;  _t1_=0; _t0_=0;  _last_val_=" ";
		  end;
		  else do;
		     _pct_+pct;  _t1_+freq_1;   _t0_+freq_all;  _last_val_=compress(value||'/'||_last_val_);
		  end;
	 run;		
     %stackW2L(work._dct3_&i, work._dct4_&i, &&var_&i, freq_1, freq_all, Apps_a) ;
	 proc freq data=work._dct4_&i  noprint;
	      weight count;
		  table  &&var_&i * &target /missing out=work._dct5_&i  chisq;
		  output  out=work._dct_s_&i  pchi lrchi;
	 run;    
     data work._dct_s_&i;
          set work._dct_s_&i;
          var_name="&&var_&i"; 
	 run;
	 %stackL2W(work._dct5_&i, work._dct5_&i, &&var_&i, Apps_a);
%end;
data &dsn_out;
     set %do i=1 %to &num_vars;
	         work._dct5_&i
	     %end;;
run;
data &dsn_out._stats;
     set %do i=1 %to &num_vars;
	         work._dct_s_&i
	     %end;;
run;

proc datasets library=work nolist;
     delete  _dct:   ;
quit;
%mend;

		    

 
		    
	     
