%macro FMTGen_Macro(lib, var_name, boundry, labels, func, if_interval) /store;
/* *******************************************************************
TESTING: note, for labeling, only numbers are allowed.
%let percent=10 20 30 40 50 60 70 80 90;
%let labeling=15.01 25.05 35 45 55 65 75 85 95 ;
%FMTGen_Macro(work, return, &percent, &labeling, infmt, range);
********************************************************************/
option nomprint nomlogic;
    %let blank=%str( );
    %let n=1; %let k=1;
/*-------------------------------------------------------------------------*/
   %if &func=&blank %then
        %let &func=FMT;
    %else %do;
        %let func=%upcase(&func);
        %if NOT((&func=FMT) OR (&func=INFMT)) %then %do;
            %put ERROR. Invalid Functionality. Only FMT or INFMT are allowed. ;
            %put Assuming FMT, continuing... ;
            %let func=FMT;
        %end;
    %end;
 /*-------------------------------------------------------------------------*/
    %let b_&n=%scan(&boundry, &n, &blank);
     %do %while (&&b_&n NE &blank);
        %let n=%eval(&n+1);
        %let b_&n=%scan(&boundry, &n, &blank);
    %end;

    %let la_&k=%scan(&labels, &k, &blank);
    %do %while (&&la_&k NE &blank);
         %let k=%eval(&k+1);
         %let la_&k=%scan(&labels, &k, &blank);
    %end;
/*-------------------------------------------------------------------------*/
    %if &if_interval=&blank %then
        %let if_interval=RANGE;
    %else %do;
        %let if_interval=%upcase(&if_interval);
        %if NOT ((&if_interval=RANGE) OR (&if_interval=POINT)) %then %do;
            %put ERROR! Invalide Value. Either RANGE or POINT is allowed. ;
            %put Assuming RANGE, continuing...;
            %let if_interval=RANGE;
         %end;
     %end;
/*-------------------------------------------------------------------------*/
    %if (&k=&n OR &k>&n) %then %do;
        %if &if_interval=RANGE %then %do;
            %put Warning! ;
            %put Label is more than needed. Last Label will be discarded! ;
        %end;
    %end;
    %else
        %if &k=%eval(&n-1) %then %do;
            %if &if_interval=POINT %then %do;
                %put Number of Label is one short of that of Points. ;
                %put Change format method to be POINT. ;
                %let if_interval=POINT;
            %end;
        %end;
        %else %do;
            %put ERROR! Not enough Labels!
            %goto exit;
        %end;
/*-------------------------------------------------------------------------*/
    %if &if_interval=RANGE %then
        %let n=%eval(&n-2);
    %else %let n=%eval(&n-1);


    %if %eval(&n-1)>-1 %then %do;
        %let FMT=;
        %do i=1 %to &n;
            %if &if_interval=RANGE %then %do;
                %let k=%eval(&i+1);
                %let FMT=&FMT
                         &&b_&i - &&b_&k = &&la_&i
                         ;
             %end;
            %else
                %let FMT=&FMT
                        &&b_&i = &&la_&i
                        ;
        %end;
    %end;
    %else %do;
        %put ERROR! Only &n values for &if_interval on job &func. ;
        %goto exit;
    %end;

    %put func is &func;
    %if &func=FMT %then %let job=value;
    %else %let job=invalue;
/*-------------------------------------------------------------------------*/
    proc format;
        &job FMT_&var_name  &FMT
                             other = -999
                             ;
    run;
%exit: %mend;

%macro Parse_Var(dsn, var_in) /store;
option mprint mlogic ;
   %global vars_N
           vars_C;
   %let blank=%str( );
   %let dsid=%sysfunc(open(&dsn,i));
   %if (&dsid = 0) %then %do;
        %put %sysfunc(sysmsg());
        %goto exit;
   %end;
   %let varlist_N=;
   %let varlist_C=;
   %if &var_in ne &blank %then %do;
       %let i=1;
       %let vartemp=%scan(&var_in, &i, &blank);
       %do %while (&vartemp ne &blank);
           %let pos = %sysfunc(varnum(&dsid, &vartemp));
           %if (%sysfunc(vartype(&dsid, &pos)) = N) %then
               %let varlist_N=&varlist_N &vartemp;
           %if (%sysfunc(vartype(&dsid, &pos)) = C) %then
               %let varlist_C=&varlist_C &vartemp;
           %let i=%eval(&i+1);
           %let vartemp=%scan(&var_in, &i, &blank);
       %end;
   %end;
   %let rc=%sysfunc(close(&dsid));
   %let vars_N=&varlist_N;
   %let vars_C=&varlist_C;
%exit: %mend Parse_Var;


%macro profile_remote(remote_dsn, outlib, by_var, class_var, vars) /store;
/*
  To do profile for Char and Num type variables;
  Syntax: libname XXX remote ReMoteDataBase
          %profile(&dsn, &lib, &blank, &blank, &vars);
*/
option mprint  mlogic;
    %let blank=%str( );
    %let dot=%str(.);
    %let dsn=temp_r;

    %if &by_var = &blank %then
        %let by_clause=&blank;
    %else
        %let by_clause=by %scan(&by_var, 1, &blank) ;

    %if &class_var = &blank %then
        %let class_clause=&blank;
    %else
        %let class_clause=class %scan(&class_var, 1, &blank) ;

    /* Parse out Numeric variables only */

    proc sql outobs=1;
       connect to odbc as myConn(dsn=MARKET);
       create table &dsn as
       select * from connection to myConn
           (select * from &remote_dsn)
    quit;
    %Parse_Var(&dsn, &vars);

    /*if not outlib, then use work default*/
    %if &outlib=&blank %then
        %let outlib=work;

    /*
    %let lib=%scan(&dsn, 1, &dot);
    %if &lib = &blank %then
        %let lib=work;
    %put &vars_N &vars_C;
    */
    %if (&vars_N ne &blank) %then %do;
        title "Profile Analysis for Numeric Type";
        %put Numeric Type Variables are: ;
        %put  --- &vars_N;
        /*
        ods select BasicMeasures Quantiles ExtremeObs MissingValues;
        ods output BasicMeasures=&outlib.._Basics
                   Quantiles=&outlib.._Quant
                   MissingValues=&outlib.._missing
                   ExtremeObs=&outlib.._Extreme;
        */
        %let i=1;
        %let var_N=%scan(&vars_N, &i, &blank);
        %do %while (&var_N NE &blank);
            %put &i &blank &blank &var_N;
            /*reduce data dimension for PROC univeriate*/
            proc sql;
               connect to odbc as myConn(dsn=MARKET);
               create table &dsn as
               select &var_N from connection to myConn
                   (select &var_N
                    from &remote_dsn
                    where Status='FND')
            quit;
            proc univariate data=&dsn nextrval=5 noprint;
                 &by_clause;
                 &class_clause;
                 var &var_N;
                 output out=Stat_&i mean=Avg Std=Std min=Min max=Max
                                    kurtosis=Kurto Skewness=skew
                                    nobs=num_obs nmiss=num_miss;
                 output out=Pctl_&i pctlpts=0 10 20 30 40 50 60 70 80 90 100
                                    pctlpre=P;
            run;
            data Stat_&i;
               set Stat_&i;
               length Var_Name $12.;
               Var_Name="&var_N";
            run;
            data Pctl_&i;
               set Pctl_&i;
               length Var_Name $12.;
               Var_Name="&var_N";
            run;
            %let i=%eval(&i+1);
            %let var_N=%scan(&vars_N, &i, &blank);
       %end;

        %let n=%eval(&i-1);
        %let StatSets=; %let PctlSets=;
        %do i=1 %to &n;
            %let StatSets=&StatSets Stat_&i;
            %let PctlSets=&PctlSets Pctl_&i;
        %end;
        data &outlib..Stats_All;
            set &StatSets;
            label Avg="Mean"
                  Std="Std Deviation"
                  Min="Minimum"
                  Max="Maximum"
                  kurto="Kurtosis"
                  Skew="Skewness"
                  num_obs="Number of Obs"
                  num_miss="Number of missing";
        run;
        data &outlib..Pctl_All;
            set &PctlSets;
            label P0 ="Min"
                  P10="10%"
                  P20="20%"
                  P30="30%"
                  P40="40%"
                  P50="50%"
                  P60="60%"
                  P70="70%"
                  P80="80%"
                  P90="90%"
                  P100="Max";
        run;
    %end;
    %else %put No Numeric Type Variables;

    %if (&vars_C ne &blank) %then %do;
         title "Profile Analysis for Char Type";
         %put Char Type Variables are: ;
         %put --- &vars_C;
         %let string_out=;
         %translate(&vars_C, %str(,), %str( ));
         %let i=1;
         %let var_C=%scan(&vars_C, &i, &blank); %put &var_C;
         proc sql;
             connect to odbc as myConn(dsn=MARKET);
             create table &dsn as
             select &string_out from connection to myConn
                   (select &string_out
                    from &remote_dsn
                    where Status='FND')
         quit;
         proc freq data=&dsn noprint;
              %do %while (&var_C NE &blank); %put &i;
                  tables &var_C / missing
                                  outcum
                                  out=work.OneWayFreq_&i;
                  %let i=%eval(&i+1);
                  %let var_C=%scan(&vars_C, &i, &blank);
              %end;
         run;

         %let n=%eval(&i-1);
         %let smallsets=;
         %do i=1 %to &n;
              %let var_C=%scan(&vars_C, &i, &blank);
              data work.OneWayFreq_&i;   /*try to keep the order of variables*/
                 array _C{*} $ 128 var_name  value ;
                 array _N{*} count percent cum_freq cum_pct;
                 set onewayfreq_&i;
                 value=&var_C;
                 var_name="&var_C";
                 keep var_name value count percent cum_freq cum_pct;
              run;
              %let smallsets=&smallsets work.OneWayFreq_&i;
         %end;
         data &outlib..OneWayFreq_All;
              set &smallsets;
         run;
   %end;
   %else %put No Char Type Variables;
%mend profile_remote;

%macro xway_hist(dsn, class_var, vars);
/*
   For generating graphics for presentation purpose only
*/
option mprint mlogic ;
    %let blank=%str( );
    %let dot=%str(.);
    %let n=1;

    %let class_clause=class ;
    %let class_temp_&n=%scan(&class_var, &n, &blank);

    %if &class_var = %then %do;
        %let class_clause=&blank;
        %let n=0;
        %let Count_1=1;
        %let Count_2=1;
    %end;
    %else  %do;
       %do %while (%eval(&n<2)AND(&&class_temp_&n ne "&blank"));
           %let class_clause=&class_clause &&class_temp_&n;
           %let n=%eval(&n+1);
           %let class_temp_&n=%scan(&class_var, &n, &blank);
       %end;

       proc sql noprint;
          %do i=1 %to &n;
              %global Count_&i;
              select count(distinct &&class_temp_&i) into :Count_&i
              from &dsn;
          %end;
       quit;

       %put &n;
       %do i=1 %to &n;
           %if &&Count_&i=. %then %let Count_&i=1;
           %else
               %if (&&Count_&i < 50) %then
                   %let Count_&i = %sysfunc(min(3, &&Count_&i));
               %else %do;
                   %put Warning! &&Class_temp_&i has too many classes!
                   %goto exit;
               %end;
       %end;
    %end; /*else do*/

    %let vars=%scan(&vars, 1, &blank);   /* only draw w.r.t one variable */

    title "&n - Way Comparative Histogram for &vars";
    ods select Quantiles;
    proc univariate data=&dsn;
         &class_clause;
         var &vars;
         histogram &vars / ncols = &Count_1
                           nrows = &Count_2
                           ;

         inset mean std="Std Dev" skewness Kurtosis / pos = ne;
    run;
%exit: %mend xway_hist;

%macro translate(string_in, trans_char, target_char) /store;
    %let blank=%str( );
    %global string_out;
    %let string_out=;
    %if (&string_in NE &blank) %then %do;
        %let i=1;
        %let substring=%scan(&string_in, &i, &target_char);
        %let string_out=&string_out &substring;
        %let i=%eval(&i+1);
        %let substring=%scan(&string_in, &i, &target_char);
        %do %while (&substring NE &blank);
            %let string_out=&string_out.&trans_char &substring;
            %let i=%eval(&i+1);
            %let substring=%scan(&string_in, &i, &target_char);
        %end;
    %end;
    %else
        %let string_out=&blank;
%mend  translate;

%macro FMT_stat(dsn, var, id, func) /store;
/*
  This macro generate FORMAT for each subgroup according
  to ID variable, based either on MEAN or MEDIAN.
  Later, to assign an ID for &var, use:
  data &dsn;
     set &dsn;
     &id=put(&var, FMT_&var);
  run;
*/
/*-----------------------------------------------------------------------*/
    %let blank=%str( );
    %if &var NE &blank %then
        %let var=%scan(&var, 1, &blank);
    %else %do;
        %put ERROR! No variable is specified! ;
        %goto exit;
    %end;

    %if &id NE &blank %then %do;
        %let id=%scan(&id, 1, &blank);
        data _temp_&var;
             set &dsn;
             &var=&var;
             id_&var=&id;
        run;
    %end;
    %else %do;
        data _temp_&var;
             set &dsn;
             &var=&var;
             id_&var=put(&var, FMT_&var..);
        run;
    %end;

    %let func=%upcase(&func);
    %if &func=&blank %then
        %let func=mean;
    %else %do;
        %if NOT ((&func=MEAN) OR (&func=MEDIAN)) %then %do;
            %put Statistic &func is not supported. ;
            %put Assuming MEAN ...;
            %let &func=MEAN;
        %end;
    %end;
/*-----------------------------------------------------------------------*/
    %put &var &func;
    proc means data=_temp_&var;
       class id_&var;
       var &var;
       output out=&func._&var &func=Value;
    run;

    %global labeling id_seq;
    %let labeling=;
    %let id_seq=;
    proc sql noprint;
       select id_&var, Value into :id_seq separated by ' ',
                                  :labeling separated by ' '
       from &func._&var;
    quit;
/*-----------------------------------------------------------------------*/
    %FMTGen_Macro(work, id_&var, &id_seq, &labeling, FMT, POINT);
    proc datasets library=work;
         delete _temp_&var;
         delete &func._&var;
    run;

%exit: %mend FMT_stat;

%macro Quasi_Separation(dsn, target, sep_var) /store;                                                                                   
/*                                                                                                                                      
   used the data-driven method of Greenacre 1988.                                                                                       
   The levels (rows) are hierarchically clustered based on the reduction in the                                                         
   chi-squared test statistics of association between the categorical variable                                                          
   and the target. At such level of cluster, the categorical independent variable                                                       
   has the largest association with response variable.                                                                                  
*/                                                                                                                                      
  proc means data=&dsn noprint;                                                                                                         
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
                                                                                                                                        
  proc freq data=&dsn noprint;                                                                                                          
      table &sep_var*&target  /chisq;                                                                                                   
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
                                                                                                                                        
   proc tree data=_tree                                                                                                                 
             nclusters=&opt_cluster                                                                                                     
             out=_clus                                                                                                                  
             h=rsq                                                                                                                      
             noprint;                                                                                                                   
       id &sep_var;                                                                                                                     
   run;                                                                                                                                 
                                                                                                                                        
   proc sort data=_clus;                                                                                                                
       by clusname;                                                                                                                     
   run;                                                                                                                                 
                                                                                                                                        
   proc print data=_clus;                                                                                                               
      by clusname;                                                                                                                      
      id clusname;                                                                                                                      
   run;                                                                                                                                 
                                                                                                                                        
%mend quasi_separation; 

%macro cts_profile(dsn, outlib, by_var, class_var, vars_N) /store ;
       %let blank=%str( );
       %if &by_var = &blank %then
           %let by_clause=&blank;
       %else
           %let by_clause=by %scan(&by_var, 1, &blank) ;

       %if &class_var = &blank %then
           %let class_clause=&blank;
       %else
           %let class_clause=class %scan(&class_var, 1, &blank) ;

       %let i=1;
       %let var_N=%scan(&vars_N, &i, &blank);
       %do %while (&var_N NE &blank);
           %put &i &blank &blank &var_N;
           data _temp;
               set &dsn(keep=&var_N);
           run;
           proc univariate data=&dsn nextrval=5 noprint;
                &by_clause;
                &class_clause;
                var &var_N;
                output out=Stat_&i mean=Avg Std=Std min=Min max=Max
                                   kurtosis=Kurto Skewness=skew
                                   nobs=num_obs nmiss=num_miss;
                output out=Pctl_&i pctlpts=0 10 20 30 40 50 60 70 80 90 100
                                   pctlpre=P;
           run;
           data Stat_&i;
              set Stat_&i;
              length Var_Name $12.;
              Var_Name="&var_N";
           run;
           data Pctl_&i;
             set Pctl_&i;
              length Var_Name $12.;
              Var_Name="&var_N";
           run;
           %let i=%eval(&i+1);
           %let var_N=%scan(&vars_N, &i, &blank);
       %end;
       %let n=%eval(&i-1);
       %let StatSets=; %let PctlSets=;
       %do i=1 %to &n;
           %let StatSets=&StatSets Stat_&i;
           %let PctlSets=&PctlSets Pctl_&i;
       %end;
       data &outlib..Stats_All;
            set &StatSets;
            label Avg="Mean"
                 Std="Std Deviation"
                  Min="Minimum"
                  Max="Maximum"
                  kurto="Kurtosis"
                  Skew="Skewness"
                  num_obs="Number of Obs"
                  num_miss="Number of missing";
        run;
        data &outlib..Pctl_All;
            set &PctlSets;
            label P0 ="Min"
                  P10="10%"
                  P20="20%"
                  P30="30%"
                  P40="40%"
                  P50="50%"
                  P60="60%"
                  P70="70%"
                  P80="80%"
                  P90="90%"
                  P100="Max";
        run;
        proc datasets nolist;
             delete _temp;
             %do i=1 %to &n;
                 delete Stat_&i;
                 delete Pctl_&i;
             %end;
        run;
%mend cts_profile;

%macro dct_profile(dsn, outlib, vars_in) /store;
       title "Profile Analysis for Char Type";
       %let blank=%str( );

       %let i=1;
       %let var_C=%scan(&vars_in, &i, &blank);

       proc freq data=&dsn noprint;
            %do %while (&var_C NE &blank); %put &i &var_C;
                tables &var_C / missing
                                outcum
                                out=work.OneWayFreq_&i;
                %let i=%eval(&i+1);
                %let var_C=%scan(&vars_in, &i, &blank);
            %end;
       run;
       %let n=%eval(&i-1);
       %let smallsets=;
       %do i=1 %to &n;
            %let var_C=%scan(&vars_in, &i, &blank);
            data work.OneWayFreq_&i;   /*try to keep the order of variables*/
                 array _C{*} $ 24 var_name;
                 array _D{*} $ 128 value ;
                 array _N{*} count percent cum_freq cum_pct;
                 set onewayfreq_&i;
                 value=&var_C;
                 var_name="&var_C";
                 keep var_name value count percent cum_freq cum_pct;
            run;
            %let smallsets=&smallsets work.OneWayFreq_&i;
       %end;
       data &outlib..OneWayFreq_All;
            set &smallsets;
       run;
       proc datasets nolist;
            delete _temp;
            %do i=1 %to &n;
                delete OneWayFreq_&i;
            %end;
       run;
%mend dct_profile;


%macro logit_DCT(dsn, outlib, target, DCT_var) /  store;
  %if &outlib=%str( ) %then
  %let outlib=work;
  proc sql;
       create table &outlib..plot_&DCT_var as
       select &DCT_var,&target, count(*)
       from   &dsn
       group by &DCT_var, &target;
  quit;

  proc transpose data=&outlib..plot_&DCT_var 
                 out=&outlib..plot2_&DCT_var;
       by &DCT_var;
       id  &target;
  run;

  data &outlib..plot2_&DCT_var;
       set &outlib..plot2_&DCT_var;
       OR=_1/_0;
       logit=log(OR);
  run;

  proc plot data=&outlib..plot2_&DCT_var;
       title "Logit Plot for &DCT_var on &target";
       *plot logit*&DCT_var="*";
       plot OR*&DCT_var="X";
  quit;
%mend logit_DCT;

%macro logit_cat(dsn, outlib, target, cat_covar) /store;
       %let target=%scan(&target, 1, %str( ));
       proc means data=&dsn noprint nway;
	        class &cat_covar;
			var &target;
			output out=&outlib.._&cat_covar 
                   sum(&target)=t_&target
            ;
	   run;

	   data &outlib.._&cat_covar;
	        set &outlib.._&cat_covar;
			k=sqrt(_freq_)/2;
			odds=(t_&target+k)/(_freq_-t_&target+k);
			positive_rate=t_&target/_freq_;
            logit_&cat_covar=log(odds);
			drop k;
	   run;
	   symbol1 color=red
               interpol=join
               value=dot
               height=3;
	   title "Positive Rate of &cat_covar v.s &target";
	   proc gplot data=&outlib.._&cat_covar;
	        plot positive_rate*&cat_covar;
		run;
		quit;
%mend logit_cat;

%macro classify_decile(dsn, outlib, target, covar) /store;
    %global total_obs;

*Step 1, Classify deciles directly by order;
    %let blank=%str( );
    %let covar=%scan(&covar, 1, &blank);

    data temp;
        set &dsn(keep=&target &covar);
        call symput('total_obs', _n_);
    run;

    proc sort data=temp;
         by &covar;
    run;

    data temp;
        set temp;
        decile=floor(_n_/symget('total_obs')*10);
        if decile=10 then decile=9;
    run;
*Step 2, compute group mean by percentiles;
    proc sql;
        create table temp1 as
        select decile, count(*) as obs_decile, avg(&covar) as mean_&covar
        from  temp
        group by decile;
    quit;
*Step 3, Assign Deciles by Value;
    %if &outlib=&blank
       %then %let outlib=work;
    %else
       %let outlib=&outlib;

    data &outlib..ValueDecile_&covar;
         array _Mean{*} M1-M10;
         array _Deck{*} D1-D10;
         if _n_ =1 then
            do i=1 to 10;
               set temp1;
               _Mean[i]=mean_&covar;
               _Deck[i]=decile;
            end;

          retain  M1-M10;
          set temp;
             do i=2 to 10;
                k=i-1;
                if missing(&covar) then
                   V_Decile_&covar=0;
                else
                   do;
                     if (_Mean[k] < &covar) and (&covar <= _Mean[i]) then do;
                        V_Decile_&covar=i;
                        leave;
                     end;
                     else
                        V_Decile_covar=1;
                   end;
            end;
          keep &target &covar V_Decile_&covar;
    run;

    proc datasets lib=work nolist;
         delete temp;
         delete temp1;
    quit;
%mend classify_decile;

%macro logit_pctl(dsn, outlib, if_logit, target, covar) /store;
/*
  This macro computes smoothed logit curve v.s percentiles' mean values;
  tries to identify potential curvature relationship
  Target should be 0,1 valued. If not, use "target=event", i.e. Channel=DM,
  etc
*/
*Step 1, divides covariate into designated percentiles;
    %global total_obs;
    %let if_logit=%sysfunc(upcase(&if_logit));
    %if &if_logit=Y OR %if_logit=YES %then
        %let if_logit=Y;
    %else
        %let if_logit=N;

    %let blank=%str( );
    %let covar=%scan(&covar, 1, &blank);

/*
    proc sql noprint;
        select count(*) into :total_obs
        from &dsn;
    quit;
*/

    data temp;
        set &dsn(keep=&target &covar);
        call symput('total_obs', _n_);
    run;

    proc sort data=temp;
         by &covar;
    run;

    data temp;
        set temp;
        decile=floor(_n_/symget('total_obs')*10);
        if decile=10 then decile=9;
    run;

*Step 2, compute group mean by percentiles;

    proc sql;
        create table temp1 as
        select decile, count(*) as obs_decile, sum(&target) as sum_&target, avg(&covar) as mean_&covar
        from  temp
        group by decile;
    quit;

*Step 3, count empirical logit based on percentiles;

    %if &outlib=&blank
       %then %let outlib=work;
    %else
       %let outlib=&outlib;

    data &outlib..logit_&covar;
        set temp1;
        *sum_&target=sum_&target+sum_&target;
        if "&if_logit"="Y" then
           result=log(sum_&target/(obs_decile-sum_&target));
        else
           result=sum_&target/(obs_decile-sum_&target);
    run;

    proc datasets lib=work nolist;
         delete temp;
         delete temp1;
    quit;

%mend logit_pctl;

%macro hist(dsn, var_in, bin_num) /store ;

   %if %sysevalf(&bin_num<0) %then %do;
       %put ERROR! Negative number of bins: &bin_num;
       %goto &exit;
   %end;

   %let blank=%str( );
   %let var_in=%scan(&var_in, 1, &blank);

   proc univariate data=&dsn noprint;
        title "Univariate Analysis for &var_in";
        var &var_in;
        output out=Pctl_&var_in pctlpts= 1 99
                   pctlpre= P;
   run;

   data _null_;
       %global start end;
       set Pctl_&var_in;
       call symput('start', P1);
       call symput('end', P99);
   run;
   %let bin_interval=%sysevalf((&end-&start)/&bin_num);

   proc univariate data=&dsn noprint;
        histogram &var_in / midpoints    = &start to &end by &bin_interval
                            rtinclude
                            cfill=red
                            outhistogram = OutBin_&var_in;
   run;

%exit: %mend hist;

%macro auto_varclus_select(dsn, input, cutoff);

      ods output clusterquality=clusterQ
                 rsquare(match_all)=rsq;
      proc varclus data=&dsn maxeigen=&cutoff outtree=_VarTree short;
           var &input;
      run;
      ods listing;

      data _null_;
           set clusterQ;
           call symput('ncl',trim(left(numberofclusters-2)));
      run;

      proc print data=clusters&ncl;
      run;
%mend;

                                                                                                                                          
%macro pct2fmt(dsn, covars);                                                                                                              
   %let i=1;                                                                                                                              
   %let blank=%str( );                                                                                                                    
   %let covar=%scan(&covars, &i, &blank);                                                                                                 
                                                                                                                                          
   proc freq data=&dsn;                                                                                                                   
        %do %while (&covar NE %blank);                                                                                                    
            tables &covar /missing                                                                                                        
                           out=work.freq_&i;                                                                                              
            %let i=%eval(&i+1);                                                                                                           
            %let covar=%scan(&covars, &i, &blank);                                                                                        
        %end;                                                                                                                             
   run;                                                                                                                                   
                                                                                                                                          
   %let nvars=%eval(&i-1);                                                                                                                
   %do i=1 %to &nvars;                                                                                                                    
      %let covar=%scan(&covars, &i, &blank);                                                                                              
      data work.freq_&i;                                                                                                                  
           set work.freq_&i(keep=&covar percent);                                                                                         
           percent=percent/10;                                                                                                            
           rename &covar=start                                                                                                            
                  percent=label;                                                                                                          
           retain fmtname "$fmt&covar";                                                                                                   
      run;                                                                                                                                
      proc format cntlin=work.freq_&i;                                                                                                    
      run;                                                                                                                                
   %end;                                                                                                                                  
%mend;                                                                                                                                    
                                                                                                                                          

