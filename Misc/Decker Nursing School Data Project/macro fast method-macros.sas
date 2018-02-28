libname xMacros "C:\Data";
option mstored sasmstore=xMacros mcompilenote=all;

/*
  This program is designed to performing a Long to Wide transformation
  to a 2-layer multi-record data set. Specifically, this program process the
  Census of Population and Housing 2000 data, but not limited to such
  a data set.
  
  copyright 2005 by Liang Xie
  Department of Economics
  Data/Online Office, Glenn G. Bartle Library
  State University of New York @ Binghamton
*/

/*
  This Macro counts the max number of individuals in a single household
  in a given dataset
*/
%macro count_P(ds, var, value) / store
                                 des="count the max number of individuals in a household ";
/*make pid_hi a global macro variable for further use*/
%global pid_hi;
  data _null_;
    set &ds;
	/* 
	 initialize counters. ctspbuff stores the max number
	*/
	if ctspbuff=. then do;
      ctspbuff=0;
	  ctsp=0;
	end;
	/*
	 retain counters' values to make count correct
	*/
	retain ctsp ctspbuff;
	if &var="&value" then ctsp=0;
    else do;
      ctsp=ctsp+1;
      if ctsp>ctspbuff then ctspbuff=ctsp;
    end;
	/*
	 transfer buffered max number to macro pid_hi
	*/
    call symput('pid_hi', ctspbuff) ; 

	/*
	  check the max number of individuals in single household in this 
	  dataset
	*/
	%put &pid_hi; 
  run;
%mend count_P;


/* 
   This Macro puts the names of household variables and individual variables
   to a sequential ordered macro numbers and stores the number of variables 
   for Household and Personal observations respectively, for ease of further 
   process.
*/

%macro getname / store 
                 des="Obtain the names of household and persoanl vars as macro vars";
data _null_;
  set xdata.varh;
  call symput(compress('hname'||_n_),hname);
  call symput(compress('hlength'||_n_),length);
  /*
   Macro variable numh counts up how many variables for Household observations
  */
  call symput('numh',_n_);
run;
data _null_;
  set xdata.varp;
  call symput(compress('pname'||_n_),pname);
  call symput(compress('plength'||_n_),length);
  /*
   Macro variable nump counts up how many variables for Household observations
  */
  call symput('nump',_n_);
run;/*testified*/
%mend getname;


/***************************************************************************
 This macro generates KEEP statment for individual variables in DATA STEP
***************************************************************************/
%macro m_keep(name, low, hi) / store 
                               des="Generate Keep Statement in DATA step";
  %let dash=_;
  %let name=&name&dash;
  keep 
    %do k=&low %to &hi;
	  &name&k
	%end;
%mend;


/*
  This macro tries to transform the Long format data into Wide format
  data based on observation of variable RECTYPE. This variable indicates
  if the observation is Household data or individual data
*/
%macro longtowide(longdata, widedata, IDvar, IDvalue, hvar_low, hvar_hi, pid_low, pid_hi, pvar_low, pvar_hi) 
                 / store des="Long To Wide Transformation";
  /*
   longdata   :  name of input data file that is in long form;
   widedata   :  name of input data file that is in wide form;
   hvar_low   :  the starting index number of Household variable;
   hvar_hi    :  the ending index number of Household variable;
   pvar_low   :  the starting index number of Personal variable;
   pvar_hi    :  the ending index number of Personal variable;
   pid_low    :  the starting index number of persons in a single household;
   pid_hi     :  the ending index number of persons in a single household;
  */ 

  data &widedata; 
    set &longdata;
	/*
	 make Arry for Household variables. Since each variable has different names
     for ease of process, first denote the arry with a sequential name, then change 
	 them back to their original ones for ease of display
	*/
    array H_array(&hvar_low:&hvar_hi) $ _H&hvar_low - _H&hvar_hi;
	retain _H&hvar_low - _H&hvar_hi;
	%do i=&hvar_low %to &hvar_hi;
	  rename _H&i=&&hname&i;
	%end;
    /*
     the array for individual variables is defined as:
	    for each variable there is an array, while the number of elements is the
	    max number of individuals in a single household in this dataset
	*/

    %do i=&pvar_low %to &pvar_hi;
      %let firstpart=&&pname&i;
      %let nametemp=&firstpart;
	  %let dash=_;
      %let macro_name=&nametemp&dash;
	  array P_&i(&pid_low:&pid_hi) $ &&plength&i &macro_name&pid_low - &macro_name&pid_hi;
	  retain &macro_name&pid_low - &macro_name&pid_hi;
	%end;

    /*
	  pid is used to track the number of individuals in a single household
	*/
    if pid=. then pid=1;
    else pid=pid+1;

	if (&IDvar = "&IDvalue") then do;
	  /* 
	    no output for the first observation since it is H and it is unknow
	    if there are subsequential individual observations in this household
	  */
      if _n_ >1 then output;
	  /*
       transfer values for Household variable
	  */
      %do i=&hvar_low %to &hvar_hi;
	    %let tempvarname=&&hname&i;
	    H_array[&i]=%sysfunc(left(&&tempvarname));
	  %end;
	  /*
       Initialize individual data o.w. 'retain' statement will transfer unecessary
	   values to next household group
	  */
	  %do n=&pid_low %to &pid_hi;
        %do i=&pvar_low %to &pvar_hi;
          %let tempvarname=&&pname&i;
	      %let dash=_;
          %let macro_name=&tempvarname&dash;
	      P_&i[&n]=' ';
	    %end;
	  %end;
	  pid=0;
	end;

    /*
     transfer values for individual variables given current number of individual
	*/
    if (&IDvar ne "&IDvalue") then do;
	  pidhi="&pid_hi";
	  if (pid LE pidhi) then do;
        %do i=&pvar_low %to &pvar_hi;
          %let tempvarname=&&pname&i;
	      %let dash=_;
          %let macro_name=&tempvarname&dash;
	      P_&i[pid]=%sysfunc(left(&tempvarname));
	    %end;
	  end;
	end;

    /*
     drop unecessary data fromo original data set and temporary variables
	*/
	keep
	%do i=&hvar_low %to &hvar_hi;
	  &&hname&i
	%end;;
    %do i=&pvar_low %to &pvar_hi;
	  %m_keep(&&pname&i, &pid_low, &pid_hi);
	%end;
    /* pid must be retained to keep individual counts correct*/
    retain pid;
	drop pid pidhi;

  run;
%mend longtowide;

