/*Stratified random sample with replacement, equal allocation*/
/*------------------------------------------------------------------------*/
/* Create sample data base of student grade point averages from           */
/* East High School, Grades 9 through 12, 100 or more students per grade. */

data EastHigh;
  format GPA 3.1;
  do Grade=9 to 12;
    do StudentID=1 to 100+int(201*ranuni(432098));
      GPA=2.0 + (2.1*ranuni(34280));
      output;
    end;
  end;
run;


/* Method 1: Using PROC SURVEYSELECT                          */
/*                                                            */
/* N= is the number of observations to select from            */
/* each group.  Use METHOD=URS.  The statement STRATA defines */
/* the variable that is used for grouping.  OUT= names the    */
/* SAS data set that will be created by the procedure.        */

proc surveyselect data=EastHigh method=urs n=5 out=sample;
  strata Grade;
run;

title 'Method 1: PROC SURVEYSELECT';
proc print data=sample;
run;



/* Method 2: Using Base SAS                                          */
/*                                                                   */
/* When we sample with replacement, once an obs is selected it goes  */
/* back into the pool of possible choices to be selected again.      */
/* Instead of reading through the data set sequentially, we use the  */
/* POINT= option on the SET statement to read observations directly. */
/* POINT= is incompatible with BY processing, so in order to take    */
/* the sample within each BY-Group we'll simulate BY processing in a */
/* macro.                                                            */


data _null_;
  set EastHigh end=eof;
  by Grade;

  /* On first observation of a BY-Group, increment counter, */
  /* and create macro variable for it, FIRSTn               */
  if first.grade then do;
    i+1;
    call symput('first'||trim(left(i)),trim(left(_n_)));
    end;

  /* For last observation in a BY-Group, assign the value */
  /* of the counter into a macro variable, LASTn          */
  if last.grade then do;
     call symput('last'||trim(left(i)),trim(left(_n_)));
     end;

  /* If at the end of the data set, assign the total number of */
  /* BY-Groups into a macro variable, TOTAL                    */
  if eof then do;
    call symput('total',trim(left(i))); 
    end;
run;

/* Macro code to take a simple random sample with replacement from */
/* each BY-Group, or strata                                        */

%macro samp;

  %do n=1 %to &total;
    data strata&n;
      set EastHigh(firstobs=&&first&n obs=&&last&n);
    run;
      
    data sample&n(drop=i);
      choice=int(ranuni(36830)*n)+1;
      set strata&n point=choice nobs=n;
      i+1;
 
      /* Enter the desired sample size, 5 in this case */
      if i>5 then stop;
     run;
  %end;

  data sample;
    set 
    %do j=1 %to &total;
      sample&j
    %end;
    ;;
  run;

%mend samp;

/* Invoke the macro SAMPLE */
%samp

proc sort data=sample;
  by grade studentid;
run;

title 'Method 2: Using Base SAS';
proc print data=sample;
run;
*============================================================================; 
/*Stratified random sample with replacement, unequal allocation*/
/*----------------------------------------------------------------------*/
/* Create sample data base of student grade point averages from East High */
/* School, Grades 9 through 12, 100 or more students per grade.  Sampling */
/* will be done with replacement, so an individual student's score may be */
/* sampled more than once.                                                */
 
data EastHigh;
  format GPA 3.1;
  do Grade=9 to 12;
    do StudentID=1 to 100+int(201*ranuni(432098));
      GPA=2.0 + (2.1*ranuni(34280));
      output;
    end;
  end;
run;
 
/* Method 1: Use PROC SURVEYSELECT if you have SAS/STAT Version 7 or higher   */
/*                                                                            */
/* Sample 4 students from Grade 9 , 6 from Grade 10, and 10 from both Grades  */
/* 11 and 12.  N= is the number of observations to select from each group.    */
/* Use METHOD=URS to request unrestricted random sampling, which is selection */
/* with equal probability and with replacement.  The statement STRATA defines */
/* the variable that is used for grouping.  OUT= names the SAS data set that  */
/* will be created by the procedure.                                          */
 
proc surveyselect data=EastHigh method=urs n=(4, 6, 10, 10) out=sample;
  strata Grade;
run;
 
title 'Surveyselect Method';
proc print data=sample;
run;
 
 
 
/* Method 2: Use Base SAS if you do not have SAS/STAT Version 7 or higher       */
/*                                                                              */
/* Sample 4 students from Grade 9, 6 from Grade 10, and 10 from both Grades     */
/* 11 and 12. Use RANUNI to  generate the observation numbers to be sampled.    */
/* Note that the data set is sorted by Grade.  In a following DATA step, create */
/* a counter that increments with each iteration of the step and resets to 0    */
/* when the BY-Group changes.  For each BY-Group specify the maximum number     */
/* of observations to sample.  Output until the counter matches the maximum     */
/* number to output per BY-Group.                                               */
 
/* Determine the starting observation and ending observation for each grade, */
/* or BY-Group.  Use this information to determine the random observations   */
/* to sample from each BY-Group.                                             */
 
data bygroup_range;
  keep grade start stop;
  set easthigh;
  by grade;
  retain start;
  if first.grade then start=_N_;
  else if last.grade then do;
    stop=_N_;
    output;
 end;
run;
 
proc print data=bygroup_range;
  title 'Using Base SAS';
run;
 

/* RANUNI generates a random number between 0 and 1.  To randomly sample from */
/* within a specified numeric range that starts at one, multiply the RANUNI   */
/* result by the maximum of the range.  For numeric ranges that do not start  */
/* with one,  multiply the RANUNI result by the range of data values you want */
/* to capture (maximum - minimum) plus one and add the first integer below    */
/* the minimum to the product.                                                */
 
data grab;
  do i = 1 to 4;
    grade = 9;
    obs2sample = ceil(ranuni(1234)*292);
    output;
  end;
  do i = 1 to 6;
    grade = 10;
    obs2sample = ceil(ranuni(4321)*120) + 292;
    output;
  end;
  do i = 1 to 10;
    grade = 11;
    obs2sample = ceil(ranuni(6789)*166) + 412;
    output;
  end;
  do i = 1 to 10;
    grade = 12;
    obs2sample = ceil(ranuni(9876)*223) + 578;
    output;
  end;
run;
 
/* GRAB contains the value for the observation number to sample from EASTHIGH */

data final;
  set grab(keep= obs2sample);
  set easthigh point=obs2sample;
  output;
run;
 
proc print data=final;
  title 'Using DATA step';
run;

*======================================================================;
/* Stratified random sample without replacement, equal allocation*/
/* Create sample data base of student grade point averages from           */
/* East High School, Grades 9 through 12, 100 or more students per grade. */

data EastHigh;
  format GPA 3.1;
  do Grade=9 to 12;
    do StudentID=1 to 100+int(201*ranuni(432098));
      GPA=2.0 + (2.1*ranuni(34280));
      output;
    end;
  end;
run;


/* Method 1: Using PROC SURVEYSELECT                          */
/*                                                            */
/* N= is the number of observations to select from            */
/* each group.  Use METHOD=SRS.  The statement STRATA defines */
/* the variable that is used for grouping.  OUT= names the    */
/* SAS data set that will be created by the procedure.        */

proc surveyselect data=EastHigh method=srs n=5 out=sample;
  strata Grade;
run;

title 'Method 1: PROC SURVEYSELECT';
proc print data=sample;
run;



/* Method 2: Using Base SAS                                                  */
/*                                                                           */
/* Generate a random number for each observation in the data set.            */
/* Sort the data set adding the new variable to the end of the BY statement. */
/* This will randomly order observations within the BY-Group. In the next    */
/* DATA step, reset a counter every time the sampling group changes. Output  */
/* the number of observations that you want in each sample group.            */

data temp; 
  set EastHigh; 
  abc=ranuni(15243); 
run; 

proc sort data=temp; 
  by grade abc; 
run;
 
data sample; 
  set temp; 
  by grade; 
  if first.grade then count=0; 
  count+1; 
  if count <=5 then output; 
  drop count abc; 
run;
 
title "Method 2: Using BASE SAS with sort on random variable ";
proc print data=sample; 
run; 



/* Method 3: Using Base SAS, no extra sort required            */
/*                                                             */
/* Since the process involves choosing a small sample from     */
/* each of the subgroups of the data to make the entire random */
/* sample, you need to do the following:                       */
/* - count the observations in each category                   */
/* - sort the observations into categories                     */
/* - combine the sorted data set with the category counts      */
/* - select the observations for the sample.                   */

/*  Use PROC FREQ to count the number of observations in each  */
/*  category.                                                  */

proc freq data=EastHigh;
  tables grade / out=bycount noprint;
run;


/* Combine the data sets by GRADE */

data sample(drop=k count);
  merge EastHigh bycount(drop=percent);
  by Grade;

  /*  Use a RETAIN statement to create a variable that will contain */
  /*  the number of observations that you want from each category.  */
  /*  Initialize the new variable to that number each time the      */
  /*  category changes by testing FIRST.GRADE.                      */

  retain k;
  if first.grade then k=5;

  /*  COUNT, the number of observations in each category, comes from */
  /*  the PROC FREQ step.  Decrease COUNT by 1 on each iteration of  */
  /*  the DATA step to reflect the number of observations remaining  */
  /*  in the group.                                                  */

  if ranuni(15243)<=k/count then do;
    output;
    k=k-1;
    end;
  count=count-1;
run;

title "Method 3: Using BASE SAS with no extra sort on random variable ";
proc print data=sample;
run;
 
*=======================================================================;
/*Stratified random sample without replacement, unequal allocation*/
/* Create sample data base of student grade point averages from           */
/* East High School, Grades 9 through 12, 100 or more students per grade. */

data EastHigh;
  format GPA 3.1;
  do Grade=9 to 12;
    do StudentID=1 to 100+int(201*ranuni(432098));
      GPA=2.0 + (2.1*ranuni(34280));
      output;
    end;
  end;
run;


/* Method 1: Using PROC SURVEYSELECT                                      */
/*                                                                        */
/* Sample 1% of Grade 9 and 5% from all other grades.  Rate is the        */
/* percentage of observations to select from each group.  Use METHOD=SRS. */
/* The statement STRATA defines the variable that is used for grouping.   */
/* OUT= names the SAS data set that will be created by the procedure.     */

proc surveyselect data=EastHigh method=srs
                  rate=(.01, .05, .05, .05) out=sample;
  strata Grade;
run;

title 'Method 1: PROC SURVEYSELECT';
proc print data=sample;
run;


/* Method 2: Using Base SAS                                      */
/*                                                               */
/* Count the number of observations per BY-Group and store the   */
/* result in variable N. Input the percentage of observations    */
/* to randomly select for each unique value of the BY-Group.     */
/* Create K to hold the actual number of observations that       */
/* will be selected from each BY-Group.                          */

data nselect;
  set EastHigh(keep=Grade);
  by Grade;
  n+1;
  if last.Grade;
  input num;
  k=ceil(n*(num/100));
  output;
  n=0;
datalines;
1
5
5
5
;


/* K contains the number of observations yet to be selected,       */
/* N contains the number of observations that remain in the group. */
/* K is decremented each time an observation is selected, N is     */
/* decremented on each iteration of the DATA step.                 */                                    

data sample2(drop= k n num);
  merge EastHigh nselect;
  by Grade;
  if ranuni(12345)<=k/n then do;
    output;
    k=k-1;
    end;
  n=n-1;
run;

title 'Method 2: Using Base SAS ';
proc print data=sample2;
run;



