/*
  SAS macro:
     Logistic Regression using Stochastic Gradient Descent.    
  Name: 
     %ls_sgd();
  Copyright (c) 2009, Liang Xie (Contact me @ xie1978 at gmail dot com)
  
  
  The SAS macro is a demonstration of an implementation of logistic 
  regression modelstrained by Stochastic Gradient Decent (SGD).This 
  program reads a training set specified as &amp;dsn_in, trains a logistic 
  regression model, and outputs the estimated coefficients to &amp;outest. 
  Example usage:

  %let inputdata=train_data;
  %let beta=coefficient;
  %let response=Event;
  %lr_sgd(&amp;inputdata, &amp;beta, &amp;response, &amp;covars, 
          alpha=0.008, decay=0.8, 
          criterion=0.00001, maxiter=1000);


  The following topics are not covered for simplicity:    
      - bias term    
      - regularization    
      - multiclass logistic regression (maximum entropy model)         
      - calibration of learning rate

  <i>Distributed under GNU Affero General Public License version 3. This 
  program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, only version 3 of the
  License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU Affero General Public License for more details. 

</i>*/

%macro logistic(dsn_in, outest, response, alpha=0.0005);
proc score data=&amp;dsn_in  score=&amp;outest  type=parms  out=score(keep=score);
     var intercept &amp;covars;
run;

data _xtemp/view=_xtemp;
     merge &amp;dsn score ;
     _w=&amp;response - 1/(1+exp(-score));
  /*
  array x{*} intercept &amp;covars;
  _w=&amp;response - 1/(1+exp(-score));
  do i=1 to dim(x); x[i]=x[i]*_w; end;    
  */
run;

data _x&amp;outest;
  array x{*} intercept &amp;covars;
  array _x{*} b_intercept &amp;covars2;
  retain b_intercept &amp;covars2;
  retain logneg logpos 0;
  modify _x&amp;outest;
  do i=1 to dim(x); x[i]=_x[i]; end;
  
  do until (eof);
        set _xtemp  end=eof;
     do  i=1 to dim(x);
         _x[i]=_x[i]+&amp;alpha*x[i]*_w;
     end;
  end;
     replace;
run;

%mend;

%macro compare(dsn1, dsn2);
data _null_;
     merge &amp;dsn1  &amp;dsn2;
  array _x1{*} intercept &amp;covars;
  array _x2{*} b_intercept &amp;covars2;
  retain maxdiff 0;
  do i=1 to dim(_x1);      
     maxdiff=max(maxdiff, abs(_x1[i]-_x2[i]));  
  *put _x1[*]=;
  *put _x2[*]=;
  end;
  call symput('maxdiff', maxdiff);
run;
%mend;




%macro lr_sgd(dsn, outest, response, covars, 
              alpha=0.0005, decay=0.9, 
              criterion=0.00001, maxiter=1000);
options nosource nonotes;
options nomlogic nomprint;
%local i t0 t1 dt maxdiff status  stopiter;

%let t00=%sysfunc(datetime());

data &amp;dsn;
     set &amp;dsn;
  intercept=1; _w=1;
run;

data &amp;outest;
     retain _TYPE_ "PARMS"  _NAME_ "SCORE";
     array x{*} intercept &amp;covars;
  do i=1 to dim(x); 
     x[i]=0;
  end;
  drop i;
  output;
run;

data _x&amp;outest;
     retain _TYPE_ "PARMS"  _NAME_ "SCORE";
     array bx{*} b_intercept &amp;covars2;
  array x{*}  intercept &amp;covars;
  set &amp;outest;
  do j=1 to dim(x); bx[j]=x[j]; end;
  keep b_intercept &amp;covars2 _TYPE_  _NAME_;
  drop j;
run;

sasfile _x&amp;outest load;
%let stopiter=&amp;maxiter;
%let status=Not Converged.;
%do i=1 %to &amp;maxiter;
    %let t0=%sysfunc(datetime());

    %logistic(&amp;dsn, &amp;outest, &amp;response, alpha=&amp;alpha);
    %compare(&amp;outest, _x&amp;outest);    
    data &amp;outest;
         retain _TYPE_ "PARMS"  _NAME_ "SCORE";
         array bx{*} b_intercept &amp;covars2;
      array x{*}  intercept &amp;covars;
      set _x&amp;outest;
      do j=1 to dim(x); x[j]=bx[j]; end;
      keep intercept &amp;covars _TYPE_  _NAME_;
      drop j;
    run;
 %let alpha=%sysevalf(&amp;alpha * &amp;decay);
 %let alpha=%sysfunc(max(0.00005, &amp;alpha));
    %let t1=%sysfunc(datetime());
    %let dt=%sysfunc(round(&amp;t1-&amp;t0, 0.001));
    %put Iteration &amp;i, time used &amp;dt, converge criteria is &amp;maxdiff; 
 %if %sysevalf(&amp;maxdiff&lt;&amp;criterion) %then %do;
     %let stopiter=&amp;i;
        %let i=%eval(&amp;maxiter+1);
  %let status=Converged.;
 %end;
%end;
sasfile _x&amp;outest close;
%let t11=%sysfunc(datetime());
%let dt=%sysfunc(round(&amp;t11-&amp;t00, 0.01));
%put Total Time is &amp;dt sec.;
%put Total Iteration is &amp;stopiter, convergence status is &amp;status;
%put At Final Iteration, max difference is &amp;maxdiff;
options mlogic mprint notes source;
%mend;
