
/* Below is the way to obtain trace(S), where S is the project matrix in a (regularized) linar regression. 
   For further information, check pp.68, pp.153 of Elements of Statistical Learning,2nd Ed.
*/

/* For details about TYPE=SSCP special SAS data, consult:
  Appendix A: Special SAS Data Sets, pp.8159 of SAS/STAT 9.22 User's Guide
*/

%macro Trace(dsn_in, dsn_out, depvar, covars, lambda=1)
%local k;
%let k = %sysfunc(countW(&covars));
%let k = %eval(&k+1);

proc corr data=&dsn_in sscp out=__xtx(where=(_TYPE_='SSCP'))  noprint;
     var &covars;
run;

data __xtx2;
     set __xtx;
     array _n{*} _numeric_;
     array _i{*} i1-i&k (&k*0);
     do j=1 to 5;
        if j=_n_ then _i[_n_]=&lambda
        else _i[j]=0;
     end;
     _n[_n_]=_n[_n_]+1;
     drop j _TYPE_  _NAME_;
run;

/* Obtain the inverse of (XTX+\lambda*I)
   Note that we explicitly specified Intercept term in the 
   covariate list and fit a model without implicit intercept 
   term in the model.
*/
proc reg data=__xtx2  
         outest=__S0(type=SSCP
                   drop=i1-i5 _MODEL_  _DEPVAR_  _RMSE_)
         singular=1E-17;
     model i1-i&k = Intercept &covars / noint   noprint;
run;quit;

data __S0;
     set __S0; 
     length _NAME_ $8;
     _NAME_=cats('X_', _n_);
run;

proc score data=&dsn_in  score=__S0  out=__XS0(keep=X_:)  type=parms;
     var &covars;
run;     

data __XS0X;
     merge __XS0  __xtx2;
     array _x1{*} X_:;
     array _x0{*} intercept pop school employ services;
     do i=1 to dim(_X1);
        _x1[i]=_x1[i]*_x0[i];
     end;
     rowSum=sum(of _x1[*]);
     keep rowSum;
run;
proc means data=__XS0X  noprint;
     var rowSum;
     output out=&dsn_out  sum(rowSum)=Trace;
run;
%mend Trace;
