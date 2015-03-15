
%macro fleishman(skew, kurt, maxiter=50);
/*
  Calculate Fleishman Coefficients for given Skewness and Kurtosis parameter
*/
%let converge=0.0000001;
%let X1=1; %let X2=0; %let X3=0;
proc optsave  out=_old_opt_; run;
options nomlogic nomprint nonotes;
data _coeff;
     X1=&X1; X2=&X2; X3=&X3; output;
  	 stop;
run;     
data _coeff_hist; set _coeff; run;

%do iter=1 %to &maxiter;
  data _main;
       array _F{3}  F1-F3;
  	   array _X{3}  X1-X3;
  	   retain F1-F3 X1-X3;
  	   keep F  M1-M3;
  	   if _n_=1 then do;
          set _coeff;
  	   end;
       Skewness=&Skew; Kurtosis=&Kurt;
       F=(X1**2+6*X1*X3+2*X2**2+15*X3**2-1); 
       _F[1]=abs(F);
       M1=(2*X1+6*X3); M2=(4*X2); M3=(6*X1+30*X3);
  	   output;
       F=(2*X2*(X1**2+24*X1*X3+105*X3**2+2)-SKEWNESS);  
       _F[2]=abs(F);
       M1=(4*X2*(X1+12*X3)); M2=(2*(X1**2+24*X1*X3+105*X3**2+2));
       M3=(4*X2*(12*X1+105*X3));
  	   output;
       F=(24*(X1*X3+X2**2*(1+X1**2+28*X1*X3)+X3**2*(12+48*X1*X3+141*X2**2+225*X3**2))
           -KURTOSIS);                
  	   _F[3]=abs(F);
       M1=(24*(X3+X2**2*(2*X1+28*X3)+48*X3**3));
       M2=(48*X2*(1+X1**2+28*X1*X3+141*X3**2));
       M3=(24*(X1+28*X1*X2**2+2*X3*(12+48*X1*X3+141*X2**2+225*X3**2)
           +X3**2*(48*X1+450*X3)));
  	   output;
  	   m=max(of F1-F3); put m= 7.4;
  	   if m<&converge then call symput('iter', &maxiter+1);
  run;
  %if &iter<=&maxiter %then %do;
    proc reg data=_main outest=_delta  noprint;    
         model F=M1 M2 M3/noint;
    run; quit;
    data _coeff;
         set _coeff  end=eof;     
      	 array _X{3} X1-X3;
    	   array _M{3} M1-M3;
    	   if eof then do;
    	     set _delta;
    	   end;
    	   retain M1-M3;
    	   do i=1 to 3;
    	      _X[i]=_X[i]-_M[i];
    	   end;
    	   keep X1-X3;
    run;
    data _null_; 
         set _coeff;
      	 call symput('X1', X1); call symput('X2', X2); call symput('X3', X3);
    	   stop;
    run;
    proc append data=_coeff base=_coeff_hist; run;
  %end;
%end;
proc optload data=_old_opt_; run;
%mend;

/* demo 

%fleishman(-1, 2.5);

*/
