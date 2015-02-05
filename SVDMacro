/*
  Name : SVDMacro.sas
  Author : Liang Xie
  Purpose : conduct SVD analysis for a given matrix, output left and right eigen vector matrix and singular values
  Date : Mar 26, 2010
*/
%macro SVD(
           input_dsn,
           output_V,
           output_S,
           output_U,     
           input_vars,
           ID_var,
     nfac=0
           );

%local blank   para  EV  USCORE  n  pos  dsid nobs nstmt
       shownote  showsource  ;

%let shownote=%sysfunc(getoption(NOTES));
%let showsource=%sysfunc(getoption(SOURCE));
options nonotes  nosource;

%let blank=%str( );
%let EV=EIGENVAL;
%let USCORE=USCORE;

%let n=%sysfunc(countW(&input_vars));

%let dsid=%sysfunc(open(&input_dsn));
%let nobs=%sysfunc(attrn(&dsid, NOBS));
%let dsid=%sysfunc(close(&dsid));
%if  &nfac eq 0 %then %do;
     %let nstmt=&blank; %let nfac=&n;
%end;     
%else %do;
     %let x=%sysfunc(notdigit(&nfac, 1)); 
  %if  &x eq 0 %then %do;
          %let nfac=%sysfunc(min(&nfac, &n));
          %let nstmt=%str(n=&nfac);
  %end;
  %else %do;
          %put ERROR: Only accept non-negative integer.;
          %goto exit;
  %end;
%end;

/* calculate U=XV/S */
%if &output_U ne %str() %then %do;
    %let outstmt=  out=&output_U.(keep=&ID_var  Prin:);
%end;
%else %do;
    %let outstmt=&blank;
%end;

%let options=noint cov noprint  &nstmt;

proc princomp data=&input_dsn  
             /* out=&input_dsn._score */
              &outstmt
              outstat=&input_dsn._stat(where=(_type_ in ("&USCORE", "&EV")))  &options;
     var &input_vars;
run;
data &output_S;
     set &input_dsn._stat;
     format Number 7.0;
     format EigenValue Proportion Cumulative 7.4;
     keep Number EigenValue  Proportion Cumulative;
     where _type_="&EV";
     array _X{&n} &input_vars;
     Total=sum(of &input_vars);
     Cumulative=0;
     do Number=1 to dim(_X);
     EigenValue=_X[number];
     Proportion=_X[Number]/Total;
     Cumulative=Cumulative+Proportion;  
     output;
  end;
run;

%if &output_V ne %str() %then %do;
proc transpose data=&input_dsn._stat(where=(_TYPE_="&USCORE")) 
               out=&output_V.(rename=(_NAME_=variable))
               name=_NAME_;
     var &input_vars;
     id _NAME_;
  format &input_vars 8.6;
run;
%end;

/* recompute Proportion */
%if &output_S ne %str() %then %do;
data &output_S;
     set &input_dsn._stat ;
  where _TYPE_="EIGENVAL";
  array _s{*} &input_vars;
  array _x{&nfac, 3} _temporary_; 
  Total=sum(of &input_vars, 0);
  _t=0;
  do _i=1 to &nfac;
     _x[_i, 1]=_s[_i]; _x[_i, 2]=_s[_i]/Total; 
  if _i=1 then _x[_i, 3]=_x[_i, 2]; 
  else _x[_i, 3]=_x[_i-1, 3]+_x[_i, 2];
  _t+sqrt(_x[_i, 2]);
  end;
  do _i=1 to &nfac;
     Number=_i;  
  EigenValue=_x[_i, 1]; Proportion=_x[_i, 2]; Cumulative=_x[_i, 3];
     S=sqrt(_x[_i, 2])/_t;  SinguVal=sqrt(_x[_i, 1] * &nobs);
  keep Number EigenValue  Proportion Cumulative  S SinguVal;
     output;
  end;
run;
%end;

%if &output_U ne %str() %then %do; 
data &output_U;
     array _S{&nfac}  _temporary_;  
     if _n_=1 then do;
        do j=1 to &nfac;
           set  &output_S(keep=SinguVal)  point=j;
           _S[j]=SinguVal; 
           if abs(_S[j]) < CONSTANT('MACEPS') then _S[j]=CONSTANT('BIG');
        end;
    end;
    set &output_U;
    array _A{*}  Prin1-Prin&nfac;
    do _j=1 to dim(_A);
        _A[_j]=_A[_j]/_S[_j];
    end;
    keep &ID_var Prin1-Prin&nfac ;
run;
%end;

%exit: 
options &shownote  &showsource;
%mend;
