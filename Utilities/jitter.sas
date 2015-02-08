%macro jitter(var, newname, data=last, factor=1, amount=);
/* follow the JITTER function in R, no fuzzy applied yet . 
   if amount is given, then use this value for disturbance
   else if amount is 0, then use the range of &var for disturbance
   else if amount is NULL, then use the smallest difference among
        distinct values of &var for disturbance. if all values are 
        the same, then use the value obtained from range

   if range of &var is 0, then use the lower range for disturbance
   otherwise 
*/
%let blank=%str( );
%if &data=' ' %then %let data=last;
%local fid;

data _null_;
     fid=round(ranuni(0)*10000, 1);
  call symput('fid', compress(fid));
run;
proc means data=&data  noprint;
     var &var;
  output  out=_util&fid  
          range(&var)=range  
          min(&var)=min  
          max(&var)=max;
run;
data _util&fid;
     set _util&fid;
  if range^=0 then z=range; else z=min;
  if z=0 then z=1;
run;

%if %eval(&amount=&blank) %then %do;
    proc sort data=&data.(keep=&var  where=(&var^=.))   
              out=_xuti&fid  nodupkey;
       by &var;
 run;
 data _duti&fid;
      set _xuti&fid  nobs=ntotal  end=eof;      
   array _x{2} _temporary_;
   if ntotal=1 then do;
      amount=&factor/50*abs(&var);
   keep amount;
   output;
   stop;
   end;
   else do;
      if _n_=1 then do; 
         _x[1]=&var; _x[2]=constant('BIG'); 
      end;
   else do;
               _x[2]=min(_x[2], &var - _x[1]);
      _x[1]=&var;
      if eof then do;
         amount=&factor/5*abs(_x[2]);
      keep amount;
      output;
      end;
   end;
   end;
  run;
    
%end;
%else %if %eval(&amount=0) %then %do;
    data _duti&fid;
      set _util&fid;
   amount=&factor*z/50;
   keep amount;
   output;
 run;     
%end;
%else %do;
    data _duti&fid;
      amount=&amount;
   keep amount;
   output;
 run;
%end;

proc sql noprint;
     select name into :keepvars separated by ' '
  from   sashelp.vcolumn
  where  libname='WORK' 
    and  memname=%upcase("&data") 
    and  memtype="DATA"
  ;
quit;
data &data;     
  array _x{1} _temporary_;
     if _n_=1 then do;
     set _duti&fid;
  _x[1]=amount;
  end;
     set &data;
  &newname=&var + ranuni(0)*(2*_x[1])-_x[1];
  label &newname="jitter(&var)";
  keep &keepvars  &newname;
run;
proc datasets library=work nolist;
     delete _duti&fid _xuti&fid _util&fid;
run;quit;
%mend; 
