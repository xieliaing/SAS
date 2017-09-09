%macro PLSRotate(Loading, TransMat, PatternOut, PatternShort, 
                 method=VARIMAX, threshold=0.25);
/* VARIMAX rotation of PLS loadings, based on the paper of:
   [1] Huiwen Wang; Qiang Liu , Yongping Tu, "Interpretation 
   of PLS Regression Models with VARIMAX Rotation", Computational 
   Statistics and Data Analysis, Vol.48 (2005) pp207 – 219

   Only variables having large loadings after rotation will 
   enter the final model. 

   Loading dataset contains XLoadings output from PROC PLS 
   and should have variable called NumberOfFactors
   TransMat is the generated Transformation matrix;
   PatternOut is the output Pattern after rotation;
   PatternShort is the output Pattern with selected variables
*/

%local covars;
proc sql noprint;
     select name into :covars separated by ' '
  from   sashelp.vcolumn
  where  libname="WORK" & memname=upcase("&Loading") 
        &   upcase(name) NE "NUMBEROFFACTORS"
  &   type="num"
  ;
quit;
%put &covars;

data &Loading.(type=factor);
         set &Loading;
         _TYPE_='PATTERN';
         _NAME_=compress('factor'||_n_);
run;
ods select none;
ods output OrthRotFactPat=&PatternOut;
ods output OrthTrans=&TransMat; 
proc factor  data=&Loading   method=pattern  rotate=&method  simple; 
         var &covars;
run;
ods select all;

data &PatternShort;
     set &PatternOut;
  array _f{*} factor:;
  _cntfac=0;
  do _j=1 to dim(_f);  
        _f[_j]=_f[_j]*(abs(_f[_j])>&threshold); _cntfac+(_f[_j]>0); 
     end;
  if _cntfac>0 then output;
  drop _cntfac _j;
run;
%mend;


/* Examples */