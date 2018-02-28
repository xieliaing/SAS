/* Demonstrate kernel PCA. Kernel method can be applied to any algorithm that relies on inner product */
data original;
     do ID=-314 to 314;
        x1=sin(ID/100)*1+rannor(8976565)*0.1; x2=cos(ID/100)*1+rannor(92654782)*0.1; Class=1; output;
        x1=sin(ID/100)*2+rannor(8976565)*0.1; x2=cos(ID/100)*2+rannor(92654782)*0.1; Class=2; output;
        x1=sin(ID/100)*3+rannor(8976565)*0.1; x2=cos(ID/100)*3+rannor(92654782)*0.1; Class=3; output;
     end;
run;

/*------------ Linear PCA -----------*/
proc princomp data=original  standard noint cov 
              outstat=lin_stat(where=(_TYPE_ = 'USCORE'))
              noprint;
        var x1 x2;
run;


data lin_stat_score;
     set lin_stat;
     _TYPE_= 'PARMS';
run;

proc score data=original   score=lin_stat_score  type=parms   out=lin_pca;
     var x1 x2;
run;

/*
proc gplot data=lin_pca;
        plot Prin1*Prin2=Class;
run;quit;
*/

/*--------- Kernel PCA 1.0 ------------*/
/* Inner product based kernel PCA */
proc transpose data=original(drop=ID  Class) out=original_t; run;

proc corr data=original_t  outp=inner(where=(_TYPE_='SSCP' & substr(_NAME_, 1, 3)='COL')  drop=intercept)  sscp noprint;
     var col:;
run;

/* apply kernel functon to inner product matrix. Here we use (X'Y+1)^2, the one used in wikipedia.org example. */
data inner;
     set inner;
     array _C{*} _numeric_;
     _TYPE_='COV';
     do j=1 to dim(_C); _C[j]=(_C[j]+1)**2; end;
     drop j;
run;

proc princomp data=inner    noint  cov  standard
              outstat=k_stat(where=(_TYPE_ in ('EIGENVAL', 'USCORE')) )
              noprint;
     var col:;
run;

data S(drop=_NAME_)  score;
     set k_stat;
     if _TYPE_='EIGENVAL' then output S; else output score;
     drop _TYPE_;
run;

data _null_;
     set S;
     array _S{*} _numeric_;
     do j=1 to dim(_S);
        if _S[j]<0.5*constant('MACEPS') then do;
          call symput('maxsingval', j-1); stop;
        end;
     end;
run;
%put &maxsingval;

data score; set score; if _n_>&maxsingval then delete; retain _TYPE_ 'PARMS';  run;

proc score data=inner   score=score  type=parms  out=k_U(keep=Prin:);
        var col:;
run;

data k_U;
     set original(keep=ID Class  x1 x2)  ;
     set k_U;
run;

/*
proc gplot data=k_U;
     plot Prin1*Prin2=Class;
run;quit;
*/

/*--------- Kernel PCA 2.0------------*/
/* Frobenius norm based kernel PCA */
proc transpose data=original(drop=ID  Class) out=original_t; run;

proc corr data=original_t  outp=inner2(where=(_TYPE_='SSCP' & substr(_NAME_, 1, 3)='COL')  drop=intercept)  sscp noprint;
     var col:;
run;

proc means data=original_t noprint ;
         var col:;
         output out=_uss2(drop=_TYPE_    _FREQ_)    uss= /autoname;
run;

proc transpose data=_uss2  out=_uss2t; run;

proc sql noprint;
          select nobs into :NCOLS from sashelp.vtable
          where libname='WORK'
              and memtype='DATA'
              and memname='_USS2T'
              ;
quit;
%let ncols=%sysfunc(compress(&NCOLS));
%put &ncols;

/* apply kernel functon to inner product matrix. Here we use Gaussian kernel function. */
%let sigma=1;
data inner2;
          array _X{&ncols} COL1-COL&ncols;
          array _USS0{&ncols}  _temporary_;
          if _n_=1 then do;
             do j=1 to &ncols;
                  set  _uss2t(keep=COL1)  point=j;
                  _USS0[j]=COL1;
             end;
          end;
          set inner2;
          _TYPE_='COV';
          do j=1 to &ncols;
              _X[j]=_USS0[_n_] + _USS0[j] - 2*_X[j];
              _X[j]=exp(-0.5*_X[j]/&sigma);
          end;
          keep _TYPE_  _NAME_ COL1-COL&ncols;
run;


proc princomp data=inner2    noint  cov  standard
              outstat=k_stat2(where=(_TYPE_ in ('EIGENVAL', 'USCORE')) )
              noprint;
     var col:;
run;

data S2(drop=_NAME_)  score2;
     set k_stat2;
     if _TYPE_='EIGENVAL' then output S2; else output score2;
     drop _TYPE_;
run;

data _null_;
     set S2;
     array _S{*} _numeric_;
     do j=1 to dim(_S);
        if _S[j]<0.5*constant('MACEPS') then do;
          call symput('maxsingval', j-1); stop;
        end;
     end;
run;
%put &maxsingval;

data score2; set score2; if _n_>&maxsingval then delete; retain _TYPE_ 'PARMS';  run;

proc score data=inner2   score=score2  type=parms  out=k_U2(keep=Prin:);
        var col:;
run;

data k_U2;
     set original(keep=ID Class  x1 x2)  ;
     set k_U2;
run;

/*@ Draw figure using R @*/

%macro RScript(Rscript);
data _null_;
     file "&Rscript";
     infile cards ;
     input;
     put _infile_;
%mend;

 
%macro CallR(Rscript, Rlog);
systask command "D:\Progra~1\Analyt~1\R\bin\R.exe CMD BATCH --vanilla --quiet  
                          &Rscript  &Rlog"   taskname=rjob1  wait  status=rjobstatus1;
%mend;

options nosource;
proc export data=original   outfile="c:\o.csv"  dbms=csv; run;
proc export data=lin_PCA    outfile="c:\a.csv"  dbms=csv; run;
proc export data=k_U       outfile="c:\b.csv"  dbms=csv; run;
proc export data=k_U2     outfile="c:\c.csv"   dbms=csv; run;
options source;

%RScript(c:\rscript.r)
cards;
odsn <- read.csv('c:/o.csv', header=T)
adsn <- read.csv('c:/a.csv', header=T)
bdsn <- read.csv('c:/b.csv', header=T)
cdsn <- read.csv('c:/c.csv', header=T)
png(file='c:/figure.png')
par(mfcol=c(2,2), mar=c(4,4,3,2))
plot(x1~x2, data=odsn, col=Class, main='Original')
plot(Prin1~Prin2, data=adsn, col=Class, main='Linear PCA')
plot(Prin1~Prin2, data=bdsn, col=Class, main='Polynomial Kernel')
plot(Prin1~Prin2, data=cdsn, col=Class, main='Gauss Kernal')
dev.off()
;
run;


%CallR(c:\rscript.r, c:\rlog1.txt);
