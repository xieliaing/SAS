%macro AUC( dsn, Target, score);
ods select none;
ods output WilcoxonScores=WilcoxonScore;
proc npar1way wilcoxon data=&dsn ;
     where &Target^=.;
     class &Target;
     var  &score; 
run;
ods select all;

data AUC;
    set WilcoxonScore end=eof;
    retain v1 v2 1;
    if _n_=1 then v1=abs(ExpectedSum - SumOfScores);
    v2=N*v2;
    if eof then do;
       d=v1/v2;
       Gini=d * 2;    AUC=d+0.5;    
       put AUC=  GINI=;
       keep AUC Gini;
     output;
   end;
run;
%mend;

data test;
  do i = 1 to 10000;
     x = ranuni(1);
     y=(x + rannor(2315)*0.2 > 0.35 ) ; 
     output;
  end;
run;

ods select none;
ods output Association=Asso;
proc logistic data = test desc;
    model y = x;
    score data = test out = predicted ; 
run;
ods select all;

data _null_;
     set Asso;
     if Label2='c' then put 'c-stat=' nValue2;
run;
%AUC( predicted, y, p_0);
