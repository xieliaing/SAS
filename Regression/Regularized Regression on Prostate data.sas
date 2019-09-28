
proc import datafile="c:\data\prostate.csv"  dbms=csv  replace  
            out=prostate;
run;

%let covars=lcavol  lweight  age  lbph  svi  lcp  gleason  pgg45;
%let depvar=lpsa;
proc standard data=prostate  out=psa_std  mean=0  std=1;
     var &covars;
run;
proc standard data=psa_std  out=psa_std  mean=0;
     var &depvar;
run;

%let lambda2=%sysfunc(sqrt(1000));
%let denum=%sysfunc(sqrt(1+1000));
%let nvars=%sysfunc(countW(&covars));
%put &nvars;

data augment;
     array _v{&nvars} &covars (&nvars *0);
     retain &depvar 0;
     do id=1 to dim(_v);
        _v[id]=&lambda2;
		if id>=2 then _v[id-1]=0;
	    output;
		drop id;
	 end;
run;

proc append base=psa_std  data=augment force;
run;

data psa_std2;
     set psa_std;
	 array _x{*} &covars;
	 do j=1 to dim(_x); _x[j]=_x[j]/&denum; end;
	 drop j;
run;

ods graphics on;
ods output SelectionSummary=SelectionSummary;
proc glmselect data=psa_std2   
               plots(stepaxis=normb  unpack)=all;
     model &depvar = &covars / selection=lasso(stop=none)  
                               details=summary;
run;quit;
ods graphics off;
ods select all;


/* Ridge Regression with lambda=1*/
proc reg data=psa_std  outest=beta_reg  noprint;
     model &depvar = &covars /noint;
run;quit;
