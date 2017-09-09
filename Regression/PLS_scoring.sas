%macro PLSSCORE(dsn, XCenScale, XWeights, outdsn, prefix=_xscr_);
/*
In some predictive modelling projects, we may have variables that most of the observations have the same value, while the small percentage rest ones are populated with meaningful values. For example, 90% observations have values=0 but the rest 10% have value=1, 2, 3.....for a variable called WebHits, etc. We may have a large number of such variables, say web hits at different pages, and due to the small percentage of differently valued observations, each variable show minimal predictive power.

But sometimes we have a large number of such variables, and a quick way to figure out whether they collectively show up predictive power, we may use Partial Least Square method.

*/
%if &prefix eq %str(' ') %then %let prefix=_xscr_;
proc sql noprint;
     select variable into :covars separated by ' '
  from   &XCenScale;
quit;
proc transpose data=&XCenScale out=&XCenScale._t;
        id variable;
run;
data &XCenScale._t;
   if _n_=1 then _TYPE_='MEAN';
   else _TYPE_='STD';
   set &XCenSCale._t;
run;
data &XWeights;
       length _TYPE_ $ 5;
       _TYPE_='PARMS';
       set &Xweights;
      _NAME_=compress("&prefix"||_n_);
run;
data Xscore/view=Xscore;
     length _TYPE_ $ 5;
     set &XCenScale._t &XWeights;
run;
proc score data=&dsn  score=Xscore type=PARMS out=&outdsn;
      var &covars;
run;
%mend;