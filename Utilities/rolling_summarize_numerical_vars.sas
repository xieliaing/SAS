data fmt;
      retain fmtname 'rollwindow'  type 'n'  hlo 'M';
      do start=1 to 10;
         end=start+5; 
        label=cats('time', start);
        output;
      end; 
      hlo='O'; label='Out-Of-Bound';
      output;
run;

data dsn;
      do time=1 to 20;
         x=rannor(0);
         y=ranuni(0);
         output;
      end;
run;

proc format cntlin=fmt; run;

proc means data=dsn noprint;
      class time /preloadfmt  mlf; 
      format time rollwindow.;
      var x y;
      output  out=summary_roll mean(x y)=  std(x y)= /autoname;
run;
    

data dsn2;
      do time=1 to 20;
         k=ranpoi(10, 10);
         do j=1 to k;
            time=time+j/(k+1);
            x=rannor(0); y=ranuni(0);
            output;
         end;
      end;
      drop k j;
run;
proc means data=dsn2  noprint;
      class time/preloadfmt mlf  exclusive;
      format time rollwindow.;
      var x y;
      output  out=summary_roll2  mean()=  std()=/autoname;
run;


/**************************************************** 
          non-rolling but shrinking time window, 
          similar for growing time window 
*****************************************************/
data fmt2;
      retain fmtname 'winx'  type 'n'  hlo 'M';
      do start=1 to 10;
         end=18; 
        label=cats('time', start);
        output;
      end; 
      hlo='O'; label='Out-Of-Bound';
      output;
run;      

proc format cntlin=fmt2  cntlout=fmt_all;
run;

proc means data=dsn2  noprint;
      class time/preloadfmt mlf  exclusive;
      format time winx.;
      var x y;
      output  out=summary_roll3  mean()=  std()=/autoname;
run;

**************** An example **************;
data TradeDate;
input TradeDate yymmdd10.;
format TradeDate yymmdd10.;
cards;
2007-01-04
2007-01-05
2007-01-08
2007-01-09
2007-01-10
2007-01-11
2007-01-12
2007-01-15
2007-01-16
2007-01-17
2007-01-18
2007-01-19
2007-01-22
2007-01-23
2007-01-24
2007-01-25
2007-01-26
2007-01-29
2007-01-30
2007-01-31
 ;
run;
data raw;
input id $ Date_S yymmdd10. +1 Date_e yymmdd10. Buy;
format Date_S Date_E yymmdd10.;
cards;
A001 2007-01-09 2007-01-24 24.5
A001 2007-01-12 2007-01-16 56.6
 ;
run;

/*------------- Desired Output ------------*
id      Date_S       Date_E        Buy    Hold_Days
A001 2007-01-09 2007-01-24 24.5    12
A001 2007-01-12 2007-01-30 56.6     3
-------------------------------------------*/
data fmt;
     set raw;
     retain fmtname 'tdate' type 'n'  hlo 'M';
     start=Date_S; end=Date_e;
     label=cats(ID, _n_);
run;
proc format cntlin=fmt  out=fmt_ref;
run;

proc means data=tradeDate noprint  nway;
     class TradeDate/mlf exclusive preloadfmt ;
     format TradeDate tdate.;
     var TradeDate;
     output  out=_test  n()=_freq_;
run;