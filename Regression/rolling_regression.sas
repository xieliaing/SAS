proc datasets library=work kill; run;


options fullstimer;
data test;
     do seq=1 to 500000;
          x1=rannor(9347957);
          *x2=rannor(876769)+0.1*x1;
          epsilon=rannor(938647)*0.5;
          y = 1.5 + 0.5*x1 +epsilon;
          output;
     end;
run;

/* Method 0.*/
sasfile test load;
data res0;
        set test;
  array _x{3,3} _temporary_ ;
  array _a{3,3} _temporary_ ;
  array _tempval{5, 20} _temporary_ ;
  m=mod(_n_-1, 20)+1;
  _tempval[1, m]=x1; 
  _tempval[2, m]=y;
  _tempval[3, m]=x1**2;
  _tempval[4, m]=x1*y;
  _tempval[5, m]=y**2;

  link filler;
  if _n_>=20 then do;
        if _n_>20 then do; 
                   m2=mod(_n_-20, 20)+1;
       _x[1,2]+(-_tempval[1, m2]);
       _x[1,3]+(-_tempval[2, m2]);
       _x[2,2]+(-_tempval[3, m2]);
       _x[2,3]+(-_tempval[4, m2]);
       _x[3,3]+(-_tempval[5, m2]);
     end;
        do i=1 to dim(_a, 1);
         do j=1 to dim(_a, 2);
          _a[i, j]=_x[i, j];
      end;
     end;
            
              do k=1 to dim(_a, 1)-1;
         link adjust;
              end;
     Intercept=_a[1,3]; beta=_a[2,3];
     keep seq   intercept  beta;
     output;
  end;

  return;
filler:
   _x[1,1]=20; _x[1,2]+x1; _x[1,3]+y;
   _x[2,2]+_tempval[3,m];  _x[2,3]+_tempval[4,m]; _x[3,3]+_tempval[5,m];
   _x[2,1]=_x[1,2]; _x[3,1]=_x[1,3]; _x[3,2]=_x[2,3]; 
return;

adjust:
    B=_a[k, k];
 do j=1 to dim(_a, 2);
     _a[k, j]=_a[k, j]/B;
 end;
 do i=1 to dim(_a, 1);
     if i ^=k then do;
          B=_a[i, k];
    do j=1 to dim(_a, 2);
        _a[i, j]=_a[i, j]-B*_a[k, j];
    end;
  end;
 end;
return;

run;
sasfile test close;



/* Method 1.*/

sasfile test load;
data rest0;
        set test;
  array _x{4} _temporary_;
  array _a{2,20}  _temporary_;
  m=mod(_n_-1, 20)+1;
  _a[1, m]=x1; _a[2,m]=y;
  link filler;

  m2=mod(_n_-20, 20)+1;
  if _n_>=20 then do;
    if _n_>20 then do;
              link deduct;
    end;
    beta=(_x[2]-_x[1]*_x[4]/20)/(_x[3]-_x[1]**2/20);
    intercept=_x[4]/20 - beta*_x[1]/20;
    keep  seq   intercept  beta  ;
    output;
  end;
  return;       
filler:
     _x[1]+x1;
  _x[2]+x1*y;
  _x[3]+x1**2;
  _x[4]+y;
return;
deduct:
     _x[1]=_x[1]-_a[1,m2]; 
  _x[2]=_x[2]-_a[1,m2]*_a[2,m2];
  _x[3]=_x[3]-_a[1,m2]**2;
  _x[4]=_x[4]-_a[2,m2];
return;
run;
sasfile test close;



/* Method 2.*/

%macro wrap;
%let window=20;
%let diff=%eval(&window-0);
data testv/view=testv;
     set test;
       xy=x1*y;  
run;

proc expand data=testv  method=none  out=summary(keep=seq sumxy  sumx1  sumy  ussx1  may  max);
       convert  x1=sumx1/transformout=(movsum &diff);
       convert  xy=sumxy/transformout=(movsum &diff);
       convert  x1=ussx1/transformout=(movuss &diff);
       convert  y =sumy /transformout=(movsum &diff);
       convert  y =may / transformout=(movave &diff);
       convert  x1 =max / transformout=(movave &diff);  
run;

data result1;
     set summary(firstobs=&window);
       beta = (sumxy - sumx1*sumy/&window)/(ussx1 - sumx1/&window.*sumx1);  
       alpha= may - beta*max;
       keep seq  beta  alpha; 
run;
%mend;

%let t0=%sysfunc(datetime(), datetime24.);
*options nosource nonotes;
%wrap;
options source notes;
%let t1=%sysfunc(datetime(), datetime24.);
%put Start @ &t0;
%put End   @ &t1;

 

/* Method 3.*/
%let t0=%sysfunc(datetime(), datetime.);
 
data test2v/view=test2v;
       set test;
       array _x{2, 20} _temporary_ (20*0 20*0);
       k=mod(_n_-1, 20)+1;
       _x[1, k]=x1; _x[2, k]=y;
       if _n_>=20 then do;
          do j=1 to dim(_x, 2);
               x=_x[1, j]; y=_x[2, j];
               output;
               keep seq x y;
            end;
       end;
run;

ods select none;
proc  reg data=test2v  outest=res2(keep=seq x intercept);
         by seq;
         model y = x;
run;quit;
ods select all;

%let t1=%sysfunc(datetime(), datetime.);
%put Start @ &t0;
%put End   @ &t1;


/* Method 4. */
%macro wrap;
options nonotes;
ods select none;
%do i=20 %to 500000;
       %let fo=%eval(&i-19);
       proc reg data=test(firstobs=&fo  obs=&i)  outest=_xres(keep=x1 intercept);
           model y =x1;
       run;quit;
      %if %eval(&i=20) %then %do;
          data res3; set _xres; run;
      %end;
      %else %do;
        proc append base=res3  data=_xres; run;
      %end;
%end;

ods select all;
data res3;
       set res3;
       time=19+_n_;
run;
options notes;
%mend;

%let t0=%sysfunc(datetime(), datetime.);
%wrap;
%let t1=%sysfunc(datetime(), datetime.);
%put Start @ &t0;
%put End   @ &t1;