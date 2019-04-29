
/*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*/
/*@                    EXAMPLES                  @*/
/*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*/

/************ Example 1***************/

options fullstimer; 
/* The data is from SAS Knowledge Base Sample 25030 %GLIMMIX 
   macro to fit the generalized mixed model 
*/
data work.ship;
   length type $1. year $7. period $8.;
   input type year period service y;
   datalines;
B  1965-69  1975-79   9.9218  53
C  1965-69  1975-79   6.5162   1
D  1965-69  1975-79   5.2575   0
E  1965-69  1975-79   6.0799   7
A  1965-69  1975-79   6.9985   4
A  1965-69  1960-74   6.9985   3
B  1965-69  1960-74  10.2615  58
C  1965-69  1960-74   6.6606   0
D  1965-69  1960-74   5.6630   0
E  1965-69  1960-74   6.6708   7
A  1970-64  1960-74   7.3212   6
B  1970-64  1960-74   8.8628  12
C  1970-64  1960-74   6.6631   6
D  1970-64  1960-74   5.8551   2
E  1970-64  1960-74   7.0536   5
A  1970-64  1975-79   8.1176  18
B  1970-64  1975-79   9.4803  44
C  1970-64  1975-79   7.5746   2
D  1970-64  1975-79   7.0967  11
E  1970-64  1975-79   7.6783  12
A  1975-69  1975-79   7.7160  11
B  1975-69  1975-79   8.8702  18
C  1975-69  1975-79   5.6131   1
D  1975-69  1975-79   7.6261   4
E  1975-69  1975-79   6.2953   1
A  1960-64  1960-74   4.8442   0
B  1960-64  1960-74  10.7118  39
C  1960-64  1960-74   7.0724   1
D  1960-64  1960-74   5.5255   0
E  1960-64  1960-74   3.8067   0
A  1960-64  1975-79   4.1431   0
B  1960-64  1975-79   9.7513  29
C  1960-64  1975-79   6.3135   1
D  1960-64  1975-79   4.6540   0
;
run;


/*---
  poisson regression (log-linear model) with random effects,
  parameterization for TYPE matches McCullagh and Nelder's
---*/
ods listing;

proc sort data=work.ship;
      by descending type;
run;
title "Example 1. Ship data from SAS KB sample 25030";
title2 "Using HPGLIMMIX macro";
%hpglimmix(data=work.ship,
   procopt=order=internal,
   stmts=%str(
      class type year period;
      model y = type / solution;
      random year|period;
      estimate 'E vs. Others' type -1 -1 -1 -1 4/ divisor=4 cl;
   ),
   error=poisson,
   link=log,
   offset=service
)
run;

title2 "Using PROC GLIMMIX"; 
proc glimmix data=work.ship order=data;
      class type year period;
      model y = type / solution  d=poisson link=log  
                       offset=service  ddfm=residual;
      random year|period;
      estimate 'E vs. Others' type 4 -1 -1 -1 -1 
                              / divisor=4 cl;
	  random _residual_;
run; 
title;
title2;


options notes source fullstimer;


/* GENERATE A THREE-LEVEL HIERARCHY GLMM (WITH BINOMIAL CONDITIONALS),
    NORMAL RANDOM TERMS */
data work.plant;
CALL STREAMINIT(9873123);
do sim = 1 to 1;
	inter = -2;
	n = 50;
	do county = 1 to 62;
		varc = 0.65;
		uc = rand('normal')*sqrt(varc);
		do field = 1 to 10;
			varf = .50;
			uf = rand('normal')*sqrt(varf);
			do site = 1 to 20;
				vars = .07;
				us = rand('normal')*sqrt(vars);				
				eta = inter + uc + uf + us;
				p = (1-exp(-exp(eta)));
				y = rand('binomial',p,n);
				output;
				end;
			end;
		end;
	end;

run;

title 'PQL ("Penalized Quasi-Likelihood"), 3-level hierarchy, scale=1';
title2 'int=-2, vars: 0.65, .50, .07 [n=50], 16 counties';
/* As described in GLIMMIX manual, with discrete distributions, 
   the residual scale is automatically 1. */
proc glimmix data=work.plant ;
	class county field site;
	model y/n =  / dist=binomial link=cll  s ;
	random int field site(field)/subject=county;
	nloptions maxiter=100 tech=QUANEW;
	ods output CovParms=Cov_glimmix;
run;

/* 	Now use new %HPGLIMMIX, also with one more level of 
    variation (residual), HELD at 1.   */
ods select ParmSearch CovParms ParameterEstimates FitStatistics;
title3 'using new %hpglmmix, with fixed residual=1';
%hpglimmix(data =work.Plant, 
		stmts=%str(
		    class county field site;
		    model y/n =  /  s ;
		    random int field site(field) /subject=county;
		    parms (.6) (.5) (.1) (1) / hold=4; *<-- fix "residual" at 1;			
        ),
	error=binomial, maxit=50, 
	tech=QUANEW,
	link=cloglog
); 

run;

ods trace off;

/* Example 3 */
options fullstimer; 

/* Micro array example */
%let narray = 6;
%let ndye = 2;
%let nrow = 4;
%let ngene = 500;
%let ntrt = 6;
%let npin = 4;
%let ndip = 4;
%let no = %eval(&ndye*&nrow*&ngene);
%let tno = %eval(&narray*&no);
%let seed=912345;
%let scale=0.2;

data  tmp.microarrayG;
      keep Gene MArray Dye Trt Pin Dip log2i response logresponse ;
      array PinDist{&tno};
      array DipDist{&tno};
      array GeneDist{&tno};
      array ArrayEffect{&narray};
      array ArrayGeneEffect{%eval(&narray*&ngene)};
      array ArrayDipEffect{%eval(&narray*&ndip)};
      array ArrayPinEffect{%eval(&narray*&npin)};
	  call streaminit(&seed);

      do i = 1 to &tno;
         PinDist{i} = 1 + int(&npin*ranuni(12345));
         DipDist{i} = 1 + int(&ndip*ranuni(12345));
         GeneDist{i} = 1 + int(&ngene*ranuni(12345));
      end;
      igene = 0;
      idip = 0;
      ipin = 0;
      do i = 1 to &narray;
         ArrayEffect{i} = sqrt(0.014)*rannor(12345);
         do j = 1 to &ngene;
            igene = igene+1;
            ArrayGeneEffect{igene} = sqrt(0.0017)*rannor(12345);
         end;
         do j = 1 to &ndip;
            idip = idip + 1;
            ArrayDipEffect{idip} = sqrt(0.0033)*rannor(12345);
         end;
         do j = 1 to &npin;
            ipin = ipin + 1;
            ArrayPinEffect{ipin} = sqrt(0.037)*rannor(12345);
         end;
      end;
      i = 0;
      do MArray = 1 to &narray;
         do Dye = 1 to &ndye;
            do Row = 1 to &nrow;
               do k = 1 to &ngene;
                  if MArray=1 and Dye = 1 then do;
                     Trt = 0;
                     trtc = 0;
                  end;
                  else do;
                     if trtc >= &no then trtc = 0;
                     if trtc = 0 then do;
                        Trt = Trt + 1;
                        if Trt >= &ntrt then do;
                           Trt = 0;
                           trtc = 0;
                        end;
                     end;
                     trtc = trtc + 1;
                  end;
                  i = i + 1;
                  Pin = PinDist{i};
                  Dip = DipDist{i};
                  Gene = GeneDist{i};
                  a = ArrayEffect{MArray};
                  ag = ArrayGeneEffect{(MArray-1)*&ngene+Gene};
                  ad = ArrayDipEffect{(MArray-1)*&ndip+Dip};
                  ap = ArrayPinEffect{(MArray-1)*&npin+Pin};
                  eta = 1 +  Dye + Trt + Gene/1000.0 + Dye*Gene/1000.0
                        + Trt*Gene/1000.0 + Pin + a + ag + ad + ap 
                        /*+ &scale*rannor(&seed)*/;
				 /*a=1/(&shape**2); c=&scale/&shapel; b=exp(Xb)/(a**c);*/

                  mean = exp(eta);
                  response = rangam(12345,2)*mean/2;	*<--gamma with shape of 2 and scale of mean/2 (mean = scale*shape);
                  logresponse=log(response);			*<--I have NOT checked my work. This is a quick-and-dirty.;
                  output;
               end;
            end;
        end;
     end;
run;



%hpglimmix( data=tmp.microarrayG,
             stmts=%str(
                    class marray dye trt gene pin dip;
                    model response = dye trt gene dye*gene trt*gene pin;
                    random marray marray*gene dip(marray) pin*marray;
                    test trt;
				  ),
			 error=gamma,		
             link=log, 
			 tech=NRRIDG
);


options notes source;
*ods select none;

ods output ParameterEstimates =beta_glimmix;
ods output CovParms = cov_glimmix;
proc glimmix data=tmp.microarrayG   ;
      class marray dye trt gene pin dip;
      model  response = dye trt gene dye*gene trt*gene pin /dist=gamma link=log s;
      random marray marray*gene dip(marray) pin*marray;   	  
	  nloptions tech=NRRIDG  maxiter=50;
run; 
ods select all;

/* Compare results */
data _null_;
      set _cov;
	  put  CovParm=;
	  put @1  '%HPGLIMMIX  '  Estimate=  best10.9 @@ ;
	  set cov_glimmix;
	  put @40 'GLIMMIX     '  Estimate=  best10.9;
	  put ;
run;
