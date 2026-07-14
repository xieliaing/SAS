data train;
      input gender  $  department $  Total  Admitted;
cards;
Men  A  825  512
Men  B  560  353
Men  C  325  120
Men  D  417  138
Men  E  191  53
Men  F  272  16
Women  A  108  89
Women  B  25  17
Women  C  593  202
Women  D  375  131
Women  E  393  94
Women  F  341  24
;
run;

proc format;
      value $ dep 
	      "A"="AB"
		  "C"="AB"
		  other="Other"
	;
run;

proc means data=train noprint;
     format department $dep.; 
	 class  gender department;
	 ways   2;
	 var    total  admitted;
	 output  out=train2  sum(total)=total  sum(admitted)=admitted;
run;

proc transpose data=train2  out=train2t  name=department;
      by gender;
	  var  total;
	  id  department;
run;
data train2t;
      set train2t;
	  total=other+ab;
run;

proc probit data=train2t ;
      class gender ;	  
	  model ab/total =gender /noint dist=normal;
	 * score data=train out=scoreout;
run;

data scoreout;
      set train;
	  if gender="Men" then linear = -0.1408; else linear=-0.3002;
     /* if gender="Men" then linear = -0.2249*0.6257; else linear=-0.4810*0.6257;*/
	  imr = pdf('NORMAL', linear)/(cdf('NORMAL', linear));
run;

data scoreout_t;
      set scoreout;
	  where Department in ("A", "C");
	  ratio = Total/admitted;
run;

proc glm data=scoreout_t;
      class gender;
	  model  ratio =  imr / solution;	 
      output  out=out1; 
run;quit;


proc glm data=scoreout_t;
      class gender;
	  model  ratio = gender/noint solution;	  
	  output  out=out2;
run;quit;
