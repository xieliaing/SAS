
%macro irls;
	/* obtain initial working response */
	proc reg data=glmnet noprint;
		model g2 = ;
		output  out=_tmp  p=pred;
	run;quit;

	data _tmp;
		set _tmp;
		prob=1/(1+exp(-pred));
		w=prob*(1-prob);
		delta=(g2-prob)/w;
		z=pred + delta;
		delta2=delta**2;
		drop pred;
	run;
	proc means data=_tmp noprint;
		var delta2;	 
		output out=_mean0 mean(delta2)=lq;
	run;

	%let dlq=5;%let maxiter=10;
	%let iter=1;
	%do %while (%sysevalf(&dlq>0.001));
		proc reg data=_tmp noprint;
			model z = var2-var21;
			weight w;
			output  out=_tmp  p=pred;
		run;quit;

		data _tmp;
			set _tmp;
			prob=1/(1+exp(-pred));
			w=prob*(1-prob);
			delta=(g2-prob)/w;
			z=pred + delta;
			delta2=w*delta**2;
			drop pred;
		run;
		proc means data=_tmp noprint;
			var delta2;
			output  out=_mean1 mean(delta2)=_lq;
		run;

		data _mean0;
			merge _mean0 _mean1;
			dlq=abs(lq-_lq);
			call symput('dlq', dlq);
			lq=_lq;
			keep lq;
		run;
		%put &dlq;
		%let iter=%eval(&iter+1);
		%if  %eval(&iter>= &maxiter) %then %let dlq=-1;
	%end;
	proc reg data=_tmp ;
		model z = var2-var21;
		weight w;	   
	run;quit;
%mend;

%irls;