data training;
     set overfitting;
	 where train=1;
	 y=target_practice;
	 keep case_id y var_:;
run;

proc standard data=training  mean=0  std=1
              out=training_std;
	 var var_1-var_200;
run;

proc standard data=training_std  mean=0
              out=training_std;
	 var y;
run;

%let lambda2=%sysfunc(sqrt(4*250));

data augment;
     array _v{200} var_1-var_200 (200*0);
     retain y 0;
     do id=1 to dim(_v);
        _v[id]=&lambda2;
		if id>=2 then _v[id-1]=0;
	    output;
		drop id;
	 end;
run;

proc append base=training_std data=augment  force;
run;

proc reg data=training_std  outest=beta0 noprint;
     model y=var_1-var_200/noint;
run;quit;

proc reg data=training_std(obs=250)  outest=beta1 ridge=4 noprint;
     model y=var_1-var_200;
run;quit;


data training_std;
     set training_std;
     drop role;
run;

dm output 'clear';
ods select none;
ods graphics on;
ods output SelectionSummary=SelectionSummary;
proc glmselect data=training_std plots(stepaxis=normb)=all  ;
     model y=var_1-var_200/noint 
                           selection=lasso(stop=none  choose=cv) cvmethod=random(10)  
                           stb details=summary;
	 *partition role=role(test="TEST" train="TRAIN" validate="VALIDATE");
	 partition fraction(test=0.1);
run;quit;
ods graphics off;
ods select all;

