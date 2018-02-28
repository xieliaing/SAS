***********************************************************************;
*  PREDICT:  a macro to classify observations based on model          *;
*            information from the boost macro.                        *;
*                                                                     *;
*  Inputs:                                                            *;
*    pred_ds:    name of the data set to be predicted.  this data set *;
*                must have descriptors labeled x1, x2,..., xp.  the   *;
*                response must be labeled as y and must have values   *;
*                0 and 1.                                             *;
*    p:          the number of descriptors in the input data set.     *;
*    boost_ds:   name of the data set with boosting model information *;
*    type:       the type of boosting for prediction                  *;
*                1 = adaboost                                         *;
*                2 = real adaboost                                    *;
*                3 = gentle adaboost                                  *;
*                4 = logitboost                                       *;
*    iter:       the number of boosting iterations desired.           *;
*                                                                     *;
*  Output:                                                            *;
*    outputds:   a data set that contains the predicted               *;
*                classification for each observation in pred_ds       *;
***********************************************************************;
%macro predict(pred_ds,p,boost_ds,outputds,out_pred, type,iter);
proc iml;
    start kappa(y,y_pred,kappa);
        a = sum(y_pred=0) - sum(y>y_pred);
        b = sum(y_pred>y);
        c = sum(y>y_pred);
        d = sum(y_pred=1) - sum(y_pred>y);
        n = (a+b+c+d);
        o = (a+d)/n;
        e = ((a+c)*(a+b)+(b+d)*(c+d))/(n*n);
        if e = 1 then kappa = 1;
        else kappa = (o-e)/(1-e);
    finish kappa;

    ***import data to be predicted***;
    use &pred_ds;
        read all var('x1':"x&p") into x;
        read all var {y} into y;
    ***adaboost***;
    %if &type=1 %then %do;
        n = nrow(y);
        use &boost_ds;
        read all var {gini_var} into gini_var;
        read all var {gini_cut} into gini_cut;
        read all var {c_l} into c_l;
        read all var {c_r} into c_r;
        read all var {alpha} into alpha;

        ***initialize vector of predictions***;
        sum_pred = repeat(0,n,1);
        y_pred_t = repeat(0,n,1);

        %do t=1 %to &iter;
            ***get predicted values for tth iteration***;
            do i=1 to n;
                if x[i,gini_var[&t]] <= gini_cut[&t] then y_pred_t[i,1] = c_l[&t];
                if x[i,gini_var[&t]] >  gini_cut[&t] then y_pred_t[i,1] = c_r[&t];
            end;
            c_t = alpha[&t]*(2*y_pred_t-1);
            sum_pred = sum_pred + c_t;

            ***accuracy of classifier at current stage***;
            y_pred = (sum_pred > 0);
            error = (sum(y ^= y_pred))/n;
            run kappa(y,y_pred,kappa);
            iter = &t;
            ada_t = iter||error||kappa;
            iterinfo = iterinfo//ada_t;
        %end;

        ***compute final classifier***;
        ada_pred = (sum_pred > 0);
        ***output boosting information***;
        varname1 = {'iter' 'error' 'kappa'};
        create &outputds from iterinfo [colname=varname1];
        append from iterinfo;
        varname2 = {'ada_pred', 'sum_pred'};
		ada_pred = ada_pred||sum_pred;
        create &out_pred from ada_pred [colname=varname2];
        append from ada_pred;
    %end;

    ***real adaboost***;
    %if &type=2 %then %do;
        n = nrow(y);
        use &boost_ds;
        read all var {gini_var} into gini_var;
        read all var {gini_cut} into gini_cut;
        read all var {p0_lh} into p0_lh;
        read all var {p1_lh} into p1_lh;
        read all var {p0_rh} into p0_rh;
        read all var {p1_rh} into p1_rh;
        read all var {c_l} into c_l;
        read all var {c_r} into c_r;

        ***initialize vector of predictions***;
        sum_pred = repeat(0,n,1);
        y_pred_t = repeat(0,n,1);

        %do t=1 %to &iter;
            ***get predicted values for tth iteration***;
            do i=1 to n;
                if x[i,gini_var[&t]] <= gini_cut[&t] then do;
                    if c_l[&t] = 0 then y_pred_t[i,1] = 1-p0_lh[&t];
                    if c_l[&t] = 1 then y_pred_t[i,1] = p1_lh[&t];
                end;
                if x[i,gini_var[&t]] >  gini_cut[&t] then do;
                    if c_r[&t] = 0 then y_pred_t[i,1] = 1-p0_rh[&t];
                    if c_r[&t] = 1 then y_pred_t[i,1] = p1_rh[&t];
                end;
            end;
            y_pred_t = y_pred_t + 0.0001*(y_pred_t = 0) - 0.0001*(y_pred_t = 1);
            c_t = 0.5*log(y_pred_t#(1/(1-y_pred_t)));
            sum_pred = sum_pred + c_t;

            ***accuracy of classifier at current stage***;
            y_pred = (sum_pred > 0);
            error = (sum(y ^= y_pred))/n;  
            run kappa(y,y_pred,kappa);
            iter = &t;
            real_t = iter||error||kappa;
            iterinfo = iterinfo//real_t;
        %end;

        ***compute final classifier***;
        realpred = (sum_pred > 0);

        ***output boosting information***;
        varname1 = {'iter' 'error' 'kappa'};
        create &outputds from iterinfo [colname=varname1];
        append from iterinfo;
        varname2 = {'realpred', 'sum_pred'};
        realpred = realpred || sum_pred;	
        create &out_pred from realpred [colname=varname2];
        append from realpred;
    %end;

    ***gentle adaboost***;
    %if &type=3 %then %do;
        n = nrow(y);
        use &boost_ds;
        read all var {reg_var} into reg_var;
        read all var {cut_val} into cut_val;
        read all var {ypred_l} into ypred_l;
        read all var {ypred_r} into ypred_r;

        ***initialize vector of predictions***;
        sum_pred = repeat(0,n,1);
        y_pred_t = repeat(0,n,1);

        %do t=1 %to &iter;
            y_pred_t = ypred_l[&t]*(x[,reg_var[&t]] < cut_val[&t]) + 
                       ypred_r[&t]*(x[,reg_var[&t]] >= cut_val[&t]);
            sum_pred = sum_pred + y_pred_t;

            ***accuracy of classifier at current stage***;
           * y_pred = (sum_pred > 0);
           * error = (sum(y ^= y_pred))/nrow(y);  
           * run kappa(y,y_pred,kappa);
           * iter = &t;
           * gentle_t = iter||error||kappa;
           *iterinfo = iterinfo//gentle_t;
        %end;

        ***compute final classifier***;
        genpred = (sum_pred > 0);

        ***output boosting information***;
       * varname1 = {'iter' 'error' 'kappa'};
       * create &outputds from iterinfo [colname=varname1];
       * append from iterinfo;
        varname2 = {'gen_pred', 'sum_pred'};
		genpred = genpred || sum_pred;
        create &out_pred from genpred [colname=varname2];
        append from genpred;
    %end;

    ***logitboost***;
    %if &type=4 %then %do;
        n = nrow(y);
        use &boost_ds;
        read all var {reg_var} into reg_var;
        read all var {cut_val} into cut_val;
        read all var {ypred_l} into ypred_l;
        read all var {ypred_r} into ypred_r;

        ***initialize vector of predictions***;
        sum_pred = repeat(0,n,1);
        y_pred_t = repeat(0,n,1);

        %do t=1 %to &iter;
            y_pred_t = ypred_l[&t]*(x[,reg_var[&t]] < cut_val[&t]) + 
                       ypred_r[&t]*(x[,reg_var[&t]] >= cut_val[&t]);
            sum_pred = sum_pred + y_pred_t/2;

            ***accuracy of classifier at current stage***;
           * y_pred = (sum_pred > 0);
           * error = (sum(y ^= y_pred))/nrow(y);  
           * run kappa(y,y_pred,kappa);
           * iter = &t;
           * logit_t = iter||error||kappa;
           * iterinfo = iterinfo//logit_t;
        %end;

        ***compute final classifier***;
        logpred = (sum_pred > 0);

        ***output boosting information***;
       * varname1 = {'iter' 'error' 'kappa'};
       * create &outputds from iterinfo [colname=varname1];
       * append from iterinfo;
        varname2 = {'log_pred', 'sum_pred'};
		logpred = logpred || sum_pred;
        create &out_pred from logpred [colname=varname2];
        append from logpred;
    %end;
    quit;
%mend predict;

