***********************************************************************;
*  BOOST:  A macro to perform generalized boosting algorithms using   *;
*          recursive partitioning.                                    *;
*          This macro can perform AdaBoost, Real AdaBoost, Gentle     *;
*          AdaBoost, and LogitBoost.                                  *;
*                                                                     *;
*  Inputs:                                                            *;
*    inputds:    name of the input data set.  This data set must      *;
*                have descriptors labeled x1, x2,..., xp.  The        *;
*                response must be labeled as y and must have values   *;
*                0 and 1.                                             *;
*    p:          the number of descriptors in the input data set.     *;
*    type:       the type of boosting desired                         *;
*                1 = AdaBoost                                         *;
*                2 = Real AdaBoost                                    *;
*                3 = Gentle AdaBoost                                  *;
*                4 = LogitBoost                                       *;
*    iter:       the number of boosting iterations desired.           *;
*                                                                     *;
*  Outputs:                                                           *;
*    outputds:   a data set that contains information about each      *;
*                iteration from boosting.  The data set contains the  *;
*                following variables when using AdaBoost or Real      *;
*                AdaBoost:                                            *;
*                iter :  boosting iteration number                    *;
*                gini_var :  the descriptor number at the current     *;
*                            iteration that minimizes the Gini Index  *;
*                gini_cut :  the cut-point that minimizes Gini      *;
*                p0_LH :  the probability of class 0 for observations *;
*                         less than gini_cut                          *;
*                p1_LH :  the probability of class 1 for observations *;
*                         less than gini_cut                          *;
*                C_L   :  the class label for observations less than  *;
*                         gini_cut                                    *;
*                p0_RH :  the probability of class 0 for observations *;
*                         greater than gini_cut                       *;
*                p1_RH :  the probability of class 1 for observations *;
*                         greater than gini_cut                       *;
*                C_R   :  the class label for observations greater    *;
*                         than gini_cut                               *;
*                alpha :  the weight of the SPLIT at the current      *;
*                         boosting iteration                          *;
*                error :  the misclassification error of the          *;
*                         cumulative boosting model at the current    *;
*                         iteration                                   *;
*                kappa :  the kappa statistic of the cumulative       *;
*                         boosting model at the current iteration     *;
*                                                                     *;
*                The data set contains the following variables when   *;
*                using Gentle AdaBoost or LogitBoost:                 *;
*                iter     :  boosting iteration number                *;
*                reg_var  :  the variable number that minimizes       *;
*                            the corrected sum-of-squares             *;
*                min_css  :  the minimum corrected sum-of-squares     *;
*                cut_val  :  the optimal cut-point                  *;
*                ypred_L  :  the predicted value for                  *;
*                            observations less than cut_val           *;
*                ypred_R  :  the predicted value for                  *;
*                            observations greater than cut_val        *;
*                error    :  the misclassification error of the       *;
*                            cumulative boosting model at the current *;
*                            iteration                                *;
*                kappa    :  the kappa statistic of the cumulative    *;
*                            boosting model at the current iteration  *;
*    outwts:     a data set that contains observation weights at the  *;
*                final boosting iteration.  These can be used to      *;
*                identify observations that are difficult to classify.*;
***********************************************************************;
%macro boost2(inputds,p,outputds,outwts,iter,type);
proc iml;
    start split_iml(x,y,w,g_info,out_type, y_pred);
        n = nrow(x);
        p = ncol(x);
        gini_min = 2;
        gini_var = 0;
        gini_cut = 0;
        y_pred = repeat(0,n,1);
        wsum = sum(w);
        ywsum = sum(y#w);  
        ywsum1 = wsum - ywsum; 
        do j=1 to p;
            x_curr = x[,j]||y||w;
            b=x_curr;  
            x_curr[rank(x[,j]),]=b;  free b;
            x_sort = x_curr[,1]; 
            y_sort = x_curr[,2]; 
            w_sort = x_curr[,3];
			yw_sort=(y_sort#w_sort);
			yw_sort1=(w_sort - yw_sort);		
            yw_cusum=cusum(yw_sort[1:(n-1)]);	

            lpwt = cusum(w_sort[1:(n-1)]);
			lpwt = lpwt#(lpwt >= 2*CONSTANT('SMALL')) + (lpwt < 2*CONSTANT('SMALL'))*2*CONSTANT('SMALL');
	
			p1_L = yw_cusum # (1/lpwt);
		
			gini = yw_cusum # (1-p1_L);

            rpwt = wsum - lpwt; 
			rpwt = rpwt#(rpwt >= 2*CONSTANT('SMALL')) + (rpwt < 2*CONSTANT('SMALL'))*2*CONSTANT('SMALL');
   
            yw_cusum = ywsum - yw_cusum;
			p1_R = yw_cusum # (1/rpwt);
		
		
			gini = gini + yw_cusum # (1-p1_R);

			free lpwt  rpwt  yw_cusum  yw_sort1  yw_sort  yw_sum;

            g_min=gini[><];  g_loc=gini[>:<];

            if  g_min < gini_min then do;
                gini_min=g_min;
                gini_var = j;
                gini_cut = (x_sort[g_loc] + x_sort[g_loc+1]) / 2;
                p1_RH = p1_R[g_loc];
                p0_RH = 1 - p1_R[g_loc];
                p1_LH = p1_L[g_loc];
                p0_LH = 1 - p1_L[g_loc];

                c_R = 0;
                if p1_RH > 0.5 then c_R = 1;
                c_L = 0;
                if p1_LH > 0.5 then c_L = 1;
            end;
        end;
        g_info = gini_var||gini_min||gini_cut||p0_LH||p1_LH||c_L||p0_RH||p1_RH||c_R;
        if out_type = 1 then 
            y_pred = (x[, gini_var] <=gini_cut)*c_L + (x[, gini_var] > gini_cut) *c_R
        ;
       
		if out_type=2 then
		
            y_pred[, 1] =( x[, gini_var]<=gini_cut) * ( (c_L=0)*(1-p0_LH) + (c_L=1)*p1_LH) +
		                       ( x[, gini_var] >  gini_cut) * ( (c_R=0)*(1-p0_RH) + (c_R=1)*p1_RH)
        ;
		
 
    finish split_iml;


    start regsplit_iml(x,y,w,j_info,y_pred);
        n = nrow(x);
        p = ncol(x);
        min_css = 10000000000000;
        y_pred = repeat(0,n,1);
		wy2sum = sum( w#y#y );
        wsum = sum(w);
        ywsum = sum(y#w);
		ywsum1 = wsum - ywsum;
        do j=1 to p;
            x_curr = x[,j]||y||w;
            b=x_curr;
            x_curr[rank(x[,j]),]=b;   free b;
            x_sort = x_curr[,1];
            y_sort = x_curr[,2];
            w_sort = x_curr[,3];

			yw_sort=(y_sort#w_sort);
			yw_sort1=((1-y_sort)#w_sort);
			w_sort = (w_sort);

			yw_cusum = cusum(yw_sort[1:(n-1)]);

			lpwt = cusum(w_sort[1:(n-1)]);
			lpwt = lpwt# (lpwt>constant('SMALL')) + constant('SMALL')#(lpwt<=constant('SMALL'));
			p1_L = (yw_cusum # (1/lpwt));

			rpwt = wsum - lpwt;
			rpwt = rpwt#(rpwt>constant('MACEPS')) + constant('MACEPS')#(lpwt<=constant('MACEPS'));
			p1_R = ((ywsum - yw_cusum) # (1/rpwt));	

			css=(1:n-1)*0;
		

			lpwt = cusum(w_sort); rpwt = cusum(yw_sort);
		
			css = wy2sum + p1_L##2#lpwt[1:(n-1)] + p1_R##2#(wsum - lpwt[1:(n-1)]) -
			        2*(p1_L#rpwt[1:(n-1)] + p1_R#(ywsum - rpwt[1:(n-1)]));

           
			free  lpwt  rpwt  yw_cusum  yw_sort1;
			css_min=css[><];  css_loc=css[>:<];

            if css_min < min_css then do;
                min_css = css_min;
                cut_val = (x_sort[css_loc] + x_sort[css_loc+1]) / 2;
                reg_var = j;
                ypred_L = (sum(yw_sort[1:css_loc]))/sum(w_sort[1:css_loc]);
                ypred_R = (sum(yw_sort[css_loc+1:n]))/
                        sum(w_sort[css_loc+1:n]);
                y_pred = ypred_L*(x[,j] < cut_val) + ypred_R*(x[,j] >= cut_val);
                j_info = reg_var||min_css||cut_val||ypred_L||ypred_R;
            end;
        end;
    finish regsplit_iml;

    start kappa(y,y_pred,kappa);
        A = sum(y_pred=0) - sum(y>y_pred);
        B = sum(y_pred>y);
        C = sum(y>y_pred);
        D = sum(y_pred=1) - sum(y_pred>y);
        N = (A+B+C+D);
        O = (A+D)/N;
        E = ((A+C)*(A+B)+(B+D)*(C+D))/(N*N);
        if E = 1 then kappa = 1;
        else kappa = (O-E)/(1-E);
    finish kappa;


    * 0.  Import data;
    use &inputds;
        read all var('x1':"x&p") into x;
        read all var {y} into y;

		%global t;
    * AdaBoost;
    %if &type=1 %then %do;
        out_type = &type;
        * 1.  Initialize observation weights and overall prediction;
        w = repeat(1/nrow(y),nrow(y),1);
        sum_pred = repeat(0,nrow(y),1);
        * 2.  For t=1 to T do;
        %do t=1 %to &iter;

            ***2.a.  Fit model ***;
            run split_iml(x,y,w,g_info,out_type,y_pred_t);

            ***2.b.  Compute contribution of each observation***;
            err_t  = (w#(y_pred_t ^= y))[+,];
            if err_t = 0 then err_t = 0.0001;
            if err_t = 1 then err_t = 0.9999;
            alpha_t = log((1-err_t)/err_t);
            c_t = alpha_t*(2*y_pred_t-1);
            sum_pred = sum_pred + c_t;

            ***2.c.  Update weights***;
            w = w#exp(alpha_t*(y_pred_t ^= y));
            w = w#(w >= 2*CONSTANT('SMALL')) + (w < 2*CONSTANT('SMALL'))*2*CONSTANT('SMALL');
            w = w / sum(w);

           ***Accuracy of classifier at current stage***;
            y_pred = (sum_pred > 0);
            error = (sum(y ^= y_pred))/nrow(y);
            run kappa(y,y_pred,kappa);
            iter = &t; type=1;
            ada_t = iter||g_info||alpha_t||error||kappa||type;
            iterinfo = iterinfo//ada_t;
        %end;

        ***3.  Compute final classifier***;
        ada_pred = (sum_pred > 0);

        ***Output boosting information***;
        varname1 = {'iter' 'gini_var' 'gini_min' 'gini_cut' 'p0_LH' 'p1_LH' 'c_L' 'p0_RH' 
                  'p1_RH' 'c_R' 'alpha' 'error' 'kappa' 'type'};
        create &outputds from iterinfo [colname=varname1];
        append from iterinfo;
        varname2 = {'weight'};
        create &outwts from w [colname=varname2];
        append from w;
    %end;

    ***Real AdaBoost***;
    %if &type=2 %then %do;
        out_type = &type;
        ***1.  Initialize observation weights and overall prediction***;
        w = repeat(1/nrow(y),nrow(y),1);
        sum_pred = repeat(0,nrow(y),1);
        ***2.  For t=1 to T do***;
        %do t=1 %to &iter;		   
            ***2.a.  Fit model ***;
            run SPLIT_IML(x,y,w,g_info,out_type, y_pred_t);           			
			y_pred_t =y_pred_t + 0.00001*(y_pred_t <= 0) - 0.00001*(y_pred_t >= 1);            

            ***2.b.  Compute contribution of each observation***;

            c_t = 0.5*log(y_pred_t#(1/(1-y_pred_t)));
            sum_pred = sum_pred + c_t;

            ***2.c.  Update weights***;
            w = w#exp((-(2*y-1))#c_t);
            w = w#(w >= 2*CONSTANT('SMALL')) + (w < 2*CONSTANT('SMALL'))*2*CONSTANT('SMALL');
            w  = w / sum(w);

            ***Accuracy of classifier at current stage***;
            y_pred = (sum_pred > 0);
            error = (sum(y ^= y_pred))/nrow(y);  
            run KAPPA(y,y_pred,kappa);
            iter = &t; type=2;
            real_t = iter||g_info||error||kappa||type;
            iterinfo = iterinfo//real_t;
        %end;

        ***3.  Compute final classifier***;
        realpred = (sum_pred > 0);

        ***Output boosting information***;
        varname1 = {'iter' 'gini_var' 'gini_min' 'gini_cut' 'p0_LH' 'p1_LH' 'c_L' 'p0_RH' 
                  'p1_RH' 'c_R'  'error' 'kappa' 'type'};
        create &outputds from iterinfo [colname=varname1];
        append from iterinfo;
        varname2 = {'weight'};
        create &outwts from w [colname=varname2];
        append from w;
    %end;

    ***Gentle Boost***;
    %if &type=3 %then %do;
        ***1.  Initialize observation weights and overall prediction***;
        w = repeat(1/nrow(y),nrow(y),1);
        sum_pred = repeat(0,nrow(y),1);
        y_work = 2*y-1;

        ***2.  For t=1 to T do***;
        %do t=1 %to &iter;
            ***2.a.  Fit model ***;
            run regsplit_iml(x,y_work,w,j_info,y_pred_t);

            ***2.b.  Compute contribution of each observation***;
            sum_pred = sum_pred + y_pred_t;

            ***2.c.  Update weights***;
            w = w#exp((-(2*y-1))#y_pred_t);
            w = w#(w >= 2*CONSTANT('SMALL')) + (w < 2*CONSTANT('SMALL'))*2*CONSTANT('SMALL');
            w  = w / sum(w);

            ***Accuracy of classifier at current stage***;
            y_pred = (sum_pred > 0);
            error = (sum(y ^= y_pred))/nrow(y);  
            run KAPPA(y,y_pred,kappa);
            iter = &t; type=3;
            gentle_t = iter||j_info||error||kappa||type;
            iterinfo = iterinfo//gentle_t;
        %end;

        ***3.  Compute final classifier***;
        gen_pred = (sum_pred > 0);

        ***Output boosting information***;
        varname1 = {'iter' 'reg_var' 'min_css' 'cut_val' 'ypred_L' 'ypred_R' 
                  'error' 'kappa' 'type'};
        create &outputds from iterinfo [colname=varname1];
        append from iterinfo;
        varname2 = {'weight'};
        create &outwts from w [colname=varname2];
        append from w;
    %end;

    ***LogitBoost***;
    %if &type=4 %then %do;
        ***1.  Initialize observation weights and overall prediction***;
        sum_pred = repeat(0,nrow(y),1);
        w = repeat(0.25,nrow(y),1);
        y_work = 4*y-2;

        ***2.  For t=1 to T do***;
        %do t=1 %to &iter;

            ***2.a.  Fit model ***;
            run REGSPLIT_IML(x,y_work,w,j_info,y_pred_t);

            ***2.b.  Compute contribution of each observation***;
            sum_pred = sum_pred + y_pred_t/2;

            ***2.c.  Update weights and y***;
            w = 1/((exp(sum_pred) + exp(-sum_pred))#(exp(sum_pred) + 
            exp(-sum_pred)));
            ***Enforce a lower threshold on the weights***;
            w = w#(w >= 2*CONSTANT('SMALL')) + (w < 2*CONSTANT('SMALL'))*2*CONSTANT('SMALL');

            y_work = (y - 1/(1+exp(-2*sum_pred)))#(1/w);

            ***Accuracy of classifier at current stage***;
            y_pred = (sum_pred > 0);
            error = (sum(y ^= y_pred))/nrow(y);  
            run KAPPA(y,y_pred,kappa);
            iter = &t; type=4;
            logit_t = iter||j_info||error||kappa||type;
            iterinfo = iterinfo//logit_t;
        %end;

        ***Output boosting information***;
        varname1 = {'iter' 'reg_var' 'min_css' 'cut_val' 'ypred_L' 'ypred_R' 
                  'error' 'kappa' 'type'};
        create &outputds from iterinfo [colname=varname1];
        append from iterinfo;
        varname2 = {'weight'};
        create &outwts from w [colname=varname2];
        append from w;
    %end;
    quit;  
%mend boost;
