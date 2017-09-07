/*************************************************************************
  These two macros are to be used to replace the original same-name split
  macros in the book:
  Pharmaceutical Statistics Using SAS®: A Practical Guide
  URL: https://www.sas.com/store/books/categories/usage-and-reference/pharmaceutical-statistics-using-sas-a-practical-guide/prodBK_60622_en.html
  
  As of now, SAS no longer offer code for download unless you login to their
  PartnerNet for SAS Aliance.
  
  Please refer to my blog post on the performance improvement:
  URL: http://www.sas-programming.com/2010/04/improve-boost-macro-from-rayens-w-and.html?view=sidebar
*************************************************************************/
/*************************************************************************
This is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 or 3 of the License
(at your option).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
**************************************************************************/
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
            lpwt = lpwt#(lpwt >= 2*CONSTANT('SMALL')) + 
                   (lpwt < 2*CONSTANT('SMALL'))*2*CONSTANT('SMALL');
    
            p1_L = yw_cusum # (1/lpwt);
            gini = yw_cusum # (1-p1_L);
                       
            rpwt = wsum - lpwt; 
            rpwt = rpwt#(rpwt >= 2*CONSTANT('SMALL')) + 
                   (rpwt < 2*CONSTANT('SMALL'))*2*CONSTANT('SMALL');
    
            yw_cusum = ywsum - yw_cusum;
            p1_R = yw_cusum # (1/rpwt);
            
            gini = gini + yw_cusum # (1-p1_R);

            free lpwt  rpwt  yw_cusum  yw_sort1;

            g_min=gini[><];  g_loc=gini[>:<];

            if g_min < gini_min then do;
                gini_min=g_min;
                gini_var = j;
                gini_cut = (x_sort[g_loc] + x_sort[g_loc+1]) / 2;
                p1_RH = p1_R[g_loc];
                p0_RH = 1-p1_R[g_loc];
                p1_LH = p1_L[g_loc];
                p0_LH = 1-p1_L[g_loc];

                c_R = 0;
                if p1_RH > 0.5 then c_R = 1;
                c_L = 0;
                if p1_LH > 0.5 then c_L = 1;
            end;
        end;
        g_info = gini_var||gini_min||gini_cut||p0_LH||p1_LH||c_L||p0_RH||p1_RH||c_R;
        if out_type = 1 then 
           y_pred = (x[, gini_var] <=gini_cut)*c_L + 
                    (x[, gini_var] > gini_cut) *c_R
        ;
       
        if out_type=2 then
           y_pred[, 1] =( x[, gini_var]<=gini_cut) * ( (c_L=0)*(1-p0_LH) + (c_L=1)*p1_LH) +
                        ( x[, gini_var] >  gini_cut) * ( (c_R=0)*(1-p0_RH) + (c_R=1)*p1_RH)
        ;
 
 
    finish split_iml;
	
/*************************************************************************
This is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 or 3 of the License
(at your option).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
**************************************************************************/
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
   lpwt = lpwt# (lpwt>constant('SMALL')) + 
                         constant('SMALL')#(lpwt<=constant('SMALL'));
   p1_L = (yw_cusum # (1/lpwt));

   rpwt = wsum - lpwt;
   rpwt = rpwt#(rpwt>constant('MACEPS')) + 
                         constant('MACEPS')#(lpwt<=constant('MACEPS'));
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