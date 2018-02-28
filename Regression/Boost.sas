
%macro gini(_y0wsum, _y1wsum, i, nobs);
data _giniout&i.(keep=varname  mingini cut_val   p0_LH  p1_LH  c_L  p0_RH  p1_RH  c_R);     
     length varname $ 8;
     set sorted  end=eof;
	 retain  _y0w  _y1w  _w  _ginik  0;
	 retain  p0_LH  p1_LH  p0_RH  p1_RH  c_L  c_R  0;	
	 array _mingini{4}  _temporary_;	
	 if w<1e-20 then w=1e-20;
	 if _n_=1 then do;
	    _y0w = (ywork^=1)*w;  _y1w = (ywork=1)*w;   _w = w;				
		_mingini[1] = 2;
        _mingini[2] = 1; 
        _mingini[3] = x&i; 
        _mingini[4] = x&i;	       
	 end;
	 else do;
	    _y0w + (ywork^=1)*w; _y1w + (ywork=1)*w; _w + w;
	 end;

	 if ^eof then do;	    		
        p0_L = _y0w/_w;  p0_R = (&_y0wsum - _y0w)/(1-_w);
        p1_L = _y1w/_w;  p1_R = (&_y1wsum - _y1w)/(1-_w);
        _ginik= p1_L*p0_L*_w + p1_R*p0_R*(1-_w);
	 end;

	 if _ginik<_mingini[1] then do;	    
		_mingini[1]=_ginik;   _mingini[2]=_n_; _mingini[3]=x&i;
		p0_LH=p0_L;  p1_LH=p1_L;  p0_RH=p0_R;  p1_RH=p1_R;
		/*c_L = (p1_LH > &_y1wsum); c_R = (p1_RH > &_y1wsum);		*/
        c_L = (p1_LH > 0.5); c_R = (p1_RH > 0.5);
	 end;	
	 if _n_=(_mingini[2]+1) then _mingini[4]=x&i;

	 if eof then do;	  
	    cut_val=(_mingini[3]+_mingini[4])/2;
		mingini=_mingini[ 1]; 
        varname="x&i"; 		
		output  ;	
	 end;   

run;
%mend;

%macro stump_gini(dsn, p, outdsn);
/***************************************************/
/*    dsn: Name of input SAS data sets. All        */
/*          independent variables should be named  */
/*          as X1, X2,....,Xp and be continous     */
/*          numeric variables                      */
/*      p: Number of independent variables         */
/* outdsn: Name of output SAS data sets. Used for  */
/*          Subsequent scoring. Not to named as    */
/*          _giniout.....                          */
/***************************************************/
%local i  p  ;
data _tmpv/view=_tmpv;
       set &dsn.(keep=ywork  w)  nobs=ntotal;
	   y1=(ywork=1); y0=(ywork^=1);	   
       drop ywork;
run;
proc means data=_tmpv(keep=y0 y1 w)   noprint;
        var y0  y1;
		 weight w;
		 output out=_ywsum(keep=_y0wsum  _y1wsum  _FREQ_)  
                   sum(y0)=_y0wsum  sum(y1)=_y1wsum;
run;
%do i=1 %to &p;
    data _tmpv/view=_tmpv;
	       set  &dsn.(keep=x&i  ywork   w);
		   u=ranuni(0);
	run;
    proc sort data=_tmpv   out=sorted(compress=binary)  sortsize=max;
	     by x&i   u;
	run;

    data _null_;
	     set _ywsum;
		 call execute('%gini('|| compress(_y0wsum) || ','
		                      || compress(_y1wsum) || ','
                              || compress(&i)      || ','
                              || compress(_FREQ_)  || ')'
                      );
	run;
%end;
data &outdsn;
     set %do i=1 %to &p;
	        _giniout&i
		 %end;;
run;
proc sort data=&outdsn; by mingini; run;
proc datasets library=work nolist;
     delete %do i=1 %to &p;
	           _giniout&i
			%end;;
run;quit;
%mend;


%macro css(_ywsum, i, nobs);
data _regout&i.(keep=varname  mincss cut_val  ypred_L  ypred_R);     
     length varname $ 8;
     set sorted  end=eof;
	 retain _yw  _w  cssk 0;
	 retain  ypred_L  ypred_R 0;
	 array _mincss{4}  _temporary_;	
	 if _n_=1 then do;
	    _yw = (ywork=1)*w;  _w = w;		
		_mincss[1] = constant('BIG'); 
        _mincss[2] = 1; 
        _mincss[3] = x&i; 
        _mincss[4] = x&i;
        ypred_L = _yw/_w;  ypred_R = (&_ywsum-_yw)/(1-_w);		
	 end;
	 else do;
	    _yw + (ywork=1)*w; _w + w;
	 end;
	 if ^eof then do;	    
		/*cssk = 1 - _yw/_w*_yw - (&_ywsum-_yw)/(1-_w)*(&_ywsum-_yw);*/
	     if _yw=0 then 
		    cssk = 1 - (&_ywsum)**2/(1-_w);
		 else 
		    cssk = 1 - exp(2*log(abs(_yw)) - log(_w)) -
		              exp(2*log(abs(&_ywsum - _yw)) - log(1-_w));
	 end;
	 /*
	 else do;
	    cssk = 1 -_yw**2;
	 end;
	 */
	 if cssk<_mincss[1] then do;	    
		_mincss[1]=cssk;   _mincss[2]=_n_; _mincss[3]=x&i;
		ypred_L=_yw/_w;  ypred_R=(&_ywsum-_yw)/(1-_w);
	 end;	
	 if _n_=(_mincss[2]+1) then _mincss[4]=x&i;

	 if eof then do;	  
	    cut_val=(_mincss[3]+_mincss[4])/2;
		mincss=_mincss[ 1]; 
        varname="x&i"; 		
		output  ;
	 end;   
	    
run;
%mend;

%macro stump_css(dsn, p, outdsn);
/***************************************************/
/*    dsn: Name of input SAS data sets. All        */
/*          independent variables should be named  */
/*          as X1, X2,....,Xp and be continuous     */
/*          numeric variables                      */
/*      p: Number of independent variables         */
/* outdsn: Name of output SAS data sets. Used for  */
/*          Subsequent scoring. Not to named as    */
/*          _giniout.....                          */
/***************************************************/
%local i  p  ;
data _tmpv/view=_tmpv;
       set &dsn.(keep=ywork  w);
	   y1=(ywork=1); y0=(ywork^=1);
       drop ywork;
run;
proc means data=_tmpv(keep=y0 y1 w)   noprint;
        var y0  y1;
		 weight w;
		 output out=_ywsum(keep=_y0wsum  _y1wsum  _FREQ_)  
                   sum(y0)=_y0wsum  sum(y1)=_y1wsum;
run;
%do i=1 %to &p;
    data _tmpv/view=_tmpv;
	       set  &dsn.(keep=x&i  ywork   w);
		   u=ranuni(0);
	run;
    proc sort data=_tmpv   out=sorted(compress=binary)  sortsize=max;
	     by x&i   u;
	run;
    data _null_;
	     set _ywsum;
		 call execute('%css(' || compress(_y1wsum) || ','
                              || compress(&i)     || ','
                              || compress(_FREQ_) || ')'
                      );
	run;
%end;
data &outdsn;
     set %do i=1 %to &p;
	        _regout&i
		 %end;;
run;
proc sort data=&outdsn; by mincss; run;
options source;
proc datasets library=work nolist;
     delete %do i=1 %to &p;
	           _regout&i
			%end;;
run;quit;
%mend;



%macro ada(dsn, gini_cut, varname, c_L, c_R);
     data sumpred(keep=sumpred  w)                
          _boostinfo(keep=kappa   err   _type_);
	      retain err  pa  py  psumy  0;
		  do until (eof1);
		     set &dsn.(keep=ywork   w  &varname)  end=eof1;
			 if &varname <= &gini_cut then
			    ypred=&c_L;
			 else 
			    ypred=&c_R;
			 err + w*((ypred=1 & ywork^=1) | (ypred^=1 & ywork=1));
		  end;
		  err = max(min(0.999999, err), 0.000001);
		  alpha= log(1 - err) - log(err);
		  put alpha = ;

          err=0; ;
		  do until (eof2);
		     set  sumpred(keep=sumpred)   end=eof2  nobs=ntotal;
			 set  &dsn.(keep=ywork  w &varname  );
			 if &varname <= &gini_cut then
			    ypred=&c_L;
			 else 
			    ypred=&c_R;
	
			 sumpred = sumpred + alpha * (2*ypred-1);
             sumy = (sumpred >0);
             pa + ((sumy = 1 & ywork =1)|(sumy ^=1 & ywork ^=1));
             py + (ywork=1);
		     psumy + (sumy=1);
			 err + ((ypred = 1 & ywork ^=1)|(ypred ^=1 & ywork =1));
			 w = w*exp(alpha*( ((ypred=1 & ywork^=1) | (ypred^=1 & ywork=1)) ));
			 output sumpred;			
		  end;
		   pa=pa/ntotal;  psumy=psumy/ntotal; py = py/ ntotal;
           pe = psumy*py + (1-psumy)*(1-py);
		   kappa =( pa -  pe)/(1 - pe);
		   err=err/ntotal;
           _type_=1;
		   output _boostinfo;
	run;     
%mend;

%macro real(dsn, gini_cut, varname, c_L, c_R);
    data sumpred(keep=sumpred  w )	            
		   _boostinfo(keep=kappa   err   _type_);
		   retain  err   pa  py  psumy   0;		  
		   do until (eof1);
		        set  &dsn.(keep=ywork   w  &varname);
				set  sumpred(keep=sumpred)   end=eof1   nobs=ntotal;
				if  &varname <= &gini_cut  then 
				    ypred = &c_L;
				else
				    ypred = &c_R;
				ypred=ypred + 0.0001*(ypred=0) - 0.0001*(ypred=1);
				c=0.5*( log(ypred) - log(1-ypred) );
				sumpred = sumpred + c;
				sumy = (sumpred > 0);
				err + ((sumy = 1 & ywork ^=1)|(sumy ^=1 & ywork =1));
				pa + ((sumy = 1 & ywork =1)|(sumy ^=1 & ywork ^=1));
				py + (ywork =1);
				psumy + (sumy=1);
				w=w*exp( -(c * (1*ywork-0)) );
                output  sumpred;		       
		   end;
		   pa=pa/ntotal;  psumy=psumy/ntotal; py = py/ ntotal;
           pe = psumy*py + (1-psumy)*(1-py);
		   kappa =( pa -  pe)/(1 - pe);
		   err = err / ntotal;
		   _type_=2;
		   output  _boostinfo;
	 run;
%mend;

%macro  gentl(dsn, gini_cut, varname, ypred_L, ypred_R); 
           data sumpred(keep=sumpred  w )	             
		          _boostinfo(keep=kappa   err   _type_);
				  retain err  pa   psumy   py  0;
				  do until (eof2);
				      set   &dsn.(keep=ywork  w  &varname);
					  set   sumpred(keep=sumpred)  end=eof2  nobs=ntotal;
					  if &varname < &gini_cut then 
					      ypred=&ypred_L;
					  else 
					      ypred=&ypred_R;
					  sumpred = sumpred + ypred;
					  w = w*(exp( -(ypred * ywork) );
					  sumy = (sumpred > 0);
					  err + ( (y=1 & sumy ^=1) | (y^=1 & sumy=1));
                      pa + ((sumy = 1 & y =1)|(sumy ^=1 & y ^=1));
                      py + (y=1);
                      psumy + (sumy=1);
					  output  sumpred ;					  
				 end;
		         pa=pa/ntotal;  psumy=psumy/ntotal; py = py/ ntotal;
                 pe = psumy*py + (1-psumy)*(1-py);
		         kappa =( pa -  pe)/(1 - pe);
		         err = err / ntotal;
		         _type_=3;
		         output  _boostinfo;
	     run;		  
%mend;

%macro  logit(dsn, gini_cut, varname, ypred_L, ypred_R);
          data sumpred(keep=sumpred  w  ywork)	                 
		          _boostinfo(keep=kappa   err   _type_);
				  retain err  pa   psumy   py  0;
				  do until (eof2);
				      set   &dsn.(keep=y  w  &varname);
					  set   sumpred(keep=sumpred)  end=eof2  nobs=ntotal;
					  if &varname < &gini_cut then 
					      ypred=&ypred_L;
					  else 
					      ypred=&ypred_R;
					  sumpred = sumpred + ypred*0.5;

					  w = (exp(sumpred) + exp(-sumpred))**2;  
					  if w<constant('SMALL') then w = 2*constant('SMALL');
					  else if w>constant('BIG') then w = constant('BIG')/2; 

					  ywork = ( y - 1/(1 + exp(-2*sumpred)))*w;
					  w = 1 / w;					 

					  sumy = (sumpred > 0);
					  err + ((sumy = 1 & y ^=1)|(sumy ^=1 & y =1));
                      pa + ((sumy = 1 & y =1)|(sumy ^=1 & y ^=1));
                      py + (y=1);
                      psumy + (sumy=1);
					  output  sumpred ;					 
				 end;
		         pa=pa/ntotal;  psumy=psumy/ntotal; py = py/ ntotal;
                 pe = psumy*py + (1-psumy)*(1-py);
		         kappa =( pa -  pe)/(1 - pe);
		         err = err / ntotal;
		         _type_=4;
		         output  _boostinfo;
	     run;		  
%mend;

%macro dsboost(inputdsn, p, type, maxiter, outinfo);
%local dsn dsid  outdsn nobs ;

%let dsn=&inputdsn;
%let outdsn=&outinfo.x;
%let dsid=%sysfunc(open(&dsn));
%let nobs=%sysfunc(attrn(&dsid, NOBS));
%let dsid=%sysfunc(close(&dsid));

data sumpred(compress=binary);
     do i=1 to &nobs;	    sumpred=0;  drop i; output;	 end;
run;
data &dsn;
       set &dsn  end=eof;
	   retain  ybad  0;
	   %if  (&type eq 4) %then  %do;
              ywork = 4*y-1;  if y not in (1, 0) then ybad +1;
	   %end;
	   %else   %do;
              ywork = y;  if y not in (-1, 1) then ybad +1;
		%end;
		if eof then do;
		   put "Not corrected code response variable: "  ybad=;
		   if ybad>0 then call symput('_ERROR_', 2);   else call symput('_ERROR_', 0);
		end;
		drop ybad;	
run;
%if  (&type eq 4) %then %do;
   data &dsn;     set &dsn;  w=1/4; 	run;
%end;
%else %do;
   proc stdize data=&dsn  out=&dsn  method=sum  ;      var w;   run;
%end;
%if  (&type eq 1)  or (&type eq 2) %then %do;
       %do i=1 %to &maxiter;
             %stump_gini(&dsn, &p, &outdsn);             
              data _ginfo;
	                 set &outdsn.(firstobs=1  obs=1);
                     dsn="&dsn";
					 %if (&type eq 1) %then %do;  					      
                           call execute('%ada(' || compress(dsn)         || ','
					  %end;
					  %else %do;					      
                           call execute('%real(' || compress(dsn)         || ','
					  %end;
                                                         || compress(cut_val)    || ','
                                                         || compress(varname)  || ','
                                                         || compress(c_L)         || ','
                                                         || compress(c_R)         || ');'
					                       );
		               drop dsn;
               run;			 
	           data _ginfov/view=_ginfov;      set _ginfo; set _boostinfo;      run;
               %if &i=1 %then %do;
                     data &outinfo;     set _ginfov;     run;
	           %end;
	           %else %do;
	                 proc append base=&outinfo  data=_ginfov;   run;
	           %end;
	            proc stdize data=sumpred  out=sumpred   method=sum ;   var w;    run;
				data &dsn;    set &dsn.(drop=w);  set sumpred(keep=w);  run;
         %end;
%end;
%else %do;
          %do   i=1 %to &maxiter;
                   %stump_css(&dsn, &p,  &outdsn); 
                    data _ginfo;
	                       set &outdsn.(firstobs=1  obs=1);
                           dsn="&dsn";
	                       %if &type eq 1 %then %do;
                                 call execute('%gentl(' || compress(dsn)         || ','
					       %end;
					        %else %do;
                                 call execute('%logit(' || compress(dsn)         || ','
					        %end;
                                                               || compress(cut_val)    || ','
                                                               || compress(varname)  || ','
                                                               || compress(ypred_L)         || ','
                                                               || compress(ypred_R)         || ');'
					                              );
		               drop dsn;
					run;
	                data _ginfov/view=_ginfov;    set _ginfo; set _boostinfo;    run;
                    %if &i=1 %then %do;
                           data &outinfo;   set _ginfov;      run;
	                %end;
	                %else %do;
	                       proc append base=&outinfo  data=_ginfov;   run;
	                %end;
	                 proc stdize data=sumpred  out=sumpred   method=sum ;   var w;   run;
	                 data &dsn;
				            %if &type eq 4 %then %do;
	                              set &dsn.(drop=w ywork);  set sumpred(keep=w ywork);
					        %end;
					        %else %do;
                                 set &dsn.(drop=w );  set sumpred(keep=w);
					        %end;
	                 run;
		  %end;
%end;
%mend;
