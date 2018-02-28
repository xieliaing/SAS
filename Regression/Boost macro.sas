

%macro stump_dct(dsn, p, outdsn);
/************************************************/
/* Binary split for AdaBoost & RealBoost        */
/* dsn: Name of input SAS data sets. All        */
/*        independent variables should be named */
/*        as X1, X2,....,Xp and be continous    */
/*        numeric variables                     */
/*   p: Number of independent variables         */
/************************************************/
options nonotes;
%local i  ysum ntotal  factor;
%do i=1 %to &p;
proc sort data=&dsn.(keep=y w x&i)  out=work.sort; by x&i; run;

/* everytime, weight is sum upto unit */
data _null_;
     set work.sort  end=eof  nobs=ntotal;
	 retain _wsumint  _wsumdec
            _ysumintpos  _ysumdecpos
            _ysumintneg  _ysumdecneg
     ;	 
	 _w2=w*10; _w2int=int(_w2); _w2dec=_w2-_w2int;
	 _wsumint+_w2int; _wsumdec+ _w2dec; 
	 if y>0 then do;
        _ysumintpos+ y*_w2int; _ysumdecpos + y*_w2dec;
	 end;
	 else do;
	    _ysumintneg+ y*_w2int; _ysumdecneg + y*_w2dec;
	 end;
	 if _wsumdec>1 then do;
	    _wsumint+1; _wsumdec-1;
	 end;
	 if _ysumdecpos>1 then do;	  
	    _ysumintpos+1; _ysumdecpos-1;
	 end;
	 if _ysumdecneg<-1 then do;
	    _ysumintneg-1; _ysumdecneg+1;
	 end;

	 if eof then do;
	    _wsum=(_wsumint+_wsumdec)/10;
		_ysum=((_ysumintpos+_ysumintneg)+(_ysumdecpos+_ysumdecneg))/10;
	    factor=1/_wsum;
	    call symput('ysum', compress(_ysum*factor));
		call symput('ntotal', compress(ntotal));
		call symput('factor', compress(factor));
	 end;
run;

%put factor=&factor;
%put ysum=&ysum;

data _giniout&i;
     array _p[2] _temporary_;
     array _g[2] _temporary_;
     array _x[1] _temporary_;
     retain ginimin   xmin  _y1  _oldx _cn ;
     if _n_=1 then do;
        ginimin=2; xmin=x&i; _y1=0; _oldx=0; _cn=0;
     end;    
     do while (^eof);
        set work.sort  end=eof;
        _y1+y*w*&factor; _cn=_cn+w*&factor;
        if _cn=0 then _p[1]=0; else _p[1]=_y1/_cn; 
        if _cn=&ntotal then _p[2]=0; else _p[2]=(&ysum-_y1)/(&ntotal-_cn);
        ppn1=_cn/&ntotal;  ppn2=1-ppn1;
        _g[1]=2*(ppn1*(1-_p[1])*_p[1]+ppn2*(1-_p[2])*_p[2]);
        if _n_=1 then _g[2]=x&i; else _g[2]=(_oldx+x&i)/2;
        _oldx=x&i;
        if _g[1]<ginimin then do;
           ginimin=_g[1]; xmin=_g[2];      
        end;
     end;
     if ginimin>2 then do; 
        ginimin=2; xmin=(_oldx+x&i)/2; 
     end; 
     varname="x&i";
     keep varname  ginimin  xmin;
     output;
     stop;
run;
%end;
data outsplit;
     set %do i=1 %to &p;
            _giniout&i
         %end;;
run;
proc datasets library=work nolist;
     delete _giniout:;
quit;
option notes;    
%mend;




%macro stump_reg(dsn, p, outdsn);
/************************************************/
/* Binary split for AdaBoost & RealBoost        */
/* dsn: Name of input SAS data sets. All        */
/*        independent variables should be named */
/*        as X1, X2,....,Xp and be continous    */
/*        numeric variables                     */
/*   p: Number of independent variables         */
/************************************************/
options nonotes;
%local i  ysum ntotal  factor  dsid;
%do i=1 %to &p;
proc sort data=&dsn.(keep=y w x&i)  out=work.sort; by x&i; run;

%let dsid=%sysfunc(open(&dsn));
%let nobs=%sysfunc(attrn(&dsid, nobs));
%let dsid=%sysfunc(close(&dsid));

/* everytime, weight is sum upto unit */
data _iterout&i;         
	 retain _wsumint  _wsumdec
            _ywsumintpos  _ywsumdecpos
            _ywsumintneg  _ywsumdecneg
			_yw2sumintpos  _yw2sumdecpos
			_yw2sumintneg  _yw2sumdecneg
			0
     ;
	 array _M{&nobs, 3} _temporary_;
	 do j=1 to &nobs;
         set work.sort  end=eof1  nobs=ntotal; 
	     _w2=w*10; _w2int=int(_w2); _w2dec=_w2-_w2int;
	     _wsumint+_w2int; _wsumdec+ _w2dec; 
	     _ws=w**2*10; _wsint=int(_ws); _wsdec=_ws-_wsint;

	     if y>0 then do;
            _ywsumintpos+ y*_w2int; _ywsumdecpos + y*_w2dec;
		    _yw2sumintpos+ y*_wsint; _yw2sumdecpos + y*_wsdec;
	     end;
	     else do;
	        _ywsumintneg+ y*_w2int; _ywsumdecneg + y*_w2dec;
            _yw2sumintneg+ y*_wsint; _yw2sumdecneg + y*_wsdec;
	     end;
	     if _wsumdec>1 then do;
	        _wsumint+1; _wsumdec=_wsumdec-1;
	     end;
	     if _ywsumdecpos>1 then do;	  
	        _ywsumintpos+1; _ywsumdecpos=_ywsumdecpos-1;
	     end;
	     if _ywsumdecneg<-1 then do;
	        _ywsumintneg=_ywsumintneg-1; _ywsumdecneg=_ywsumdecneg+1;
	     end;
	     if _yw2sumdecpos>1 then do;
           _yw2sumintpos+1; _yw2sumdecpos=_yw2sumdecpos-1;
	     end;
	     if _yw2sumdecneg<-1 then do;
            _yw2sumintpos=_yw2sumintpos-1; _yw2sumdecpos=_yw2sumdecpos+1;
	     end;
         _wsum=(_wsumint+_wsumdec)/10;
		 _ywsum=((_ywsumintpos+_ywsumintneg)+(_ywsumdecpos+_ywsumdecneg))/10;
         _yw2sum=((_yw2sumintpos+_yw2sumintneg)+(_yw2sumdecpos+_yw2sumdecneg))/10;
         _M[j, 1]=_ywsum; _M[j, 2]=_wsum; _M[j, 3]=_yw2sum;
	 end;
	 
	 if eof1 then do;	        
	        put _wsum=   _ywsum=  _yw2sum=;
	 end;
	 
     criteria=constant('BIG'); mini_j=1; cut_point=0;
	 do j=1 to &nobs;
          css=_M[j, 1]**2/_M[j, 2] - 
                 2*_M[j, 1]/_M[j, 2]*_M[j,3] + 
                 (_yw2sum-_M[j,1])**2/(1-_M[j,2]) - 
                 2*(_ywsum-_M[j,1])/(1-_M[j,2])*(_yw2sum-_M[j,3])
                 ;
	      if criteria>css then do;
              criteria=css; mini_j=j;
		  end;
	 end;
	 put _M[1, 1]=  _M[int(&nobs/2), 1]=  _M[&nobs, 1]=;

	 ypred_L = _M[mini_j, 1]/_M[mini_j, 2];
	 ypred_R = (_ywsum - _M[mini_j, 1])/ (1 - _M[mini_j, 2]);

	 if mini_j>1 then mini_j=mini_j-1;
	 else mini_j=1;
	 
	 do _j=mini_j to mini_j+1;
	     set work.sort  point=_j;
		 cut_point+x&i;
	end;
	cut_point=cut_point/2;
	
    varname="x&i";
	keep varname  cut_point  criteria  ypred_L  ypred_R;
	output _iterout&i;  
run;
%end;

data outsplit;
     set %do i=1 %to &p;
            _iterout&i
         %end;;
run;
data &outdsn;
       set outsplit;
	   if _n_=1 then output;
	   else stop;
run;
proc sort data=outsplit; by criteria; run;

proc datasets library=work nolist;
     delete _iterout:;
quit;

data _null_;
       set &outdsn;
	   call execute('data pred;');
	   call execute(cat("set &dsn.(keep=", varname, " y w) end=eof;"));
	   call execute("retain _wsumint  _wsumdec 0;");
	   call execute("array _M[&nobs, 2]  _temporary_;");
       call execute(cat("if ", varname, "< ", cut_point, " then ypred=", ypred_L, ";"));
       call execute(cat("else ", "ypred=", ypred_R, ";"));
	   call execute("w=w*exp(-((2*y-1)*ypred));");
	   call execute("_wsumint+int(w); _wsumdec+(w-int(w));");
	   call execute("if _wsumdec>1 then do; _wsumint+1; _wsumdec=_wsumdec-1; end;");
       call execute("_M[_n_, 1]=ypred; _M[_n_, 2]=w;");
	   call execute("if eof then do;");
	   call execute("   _wsum=_wsumint+_wsumdec;");
	   call execute("   do j=1 to &nobs;");
	   call execute("       _M[_n_, 2]=_M[_n_, 2]/_wsum;");
	   call execute("       pred=_M[_n_, 1];  w=_M[_n_, 2]; keep pred w;");
	   call execute("       output;");
	   call execute("   end;");
	   call execute("end;");
       call execute('run;');
run;
options notes;
%mend;




data _temp;
     array _X{*}  X1-X10;
     do ID=1 to 10000; 
	    _z=0;
	    do j=1 to dim(_X); _X[j]=rannor(978766); _z+_X[j]**2; end;
		if _z>4 then Y=0; else Y=1;
		w=1/10000;
		output;
		drop _z j;
	 end;
run;

%stump_reg(_temp, 10, _out);

