proc fcmp outlib=sasuser.funcs.Erlc;   

     function ErlcFractionDelay(Nsrv, traffErlang); 
  /*
        Entry        : ErlcFractionDelay
     Purpose      : Compute the Probability of Delay when an arriving call is not
                    answered immediately and put in queue
     Usage        : P=ErlcFractionDelay(Nsrv, traffErlang)
     Inputs       : Nsrv=Number of Server (CSR)
                    traffErlang=Traffic in Erlang
     Handle Error : Return -1 if traffic in Erlang is larger than the number of 
                           servers;
                    Return -2 if either traffic in Erlang or Number of servers is 
                           negative;
                    Return -3 if number of servers is not integer;                          
  */
         if traffErlang>Nsrv then do;
            *put "Erlang amount should be smaller than Number of Servers";
   Ec=-1; return(Ec);
         end; 
   else if (traffErlang<0) or (Nsrv<0) then do;
            *put "Both Erlang amount and Number of Servers should be larger than 0";
   Ec=-2; return(Ec);
   end;
   else if abs(int(Nsrv)-Nsrv)>1E-8 then do;
            *put "Number of Servers should be integer";
   Ec=-3; return(Ec);
   end;  
         else do; 
            _p= PDF('POISSON', Nsrv, traffErlang);
         denom = _p
                  + (1-traffErlang/Nsrv)*CDF('POISSON', Nsrv-1, traffErlang);
            Ec = _p / denom;
            return (Ec);
   end;
     endsub;

  function ErlcGoS(Nsrv, traffErlang, AHT, AWT);
  /*
        Entry        : ErlcGoS
     Purpose      : Compute the Probability an arriving call will be put in
                    queue and wait for no more than AWT when the average
                    handling time is AHT. This term is also called Grade of 
                    Service (GoS). Note that the probability that an
                    arriving call will NOT be put in queue is:
                      P=1-ErlcFractionDelay();
     Usage        : GoS=ErlcGoS(Nsrv, traffErlang, AHT, AWT)
     Inputs       : Nsrv=Number of Server (CSR)
                    traffErlang=Traffic in Erlang
                    AHT=Average Handling Time
                    AWT=Average Waiting Time
     Handle Error : Return -1 if traffic in Erlang is larger than the number of 
                           servers;
                    Return -2 if either traffic in Erlang or Number of servers is 
                           negative;
                    Return -3 if number of servers is not integer;     
                       Return -4 if either of two time parameters is negative; 
  */
         if traffErlang>Nsrv then do;
            *put "Erlang amount should be smaller than Number of Servers";
      GoS=-1; return(GoS);
         end; 
   else if (traffErlang<0) or (Nsrv<0) then do;
            *put "Both Erlang amount and Number of Servers should be larger than 0";
   GoS=-2; return(GoS);
   end;
   else if  abs(int(Nsrv)-Nsrv)>1E-8 then do;
            *put "Number of Servers should be integer";
   GoS=-3; return(GoS);
   end;   
         else  if AHT<0 or AWT<0 then do;
            put "Both Average Holding Time and Average Waiting Time should be >0"; 
   GoS=-4; return(GoS);
   end;
   else do;
      GoS = 1-ErlcFractionDelay(Nsrv, traffErlang)*exp(-1*(Nsrv-traffErlang)*AWT/AHT);
      return(GoS);
    end;
  endsub;

  function ErlcNsrv(traffErlang, Erlc_max);
  /*
        Entry        : ErlcNsrv
     Purpose      : Compute the required number of servers (CSRs) to handle
                    given traffic load with queuing probability no more than
                    Erlc_max
     Usage        : N=ErlcNsrv(traffErlang, Erlc_max)
     Inputs       : traffErlang=Traffic load in Erlang
                    Erlc_max=max Queuing probability
     Handle Error : Return -1 if max queuing probability is not within (0, 1) 
                           range so to violate probability definition;
                    Return -2 if traffic in Erlang is negative;
  */

  /*REF:
       Algorithm is based on that from paper "Method for Fast 
             Estimation of Contact Centre Parameters Using 
             Erlang C Model" by Tibor Mišuth, Erik Chromý 
             and Ivan Baronák from Slovak University of 
             Technology. This paper appeared at "The Third 
             International Conference on Communication 
             Theory, Reliability, and Quality of Service", 
             2010
  */
        if Erlc_max>=1 or Erlc_max<=0 then do;
        *put "Max Erlang-C value should be between 0 and 1";
     N=-1; return(N);
     end;
     else if traffErlang<0 then do;
              *put "Traffic in Erlang should be >0";
     N=-2; return(N);
     end;
     else do;
           _k=floor(traffErlang)+1; s=0;
           do N=1 to _k; 
        s=(1+s)*N/traffErlang;
              end;
              _threshold=(1-Erlc_max)/((1-traffErlang/N)*Erlc_max);
              do while (s<=_threshold);
                 N=N+1;
        s=(1+s)*N/traffErlang;
        _threshold=(1-Erlc_max)/((1-traffErlang/N)*Erlc_max);
     end;
              return(N);
     end;
  endsub;

  function ErlcNsrvFromGoS( traffErlang, AHT, AWT, GoS);
  /*
        Entry        : ErlcNsrvFromGoS
     Purpose      : Compute the required number of servers (CSRs) to handle
                    given traffic load with given service level (GoS):
                           Average Handling Time /
                           the Probability an arriving call is put in queue
                           and wait no more than Average Waiting Time
     Usage        : N=ErlcNsrvFromGoS( traffErlang, AHT, AWT, GoS)
     Inputs       : traffErlang=Traffic load in Erlang
                    AHT=Average Handling Time
                    AWT=Average Waiting Time
                    GoS=probability an arriving Call in put in queue
                        but wait no more than AWT
     Handle Error : Return -1 if either time parameter is negative;
                    Return -2 if queuing probability is not in (0, 1) range;
                    Return -3 if traffic in Erlang is negative
  */
         if AHT<0 or AWT<0 then do;
            *put "Both Average Holding Time and Average Waiting Time should be >0"; 
      N0=-1; return(N0);
   end; 
   else if GoS<0 or GoS>1 then do;
      *put "GoS should be between 0 and 1";
   N0=-2; return(N0);
   end;
   else if traffErlang<0 then do;
            *put "Traffic in Erlang must be positive real number";
   N0=-3; return(N0);
   end;
         else do; 
            N0=floor(traffErlang)+1;
         _GoS=ErlcGoS(N0, traffErlang, AHT, AWT);
         do while (_GoS<=GoS);
            N0=N0+1;
            _GoS=ErlcGoS(N0, traffErlang, AHT, AWT);
         end;
         return(N0);
   end;
  endsub;

  function ErlcNsrvFromWait( traffErlang, AHT, AWT);
  /*
        Entry        : ErlcNsrvFromWait
     Purpose      : Compute the required number of servers (CSRs) to handle
                    given traffic load with given Average Handling Time and
                    no more than given Average Waiting Time
     Usage        : N=ErlcNsrvFromWait( traffErlang, AHT, AWT)
     Inputs       : traffErlang=Traffic load in Erlang
                    AHT=Average Handling Time
                    AWT=Average Waiting Time                    
     Handle Error : Return -1 if either time parameter is negative;                   
                    Return -2 if traffic in Erlang is negative
  */
         if AHT<0 or AWT<0 then do;
            *put "Both Average Holding Time and Average Waiting Time should be >0"; 
   Nsrv=-1; return(Nsrv);
   end;
   else if traffErlang<0 then do;
            *put "Traffic in Erlang must be positive real number";
   N0=-2; return(N0);
   end;
   else do;
           Nsrv=floor(traffErlang)+1;
           c=ErlcWait (Nsrv, traffErlang, AHT);
           do while(c>AWT);
                 Nsrv=Nsrv+1;
           c=ErlcWait (Nsrv, traffErlang, AHT);;
            end;
            return (Nsrv);
   end;
  endsub;

  function ErlcNwaiting(Nsrv, traffErlang);
  /*
        Entry        : ErlcNwaiting
     Purpose      : Compute the average number of calls waiting 
                    in queue, given number of servers and traffic
                    in Erlang
     Usage        : N=ErlcNwaiting(Nsrv, traffErlang)
     Inputs       : Nsrv=Number of Server
                    traffErlang=Traffic load in Erlang       
     Handle Error : Return -1 if traffic in Erlang is larger than number
                              of servers;                   
                    Return -2 if number of servers is negative
                    Return -3 if traffic in Erlang is negative
                    Return -4 if number of servers is not integer
  */
         if traffErlang>Nsrv then do;
            *put "Erlang amount should be smaller than Number of Servers";
   Q=-1; return(Q);
         end; 
   else if (Nsrv<0) then do;
            *put "Both Erlang amount and Number of Servers should be larger than 0";
   Q=-2; return(Q);
   end;
   else if traffErlang<0 then do;
            *put "Traffic in Erlang must be positive real number";
   Q=-3; return(Q);
   end;
   else if  abs(int(Nsrv)-Nsrv)>1E-8 then do;
            *put "Number of Servers should be integer";
   Q=-4; return(Q);
   end;  
         else do; 
            Q=traffErlang/(Nsrv-traffErlang)*ErlcFractionDelay(Nsrv, traffErlang);
      return(Q);
   end;
  endsub;

  function ErlcTrafFromFractionDelay(Nsrv, fractionDelay);
  /*
        Entry        : ErlcTrafFromFractionDelay
     Purpose      : Compute the max traffic load that N servers
                    can handle with queuing probability less or equal
                    to fractionDelay. 
     Usage        : A=ErlcTrafFromFractionDelay(Nsrv, fractionDelay)
     Inputs       : Nsrv=Number of Server
                    fractionDelay=Probability of waiting in queue
                                  when a call arrives                    
     Handle Error : Return -1 if probability of waiting in Queue is not
                              in range of (0, 1) 
                    Return -2 if number of servers is negative                   
                    Return -3 if number of servers is not integer
  */
  /*  NOTE:
         Using Bisection Method to solve for Erlang 
      given N server and Erlang C number. Bisection 
      is simple and robust. Speed is not of primiary
      concern here.
  */
            if fractionDelay>1 or fractionDelay<0  then do;
                *put "Number of servers must be positive integers";
    c=-1; return(c);
    end;
    else if Nsrv<=0 then do;  
                *put "Fraction of Delay must be real number between 0 and 1";
    c=-2; return(c);
    end;
    else if abs(int(Nsrv)-Nsrv)>1E-8 then do;;
       c=-3; return(c);
    end;
    else do;
                min=1E-8; max=Nsrv-1E-8;
             itermax=500; iter=1;
             do while (iter<=itermax);
                   c=(min+max)/2;
             _x=ErlcFractionDelay(Nsrv, c)-fractionDelay;
             if abs(_x)<1E-8 or abs(max-min)/2<1E-8 then do;                    
                iter=itermax+1;
             end;
             else do;
                      iter=iter+1;
       _tmp=ErlcFractionDelay(Nsrv, max)-fractionDelay;
                if sign(_x)=sign(_tmp) then max=c;
                      else min=c;
             end;
            end;      
      return (c);
   end;
  endsub;

  function ErlcWait (Nsrv, traffErlang, AHT);
  /*
        Entry        : ErlcWait
     Purpose      : Compute the Average Speed of Answer (ASA, also called
                    Average Waiting Time, AWT), given the number of servers, 
                    traffic in Erlang and average handling time. 
     Usage        : N=ErlcWait(Nsrv, traffErlang, AHT)
     Inputs       : Nsrv=Number of Server
                    traffErlang=Traffic load in Erlang       
                    AHT=Average Handling Time
     Handle Error : Return -1 if Average Handling Time is negative           
                    Return -2 if number of servers is negative
                    Return -3 if traffic in Erlang is negative
                    Return -4 if number of servers is not integer
                    Return -5 if traffic in Erlang is larger than the
                           number of servers
  */
      if AHT<0 then do;
            *put "Average Holding Time should be >0"; 
      ASA=-1; return(ASA);
   end;
   else if (Nsrv<0) then do;
            *put "Both Erlang amount and Number of Servers should be larger than 0";
   ASA=-2; return(ASA);
   end;
   else if traffErlang<0 then do;
            *put "Traffic in Erlang must be positive real number";
   ASA=-3; return(ASA);
   end;
   else if  abs(int(Nsrv)-Nsrv)>1E-8 then do;
            *put "Number of Servers should be integer";
   ASA=-4; return(ASA);
   end; 
   else if traffErlang>Nsrv then do;
      ASA=-5; return(ASA);
   end;  
   else do;
            ASA=ErlcFractionDelay(Nsrv, traffErlang)*AHT/(Nsrv-traffErlang);
      return(ASA);
   end;
  endsub;

  function ErlcK (Nsrv, traffErlang);
  /*
        Entry        : ErlcK
     Purpose      : Compute the average number of calls in queue, given 
                    number of servers and traffic in Erlang. It is the 
                    sum of average number of calls being served (traffErlang) 
                    and average number of calls waiting in queue.
     Usage        : N=ErlcK(Nsrv, traffErlang)
     Inputs       : Nsrv=Number of Server
                    traffErlang=Traffic load in Erlang       
     Handle Error : Return -1 if traffic in Erlang is larger than number
                              of servers;                   
                    Return -2 if number of servers is negative
                    Return -3 if traffic in Erlang is negative
                    Return -4 if number of servers is not integer
  */
        if traffErlang>Nsrv then do;
            *put "Erlang amount should be smaller than Number of Servers";
   K=-1; return(K);
         end; 
      else if (Nsrv<0) then do;
            *put "Both Erlang amount and Number of Servers should be larger than 0";
   K=-2; return(K);
   end;
   else if traffErlang<0 then do;
            *put "Traffic in Erlang must be positive real number";
   K=-3; return(K);
   end;
   else if  abs(int(Nsrv)-Nsrv)>1E-8 then do;
            *put "Number of Servers should be integer";
   K=-4; return(K);
   end;  
   else do;
            K = traffErlang*(1+ ErlcFractionDelay(Nsrv, traffErlang)/(Nsrv-traffErlang));
      return (K);
   end;
  endsub;

  function ErlcT( Nsrv, traffErlang, AHT);
  /*
        Entry        : ErlcT
     Purpose      : Compute the average time a call will spend in the system,
                    which is the sum of average handling time and average 
                       waiting time (ASA). Three relavent parameters are
                    number of servers, traffic in Erlang and average handling
                    time. 
     Usage        : N=ErlcT(Nsrv, traffErlang, AHT)
     Inputs       : Nsrv=Number of Server
                    traffErlang=Traffic load in Erlang   
                       AHT=Average Handling Time 
     Handle Error : Return -1 if average handling time is negative;                   
                    Return -2 if number of servers is negative
                    Return -3 if traffic in Erlang is negative
                    Return -4 if number of servers is not integer
                    Return -5 if traffic in Erlang is larger than number 
                           of servers
  */
      if AHT<0 then do;
            *put "Average Holding Time should be >0"; 
      T=-1; return(T);
   end;
   else if (Nsrv<0) then do;
            *put "Both Erlang amount and Number of Servers should be larger than 0";
   T=-2; return(T);
   end;
   else if traffErlang<0 then do;
            *put "Traffic in Erlang must be positive real number";
   T=-3; return(T);
   end;
   else if  abs(int(Nsrv)-Nsrv)>1E-8 then do;
            *put "Number of Servers should be integer";
   T=-4; return(T);
   end; 
   else if traffErlang>Nsrv then do;
      T=-5; return(T);
   end;
         else do; 
            T=AHT + AHT*ErlcFractionDelay(Nsrv, traffErlang)/((Nsrv-traffErlang));
      return(T);
   end;
  endsub;

run;
options cmplib=sasuser.funcs;

/* Test Computing results against:
          Tibor Mišuth, Erik Chromý, Ivan Baroňák. : 
               Method for Fast Estimation of Contact Centre Parameters Using Erlang-C Model, 
               in Proceedings of 2010 Third International Conference on Communication Theory, Reliability, and Quality of Service, 
               DOI 10.1109/CTRQ.2010.38
*/
data _null_;
     lambda=667; AHT=150; AWT=20;
  A=lambda*150/3600;
  
  do j=1 to 10;
     N=floor(A)+j;
     Pc=ErlcFractionDelay(N, A);
  GoS=ErlcGoS(N, A, AHT, AWT); 
        Q=ErlcNwaiting(N, A);
        T=ErlcT(N, A, AHT); 
  K=ErlcK(N, A);
  rho=A/N;  
  put N=  @8  Pc= percent.4  @16 K= bestd8.1    @26  T= bestd8.1 
                @38 Q= bestd8.1    @50 Gos= bestd8.1  @62  rho= percent8.4;
  end;
run;
