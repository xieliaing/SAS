/*
  Project ICPSR Study No. 13511,
  CENSUS OF POPULATION AND HOUSING, 2000 [UNITED STATES]:
  PUBLIC USE MICRODATA SAMPLE 1%
  
  Functions of following Macroes:
  Macro Count_ph: This Macro counts the number of household
                  record observations and the number of personal
                  record observations in a given dataset, as well
                  as the max number of individuals in a single 
                  household, which is entitled as ctsp.
  Macro Tag_h:    This Macro gives identification number for each 
                  household record and associated personal record.
                  Therefore household record and personal record 
                  belonging to the same household are identified as
                  the same group.
  Macro movedata: This Macro moves personal data which was vertically 
                  placed downwards in the original data set to the 
                  same line as associated household record, with the 
                  number of persons in a single household set to be
                  &ctsp which is the the max number of individuals 
                  in a single household. The final data set is a 
                  balanced panel data with each line corresponding to
                  records associated with a single household.


  Copyright(c) 2004 by Liang, Xie;
  Department of Economics/Data Office, Bartle Main Library 
  Binghamton University 
  State University of New York
*/
libname xdata "C:\Documents and Settings\Liang Xie\My Documents\My Project\Decker Nursing School Data Project";

data xdata.varh;
  input hname $;
datalines;
SERIALNO
SAMPLE
STATE
REGION
DIVISION
PUMA1
MSACMSA1
MSAPMSA1
AREATYP1
TOTPUMA1
LNDPUMA1
SUBSAMPL
HWEIGHT
PERSONS
UNITTYPE
HSUB
HAUG
VACSTAT
VACSTATA
TENURE
TENUREA
BLDGSZ
BLDGSZA
YRBUILT
YRBUILTA
YRMOVED
YRMOVEDA
ROOMS
ROOMSA
BEDRMS
BEDRMSA
CPLUMB
CPLUMBA
CKITCH
CKITCHA
PHONE
PHONEA
FUEL
FUELA
VEHICL
VEHICLA
BUSINES
BUSINESA
ACRES
ACRESA
AGSALES
AGSALESA
ELEC
ELECA
GAS
GASA
WATER
WATERA
OIL
OILA
RENT
RENTA
MEALS
MEALSA
MORTG1
MORTG1A
MRT1AMT
MRT1AMTA
MORTG2
MORTG2A
MRT2AMT
MRT2AMTA
TAXINCL
TAXINCLA
TAXAMT
TAXAMTA
INSINCL
INSINCLA
INSAMT
INSAMTA
CONDFEE
CONDFEEA
VALUE
VALUEA
MHLOAN
MHLOANA
MHCOST
MHCOSTA
HHT
P65
P18
NPF
NOC
NRC
PSF
PAOC
PARC
SVAL
SMOC
SMOCAPI
SRNT
GRENT
GRAPI
FNF
HHL
LNGI
WIF
EMPSTAT
WORKEXP
HINC
FINC
;
run;

data xdata.varp;
  input pname $;
datalines;
SERIALNO
PNUM
PAUG
DDP
PWEIGHT
RELATE
RELATEA
OC
RC
PAOCF
SEX
SEXA
AGE
AGEA
HISPAN
HISPANA
NUMRACE
WHITE
BLACK
AIAN
ASIAN
NHPI
OTHER
RACE1
RACE3
RACE3
RACEA
MARSTAT
MARSTATA
MSP
SFN
SFREL
ENROLL
ENROLLA
GRADE
GRADEA
EDUC
EDUCA
ANCFRST1
ANCSCND1
ANCA
ANCR
SPEAK
SPEAKA
LANG1
LANGA
ENGABIL
ENGABILA
POB1
POBA
CITIZEN
CITIZENA
YR2US
YR2USA
MOB
MOBA
MIGST1
MIGSTA
MIGPUMA1
MIGAREA1
MIGCMA1
MIGPMA1
SENSORY
SENSORYA
PHYSCL
PHYSCLA
MENTAL
MENTALA
SLFCARE
SLFCAREA
ABGO
ABGOA
ABWORK
ABWORKA
DISABLE
GRANDC
GRANDCA
RSPNSBL
RSPNSBLA
HOWLONG
HOWLONGA
MILTARY
MILTARYA
VPS1
VPS2
VPS3
VPS4
VPS5
VPS6
VPS7
VPS8
VPS9
VPSA
MILYRS
MILYRSA
VPSR
ESR
ESRA
ESP
POWST1
POWSTA
POWPUMA1
POWAREA1
POWCMA1
POWPMA1
TRVMNS
TRVMNSA
CARPOOL
CARPOOLA
LVTIME
LVTIMEA
TRVTIME
TRVTIMEA
LAYOFF
ABSENT
RECALL
LOOKWRK
BACKWRK
LASTWRK
LASTWRKA
INDCEN
INDCENA
INDNAICS
OCCCEN1
OCCCENA
OCCSOC1
CLWKR
CLWKRA
WRKLYR
WRKLYRA
WEEKS
WEEKSA
HOURS
HOURSA
INCWS
INCWSA
INCSE
INCSEA
INCINT
INCINTA
INCSS
INCSSA
INCSSI
INCSSIA
INCPA
INCPAA
INCRET
INCRETA
INCOTH
INCOTHA
INCTOT
INCTOTA
EARNS
POVERTY
;
run;

/***************************************************
  this macro index the whole dataset for further
  manipulation.
***************************************************/
%macro indexdata(dataset);
%global numobs;
%let dsn=&dataset;
%let dsid=%sysfunc(open(&dsn));
%if &dsid^=0 %then %do;
  %let dsid=%sysfunc(close(&dsid));
  data &dsn;
    set &dsn;
    index=_n_;
    call symput('numobs',_n_);
  run;
%end;
%else %let dsid=%sysfunc(close(&dsid));
%mend;


/***************************************************
  this macro counts the max P valued observations
  of RecType under one household indicated as H.
***************************************************/
%macro count_ph(dataset);
%global ctsp totalH totalP;
%local ctspbuff;
%let dsn=&dataset;
%let dsid=%sysfunc(open(&dsn,i));
%let totalH=0;
%let totalP=0;
%let soleH=0;
%let ctsp=0;
%let ctspbuff=0;

%syscall set(dsid); 
%if &dsid=0 %then
   %put Data Set Open Failed;
%else %do;
  %let obsid=%sysfunc(fetch(&dsid));
  %let rec=%sysfunc(rewind(&dsid));
  %do %while (%sysfunc(fetch(&dsid))=0);
     %if &rectype=H %then %do;
       %let ctsp=0;
       %let totalH=%eval(&totalH+1);
     %end; /* if rectype= H */
     %else %if &rectype=P %then %do;
       %let totalP=%eval(&totalP+1);
       %let ctsp=%eval(&ctsp+1);
       %if &ctsp>&ctspbuff %then 
         %let ctspbuff=&ctsp;
     %end; /* else if rectype = P*/
  %end; /* end of do_while section*/ 

  %let ctsp=&ctspbuff;   
  %let rc=%sysfunc(close(&dsid)); 
%end; /*end of else do*/
%mend count_ph;


/***************************************************
  this macro tag all observations under one household
  and assign them a unique indentification number for
  further re-grouping. 
***************************************************/
%macro tag_h(dataset);
%let dsn=&dataset;
data &dsn;
   set &dsn;
   if _n_=1 then do;
      call symput('ih',1);
	  hid=symget('ih');
	  call symput('buff',hid);
   end;
   else if rectype='H' then do;
      hid=symget('buff');
      call symput('ih',hid+1);
	  hid=symget('ih');
	  call symput('buff',hid);
   end;
   else if rectype='P' then
      hid=symget('ih');
run;
%mend tag_h;

/*main frame*/
option nonotes;

%macro movedata(dataset);

data _null_;
  set xdata.varh;
  call symput(compress('hname'||_n_),hname);
  call symput('numh',_n_);
run;
data _null_;
  set xdata.varp;
  call symput(compress('pname'||_n_),pname);
  call symput('nump',_n_);
run;/*testified*/

data xdata.temp;
 x='a';
run;
data xdata.temp2;
 x='a';
run;
%let dsn=&dataset;
%let dsid=%sysfunc(open(&dsn));
%syscall set(dsid);
%if &dsid=0 %then 
    %put Data Set open failed!;
%else %do;
%let obsid=%sysfunc(fetch(&dsid));
%let rc=%sysfunc(rewind(&dsid));
%let nobs=%sysfunc(attrn(&dsid,NOBS));
/*%let nobs=15;*/
%put num of obs in xdata.pums2 is &nobs 
     rewind record is &rc    
     open file record is &dsid;

%let pid=0;

%do %while(%sysfunc(fetch(&dsid))=0);
 %put Current group is &hid;
 %if (&hid=1 AND &rectype=H) %then %do; /*first obs must be H-type*/
  %put 1st group obs, #of persons in this household is &persons; 
   /*%fetch_h(pid=&pid);*/
  %let pid=0;
  %let n=0;
  %do %until(&n=&numh);
    %let n=%eval(&n+1);
    %let firstpart=&&hname&n;
    %let nametemp=&firstpart;
    %let varval&n=&&&firstpart;/* this can get right value */
    %let name&n=&nametemp&pid;  
/*    %put fetch H values: &varval &name1;*/
  %end;/*do until*/
   data xdata.temp;
      set xdata.temp;
      &name1=compress(symget('varval1')); 
      &name2=compress(symget('varval2')); 
      &name3=compress(symget('varval3')); 
      &name4=compress(symget('varval4')); 
      &name5=compress(symget('varval5')); 
      &name6=compress(symget('varval6')); 
      &name7=compress(symget('varval7')); 
      &name8=compress(symget('varval8')); 
      &name9=compress(symget('varval9')); 
      &name10=compress(symget('varval10')); 
      &name11=compress(symget('varval11')); 
      &name12=compress(symget('varval12')); 
      &name13=compress(symget('varval13')); 
      &name14=compress(symget('varval14')); 
      &name15=compress(symget('varval15')); 
      &name16=compress(symget('varval16')); 
      &name17=compress(symget('varval17')); 
      &name18=compress(symget('varval18')); 
      &name19=compress(symget('varval19')); 
      &name20=compress(symget('varval20')); 
      &name21=compress(symget('varval21')); 
      &name22=compress(symget('varval22')); 
      &name23=compress(symget('varval23')); 
 	  &name24=compress(symget('varval24')); 
	  &name25=compress(symget('varval25')); 
	  &name26=compress(symget('varval26')); 
      &name27=compress(symget('varval27')); 
      &name28=compress(symget('varval28')); 
      &name29=compress(symget('varval29')); 
      &name30=compress(symget('varval30')); 
      &name31=compress(symget('varval31')); 
      &name32=compress(symget('varval32')); 
      &name33=compress(symget('varval33')); 
      &name34=compress(symget('varval34')); 
      &name35=compress(symget('varval35')); 
      &name36=compress(symget('varval36')); 
      &name37=compress(symget('varval37')); 
      &name38=compress(symget('varval38')); 
      &name39=compress(symget('varval39')); 
      &name40=compress(symget('varval40')); 
      &name41=compress(symget('varval41')); 
      &name42=compress(symget('varval42')); 
      &name43=compress(symget('varval43')); 
      &name44=compress(symget('varval44')); 
      &name45=compress(symget('varval45')); 
      &name46=compress(symget('varval46')); 
      &name47=compress(symget('varval47')); 
      &name48=compress(symget('varval48')); 
      &name49=compress(symget('varval49')); 
      &name50=compress(symget('varval50')); 
      &name51=compress(symget('varval51')); 
      &name52=compress(symget('varval52')); 
      &name53=compress(symget('varval53')); 
      &name54=compress(symget('varval54')); 
      &name55=compress(symget('varval55')); 
      &name56=compress(symget('varval56')); 
      &name57=compress(symget('varval57')); 
      &name58=compress(symget('varval58')); 
      &name59=compress(symget('varval59')); 
      &name60=compress(symget('varval60')); 
      &name61=compress(symget('varval61')); 
      &name62=compress(symget('varval62')); 
      &name63=compress(symget('varval63')); 
      &name64=compress(symget('varval64')); 
      &name65=compress(symget('varval65')); 
      &name66=compress(symget('varval66')); 
      &name67=compress(symget('varval67')); 
      &name68=compress(symget('varval68')); 
      &name69=compress(symget('varval69')); 
      &name70=compress(symget('varval70')); 
      &name71=compress(symget('varval71')); 
      &name72=compress(symget('varval72')); 
      &name73=compress(symget('varval73')); 
      &name74=compress(symget('varval74')); 
      &name75=compress(symget('varval75')); 
      &name76=compress(symget('varval76')); 
      &name77=compress(symget('varval77')); 
      &name78=compress(symget('varval78')); 
      &name79=compress(symget('varval79')); 
      &name80=compress(symget('varval80')); 
      &name81=compress(symget('varval81')); 
      &name82=compress(symget('varval82')); 
      &name83=compress(symget('varval83')); 
      &name84=compress(symget('varval84')); 
      &name85=compress(symget('varval85')); 
      &name86=compress(symget('varval86')); 
      &name87=compress(symget('varval87')); 
      &name88=compress(symget('varval88')); 
      &name89=compress(symget('varval89')); 
      &name90=compress(symget('varval90')); 
      &name91=compress(symget('varval91')); 
      &name92=compress(symget('varval92')); 
      &name93=compress(symget('varval93')); 
      &name94=compress(symget('varval94')); 
      &name95=compress(symget('varval95')); 
      &name96=compress(symget('varval96')); 
      &name97=compress(symget('varval97')); 
      &name98=compress(symget('varval98')); 
      &name99=compress(symget('varval99')); 
      &name100=compress(symget('varval100')); 
      &name101=compress(symget('varval101')); 
      &name102=compress(symget('varval102')); 
      &name103=compress(symget('varval103')); 
      &name104=compress(symget('varval104')); 
      &name105=compress(symget('varval105')); 
      &name106=compress(symget('varval106')); 
    run;
  %let pid=1;
 %end;
 %else %if &rectype=H %then %do; /*arrive at a new group*/
  %if &pid<&ctsp %then %do;
    %do %until(&pid=%eval(&ctsp+1));  /*fill missing value to unbalanced PIDs*/
      /*%fill_p(pid=&pid);*/
	  %let n=0;
      %do %until(&n=&nump);
         %let n=%eval(&n+1);
         %let firstpart=&&pname&n;
         %let nametemp=&firstpart;
		 %let varval&n=.;
         %let name&n=&nametemp&pid;  
/*         %put fill missing values: &name1;*/
      %end;/*do until*/
      data xdata.temp;
        set xdata.temp;
        &name1=compress(symget('varval1'));
        &name2=compress(symget('varval2'));
        &name3=compress(symget('varval3'));
        &name4=compress(symget('varval4'));
        &name5=compress(symget('varval5'));
        &name6=compress(symget('varval6'));
        &name7=compress(symget('varval7'));
        &name8=compress(symget('varval8'));
        &name9=compress(symget('varval9'));
        &name10=compress(symget('varval10'));
        &name11=compress(symget('varval11'));
        &name12=compress(symget('varval12'));
        &name13=compress(symget('varval13'));
        &name14=compress(symget('varval14'));
        &name15=compress(symget('varval15'));
        &name16=compress(symget('varval16'));
        &name17=compress(symget('varval17'));
        &name18=compress(symget('varval18'));
        &name19=compress(symget('varval19'));
        &name20=compress(symget('varval20'));
        &name21=compress(symget('varval21'));
        &name22=compress(symget('varval22'));
        &name23=compress(symget('varval23'));
        &name24=compress(symget('varval24'));
        &name25=compress(symget('varval25'));
        &name26=compress(symget('varval26'));
        &name27=compress(symget('varval27'));
        &name28=compress(symget('varval28'));
        &name29=compress(symget('varval29'));
        &name30=compress(symget('varval30'));
        &name31=compress(symget('varval31'));
        &name32=compress(symget('varval32'));
        &name33=compress(symget('varval33'));
        &name34=compress(symget('varval34'));
        &name35=compress(symget('varval35'));
        &name36=compress(symget('varval36'));
        &name37=compress(symget('varval37'));
        &name38=compress(symget('varval38'));
        &name39=compress(symget('varval39'));
        &name40=compress(symget('varval40'));
        &name41=compress(symget('varval41'));
        &name42=compress(symget('varval42'));
        &name43=compress(symget('varval43'));
        &name44=compress(symget('varval44'));
        &name45=compress(symget('varval45'));
        &name46=compress(symget('varval46'));
        &name47=compress(symget('varval47'));
        &name48=compress(symget('varval48'));
        &name49=compress(symget('varval49'));
        &name50=compress(symget('varval50'));
        &name51=compress(symget('varval51'));
        &name52=compress(symget('varval52'));
        &name53=compress(symget('varval53'));
        &name54=compress(symget('varval54'));
        &name55=compress(symget('varval55'));
        &name56=compress(symget('varval56'));
        &name57=compress(symget('varval57'));
        &name58=compress(symget('varval58'));
        &name59=compress(symget('varval59'));
        &name60=compress(symget('varval60'));
        &name61=compress(symget('varval61'));
        &name62=compress(symget('varval62'));
        &name63=compress(symget('varval63'));
        &name64=compress(symget('varval64'));
        &name65=compress(symget('varval65'));
        &name66=compress(symget('varval66'));
        &name67=compress(symget('varval67'));
        &name68=compress(symget('varval68'));
        &name69=compress(symget('varval69'));
        &name70=compress(symget('varval70'));
        &name71=compress(symget('varval71'));
        &name72=compress(symget('varval72'));
        &name73=compress(symget('varval73'));
        &name74=compress(symget('varval74'));
        &name75=compress(symget('varval75'));
        &name76=compress(symget('varval76'));
        &name77=compress(symget('varval77'));
        &name78=compress(symget('varval78'));
        &name79=compress(symget('varval79'));
        &name80=compress(symget('varval80'));
        &name81=compress(symget('varval81'));
        &name82=compress(symget('varval82'));
        &name83=compress(symget('varval83'));
        &name84=compress(symget('varval84'));
        &name85=compress(symget('varval85'));
        &name86=compress(symget('varval86'));
        &name87=compress(symget('varval87'));
        &name88=compress(symget('varval88'));
        &name89=compress(symget('varval89'));
        &name90=compress(symget('varval90'));
        &name91=compress(symget('varval91'));
        &name92=compress(symget('varval92'));
        &name93=compress(symget('varval93'));
        &name94=compress(symget('varval94'));
        &name95=compress(symget('varval95'));
        &name96=compress(symget('varval96'));
        &name97=compress(symget('varval97'));
        &name98=compress(symget('varval98'));
        &name99=compress(symget('varval99'));
        &name100=compress(symget('varval100'));
        &name101=compress(symget('varval101'));
        &name102=compress(symget('varval102'));
        &name103=compress(symget('varval103'));
        &name104=compress(symget('varval104'));
        &name105=compress(symget('varval105'));
        &name106=compress(symget('varval106'));
        &name107=compress(symget('varval107'));
        &name108=compress(symget('varval108'));
        &name109=compress(symget('varval109'));
        &name110=compress(symget('varval110'));
        &name111=compress(symget('varval111'));
        &name112=compress(symget('varval112'));
        &name113=compress(symget('varval113'));
        &name114=compress(symget('varval114'));
        &name115=compress(symget('varval115'));
        &name116=compress(symget('varval116'));
        &name117=compress(symget('varval117'));
        &name118=compress(symget('varval118'));
        &name119=compress(symget('varval119'));
        &name120=compress(symget('varval120'));
        &name121=compress(symget('varval121'));
        &name122=compress(symget('varval122'));
        &name123=compress(symget('varval123'));
        &name124=compress(symget('varval124'));
        &name125=compress(symget('varval125'));
        &name126=compress(symget('varval126'));
        &name127=compress(symget('varval127'));
        &name128=compress(symget('varval128'));
        &name129=compress(symget('varval129'));
        &name130=compress(symget('varval130'));
        &name131=compress(symget('varval131'));
        &name132=compress(symget('varval132'));
        &name133=compress(symget('varval133'));
        &name134=compress(symget('varval134'));
        &name135=compress(symget('varval135'));
        &name136=compress(symget('varval136'));
        &name137=compress(symget('varval137'));
        &name138=compress(symget('varval138'));
        &name139=compress(symget('varval139'));
        &name140=compress(symget('varval140'));
        &name141=compress(symget('varval141'));
        &name142=compress(symget('varval142'));
        &name143=compress(symget('varval143'));
        &name144=compress(symget('varval144'));
        &name145=compress(symget('varval145'));
        &name146=compress(symget('varval146'));
        &name147=compress(symget('varval147'));
        &name148=compress(symget('varval148'));
        &name149=compress(symget('varval149'));
        &name150=compress(symget('varval150'));
        &name151=compress(symget('varval151'));
        &name152=compress(symget('varval152'));
        &name153=compress(symget('varval153'));
        &name154=compress(symget('varval154'));
      run;
	  %let pid=%eval(&pid+1);
    %end;
  %end;
  %if &hid=2 %then %do;
    %put fill data finished, set up temp2 dataset;
    data xdata.temp2;         /*append one group's H & P obs to the major dataset*/
     set xdata.temp2 xdata.temp;
    run;
  %end;
  %else %do;
    proc datasets library=xdata nolist;
	   append base=xdata.temp2 data=xdata.temp force;
	run;
  %end;
/*
  proc print data=xdata.temp2 noobs;
  run;
*/
  /*%fetch_h(pid=&pid);*/
  %let pid=0;
  %let n=0;
  %do %until(&n=&numh);
    %let n=%eval(&n+1);
    %let firstpart=&&hname&n;
    %let nametemp=&firstpart;
    %let varval&n=&&&firstpart;/* this can get right value */
    %let name&n=%sysfunc(trim(&nametemp&pid));  
/*    %put fetch H values: &varval &name1;*/
  %end;/*do until*/ 
   data xdata.temp;
      set xdata.temp;
      &name1=compress(symget('varval1')); 
      &name2=compress(symget('varval2')); 
      &name3=compress(symget('varval3')); 
      &name4=compress(symget('varval4')); 
      &name5=compress(symget('varval5')); 
      &name6=compress(symget('varval6')); 
      &name7=compress(symget('varval7')); 
      &name8=compress(symget('varval8')); 
      &name9=compress(symget('varval9')); 
      &name10=compress(symget('varval10')); 
      &name11=compress(symget('varval11')); 
      &name12=compress(symget('varval12')); 
      &name13=compress(symget('varval13')); 
      &name14=compress(symget('varval14')); 
      &name15=compress(symget('varval15')); 
      &name16=compress(symget('varval16')); 
      &name17=compress(symget('varval17')); 
      &name18=compress(symget('varval18')); 
      &name19=compress(symget('varval19')); 
      &name20=compress(symget('varval20')); 
      &name21=compress(symget('varval21')); 
      &name22=compress(symget('varval22')); 
      &name23=compress(symget('varval23')); 
 	  &name24=compress(symget('varval24')); 
	  &name25=compress(symget('varval25')); 
	  &name26=compress(symget('varval26')); 
      &name27=compress(symget('varval27')); 
      &name28=compress(symget('varval28')); 
      &name29=compress(symget('varval29')); 
      &name30=compress(symget('varval30')); 
      &name31=compress(symget('varval31')); 
      &name32=compress(symget('varval32')); 
      &name33=compress(symget('varval33')); 
      &name34=compress(symget('varval34')); 
      &name35=compress(symget('varval35')); 
      &name36=compress(symget('varval36')); 
      &name37=compress(symget('varval37')); 
      &name38=compress(symget('varval38')); 
      &name39=compress(symget('varval39')); 
      &name40=compress(symget('varval40')); 
      &name41=compress(symget('varval41')); 
      &name42=compress(symget('varval42')); 
      &name43=compress(symget('varval43')); 
      &name44=compress(symget('varval44')); 
      &name45=compress(symget('varval45')); 
      &name46=compress(symget('varval46')); 
      &name47=compress(symget('varval47')); 
      &name48=compress(symget('varval48')); 
      &name49=compress(symget('varval49')); 
      &name50=compress(symget('varval50')); 
      &name51=compress(symget('varval51')); 
      &name52=compress(symget('varval52')); 
      &name53=compress(symget('varval53')); 
      &name54=compress(symget('varval54')); 
      &name55=compress(symget('varval55')); 
      &name56=compress(symget('varval56')); 
      &name57=compress(symget('varval57')); 
      &name58=compress(symget('varval58')); 
      &name59=compress(symget('varval59')); 
      &name60=compress(symget('varval60')); 
      &name61=compress(symget('varval61')); 
      &name62=compress(symget('varval62')); 
      &name63=compress(symget('varval63')); 
      &name64=compress(symget('varval64')); 
      &name65=compress(symget('varval65')); 
      &name66=compress(symget('varval66')); 
      &name67=compress(symget('varval67')); 
      &name68=compress(symget('varval68')); 
      &name69=compress(symget('varval69')); 
      &name70=compress(symget('varval70')); 
      &name71=compress(symget('varval71')); 
      &name72=compress(symget('varval72')); 
      &name73=compress(symget('varval73')); 
      &name74=compress(symget('varval74')); 
      &name75=compress(symget('varval75')); 
      &name76=compress(symget('varval76')); 
      &name77=compress(symget('varval77')); 
      &name78=compress(symget('varval78')); 
      &name79=compress(symget('varval79')); 
      &name80=compress(symget('varval80')); 
      &name81=compress(symget('varval81')); 
      &name82=compress(symget('varval82')); 
      &name83=compress(symget('varval83')); 
      &name84=compress(symget('varval84')); 
      &name85=compress(symget('varval85')); 
      &name86=compress(symget('varval86')); 
      &name87=compress(symget('varval87')); 
      &name88=compress(symget('varval88')); 
      &name89=compress(symget('varval89')); 
      &name90=compress(symget('varval90')); 
      &name91=compress(symget('varval91')); 
      &name92=compress(symget('varval92')); 
      &name93=compress(symget('varval93')); 
      &name94=compress(symget('varval94')); 
      &name95=compress(symget('varval95')); 
      &name96=compress(symget('varval96')); 
      &name97=compress(symget('varval97')); 
      &name98=compress(symget('varval98')); 
      &name99=compress(symget('varval99')); 
      &name100=compress(symget('varval100')); 
      &name101=compress(symget('varval101')); 
      &name102=compress(symget('varval102')); 
      &name103=compress(symget('varval103')); 
      &name104=compress(symget('varval104')); 
      &name105=compress(symget('varval105')); 
      &name106=compress(symget('varval106')); 
  run;
  %let pid=1;
 %end;
%else %if &rectype=P %then %do;   /*P type obs*/
/*  %put rectype=P, HID= &hid;*/
  /*%fetch_p(pid=&pid);*/
  %let n=0;
  %do %until(&n=&nump);
    %let n=%eval(&n+1);
    %let firstpart=&&pname&n;
    %let nametemp=&firstpart;
    %let varval&n=&&&firstpart;/* this can get right value */
    %let name&n=&nametemp&pid;  
  %end;/*do until*/
/*    %put fetch P values: &varval &name1;*/
  data xdata.temp;
        set xdata.temp;
        &name1=compress(symget('varval1'));
        &name2=compress(symget('varval2'));
        &name3=compress(symget('varval3'));
        &name4=compress(symget('varval4'));
        &name5=compress(symget('varval5'));
        &name6=compress(symget('varval6'));
        &name7=compress(symget('varval7'));
        &name8=compress(symget('varval8'));
        &name9=compress(symget('varval9'));
        &name10=compress(symget('varval10'));
        &name11=compress(symget('varval11'));
        &name12=compress(symget('varval12'));
        &name13=compress(symget('varval13'));
        &name14=compress(symget('varval14'));
        &name15=compress(symget('varval15'));
        &name16=compress(symget('varval16'));
        &name17=compress(symget('varval17'));
        &name18=compress(symget('varval18'));
        &name19=compress(symget('varval19'));
        &name20=compress(symget('varval20'));
        &name21=compress(symget('varval21'));
        &name22=compress(symget('varval22'));
        &name23=compress(symget('varval23'));
        &name24=compress(symget('varval24'));
        &name25=compress(symget('varval25'));
        &name26=compress(symget('varval26'));
        &name27=compress(symget('varval27'));
        &name28=compress(symget('varval28'));
        &name29=compress(symget('varval29'));
        &name30=compress(symget('varval30'));
        &name31=compress(symget('varval31'));
        &name32=compress(symget('varval32'));
        &name33=compress(symget('varval33'));
        &name34=compress(symget('varval34'));
        &name35=compress(symget('varval35'));
        &name36=compress(symget('varval36'));
        &name37=compress(symget('varval37'));
        &name38=compress(symget('varval38'));
        &name39=compress(symget('varval39'));
        &name40=compress(symget('varval40'));
        &name41=compress(symget('varval41'));
        &name42=compress(symget('varval42'));
        &name43=compress(symget('varval43'));
        &name44=compress(symget('varval44'));
        &name45=compress(symget('varval45'));
        &name46=compress(symget('varval46'));
        &name47=compress(symget('varval47'));
        &name48=compress(symget('varval48'));
        &name49=compress(symget('varval49'));
        &name50=compress(symget('varval50'));
        &name51=compress(symget('varval51'));
        &name52=compress(symget('varval52'));
        &name53=compress(symget('varval53'));
        &name54=compress(symget('varval54'));
        &name55=compress(symget('varval55'));
        &name56=compress(symget('varval56'));
        &name57=compress(symget('varval57'));
        &name58=compress(symget('varval58'));
        &name59=compress(symget('varval59'));
        &name60=compress(symget('varval60'));
        &name61=compress(symget('varval61'));
        &name62=compress(symget('varval62'));
        &name63=compress(symget('varval63'));
        &name64=compress(symget('varval64'));
        &name65=compress(symget('varval65'));
        &name66=compress(symget('varval66'));
        &name67=compress(symget('varval67'));
        &name68=compress(symget('varval68'));
        &name69=compress(symget('varval69'));
        &name70=compress(symget('varval70'));
        &name71=compress(symget('varval71'));
        &name72=compress(symget('varval72'));
        &name73=compress(symget('varval73'));
        &name74=compress(symget('varval74'));
        &name75=compress(symget('varval75'));
        &name76=compress(symget('varval76'));
        &name77=compress(symget('varval77'));
        &name78=compress(symget('varval78'));
        &name79=compress(symget('varval79'));
        &name80=compress(symget('varval80'));
        &name81=compress(symget('varval81'));
        &name82=compress(symget('varval82'));
        &name83=compress(symget('varval83'));
        &name84=compress(symget('varval84'));
        &name85=compress(symget('varval85'));
        &name86=compress(symget('varval86'));
        &name87=compress(symget('varval87'));
        &name88=compress(symget('varval88'));
        &name89=compress(symget('varval89'));
        &name90=compress(symget('varval90'));
        &name91=compress(symget('varval91'));
        &name92=compress(symget('varval92'));
        &name93=compress(symget('varval93'));
        &name94=compress(symget('varval94'));
        &name95=compress(symget('varval95'));
        &name96=compress(symget('varval96'));
        &name97=compress(symget('varval97'));
        &name98=compress(symget('varval98'));
        &name99=compress(symget('varval99'));
        &name100=compress(symget('varval100'));
        &name101=compress(symget('varval101'));
        &name102=compress(symget('varval102'));
        &name103=compress(symget('varval103'));
        &name104=compress(symget('varval104'));
        &name105=compress(symget('varval105'));
        &name106=compress(symget('varval106'));
        &name107=compress(symget('varval107'));
        &name108=compress(symget('varval108'));
        &name109=compress(symget('varval109'));
        &name110=compress(symget('varval110'));
        &name111=compress(symget('varval111'));
        &name112=compress(symget('varval112'));
        &name113=compress(symget('varval113'));
        &name114=compress(symget('varval114'));
        &name115=compress(symget('varval115'));
        &name116=compress(symget('varval116'));
        &name117=compress(symget('varval117'));
        &name118=compress(symget('varval118'));
        &name119=compress(symget('varval119'));
        &name120=compress(symget('varval120'));
        &name121=compress(symget('varval121'));
        &name122=compress(symget('varval122'));
        &name123=compress(symget('varval123'));
        &name124=compress(symget('varval124'));
        &name125=compress(symget('varval125'));
        &name126=compress(symget('varval126'));
        &name127=compress(symget('varval127'));
        &name128=compress(symget('varval128'));
        &name129=compress(symget('varval129'));
        &name130=compress(symget('varval130'));
        &name131=compress(symget('varval131'));
        &name132=compress(symget('varval132'));
        &name133=compress(symget('varval133'));
        &name134=compress(symget('varval134'));
        &name135=compress(symget('varval135'));
        &name136=compress(symget('varval136'));
        &name137=compress(symget('varval137'));
        &name138=compress(symget('varval138'));
        &name139=compress(symget('varval139'));
        &name140=compress(symget('varval140'));
        &name141=compress(symget('varval141'));
        &name142=compress(symget('varval142'));
        &name143=compress(symget('varval143'));
        &name144=compress(symget('varval144'));
        &name145=compress(symget('varval145'));
        &name146=compress(symget('varval146'));
        &name147=compress(symget('varval147'));
        &name148=compress(symget('varval148'));
        &name149=compress(symget('varval149'));
        &name150=compress(symget('varval150'));
        &name151=compress(symget('varval151'));
        &name152=compress(symget('varval152'));
        &name153=compress(symget('varval153'));
        &name154=compress(symget('varval154'));
  run;
  %let pid=%eval(&pid+1);
%end;
%end; /*do until*/

data xdata.temp2;
   set xdata.temp2 xdata.temp;
   drop x;
run;
%let dsid=%sysfunc(close(&dsid));
%end; /*else do*/
proc print data=xdata.temp2;
run;
%put _user_;
%mend movedata;


/*
  This Macro extracts out same sex couples observations to 
  a new data set.
  
  Copyright(c) 2004 by 
  Liang, Xie
  Data/Online Office, Bartle Main Library
  Binghamton University
  State University of New York
*/

%macro extractssc(dataset);

%global malenum femalenum unma;
%let malenum=0;
%let femalenum=0;
%let unma=0;

data _null_;
  set xdata.varh;
  call symput(compress('hname'||_n_),hname);
  call symput('numh',_n_);
run;
data _null_;
  set xdata.varp;
  call symput(compress('pname'||_n_),pname);
  call symput('nump',_n_);
run;/*testified*/

data xdata.temp;
 x='a';
run;
data xdata.temp2;
 x='a';
run;

%let dsn=&dataset;
%let dsid=%sysfunc(open(&dsn));
%syscall set(dsid);

%if &dsid=0 %then 
    %put Data Set open failed!;
%else %do;
  %let nssex=0;
  %let obsid=%sysfunc(fetch(&dsid));
  %let rc=%sysfunc(rewind(&dsid));
  %let nobs=%sysfunc(attrn(&dsid,NOBS));
  %do %while(%sysfunc(fetch(&dsid))=0);
     %if &rectype=H %then %do;
	    %put Household No. &hid;
	    %if &persons>1 %then %do;
           %let householdmark=%sysfunc(note(&dsid));
		   %let i=1;
		   %do %until(&i>&persons);
		      %let rc=%sysfunc(fetch(&dsid));
			  %let cur=%sysfunc(curobs(&dsid));
			  %put i is &i, obs # is &cur;
			  %if &i=1 %then %do;
			      %let housemastermark=%sysfunc(note(&dsid));
				  %let sex1=&sex;
			  %end;
			  %else %if &i=&persons %then %do;
			      %let partnermark=%sysfunc(note(&dsid));
				  %let sex2=&sex;
			  %end;
			  %let i=%eval(&i+1);
		   %end;/*until i*/
		   %put sex1 is &sex1 | sex2 is &sex2 | relation is &relate;
		   %if &relate=19 %then 
		      %let unma=%eval(&unma+1);
		   %if (&relate=19 AND &sex1=&sex2) %then %do; /*same sex unmarried couple*/
		      %let nssex=%eval(&nssex+1);
			  %if &sex1=1 %then 
                   %let malenum=%eval(&malenum+1);
			  %else %let femalenum=%eval(&femalenum+1);
              %let rc=%sysfunc(point(&dsid,&householdmark));  /*retro back to H record*/
              %let rc=%sysfunc(fetch(&dsid));
			  %let pid=_H;
              %let n=0;
/*			  %put current obs is of type &rectype;  */
              %do %until(&n=&numh);
    			%let n=%eval(&n+1);
    		  	%let firstpart=&&hname&n;
    		  	%let nametemp=&firstpart;
    		  	%let varval&n=&&&firstpart;/* this can get right value */
   		      	%let name&n=&nametemp&pid;  
			  %end;/*do until*/
   			  data xdata.temp;
      			set xdata.temp;
      			&name1=compress(symget('varval1')); 
      			&name2=compress(symget('varval2')); 
      			&name3=compress(symget('varval3')); 
      			&name4=compress(symget('varval4')); 
      			&name5=compress(symget('varval5')); 
      			&name6=compress(symget('varval6')); 
      			&name7=compress(symget('varval7')); 
      			&name8=compress(symget('varval8')); 
      			&name9=compress(symget('varval9')); 
      			&name10=compress(symget('varval10')); 
      			&name11=compress(symget('varval11')); 
      			&name12=compress(symget('varval12')); 
      			&name13=compress(symget('varval13')); 
      			&name14=compress(symget('varval14')); 
      			&name15=compress(symget('varval15')); 
      			&name16=compress(symget('varval16')); 
      			&name17=compress(symget('varval17')); 
      			&name18=compress(symget('varval18')); 
      			&name19=compress(symget('varval19')); 
      			&name20=compress(symget('varval20')); 
      			&name21=compress(symget('varval21')); 
      			&name22=compress(symget('varval22')); 
      			&name23=compress(symget('varval23')); 
 	  			&name24=compress(symget('varval24')); 
	  			&name25=compress(symget('varval25')); 
	  			&name26=compress(symget('varval26')); 
      			&name27=compress(symget('varval27')); 
      			&name28=compress(symget('varval28')); 
      			&name29=compress(symget('varval29')); 
      			&name30=compress(symget('varval30')); 
      			&name31=compress(symget('varval31')); 
      			&name32=compress(symget('varval32')); 
      			&name33=compress(symget('varval33')); 
      			&name34=compress(symget('varval34')); 
      			&name35=compress(symget('varval35')); 
      			&name36=compress(symget('varval36')); 
      			&name37=compress(symget('varval37')); 
      			&name38=compress(symget('varval38')); 
      			&name39=compress(symget('varval39')); 
      			&name40=compress(symget('varval40')); 
      			&name41=compress(symget('varval41')); 
      			&name42=compress(symget('varval42')); 
      			&name43=compress(symget('varval43')); 
      			&name44=compress(symget('varval44')); 
      			&name45=compress(symget('varval45')); 
      			&name46=compress(symget('varval46')); 
      			&name47=compress(symget('varval47')); 
      			&name48=compress(symget('varval48')); 
      			&name49=compress(symget('varval49')); 
      			&name50=compress(symget('varval50')); 
      			&name51=compress(symget('varval51')); 
      			&name52=compress(symget('varval52')); 
      			&name53=compress(symget('varval53')); 
      			&name54=compress(symget('varval54')); 
      			&name55=compress(symget('varval55')); 
      			&name56=compress(symget('varval56')); 
      			&name57=compress(symget('varval57')); 
      			&name58=compress(symget('varval58')); 
      			&name59=compress(symget('varval59')); 
      			&name60=compress(symget('varval60')); 
      			&name61=compress(symget('varval61')); 
      			&name62=compress(symget('varval62')); 
      			&name63=compress(symget('varval63')); 
      			&name64=compress(symget('varval64')); 
      			&name65=compress(symget('varval65')); 
      			&name66=compress(symget('varval66')); 
      			&name67=compress(symget('varval67')); 
      			&name68=compress(symget('varval68')); 
      			&name69=compress(symget('varval69')); 
      			&name70=compress(symget('varval70')); 
      			&name71=compress(symget('varval71')); 
      			&name72=compress(symget('varval72')); 
      			&name73=compress(symget('varval73')); 
      			&name74=compress(symget('varval74')); 
      			&name75=compress(symget('varval75')); 
      			&name76=compress(symget('varval76')); 
      			&name77=compress(symget('varval77')); 
      			&name78=compress(symget('varval78')); 
      			&name79=compress(symget('varval79')); 
      			&name80=compress(symget('varval80')); 
      			&name81=compress(symget('varval81')); 
      			&name82=compress(symget('varval82')); 
      			&name83=compress(symget('varval83')); 
      			&name84=compress(symget('varval84')); 
      			&name85=compress(symget('varval85')); 
      			&name86=compress(symget('varval86')); 
      			&name87=compress(symget('varval87')); 
      			&name88=compress(symget('varval88')); 
      			&name89=compress(symget('varval89')); 
      			&name90=compress(symget('varval90')); 
      			&name91=compress(symget('varval91')); 
      			&name92=compress(symget('varval92')); 
      			&name93=compress(symget('varval93')); 
      			&name94=compress(symget('varval94')); 
      			&name95=compress(symget('varval95')); 
      			&name96=compress(symget('varval96')); 
      			&name97=compress(symget('varval97')); 
      			&name98=compress(symget('varval98')); 
      			&name99=compress(symget('varval99')); 
      			&name100=compress(symget('varval100')); 
      			&name101=compress(symget('varval101')); 
      			&name102=compress(symget('varval102')); 
      			&name103=compress(symget('varval103')); 
      			&name104=compress(symget('varval104')); 
      			&name105=compress(symget('varval105')); 
      			&name106=compress(symget('varval106')); 
    		  run;  /*write H type values to temporary dataset*/
       
			  
              %let rc=%sysfunc(point(&dsid,&housemastermark));  /*go to Householder's record*/
              %let rc=%sysfunc(fetch(&dsid));
/*              %put current obs is of type &rectype;   */
			  %let pid=_Master;
              %let n=0;
              %do %until(&n=&nump);
    			%let n=%eval(&n+1);
    		  	%let firstpart=&&pname&n;
    		  	%let nametemp=&firstpart;
    		  	%let varval&n=&&&firstpart;/* this can get right value */
   		      	%let name&n=&nametemp&pid;  
			  %end;/*do until*/
   			  data xdata.temp;
      			set xdata.temp;
      			&name1=compress(symget('varval1')); 
      			&name2=compress(symget('varval2')); 
      			&name3=compress(symget('varval3')); 
      			&name4=compress(symget('varval4')); 
      			&name5=compress(symget('varval5')); 
      			&name6=compress(symget('varval6')); 
      			&name7=compress(symget('varval7')); 
      			&name8=compress(symget('varval8')); 
      			&name9=compress(symget('varval9')); 
      			&name10=compress(symget('varval10')); 
      			&name11=compress(symget('varval11')); 
      			&name12=compress(symget('varval12')); 
      			&name13=compress(symget('varval13')); 
      			&name14=compress(symget('varval14')); 
      			&name15=compress(symget('varval15')); 
      			&name16=compress(symget('varval16')); 
      			&name17=compress(symget('varval17')); 
      			&name18=compress(symget('varval18')); 
      			&name19=compress(symget('varval19')); 
      			&name20=compress(symget('varval20')); 
      			&name21=compress(symget('varval21')); 
      			&name22=compress(symget('varval22')); 
      			&name23=compress(symget('varval23')); 
 	  			&name24=compress(symget('varval24')); 
	  			&name25=compress(symget('varval25')); 
	  			&name26=compress(symget('varval26')); 
      			&name27=compress(symget('varval27')); 
      			&name28=compress(symget('varval28')); 
      			&name29=compress(symget('varval29')); 
      			&name30=compress(symget('varval30')); 
      			&name31=compress(symget('varval31')); 
      			&name32=compress(symget('varval32')); 
      			&name33=compress(symget('varval33')); 
      			&name34=compress(symget('varval34')); 
      			&name35=compress(symget('varval35')); 
      			&name36=compress(symget('varval36')); 
      			&name37=compress(symget('varval37')); 
      			&name38=compress(symget('varval38')); 
      			&name39=compress(symget('varval39')); 
      			&name40=compress(symget('varval40')); 
      			&name41=compress(symget('varval41')); 
      			&name42=compress(symget('varval42')); 
      			&name43=compress(symget('varval43')); 
      			&name44=compress(symget('varval44')); 
      			&name45=compress(symget('varval45')); 
      			&name46=compress(symget('varval46')); 
      			&name47=compress(symget('varval47')); 
      			&name48=compress(symget('varval48')); 
      			&name49=compress(symget('varval49')); 
      			&name50=compress(symget('varval50')); 
      			&name51=compress(symget('varval51')); 
      			&name52=compress(symget('varval52')); 
      			&name53=compress(symget('varval53')); 
      			&name54=compress(symget('varval54')); 
      			&name55=compress(symget('varval55')); 
      			&name56=compress(symget('varval56')); 
      			&name57=compress(symget('varval57')); 
      			&name58=compress(symget('varval58')); 
      			&name59=compress(symget('varval59')); 
      			&name60=compress(symget('varval60')); 
      			&name61=compress(symget('varval61')); 
      			&name62=compress(symget('varval62')); 
      			&name63=compress(symget('varval63')); 
      			&name64=compress(symget('varval64')); 
      			&name65=compress(symget('varval65')); 
      			&name66=compress(symget('varval66')); 
      			&name67=compress(symget('varval67')); 
      			&name68=compress(symget('varval68')); 
      			&name69=compress(symget('varval69')); 
      			&name70=compress(symget('varval70')); 
      			&name71=compress(symget('varval71')); 
      			&name72=compress(symget('varval72')); 
      			&name73=compress(symget('varval73')); 
      			&name74=compress(symget('varval74')); 
      			&name75=compress(symget('varval75')); 
      			&name76=compress(symget('varval76')); 
      			&name77=compress(symget('varval77')); 
      			&name78=compress(symget('varval78')); 
      			&name79=compress(symget('varval79')); 
      			&name80=compress(symget('varval80')); 
      			&name81=compress(symget('varval81')); 
      			&name82=compress(symget('varval82')); 
      			&name83=compress(symget('varval83')); 
      			&name84=compress(symget('varval84')); 
      			&name85=compress(symget('varval85')); 
      			&name86=compress(symget('varval86')); 
      			&name87=compress(symget('varval87')); 
      			&name88=compress(symget('varval88')); 
      			&name89=compress(symget('varval89')); 
      			&name90=compress(symget('varval90')); 
      			&name91=compress(symget('varval91')); 
      			&name92=compress(symget('varval92')); 
      			&name93=compress(symget('varval93')); 
      			&name94=compress(symget('varval94')); 
      			&name95=compress(symget('varval95')); 
      			&name96=compress(symget('varval96')); 
      			&name97=compress(symget('varval97')); 
      			&name98=compress(symget('varval98')); 
      			&name99=compress(symget('varval99')); 
      			&name100=compress(symget('varval100')); 
      			&name101=compress(symget('varval101')); 
      			&name102=compress(symget('varval102')); 
      			&name103=compress(symget('varval103')); 
      			&name104=compress(symget('varval104')); 
      			&name105=compress(symget('varval105')); 
      			&name106=compress(symget('varval106')); 
				&name107=compress(symget('varval107'));
      			&name108=compress(symget('varval108'));
      			&name109=compress(symget('varval109'));
      			&name110=compress(symget('varval110'));
    			&name111=compress(symget('varval111'));
    			&name112=compress(symget('varval112'));
    			&name113=compress(symget('varval113'));
    			&name114=compress(symget('varval114'));
    			&name115=compress(symget('varval115'));
    			&name116=compress(symget('varval116'));
    			&name117=compress(symget('varval117'));
    			&name118=compress(symget('varval118'));
    			&name119=compress(symget('varval119'));
    			&name120=compress(symget('varval120'));
    			&name121=compress(symget('varval121'));
    			&name122=compress(symget('varval122'));
    			&name123=compress(symget('varval123'));
    			&name124=compress(symget('varval124'));
    			&name125=compress(symget('varval125'));
    			&name126=compress(symget('varval126'));
    			&name127=compress(symget('varval127'));
    			&name128=compress(symget('varval128'));
    			&name129=compress(symget('varval129'));
    			&name130=compress(symget('varval130'));
    			&name131=compress(symget('varval131'));
    			&name132=compress(symget('varval132'));
    			&name133=compress(symget('varval133'));
    			&name134=compress(symget('varval134'));
    			&name135=compress(symget('varval135'));
    			&name136=compress(symget('varval136'));
    			&name137=compress(symget('varval137'));
    			&name138=compress(symget('varval138'));
    			&name139=compress(symget('varval139'));
    			&name140=compress(symget('varval140'));
    			&name141=compress(symget('varval141'));
    			&name142=compress(symget('varval142'));
    			&name143=compress(symget('varval143'));
    			&name144=compress(symget('varval144'));
    			&name145=compress(symget('varval145'));
    			&name146=compress(symget('varval146'));
    			&name147=compress(symget('varval147'));
    			&name148=compress(symget('varval148'));
    			&name149=compress(symget('varval149'));
    			&name150=compress(symget('varval150'));
    			&name151=compress(symget('varval151'));
    			&name152=compress(symget('varval152'));
    			&name153=compress(symget('varval153'));
    			&name154=compress(symget('varval154'));
    		  run;  /*write Householder's values to temporary dataset*/
           
              %let rc=%sysfunc(point(&dsid,&partnermark));  /*go to Parterner record*/
              %let rc=%sysfunc(fetch(&dsid));
			  %let pid=_Partner;
              %let n=0;
/*			  %put current obs is of type &rectype;  */
              %do %until(&n=&nump);
    			%let n=%eval(&n+1);
    		  	%let firstpart=&&pname&n;
    		  	%let nametemp=&firstpart;
    		  	%let varval&n=&&&firstpart;/* this can get right value */
   		      	%let name&n=&nametemp&pid;  
			  %end;/*do until*/
   			  data xdata.temp;
      			set xdata.temp;
      			&name1=compress(symget('varval1')); 
      			&name2=compress(symget('varval2')); 
      			&name3=compress(symget('varval3')); 
      			&name4=compress(symget('varval4')); 
      			&name5=compress(symget('varval5')); 
      			&name6=compress(symget('varval6')); 
      			&name7=compress(symget('varval7')); 
      			&name8=compress(symget('varval8')); 
      			&name9=compress(symget('varval9')); 
      			&name10=compress(symget('varval10')); 
      			&name11=compress(symget('varval11')); 
      			&name12=compress(symget('varval12')); 
      			&name13=compress(symget('varval13')); 
      			&name14=compress(symget('varval14')); 
      			&name15=compress(symget('varval15')); 
      			&name16=compress(symget('varval16')); 
      			&name17=compress(symget('varval17')); 
      			&name18=compress(symget('varval18')); 
      			&name19=compress(symget('varval19')); 
      			&name20=compress(symget('varval20')); 
      			&name21=compress(symget('varval21')); 
      			&name22=compress(symget('varval22')); 
      			&name23=compress(symget('varval23')); 
 	  			&name24=compress(symget('varval24')); 
	  			&name25=compress(symget('varval25')); 
	  			&name26=compress(symget('varval26')); 
      			&name27=compress(symget('varval27')); 
      			&name28=compress(symget('varval28')); 
      			&name29=compress(symget('varval29')); 
      			&name30=compress(symget('varval30')); 
      			&name31=compress(symget('varval31')); 
      			&name32=compress(symget('varval32')); 
      			&name33=compress(symget('varval33')); 
      			&name34=compress(symget('varval34')); 
      			&name35=compress(symget('varval35')); 
      			&name36=compress(symget('varval36')); 
      			&name37=compress(symget('varval37')); 
      			&name38=compress(symget('varval38')); 
      			&name39=compress(symget('varval39')); 
      			&name40=compress(symget('varval40')); 
      			&name41=compress(symget('varval41')); 
      			&name42=compress(symget('varval42')); 
      			&name43=compress(symget('varval43')); 
      			&name44=compress(symget('varval44')); 
      			&name45=compress(symget('varval45')); 
      			&name46=compress(symget('varval46')); 
      			&name47=compress(symget('varval47')); 
      			&name48=compress(symget('varval48')); 
      			&name49=compress(symget('varval49')); 
      			&name50=compress(symget('varval50')); 
      			&name51=compress(symget('varval51')); 
      			&name52=compress(symget('varval52')); 
      			&name53=compress(symget('varval53')); 
      			&name54=compress(symget('varval54')); 
      			&name55=compress(symget('varval55')); 
      			&name56=compress(symget('varval56')); 
      			&name57=compress(symget('varval57')); 
      			&name58=compress(symget('varval58')); 
      			&name59=compress(symget('varval59')); 
      			&name60=compress(symget('varval60')); 
      			&name61=compress(symget('varval61')); 
      			&name62=compress(symget('varval62')); 
      			&name63=compress(symget('varval63')); 
      			&name64=compress(symget('varval64')); 
      			&name65=compress(symget('varval65')); 
      			&name66=compress(symget('varval66')); 
      			&name67=compress(symget('varval67')); 
      			&name68=compress(symget('varval68')); 
      			&name69=compress(symget('varval69')); 
      			&name70=compress(symget('varval70')); 
      			&name71=compress(symget('varval71')); 
      			&name72=compress(symget('varval72')); 
      			&name73=compress(symget('varval73')); 
      			&name74=compress(symget('varval74')); 
      			&name75=compress(symget('varval75')); 
      			&name76=compress(symget('varval76')); 
      			&name77=compress(symget('varval77')); 
      			&name78=compress(symget('varval78')); 
      			&name79=compress(symget('varval79')); 
      			&name80=compress(symget('varval80')); 
      			&name81=compress(symget('varval81')); 
      			&name82=compress(symget('varval82')); 
      			&name83=compress(symget('varval83')); 
      			&name84=compress(symget('varval84')); 
      			&name85=compress(symget('varval85')); 
      			&name86=compress(symget('varval86')); 
      			&name87=compress(symget('varval87')); 
      			&name88=compress(symget('varval88')); 
      			&name89=compress(symget('varval89')); 
      			&name90=compress(symget('varval90')); 
      			&name91=compress(symget('varval91')); 
      			&name92=compress(symget('varval92')); 
      			&name93=compress(symget('varval93')); 
      			&name94=compress(symget('varval94')); 
      			&name95=compress(symget('varval95')); 
      			&name96=compress(symget('varval96')); 
      			&name97=compress(symget('varval97')); 
      			&name98=compress(symget('varval98')); 
      			&name99=compress(symget('varval99')); 
      			&name100=compress(symget('varval100')); 
      			&name101=compress(symget('varval101')); 
      			&name102=compress(symget('varval102')); 
      			&name103=compress(symget('varval103')); 
      			&name104=compress(symget('varval104')); 
      			&name105=compress(symget('varval105')); 
      			&name106=compress(symget('varval106')); 
				&name107=compress(symget('varval107'));
      			&name108=compress(symget('varval108'));
      			&name109=compress(symget('varval109'));
      			&name110=compress(symget('varval110'));
    			&name111=compress(symget('varval111'));
    			&name112=compress(symget('varval112'));
    			&name113=compress(symget('varval113'));
    			&name114=compress(symget('varval114'));
    			&name115=compress(symget('varval115'));
    			&name116=compress(symget('varval116'));
    			&name117=compress(symget('varval117'));
    			&name118=compress(symget('varval118'));
    			&name119=compress(symget('varval119'));
    			&name120=compress(symget('varval120'));
    			&name121=compress(symget('varval121'));
    			&name122=compress(symget('varval122'));
    			&name123=compress(symget('varval123'));
    			&name124=compress(symget('varval124'));
    			&name125=compress(symget('varval125'));
    			&name126=compress(symget('varval126'));
    			&name127=compress(symget('varval127'));
    			&name128=compress(symget('varval128'));
    			&name129=compress(symget('varval129'));
    			&name130=compress(symget('varval130'));
    			&name131=compress(symget('varval131'));
    			&name132=compress(symget('varval132'));
    			&name133=compress(symget('varval133'));
    			&name134=compress(symget('varval134'));
    			&name135=compress(symget('varval135'));
    			&name136=compress(symget('varval136'));
    			&name137=compress(symget('varval137'));
    			&name138=compress(symget('varval138'));
    			&name139=compress(symget('varval139'));
    			&name140=compress(symget('varval140'));
    			&name141=compress(symget('varval141'));
    			&name142=compress(symget('varval142'));
    			&name143=compress(symget('varval143'));
    			&name144=compress(symget('varval144'));
    			&name145=compress(symget('varval145'));
    			&name146=compress(symget('varval146'));
    			&name147=compress(symget('varval147'));
    			&name148=compress(symget('varval148'));
    			&name149=compress(symget('varval149'));
    			&name150=compress(symget('varval150'));
    			&name151=compress(symget('varval151'));
    			&name152=compress(symget('varval152'));
    			&name153=compress(symget('varval153'));
    			&name154=compress(symget('varval154'));
    		  run;  /*write Householder's values to temporary dataset*/
              
			  %if &nssex=1 %then %do;
			     %put Adding first same sex couple data;
			     data xdata.temp2;
				   set xdata.temp2 xdata.temp;
				 run;
			  %end;
			  %else %do;
			     %put Appending same sex couple data, sequencial number &nssex;
			     proc datasets library=xdata nolist;
				    append base=xdata.temp2 data=xdata.temp force;
				 run;
			  %end;
		    %end; /*if same sex unmarried partner*/
		 %end;/*if &persons>1*/
		 %else %if &persons=1 %then 
		    %let rc=%sysfunc(fetch(&dsid));
	   %end;/*if H*/
	 %end;/*do while fetch=0*/
   %end;/*else: Open Successful*/
   data xdata.temp2;
     set xdata.temp2;
     drop x;
   run;

   %put Total Household record is &hid;
   %put Total Same Sex Couple Household number is &nssex;
   %put Total Same Sex Female Household number is &femalenum;
   %put Total Same Sex Male Household Number is &malenum;

   %let rc=%sysfunc(close(&dsid));
%mend extractssc;




%indexdata(xdata.pums2);
%count_ph(xdata.pums2);
%put _user_;

%tag_h(xdata.pums2);
%movedata(xdata.pums2);

%extractssc(xdata.pums2);
%put _user_;
%put _global_;
%let dsid=%sysfunc(open(xdata.pums2));
%let dsid=%sysfunc(close(&dsid));
%put _user_;
