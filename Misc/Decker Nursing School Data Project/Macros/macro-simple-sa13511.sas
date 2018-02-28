/*
  Writer: Liang Xie
  Department of Economics
  Data/Online Office, Glenn G. Bartle Library
  State University of New York @ Binghamton
*/

libname xdata "d:\data\usc2000\ssd set";
options ls=80;     
option mprint;

%macro txt2ssd(ds);
%let dsn=%sysfunc(compress(13511-&ds-data.txt));
%let ssd_name=xdata.D&ds;
%let rawfile=%str(%'d:\data\usc2000\rawdata\&dsn%');
data &ssd_name;                                                           
/*                                                                                 
* -----------------------------------------------                                  
* Retain Housing Record Variables                                                  
* -----------------------------------------------                                  
*/                                                                                 
  retain SERIALNO                                                                  
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
         FINC;                                                                     
/*                                                                                 
* -------------------------------------------------                                
* Input for Housing Record                                                         
* -------------------------------------------------                                
*/                                                                                 
  infile "&rawfile" lrecl=314;                                                  
  input RECTYPE $ 1 @;                                                             
  if RECTYPE = 'H' then                                                            
      input  @2 SERIALNO $7.                                                       
             @9 SAMPLE $1.                                                         
             @10 STATE $2.                                                         
             @12 REGION $1.                                                        
             @13 DIVISION $1.                                                      
             @19 PUMA1 $5.                                                         
             @32 MSACMSA1 $4.                                                      
             @36 MSAPMSA1 $4.                                                      
             @42 AREATYP1 $2.                                                      
             @72 TOTPUMA1 $14.0                                                     
             @86 LNDPUMA1 $14.0                                                     
             @100 SUBSAMPL $2.                                                     
             @102 HWEIGHT $4.                                                      
             @106 PERSONS $2.0                                                      
             @108 UNITTYPE $1.                                                     
             @109 HSUB $1.                                                         
             @110 HAUG $1.                                                         
             @111 VACSTAT $1.                                                      
             @112 VACSTATA $1.                                                     
             @113 TENURE $1.                                                       
             @114 TENUREA $1.                                                      
             @115 BLDGSZ $2.                                                       
             @117 BLDGSZA $1.                                                      
             @118 YRBUILT $1.                                                      
             @119 YRBUILTA $1.                                                     
             @120 YRMOVED $1.                                                      
             @121 YRMOVEDA $1.                                                     
             @122 ROOMS $1.                                                        
             @123 ROOMSA $1.                                                       
             @124 BEDRMS $1.                                                       
             @125 BEDRMSA $1.                                                      
             @126 CPLUMB $1.                                                       
             @127 CPLUMBA $1.                                                      
             @128 CKITCH $1.                                                       
             @129 CKITCHA $1.                                                      
             @130 PHONE $1.                                                        
             @131 PHONEA $1.                                                       
             @132 FUEL $1.                                                         
             @133 FUELA $1.                                                        
             @134 VEHICL $1.                                                       
             @135 VEHICLA $1.                                                      
             @136 BUSINES $1.                                                      
             @137 BUSINESA $1.                                                     
             @138 ACRES $1.                                                        
             @139 ACRESA $1.                                                       
             @140 AGSALES $1.                                                      
             @141 AGSALESA $1.                                                     
             @142 ELEC $4.                                                         
             @146 ELECA $1.                                                        
             @147 GAS $4.                                                          
             @151 GASA $1.                                                         
             @152 WATER $4.                                                        
             @156 WATERA $1.                                                       
             @157 OIL $4.                                                          
             @161 OILA $1.                                                         
             @162 RENT $4.                                                         
             @166 RENTA $1.                                                        
             @167 MEALS $1.                                                        
             @168 MEALSA $1.                                                       
             @169 MORTG1 $1.                                                       
             @170 MORTG1A $1.                                                      
             @171 MRT1AMT $5.                                                      
             @176 MRT1AMTA $1.                                                     
             @177 MORTG2 $1.                                                       
             @178 MORTG2A $1.                                                      
             @179 MRT2AMT $5.                                                      
             @184 MRT2AMTA $1.                                                     
             @185 TAXINCL $1.                                                      
             @186 TAXINCLA $1.                                                     
             @187 TAXAMT $2.                                                       
             @189 TAXAMTA $1.                                                      
             @190 INSINCL $1.                                                      
             @191 INSINCLA $1.                                                     
             @192 INSAMT $4.                                                       
             @196 INSAMTA $1.                                                      
             @197 CONDFEE $4.                                                      
             @201 CONDFEEA $1.                                                     
             @202 VALUE $2.                                                        
             @204 VALUEA $1.                                                       
             @205 MHLOAN $1.                                                       
             @206 MHLOANA $1.                                                      
             @207 MHCOST $5.                                                       
             @212 MHCOSTA $1.                                                      
             @213 HHT $1.                                                          
             @214 P65 $2.                                                          
             @216 P18 $2.                                                          
             @218 NPF $2.                                                          
             @220 NOC $2.                                                          
             @222 NRC $2.                                                          
             @224 PSF $1.                                                          
             @225 PAOC $1.                                                         
             @226 PARC $1.                                                         
             @227 SVAL $1.                                                         
             @228 SMOC $5.                                                         
             @233 SMOCAPI $3.                                                      
             @236 SRNT $1.                                                         
             @237 GRENT $4.                                                        
             @241 GRAPI $3.                                                        
             @244 FNF $1.                                                          
             @245 HHL $1.                                                          
             @246 LNGI $1.                                                         
             @247 WIF $1.                                                          
             @248 EMPSTAT $1.                                                      
             @249 WORKEXP $2.                                                      
             @251 HINC $8.                                                         
             @259 FINC $8.;                                                        
/*                                                                                 
* -----------------------------------------------                                  
* Input for Person Record                                                          
* -----------------------------------------------                                  
*/                                                                                 
  else if RECTYPE = 'P' then                                                       
       input @2 SERIALNO $7.                                                       
             @9 PNUM $2.                                                           
             @11 PAUG $1.                                                          
             @12 DDP $1.                                                           
             @13 PWEIGHT $4.                                                       
             @17 RELATE $2.                                                        
             @19 RELATEA $1.                                                       
             @20 OC $1.                                                            
             @21 RC $1.                                                            
             @22 PAOCF $1.                                                         
             @23 SEX $1.                                                           
             @24 SEXA $1.                                                          
             @25 AGE $2.                                                           
             @27 AGEA $1.                                                          
             @28 HISPAN $2.                                                        
             @30 HISPANA $1.                                                       
             @31 NUMRACE $1.                                                       
             @32 WHITE $1.                                                         
             @33 BLACK $1.                                                         
             @34 AIAN $1.                                                          
             @35 ASIAN $1.                                                         
             @36 NHPI $1.                                                          
             @37 OTHER $1.                                                         
             @38 RACE1 $1.                                                         
             @39 RACE2 $2.                                                         
             @41 RACE3 $2.                                                         
             @43 RACEA $1.                                                         
             @44 MARSTAT $1.                                                       
             @45 MARSTATA $1.                                                      
             @46 MSP $1.                                                           
             @47 SFN $1.                                                           
             @48 SFREL $1.                                                         
             @49 ENROLL $1.                                                        
             @50 ENROLLA $1.                                                       
             @51 GRADE $1.                                                         
             @52 GRADEA $1.                                                        
             @53 EDUC $2.                                                          
             @55 EDUCA $1.                                                         
             @56 ANCFRST1 $3.                                                      
             @59 ANCSCND1 $3.                                                      
             @62 ANCA $1.                                                          
             @63 ANCR $1.                                                          
             @64 SPEAK $1.                                                         
             @65 SPEAKA $1.                                                        
             @66 LANG1 $3.                                                         
             @69 LANGA $1.                                                         
             @70 ENGABIL $1.                                                       
             @71 ENGABILA $1.                                                      
             @72 POB1 $3.                                                          
             @75 POBA $1.                                                          
             @76 CITIZEN $1.                                                       
             @77 CITIZENA $1.                                                      
             @78 YR2US $4.                                                         
             @82 YR2USA $1.                                                        
             @83 MOB $1.                                                           
             @84 MOBA $1.                                                          
             @85 MIGST1 $3.                                                        
             @88 MIGSTA $1.                                                        
             @94 MIGPUMA1 $5.                                                      
             @101 MIGAREA1 $2.                                                     
             @107 MIGCMA1 $4.                                                      
             @115 MIGPMA1 $4.                                                      
             @119 SENSORY $1.                                                      
             @120 SENSORYA $1.                                                     
             @121 PHYSCL $1.                                                       
             @122 PHYSCLA $1.                                                      
             @123 MENTAL $1.                                                       
             @124 MENTALA $1.                                                      
             @125 SLFCARE $1.                                                      
             @126 SLFCAREA $1.                                                     
             @127 ABGO $1.                                                         
             @128 ABGOA $1.                                                        
             @129 ABWORK $1.                                                       
             @130 ABWORKA $1.                                                      
             @131 DISABLE $1.                                                      
             @132 GRANDC $1.                                                       
             @133 GRANDCA $1.                                                      
             @134 RSPNSBL $1.                                                      
             @135 RSPNSBLA $1.                                                     
             @136 HOWLONG $1.                                                      
             @137 HOWLONGA $1.                                                     
             @138 MILTARY $1.                                                      
             @139 MILTARYA $1.                                                     
             @140 VPS1 $1.                                                         
             @141 VPS2 $1.                                                         
             @142 VPS3 $1.                                                         
             @143 VPS4 $1.                                                         
             @144 VPS5 $1.                                                         
             @145 VPS6 $1.                                                         
             @146 VPS7 $1.                                                         
             @147 VPS8 $1.                                                         
             @148 VPS9 $1.                                                         
             @149 VPSA $1.                                                         
             @150 MILYRS $1.                                                       
             @151 MILYRSA $1.                                                      
             @152 VPSR $2.                                                         
             @154 ESR $1.                                                          
             @155 ESRA $1.                                                         
             @156 ESP $1.                                                          
             @157 POWST1 $3.                                                       
             @160 POWSTA $1.                                                       
             @166 POWPUMA1 $5.                                                     
             @173 POWAREA1 $2.                                                     
             @179 POWCMA1 $4.                                                      
             @187 POWPMA1 $4.                                                      
             @191 TRVMNS $1.                                                       
             @193 TRVMNSA $1.                                                      
             @194 CARPOOL $1.                                                      
             @195 CARPOOLA $1.                                                     
             @196 LVTIME $3.                                                       
             @199 LVTIMEA $1.                                                      
             @200 TRVTIME $3.                                                      
             @203 TRVTIMEA $1.                                                     
             @204 LAYOFF $1.                                                       
             @205 ABSENT $1.                                                       
             @206 RECALL $1.                                                       
             @207 LOOKWRK $1.                                                      
             @208 BACKWRK $1.                                                      
             @209 LASTWRK $1.                                                      
             @210 LASTWRKA $1.                                                     
             @211 INDCEN $3.                                                       
             @214 INDCENA $1.                                                      
             @215 INDNAICS $8.                                                     
             @223 OCCCEN1 $3.                                                      
             @226 OCCCENA $1.                                                      
             @227 OCCSOC1 $7.                                                      
             @234 CLWKR $1.                                                        
             @235 CLWKRA $1.                                                       
             @236 WRKLYR $1.                                                       
             @237 WRKLYRA $1.                                                      
             @238 WEEKS $2.                                                        
             @240 WEEKSA $1.                                                       
             @241 HOURS $2.                                                        
             @243 HOURSA $1.                                                       
             @244 INCWS $6.                                                        
             @250 INCWSA $1.                                                       
             @251 INCSE $6.                                                        
             @257 INCSEA $1.                                                       
             @258 INCINT $6.                                                       
             @264 INCINTA $1.                                                      
             @265 INCSS $5.                                                        
             @270 INCSSA $1.                                                       
             @271 INCSSI $5.                                                       
             @276 INCSSIA $1.                                                      
             @277 INCPA $5.                                                        
             @282 INCPAA $1.                                                       
             @283 INCRET $6.                                                       
             @289 INCRETA $1.                                                      
             @290 INCOTH $6.                                                       
             @296 INCOTHA $1.                                                      
             @297 INCTOT $7.                                                       
             @304 INCTOTA $1.                                                      
             @305 EARNS $7.                                                        
             @312 POVERTY $3.;     
                                                                                   
label                                                                              
/*                                                                                 
* ------------------------------------------------                                 
* Housing Record Labels                                                            
* ------------------------------------------------                                 
*/                                                                                 
  RECTYPE = "Record Type"                                                          
  SERIALNO = "Housing/Group Quarters (GQ) Unit Serial Number"                      
  SAMPLE = "Sample Identifier"                                                     
  STATE = "State (FIPS)"                                                           
  REGION = "Region"                                                                
  DIVISION = "Division"                                                            
  PUMA1 = "Super Public Use Microdata Area Code (SuperPUMA)"                       
  MSACMSA1 = "Metropolitan Area:MSA/CMSA for SuperPUMA"                            
  MSAPMSA1 = "Metropolitan Area:MSA/PMSA for SuperPUMA"                            
  AREATYP1 = "Metropolitan Area:SuperPUMA Relationship to MA"                      
  TOTPUMA1 = "Total Area of SuperPUMA"                                             
  LNDPUMA1 = "Land Area of SuperPUMA"                                              
  SUBSAMPL = "Subsample Number"                                                    
  HWEIGHT = "Housing Unit Weight"                                                  
  PERSONS = "Number of Person Records Following This Housing Record"               
  UNITTYPE = "Type of Unit"                                                        
  HSUB = "Substitution Flag"                                                       
  HAUG = "Augmentation Flag"                                                       
  VACSTAT = "Vacancy Status"                                                       
  VACSTATA = "Vacancy Status Allocation Flag"                                      
  TENURE = "Home Ownership"                                                        
  TENUREA = "Home Ownership Allocation Flag"                                       
  BLDGSZ = "Size of Building"                                                      
  BLDGSZA = "Size of Building Allocation Flag"                                     
  YRBUILT = "Year Building Built"                                                  
  YRBUILTA = "Year Building Built Allocation Flag"                                 
  YRMOVED = "Year Moved In"                                                        
  YRMOVEDA = "Year Moved In Allocation Flag"                                       
  ROOMS = "Number of Rooms"                                                        
  ROOMSA = "Number of Rooms Allocation Flag"                                       
  BEDRMS = "Number of Bedrooms"                                                    
  BEDRMSA = "Number of Bedrooms Allocation Flag"                                   
  CPLUMB = "Complete Plumbing Facilities"                                          
  CPLUMBA = "Complete Plumbing Facilities Allocation Flag"                         
  CKITCH = "Complete Kitchen Facilities"                                           
  CKITCHA = "Complete Kitchen Facilities Allocation Flag"                          
  PHONE = "Telephone Availability"                                                 
  PHONEA = "Telephone Availability Allocation Flag"                                
  FUEL = "Heating Fuel"                                                            
  FUELA = "Heating Fuel Allocation Flag"                                           
  VEHICL = "Number of Vehicles Available"                                          
  VEHICLA = "Number of Vehicles Available Allocation Flag"                         
  BUSINES = "Commercial Business on Property"                                      
  BUSINESA = "Commercial Business on Property Allocation Flag"                     
  ACRES = "Acreage"                                                                
  ACRESA = "Acreage Allocation Flag"                                               
  AGSALES = "Sales of Agricultural Products in 1999"                               
  AGSALESA = "Sales of Agricultural Products in 1999 Allocation Flag"              
  ELEC = "Cost of Electricity (annual)"                                            
  ELECA = "Cost of Electricity (annual) Allocation Flag"                           
  GAS = "Cost of Gas (annual)"                                                     
  GASA = "Cost of Gas (annual) Allocation Flag"                                    
  WATER = "Cost of Water and Sewage (annual)"                                      
  WATERA = "Cost of Water and Sewage (annual) Allocation Flag"                     
  OIL = "Cost of Oil, Kerosene, or Wood (annual)"                                  
  OILA = "Cost of Oil, Kerosene, or Wood (annual) Allocation Flag"                 
  RENT = "Monthly Rent"                                                            
  RENTA = "Monthly Rent Allocation Flag"                                           
  MEALS = "Meals Included in Rent"                                                 
  MEALSA = "Meals Included in Rent Allocation Flag"                                
  MORTG1 = "Mortgage Status"                                                       
  MORTG1A = "Mortgage Status Allocation Flag"                                      
  MRT1AMT = "Mortgage Payment (monthly amount)"                                    
  MRT1AMTA = "Mortgage Payment (monthly amount) Allocation Flag"                   
  MORTG2 = "Second Mortgage Status"                                                
  MORTG2A = "Second Mortgage Status Allocation Flag"                               
  MRT2AMT = "Second Mortgage Payment (monthly amount)"                             
  MRT2AMTA = "Second Mortgage Payment (monthly amount) Allocation Flag"            
  TAXINCL = "Property Tax Status"                                                  
  TAXINCLA = "Property Tax Status Allocation Flag"                                 
  TAXAMT = "Property Tax Amount (annual)"                                          
  TAXAMTA = "Property Tax Amount (annual) Allocation Flag"                         
  INSINCL = "Property Insurance Status"                                            
  INSINCLA = "Property Insurance Status Allocation Flag"                           
  INSAMT = "Property Insurance Amount (annual)"                                    
  INSAMTA = "Property Insurance Amount (annual) Allocation Flag"                   
  CONDFEE = "Condominium Fee (monthly)"                                            
  CONDFEEA = "Condominium Fee (monthly) Allocation Flag"                           
  VALUE = "Property Value"                                                         
  VALUEA = "Property Value Allocation Flag"                                        
  MHLOAN = "Mobile Home Loan Status"                                               
  MHLOANA = "Mobile Home Loan Status Allocation Flag"                              
  MHCOST = "Mobile Home Costs"                                                     
  MHCOSTA = "Mobile Home Costs Allocation Flag"                                    
  HHT = "Household/Family Type"                                                    
  P65 = "Number of People 65 Years and Over in Household"                          
  P18 = "Number of People Under 18 Years in Household"                             
  NPF = "Number of People in Family"                                               
  NOC = "Number of Own Children Under 18 Years in Household"                       
  NRC = "Number of Related Children Under 18 Years in Household"                   
  PSF = "Presence of Subfamily in Household"                                       
  PAOC = "Presence and Age of Own Children Under 18 Years"                         
  PARC = "Presence and Age of Related Children Under Years"                        
  SVAL = "Specified Value Indicator"                                               
  SMOC = "Selected Monthly Owner Costs"                                            
  SMOCAPI = "Selected Monthly Owner Costs as a Percentage of Household Income"     
  SRNT = "Specified Rent Indicator"                                                
  GRENT = "Gross Rent"                                                             
  GRAPI = "Gross Rent as a Percentage of Household Income"                         
  FNF = "Fam/Nonfam Recode"                                                        
  HHL = "Household Language"                                                       
  LNGI = "Linguistic Isolation"                                                    
  WIF = "Number of Workers in Family"                                              
  EMPSTAT = "Family Type and Employment Status"                                    
  WORKEXP = "Family Type and Work Experience of Householder"                       
  HINC = "Household Total Income in 1999"                                          
  FINC = "Family Total Income in 1999"                                             
/*                                                                                 
* ------------------------------------------------------                           
* Person Record Labels                                                             
* ------------------------------------------------------                           
*/                                                                                 
  PNUM = "Person Sequence Number"                                                  
  PAUG = "Augmented Person Flag"                                                   
  DDP = "Data-defined Person Flag"                                                 
  PWEIGHT = "Person Weight"                                                        
  RELATE = "Relationship"                                                          
  RELATEA = "Relationship Allocation Flag"                                         
  OC = "Own Child Indicator"                                                       
  RC = "Related Child Indicator"                                                   
  PAOCF = "Presence and Age of Own Children, Females"                              
  SEX = "Sex"                                                                      
  SEXA = "Sex Allocation Flag"                                                     
  AGE = "Age"                                                                      
  AGEA = "Age Allocation Flag"                                                     
  HISPAN = "Hispanic or Latino Origin"                                             
  HISPANA = "Hispanic or Lationo Origin Allocation Flag"                           
  NUMRACE = "Number of Major Race Groups Marked"                                   
  WHITE = "White recode"                                                           
  BLACK = "Black or African American recode"                                       
  AIAN = "American Indian and Alaska Native recode"                                
  ASIAN = "Asian recode"                                                           
  NHPI = "Native Hawaiian and Other Pacific Islanders recode"                      
  OTHER = "Some other race recode"                                                 
  RACE1 = "Race Recode 1"                                                          
  RACE2 = "Race Recode 2"                                                          
  RACE3 = "Race Recode 3"                                                          
  RACEA = "Race Allocation Flag"                                                   
  MARSTAT = "Marital Status"                                                       
  MARSTATA = "Marital Status Allocation Flag"                                      
  MSP = "Married, Spouse Present Recode"                                           
  SFN = "Subfamily Number for this Person"                                         
  SFREL = "Subfamily Relationship"                                                 
  ENROLL = "School Enrollment: Attended since February 1, 2000"                    
  ENROLLA = "School Enrollment: Attended since February 1, 2000 Allocation Flag"   
  GRADE = "School Enrollment: Grade Level Attending"                               
  GRADEA = "School Enrollment: Grade Level Attending Allocation Flag"              
  EDUC = "Educational Attainment"                                                  
  EDUCA = "Educational Attainment Allocation Flag"                                 
  ANCFRST1 = "Ancestry Code 1 for 1-Percent File"                                  
  ANCSCND1 = "Ancestry Code 2 for 1-Percent File"                                  
  ANCA = "Ancestry Allocation Flag"                                                
  ANCR = "Ancestry Recode"                                                         
  SPEAK = "Non-English Language"                                                   
  SPEAKA = "Non-English Language Allocation Flag"                                  
  LANG1 = "Language Spoken for 1-Percent File"                                     
  LANGA = "Language Spoken for 1-Percent File Allocation Flag"                     
  ENGABIL = "English Ability"                                                      
  ENGABILA = "English Ability Allocation Flag"                                     
  POB1 = "Place of Birth for 1-Percent File"                                       
  POBA = "Place of Birth for 1-Percent File Allocation Flag"                       
  CITIZEN = "Citizenship Status"                                                   
  CITIZENA = "Citizenship Status Allocation Flag"                                  
  YR2US = "Year of Entry to United States"                                         
  YR2USA = "Year of Entry to United States Allocation Flag"                        
  MOB = "Residence 5 Years Ago"                                                    
  MOBA = "Residence 5 Years Ago Allocation Flag"                                   
  MIGST1 = "Migration State or Foreign Country Code for 1-Percent File"            
  MIGPUMA1 = "Migration Super-PUMA"                                                
  MIGAREA1 = "Migration Super-PUMA Relationship to MA"                             
  MIGCMA1 = "Migration MA: MSA/CMSA for Migration Super-PUMA"                      
  MIGPMA1 = "Migration MA: MSA/PMSA For Migration Super-PUMA"                      
  SENSORY = "Sensory Disability"                                                   
  SENSORYA = "Sensory Disability Allocation Flag"                                  
  PHYSCL = "Physical Disability"                                                   
  PHYSCLA = "Physical Disability Allocation Flag"                                  
  MENTAL = "Mental Disability"                                                     
  MENTALA = "Mental Disability Allocation Flag"                                    
  SLFCARE = "Self-Care Disability"                                                 
  SLFCAREA = "Self-Care Disability Allocation Flag"                                
  ABGO = "Able to Go Out Disability"                                               
  ABGOA = "Able to Go Out Disability Allocation Flag"                              
  ABWORK = "Employment Disability"                                                 
  ABWORKA = "Employment Disability Allocation Flag"                                
  DISABLE = "Disability Recode"                                                    
  GRANDC = "Presence of Grandchildren under 18 years"                              
  GRANDCA = "Presence of Grandchildren under 18 years Allocation Flag"             
  RSPNSBL = "Responsible for Grandchildren"                                        
  RSPNSBLA = "Responsible for Grandchildren Allocation Flag"                       
  HOWLONG = "Length of Responsibility for Grandchildren"                           
  HOWLONGA = "Length of Responsibility for Grandchildren Allocation Flag"          
  MILTARY = "Military Service"                                                     
  MILTARYA = "Military Service Allocation Flag"                                    
  VPS1 = "On active duty April 1995 or later"                                      
  VPS2 = "On active duty August 1990-March 1995 (Incl. Persian Gulf War)"          
  VPS3 = "On active duty September 1980-July 1990"                                 
  VPS4 = "On active duty May 1975-August 1980"                                     
  VPS5 = "On active duty during Vietnam Era: August 1964-April 1975"               
  VPS6 = "On active duty February 1955-July 1964"                                  
  VPS7 = "On active duty during Korean War: June 1950-January 1955"                
  VPS8 = "On active duty during World War II: September 1940-July 1947"            
  VPS9 = "On active duty any other time"                                           
  VPSA = "Veterans Period of Service Alloction Flag"                               
  MILYRS = "Years of Military Service"                                             
  MILYRSA = "Years of Military Service Allocation Flag"                            
  VPSR = "Veterans Period of Service Recode"                                       
  ESR = "Employment Status Recode"                                                 
  ESRA = "Employment Status Allocation Flag"                                       
  ESP = "Employment Status of Parent(s)"                                           
  POWST1 = "Place of Work State or Foreign Country Code for 1-Percent File"        
  POWSTA = "Place of Work State or Foreign Country Code Allocation Flag"           
  POWPUMA1 = "Place of Work Super-PUMA"                                            
  POWAREA1 = "Place of Work Super-PUMA Relationship to MA"                         
  POWCMA1 = "Place of Work MA: MSA/CMSA for Place of Work Super-PUMA"              
  POWPMA1 = "Place of Work MA: MSA/PMSA for Place of Work Super-PUMA"              
  TRVMNS = "Means of Transportation to Work"                                       
  TRVMNSA = "Means of Transportation to Work Allocation Flag"                      
  CARPOOL = "Vehicle Occupancy"                                                    
  CARPOOLA = "Vehicle Occupancy Allocation Flag"                                   
  LVTIME = "Time Leaving for Work"                                                 
  LVTIMEA = "Time Leaving for Work Allocation Flag"                                
  TRVTIME = "Travel Time to Work"                                                  
  TRVTIMEA = "Travel Time to Work Allocation Flag"                                 
  LAYOFF = "Layoff from Job"                                                       
  ABSENT = "Absent from Work"                                                      
  RECALL = "Return-to-Work Recall"                                                 
  LOOKWRK = "Looking for Work"                                                     
  BACKWRK = "Back to Work"                                                         
  LASTWRK = "Year Last Worked"                                                     
  LASTWRKA = "Year Last Worked Allocation Flag"                                    
  INDCEN = "Industry (Census)"                                                     
  INDCENA = "Industry (Census) Allocation Flag"                                    
  INDNAICS = "Industry (NAICS)"                                                    
  OCCCEN1 = "Occupation (Census) for 1-Percent File"                               
  OCCCENA = "Occupation (Census) Allocation Flag"                                  
  OCCSOC1 = "Occupation (SOC) for 1-Percent File"                                  
  CLWKR = "Class of Worker"                                                        
  CLWKRA = "Class of Worker Allocation Flag"                                       
  WRKLYR = "Worked in 1999"                                                        
  WRKLYRA = "Worked in 1999 Allocation Flag"                                       
  WEEKS = "Weeks Worked in 1999"                                                   
  WEEKSA = "Weeks Worked in 1999 Allocation Flag"                                  
  HOURS = "Hours Per Week Worked in 1999"                                          
  HOURSA = "Hours Per Week Worked in 1999 Allocation Flag"                         
  INCWS = "Wage/Salary Income in 1999"                                             
  INCWSA = "Wage/Salary Income in 1999 Allocation Flag"                            
  INCSE = "Self-Employment Income in 1999"                                         
  INCSEA = "Self-Employment Income in 1999 Allocation Flag"                        
  INCINT = "Interest Income in 1999"                                               
  INCINTA = "Interest Income in 1999 Allocation Flag"                              
  INCSS = "Social Security Income in 1999"                                         
  INCSSA = "Social Security Income in 1999 Allocation Flag"                        
  INCSSI = "Supplemental Security Income in 1999"                                  
  INCSSIA = "Supplemental Security Income in 1999 Allocation Flag"                 
  INCPA = "Public Assistance Income in 1999"                                       
  INCPAA = "Public Assistance Income in 1999 Allocation Flag"                      
  INCRET = "Retirement Income in 1999"                                             
  INCRETA = "Retirement Income in 1999 Allocation Flag"                            
  INCOTH = "Other Income in 1999"                                                  
  INCOTHA = "Other Income in 1999 Allocation Flag"                                 
  INCTOT = "Persons Total Income in 1999"                                          
  INCTOTA = "Persons Total Income in 1999 Allocation Flag"                         
  EARNS = "Persons Total Earnings in 1999"                                         
  POVERTY = "Persons Poverty Status"                                               
  ;              
  run;
%mend txt2ssd;

%macro batch_proc(sequ);
  %let blank=%str( );
  %let n=1;
  %let pos=%scan(&sequ,&n,&blank);
  %do %while(&pos ne &blank);
     %txt2ssd(&pos);
	 %let n=%eval(&n+1);
	 %let pos=%scan(&sequ,&n,&blank);
  %end;
%mend batch_proc;

%let sequ=0001 0002 0003 0004 0005 0006 0007 0008 ;
%batch_proc(&sequ);