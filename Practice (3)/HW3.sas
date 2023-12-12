/* We set a name to the folder that our data has been saved. */


LIBNAME HW3 'E:\Users\SST180003\Desktop\HW3';

/* This imports the csv dataset into SAS. */
/* You can do it by using the "Import Data" option in File on the main menu */

PROC IMPORT OUT= HW3.crackers 
            DATAFILE= "E:\Users\SST180003\Desktop\HW3\crackers.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/* generating the working dataset in Work library */

data crackersorginal;
 set HW3.crackers;
run; 
/* Summary Statistics using PROC tabulate */
proc tabulate data= crackersorginal ;
 var private sunshine keebler nabisco  priceprivate pricesunshine pricekeebler pricenabisco featprivate featsunshine featkeebler featnabisco displprivate Displsunshine Displkeebler Displnabisco; *variables described by the table; 
 table private sunshine keebler nabisco  priceprivate pricesunshine pricekeebler pricenabisco featprivate featsunshine featkeebler featnabisco displprivate Displsunshine Displkeebler Displnabisco,(N Mean StdDev Min p25 Median p75 Max); 
 title 'Sum of each variable'; 
run;

/* choosing the 80% of sample. Seed = 2 will help you have same randome samples if you repeat the analysis */

/* Create training and test datasets. 80% of sample in training  */
proc surveyselect data=crackersorginal out=crackers_sampled outall samprate=0.8 seed=2;
run;


/* reform the data */
data crackers_sampled;
set crackers_sampled;
if private  =1 then choice =1;
if sunshine =1 then choice =2;
if keebler =1  then  choice =3;
if nabisco =1 then choice =4;
run;




data  crackers_id(keep=pid decision mode selected price  feature display);
      set crackers_sampled;
      array tvec{12} priceprivate pricesunshine pricekeebler pricenabisco featprivate featsunshine featkeebler featnabisco displprivate Displsunshine Displkeebler Displnabisco;
      retain pid 0;
      pid + 1;
      do i = 1 to 4;
         mode = i;
         decision = ( choice = i );
         price = tvec{i};
         feature =  tvec{i+4};
         display = tvec{i+8};
         output;
end; 
run;

data crackers crackers_test;
 set crackers_id;
 if selected =1 then output crackers; /* Tell SAS that only keep the 80% selected one in sample. The rest will be in test data */
 else output crackers_test;
run;

proc logistic data=crackers;
 strata pid;
 class mode (ref = '1') / param=glm;
 model decision (event='1') = mode price feature Display feature*price Display*price;
run;



/* Logit with alternative-specific characteristics */
proc logistic data=crackers;
 strata pid;
 class mode (ref = '1') / param=glm;
 model decision (event='1') = mode price mode*feature mode*Display mode*feature*price mode*Display*price;
run;

/* Replicate results using Proc MDC */

/* mdc can accomodate only one id variable to identify a choice occassion - strata can have multiple nested variables */
/* mdc expects 0 1 coding for choice variable. */
/* mdc does not allow flexible options to code class variable. Need to explicitly force reference levels to zero */
/* YOU CANNOT INSERT INTERACTION EFFECT DIRECTLY INTO PROC MDC*/

 data crackers;
 set crackers;
 pricefeature = price*feature;
 pricedisplay = Display*price;
 run;
 data crackers_test;
 set crackers_test;
 pricefeature = price*feature;
 pricedisplay = Display*price;
 run;

proc mdc data = crackers;
  id pid;
  class  mode;
  model decision = mode price mode*feature mode*Display mode*pricefeature mode*pricedisplay /  type = clogit nchoice = 4;
  restrict mode1=0; /* mdc does not allow flexible options to code class variable. Need to explicitly force reference levels to zero */
 run;

/* Using Multinomial Probit to allow correlated errors across alternatives and does not suffere from IIA. However, there is no longer a closed form solution for choice probablites */
/* Multinomial Probit without restrictions */
proc mdc data = crackers;
 id pid;
 class mode;
 model decision = mode price mode*feature mode*Display mode*pricefeature mode*pricedisplay/ type = mprobit nchoice = 4;
 restrict mode1 =0;
run;
/* Multinomial Probit restricting errors to be iid  Q8(I) */
proc mdc data = crackers;
  id pid;
 class mode;
 model decision = mode price mode*feature mode*Display mode*pricefeature  / type= mprobit nchoice= 4 unitvariance= (1 2 3 4);
  restrict mode1=0; /* mdc does not allow flexible options to code class variable. Need to explicitly force reference levels to zero */
  restrict rho_21=0;
  restrict rho_31=0;
  restrict rho_32=0;
run;


/* Multinomial Probit restricting errors to be uncorrelated, but allowing heteroschedasticity Q8(II) */
proc mdc data = crackers;
 id pid;
 class mode;
 model decision = mode price mode*feature mode*Display mode*pricefeature mode*pricedisplay/ type= mprobit nchoice= 4;
  restrict mode1=0; /* mdc does not allow flexible options to code class variable. Need to explicitly force reference levels to zero */
  restrict rho_21=0;
  restrict rho_31=0;
  restrict rho_32=0;
run;

/* Multinomial Probit allowing errors to be correlated, but restricting them to be unit variance Q8(III) */
proc mdc data = crackers;
 id pid;
 class mode;
 model decision = mode price mode*feature mode*Display mode*pricefeature mode*pricedisplay/ type= mprobit nchoice= 4 unitvariance= (1 2 3 4);
  restrict mode1=0; /* mdc does not allow flexible options to code class variable. Need to explicitly force reference levels to zero */
run;
data crackers_test;
set crackers_test;
decision =.;
run;
data extdata;
   set crackers crackers_test;
run;

/* Logit with alternative-specific characteristics */
proc mdc data=extdata;
 id pid;
  class  mode;
  model decision = mode price mode*feature mode*Display mode*pricefeature mode*pricedisplay/  type = clogit nchoice = 4;
  restrict mode1=0; /* mdc does not allow flexible options to code class variable. Need to explicitly force reference levels to zero */
output out=probdata pred=p;
run;
data probdata;
set probdata;
if p >=0.5 then pchoice = 1;
if p <0.5 then pchoice = 0;
run;

proc sort data = probdata;
by mode;
run;
proc means data=probdata;
var pchoice;
by mode;
where selected = 0;
run;
data crackers_test_price;
set crackers_test;
if mode = 4 then price = price -(0.15*price);
run;

data extdata_price;
   set crackers crackers_test_price;
run;

/* Logit with alternative-specific characteristics */
proc mdc data=extdata_price;
 id pid;
  class  mode;
  model decision = mode price mode*feature mode*Display mode*pricefeature /  type = clogit nchoice = 4;
  restrict mode1=0; /* mdc does not allow flexible options to code class variable. Need to explicitly force reference levels to zero */
output out=probdata_price pred=p;
run;

data probdata_price;
set probdata_price;
if p >=0.5 then pchoice = 1;
if p <0.5 then pchoice = 0;
run;

proc sort data = probdata_price;
by mode;
run;
proc means data=probdata_price;
var pchoice;
by mode;
where selected = 0;
run;

data crackers_test_feature;
set crackers_test;
if mode = 4 then feature = 1;
if mode = 4 then price = price +(0.15*price);
run;

data extdata_feature;
   set crackers crackers_test_feature;
run;

/* Logit with alternative-specific characteristics */
proc mdc data=extdata_feature;
 id pid;
  class  mode;
  model decision = mode price mode*feature mode*Display mode*pricefeature /  type = clogit nchoice = 4;
  restrict mode1=0; /* mdc does not allow flexible options to code class variable. Need to explicitly force reference levels to zero */
output out=probdata_feature pred=p;
run;

data probdata_feature;
set probdata_feature;
if p >=0.5 then pchoice = 1;
if p <0.5 then pchoice = 0;
run;

proc sort data = probdata_feature;
by mode;
run;
proc means data=probdata_feature;
var pchoice;
by mode;
where selected = 0;
run;





