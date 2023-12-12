/* We set a name to the folder that our data has been saved. */


LIBNAME HW1 'E:\Users\SST180003\Desktop\HW1';

/* This imports the csv dataset into SAS. */
/* You can do it by using the "Import Data" option in File on the main menu */

PROC IMPORT OUT= HW1.Sales 
            DATAFILE= "E:\Users\SST180003\Desktop\HW1\Sales.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= HW1.SALES_test 
            DATAFILE= "E:\Users\SST180003\Desktop\HW1\Sales_test.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
/* generating the working dataset in Work library */
data sales;
 set HW1.sales;
run; 

/* generating the working dataset in Work library */
data sales_test;
 set HW1.Sales_test;
run; 

PROC sort data = sales;
by season;
run;
proc freq data= sales;
table LOCATION;
 title 'Summary Statistics of categorical variables';
 by season;
run;

/* Histograms */
proc sgplot data= sales;
 histogram salesunit / binwidth = 1000 ; 
 density salesunit / type = kernel; 
 density salesunit/type = normal;
 title 'salesunit Report';
run;
/* Histograms */
proc sgplot data= sales;
 histogram price / binwidth = 10 ; 
 density price / type = kernel; 
 density price/type = normal;
 title 'price Report';
run;

/* Histograms */
proc sgplot data= sales;
 histogram advertisingspending / binwidth = 100 ; 
 density advertisingspending / type = kernel; 
 density advertisingspending /type = normal;
 title 'advertising Report';
run;

ods graphics on;
/* Summary Statistics using PROC means: We can choose our variables */
proc tabulate data= sales;
var salesunit price advertisingspending;
table (salesunit price advertisingspending),(n mean stddev min p25 median p75 max);
title 'Summary Statistics';
run;
ods graphics off;
proc anova data= sales;
class season;
model  salesunit= season;
title;
run;
proc anova data= sales;
class season;
model  price= season;
title;
run;
proc anova data= sales;
class season;
model  advertisingspending= season;
title;
run;
proc ttest data=sales;
class location;
var salesunit;
run;
proc ttest data=sales;
class location;
var price;
run;
proc ttest data=sales;
class location;
var advertisingspending;
run;
proc corr data= sales;
var salesunit price advertisingspending;
 title 'corrolation matrix Report';
run;
proc corr data= sales spearman;
var salesunit price advertisingspending;
 title 'corrolation matrix Report';
run;

proc sgscatter data= sales;
 matrix salesunit price advertisingspending / diagonal= (histogram);
 title 'Relation between salsunit price advertisingspending';
run;
/*Spending Regression Model --> Regression of Spending over price  */
proc reg data = sales;
 model Salesunit = price;
run;
/* Defining the power 2 of price */
data sales;
 set sales;
 price_2 = price * price;
run;
/* Defining the power 2 of price */
data sales_test;
 set sales_test;
 price_2 = price * price;
run;
ods graphics on;
/*Spending Regression Model --> Regression of Salesunit over price and its power 2 */
proc reg data = sales;
 model Salesunit =  price price_2;
run;
ods graphics off;
/*Spending Regression Model --> Regression of Spending over advertisingspending  */
proc reg data = sales;
 model salesunit = advertisingspending;
run;
/* Defining the power 2 of advertisngspending */
data sales;
 set sales;
 advertisingspending_2 = advertisingspending**2;
run;
/* Defining the power 2 of advertisngspending */
data sales_test;
 set sales_test;
 advertisingspending_2 = advertisingspending**2;
run;
proc reg data = sales;
 model salesunit = advertisingspending  advertisingspending_2;
run;
ods graphics on;
proc reg data = sales;
 model salesunit = price price_2 advertisingspending  advertisingspending_2;
output out = regdata cookd = cookd student=sresiduals; 
run;
ods graphics off;

/* prints the observations that are influential (cook's d > 4/ n) */
proc print data=regdata ;
 var _ALL_;
 where Cookd > 4 / 1600;
run;
/* Estimate the model without the influential points */
ods graphics on;
proc reg data=regdata;
model salesunit = price price_2 advertisingspending  advertisingspending_2;
where Cookd < 4 / 1600;
run;

ods graphics on;
proc reg data = sales;
 model salesunit = price price_2 advertisingspending  advertisingspending_2 /collinoint vif ;
run;
ods graphics off;

ods graphics on;
proc reg data = sales;
 model salesunit = price  advertisingspending   /collinoint vif ;
run;
ods graphics off;
/*Question 4*/
proc glm data = sales;
class location(ref='Europe');
 model Salesunit = price price_2 location price*location /solution;
run;




proc glm data = sales;
class location(ref='Europe') promotion(ref='NoPromotion');
 model Salesunit = location promotion location*promotion /solution;
run;

/*Question 5*/
data sales;
 set sales;
 if design =1 & quality = 1 then Prodcut_Type=1;
 if design =1 & quality = 2 then Prodcut_Type=2;
 if design =2 & quality = 1 then Prodcut_Type=3;
if design =2 & quality = 2 then Prodcut_Type=4;
run;
data sales_test;
 set sales_test;
 if design =1 & quality = 1 then Prodcut_Type=1;
 if design =1 & quality = 2 then Prodcut_Type=2;
 if design =2 & quality = 1 then Prodcut_Type=3;
if design =2 & quality = 2 then Prodcut_Type=4;
run;
PROC sort data = sales;
by Prodcut_Type;
run;


/* Box Plots */
proc sgplot data= sales;
 hbox salesunit  / category= Prodcut_Type; 

 title;
run;
/* Box Plots */
proc sgplot data= sales;
 hbox salesunit  / category= Prodcut_Type group = location; 

 title;
run;

proc glm data = sales;
class Prodcut_Type(ref='4')location(ref='Europe') ;
 model Salesunit = price price*price prodcut_type location location*prodcut_type /solution;
run;

/*Question 6*/

/* Box Plots */
proc sgplot data= sales;
 hbox salesunit  / category= season ; 
 title;
run;
/* Box Plots */
proc sgplot data= sales;
 hbox salesunit  / category= season group = prodcut_type; 
 title;
run;

proc glm data = sales;
class Prodcut_Type(ref='4')season(ref='2Spring') ;
 model Salesunit = price price*price prodcut_type season season*prodcut_type /solution;
run;




/*Question 7*/

/* Box Plots */
proc sgplot data= sales;
 hbox salesunit  / category= salesforceexperience ; 
 title;
run;
/* Box Plots */
proc sgplot data= sales;
 hbox salesunit  / category= salesforceexperience group = prodcut_type; 
 title;
run;

proc glm data = sales;
class Prodcut_Type(ref='4')salesforceexperience(ref='Low') ;
 model Salesunit = price price*price prodcut_type salesforceexperience salesforceexperience*prodcut_type /solution;
run;
/*Question 8*/

/* generate indicator variables using glm_mod - useful to run proc reg for the categorical variables */
proc glmmod data=sales outdesign=sales_with_indicators noprint; 
 class Prodcut_Type  promotion salesforceexperience location season;
 model salesunit = price price_2 advertisingspending advertisingspending_2 Prodcut_Type  promotion salesforceexperience location season / noint;
run;
/* generate indicator variables using glm_mod - useful to run proc reg for the categorical variables */
proc glmmod data=sales_test outdesign=sales_test_with_indicators noprint; 
 class Prodcut_Type  promotion salesforceexperience location season;
 model salesunit = price price_2 advertisingspending advertisingspending_2 Prodcut_Type  promotion salesforceexperience location season / noint;
run;
proc contents data=sales_with_indicators; 
run;
proc contents data=sales_test_with_indicators; 
run;
/* estimate simple linear model that includes all main effects */
proc reg data=sales_with_indicators; 
 model col1 = col2-col19/ vif collinoint; 
run;
/* Best subsets regression */
proc reg data=sales_with_indicators outest = result  plots=none;
 model col1 = col2-col19 /selection=cp adjrsq aic bic best=1;
run;
quit;
proc score data=sales_test_with_indicators score=result Type=parms  predict out=predicted_data;
 var col2-col19;
run;
data predicted_data;
set  predicted_data;
residula_2 = (salesunit-model1)**2;
run;
proc means data = predicted_data mean ;
var residula_2; 
run;
ods graphics on;
/*ASE in train vs. test data */
/* Forward selection with cp as criteria */
proc glmselect data=sales testdata=sales_test  plots=ase;
 class Prodcut_Type(split)  promotion(split) salesforceexperience(split) location(split) season(split);
model salesunit = price|price_2|advertisingspending|advertisingspending_2|Prodcut_Type|promotion|salesforceexperience|location|season @2
  /selection=forward(select=cp) hierarchy=single;
 performance buildsscp=incremental;
run;


/*ASE in train vs. test data */
/* backward selection with cp as criteria */
proc glmselect data=sales testdata=sales_test  plots=ase;
 class Prodcut_Type(split)  promotion(split) salesforceexperience(split) location(split) season(split);
model salesunit = price|price_2|advertisingspending|advertisingspending_2|Prodcut_Type|promotion|salesforceexperience|location|season @2
  /selection=backward(select=cp) hierarchy=single;
 performance buildsscp=incremental;
run;
/*ASE in train vs. test data */
/* stepwise selection with cp as criteria */
proc glmselect data=sales testdata=sales_test  plots=ase;
 class Prodcut_Type(split)  promotion(split) salesforceexperience(split) location(split) season(split);
model salesunit = price|price_2|advertisingspending|advertisingspending_2|Prodcut_Type|promotion|salesforceexperience|location|season @2
  /selection=stepwise(select=cp) hierarchy=single;
 performance buildsscp=incremental;
run;
/*Question 9*/
/*ASE in train vs. test data */
/* Forward selection with cv as criteria */
proc glmselect data=sales testdata=sales_test seed=2  plots=ase;
 class Prodcut_Type(split)  promotion(split) salesforceexperience(split) location(split) season(split);
model salesunit = price|price_2|advertisingspending|advertisingspending_2|Prodcut_Type|promotion|salesforceexperience|location|season @2
  /selection=forward(select=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;

/*ASE in train vs. test data */
/* backward selection with cv as criteria */
proc glmselect data=sales testdata=sales_test seed=2  plots=ase;
 class Prodcut_Type(split)  promotion(split) salesforceexperience(split) location(split) season(split);
model salesunit = price|price_2|advertisingspending|advertisingspending_2|Prodcut_Type|promotion|salesforceexperience|location|season @2
  /selection=backward(select=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;
/*ASE in train vs. test data */
/* stepwise selection with cv as criteria */
proc glmselect data=sales testdata=sales_test seed=2  plots=ase;
 class Prodcut_Type(split)  promotion(split) salesforceexperience(split) location(split) season(split);
model salesunit = price|price_2|advertisingspending|advertisingspending_2|Prodcut_Type|promotion|salesforceexperience|location|season @2
  /selection=stepwise(select=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;

/*ASE in train vs. test data */
/* Stepwise with LASSO regression with AIC as criteria */
proc glmselect data=sales testdata=sales_test seed=2  plots=ase;
 class Prodcut_Type(split)  promotion(split) salesforceexperience(split) location(split) season(split);
model salesunit = price|price_2|advertisingspending|advertisingspending_2|Prodcut_Type|promotion|salesforceexperience|location|season @2
  /selection=lasso(choose=cv stop=none) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;
/*ASE in train vs. test data */
/* Stepwise with LASSO regression with AIC as criteria */
proc glmselect data=sales testdata=sales_test seed=2  plots=ase;
 class Prodcut_Type(split)  promotion(split) salesforceexperience(split) location(split) season(split);
model salesunit = price|price_2|advertisingspending|advertisingspending_2|Prodcut_Type|promotion|salesforceexperience|location|season @2
  /selection=elasticnet(choose=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;

