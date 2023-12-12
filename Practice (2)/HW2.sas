/* We set a name to the folder that our data has been saved. */


LIBNAME HW2 'E:\Users\SST180003\Desktop\HW2';

/* This imports the csv dataset into SAS. */
/* You can do it by using the "Import Data" option in File on the main menu */

PROC IMPORT OUT= HW3.HeinzHunts 
            DATAFILE= "E:\Users\SST180003\Desktop\HW2\Heinz Hunts Data.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/* generating the working dataset in Work library */

data HeinzHunts;
 set HW2.HeinzHunts;
run; 
/* Summary Statistics using PROC tabulate */
proc tabulate data= HeinzHunts ;
 var heinz hunts priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts; *variables described by the table; 
 table heinz hunts priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts,(N Mean StdDev Min p25 Median p75 Max); 
 title 'Sum of each variable'; 
run;

/* choosing the 80% of sample. Seed = 2 will help you have same randome samples if you repeat the analysis */

/* Create training and test datasets. 80% of sample in training  */
proc surveyselect data=HeinzHunts out=HeinzHunts_sampled outall samprate=0.8 seed=2;
run;

data HeinzHunts_training HeinzHunts_test;
 set HeinzHunts_sampled;
 if selected then output HeinzHunts_training; /* Tell SAS that only keep the 70% selected one in sample. The rest will be in test data */
 else output HeinzHunts_test;
run;
ods rtf file='result';
/* Linear probability model using linear regression */
proc glm data=HeinzHunts_sampled ;
 class displayheinz(ref='0') displayhunts(ref='0') featureheinz(ref='0') featurehunts(ref='0');
 model heinz = priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts displayheinz*priceheinz displayhunts*pricehunts featureheinz*priceheinz featurehunts*pricehunts/solution;
 weight selected; /*only training sample is used for estimation, since selected=0 for test sample */
run;
quit;
ods rtf close;

/*ASE in train vs. test data */
/* Forward selection with significant level of coefficients as criteria */
proc glmselect data=HeinzHunts_training testdata=HeinzHunts_test  plots=all;
 class displayheinz(ref='0') displayhunts(ref='0') featureheinz(ref='0') featurehunts(ref='0');
 model heinz = priceheinz|pricehunts|displayheinz|displayhunts|featureheinz|featurehunts@2
  /selection=forward(select=aic) hierarchy=single showpvalues ;
 performance buildsscp=incremental;
run;
 ods rtf file='result';
/* Logistic Regression */
proc logistic data=HeinzHunts_sampled;
class displayheinz(ref='0') displayhunts(ref='0') featureheinz(ref='0') featurehunts(ref='0');
 logit: model heinz (event='1') = priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts displayheinz*priceheinz displayhunts*pricehunts featureheinz*priceheinz featurehunts*pricehunts;
 weight selected; /*only training sample is used for estimation, since selected = 0 for test sample */
run;
quit;
 ods rtf close;
 ods rtf file='result';
/* Probit Regression */
proc logistic data=HeinzHunts_training outmodel=Probitmodel;
class displayheinz(ref='0') displayhunts(ref='0') featureheinz(ref='0') featurehunts(ref='0');
 probit: model heinz (event='1') = priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts displayheinz*priceheinz displayhunts*pricehunts featureheinz*priceheinz featurehunts*pricehunts / link=NORMIT;
run;
 ods rtf close;

/*Or for probit they can do */

proc probit data=HeinzHunts_training ;
class displayheinz(ref='0') displayhunts(ref='0') featureheinz(ref='0') featurehunts(ref='0');
 probit: model heinz (event='1') = priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts displayheinz*priceheinz displayhunts*pricehunts featureheinz*priceheinz featurehunts*pricehunts;
run;



/* Classification and ROC */

/* Linear probability model */
/* Make predictions for test observations */

/* comparing ROC curve on in-sample data repsect to different predictors */
/* To plot ROC curve based on predictions from linear model */
 ods rtf file='result';
proc logistic data=HeinzHunts_sampled plots=roc(id=prob);
class displayheinz(ref='0') displayhunts(ref='0') featureheinz(ref='0') featurehunts(ref='0');
 logit: model heinz (event='1') = priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts displayheinz*priceheinz displayhunts*pricehunts featureheinz*priceheinz featurehunts*pricehunts/nofit;
 roc 'all' priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts displayheinz*priceheinz displayhunts*pricehunts featureheinz*priceheinz featurehunts*pricehunts;
 roc 'priceheinz' priceheinz;
 roc  'pricehunts' pricehunts;
 roc  'displayheinz' displayheinz;
 roc 'displayhunts' displayhunts;
 roc  'featureheinz' featureheinz;
 roc 'featurehunts' featurehunts;
 where selected=0;
run;
 ods rtf close;

proc glm data=HeinzHunts_sampled ;
 class displayheinz(ref='0') displayhunts(ref='0') featureheinz(ref='0') featurehunts(ref='0');
 model heinz = priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts displayheinz*priceheinz displayhunts*pricehunts featureheinz*priceheinz featurehunts*pricehunts /solution;
weight selected;
output out=HeinzHunts_lin_predict p=linear_predictions; /* predictions are made for all observations - training and test */
quit;
ods graphics on;
/* To plot ROC curve based on predictions from linear model */
proc logistic data=HeinzHunts_lin_predict plots=roc(id=prob);
 class displayheinz(ref='0') displayhunts(ref='0') featureheinz(ref='0') featurehunts(ref='0');
 logit: model heinz (event='1') = priceheinz pricehunts displayheinz displayhunts featureheinz featurehunts displayheinz*priceheinz displayhunts*pricehunts featureheinz*priceheinz featurehunts*pricehunts/nofit;
 roc pred=linear_predictions;

 where selected=0;
run;




