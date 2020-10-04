***************************************************************************************************
* SFSL Analysis of Imputed Datasets
* Date: February 13, 2018
* Updated: April 16, 2020
*
* This SAS file does the following:
*   A. Conducts demographic analysis:
*       1. Univariate education models were constructed for categorical education variables 
*       2. Creates multiple imputation corrected confidence regions for all variables
*   B. Creates necessary change variables for analysis
*       2. A multivariate model including all predictors (with education and group as categorical
*          variables) was constructed
*       3. A multivariate model which included predictors that were significant in the univariate
*          analysis was constructed
*       4. Comparisons and updated values in Tables 1, 3, and supplementary 3
**************************************************************************************************;

* Import imputed datafiles from R;
proc import datafile='C:\Users\Tim\SfSL_Demo331_PostImpute.csv' out=SfSL_Demo331_PostImpute dbms=csv replace; run;
proc import datafile='C:\Users\Tim\SfSL_Demo169_PostImpute.csv' out=SfSL_Demo169_PostImpute dbms=csv replace; run;
proc import datafile='C:\Users\Tim\SfSL_All_PostImpute.csv' out=SfSL_All_PostImpute dbms=csv replace; run;

* Conduct demographic analysis: 
   1. Recode education variable. ; 

proc format;
	value edlevelF
		1 = 'Less than high school'
		2 = 'High school'
		3 = 'Some post-secondary'
		4 = 'Completed post-secondary';
run;

* Create variables needed for analysis (pre-post change, education level, 
  years between onset of suicidality and first suicide attempt).
  This must be done on all datasets (full sample, the imputed records, and
  those planned for inclusion in analysis);

data SfSL_All(rename=(X_mult_=_Imputation_));
	set SfSL_All_PostImpute;
	lagattempt=age_attempt-age_onset;
	TASIF_change=TASIF_post-TASIF_mean;
	TASDF_change=TASDF_post-TASDF_mean;
	BDI_change=BDI_post-BDI_mean;
	BHS_change=BHS_post-BHS_mean;
	PSIConf_change=PSIConf_post-PSIConf_mean;
	SWL_change=SWL_post-SWL_mean;
	if education <3.5 then edlevel=1;
	else if education<4.5 then edlevel=2;
	else if education<5.5 then edlevel=3;
	else edlevel=4;
run;

data SfSL_331(rename=(X_mult_=_Imputation_));
	set SfSL_Demo331_PostImpute;
	lagattempt=age_attempt-age_onset;
	if education <3.5 then edlevel=1;
	else if education<4.5 then edlevel=2;
	else if education<5.5 then edlevel=3;
	else edlevel=4;
run;

data SfSL_169(rename=(X_mult_=_Imputation_));
	set SfSL_Demo169_PostImpute;
	lagattempt=age_attempt-age_onset;
	if education <3.5 then edlevel=1;
	else if education<4.5 then edlevel=2;
	else if education<5.5 then edlevel=3;
	else edlevel=4;
run;

* Here we create dummy variables for various analyses: gender, group sequence, and education ;

data SfSL_All2;
	set SfSL_All;
	if gender=2 then do;
		genderM=0;
		genderT=0;
		end;
	else if gender=1 then do;
	    genderM=1;
		genderT=0;
		end;
	else if gender=3 then do;
	    genderM=0;
		genderT=1;
		end;
	if group=2 then do;
		group2=1;
		group3=0;
		end;
	else if group=3 then do;
		group2=0;
		group3=1;
		end;
	else if group=1 then do;
		group2=0;
		group3=0;
		end;
	if edlevel>1 then HSPlus=1; else HSPlus=0;
	* HSPlus indicates anyone who has completed high school or further education ;
	if edlevel>2 then GTHS=1; else GTHS=0;
	* GTHS indicates anyone who has started any education beyond high school ;
	if edlevel>3 then PS=1; else PS=0;
	* PS indicates anyone who has completed any post-secondary education ;
run;

* Now we calculate the demographic information for our imputed sample (n=331)
  and the analyzed sample (n=169) ;
  
proc freq data=SfSL_331;
	table gender edlevel;
	title '331 participants - all participants';
run;

proc sort data=SfSl_169; by _imputation_; run;

proc freq data=SfSL_169;
	table gender edlevel;
	format edlevel edlevelF.;
	title '169 participants';
run;


* Here we create a macro to output mean and variance estimates for demographic variables for 
  each of our 2 demographic datasets (a third will be made to compare those included in the analyses 
  against those that were discarded), corrected for multiple imputation;
  
%macro mi_sfsl(data,out,edf);
  *ensure data is sorted by imputation variable;
  proc sort data=&data; by _Imputation_; run;
  
  *output mean and standard error estimates for each variable;
  proc univariate data=&data noprint;
  var age age_onset age_attempt numberdx numbatmp education lagattempt;
  output out=&out mean=age age_onset age_attempt numberdx numbatmp education lagattempt
                  stderr=se_age se_age_onset se_age_attempt se_numberdx se_numbatmp se_education se_lagattempt;
  by _Imputation_;
  run;
  
  *;
  proc mianalyze data=&out edf=&edf;
  modeleffects age age_onset age_attempt numberdx numbatmp education lagattempt;
  stderr se_age se_age_onset se_age_attempt se_numberdx se_numbatmp se_education se_lagattempt;
  run;
%mend;

* Now we run this macro on the two demographic datasets (imputed and analysed samples) ;

%mi_sfsl(SfSL_331,out_331,330);
%mi_sfsl(SfSL_169,out_169,168);


/******************************************************************************
* Comparison of demographics
*   Compare those participants that had measures on either side of a treatment 
*   group with those that did not and therefore could not be included in 
*   analyses. The purpose of this comparison is to show that there is no
*   systematic difference between the characteristics we assessed at baseline.
******************************************************************************/

* This code takes the full imputed sample (n=331) and marks the analysed
  sample (n=169) versus the non-analysed (called here missing) ;
  
proc sql;
create table missing_groups as
select a.*, b.ParticipantID
from SfSL_331 a left join SfSL_169 b
on a.ParticipantID=b.ParticipantID
where b.ParticipantID is NULL;
quit;

data missing_groups;
length comparison_group $ 8;
set missing_groups;
comparison_group="missing";
run;

proc sql; 
create table SfSL_331_Comp as
select a.*, b.comparison_group
from SfSL_331 a left join missing_groups b
on a.ParticipantID=b.ParticipantID and a._imputation_=b._imputation_;
quit;

data SfSL_331_Comp;
set SfSL_331_Comp;
if comparison_group ne 'missing' then comparison_group='complete';
run;

proc sort data=SfSL_331_Comp; by _imputation_; run;

* Compare gender (this had no missing, therefore _imputation_=1 can be used
  and no correction for multiple imputation is needed) ;

proc freq data=SfSL_331_Comp;
table comparison_group*gender /chisq exact;
where _imputation_=1;
run;

* Compare education level ;

proc freq data=SfSL_331_Comp;
table comparison_group*edlevel /chisq;
output out=educationChiSq pchi lrchi;
by _imputation_;
run;

* Here we create a macro to create an MI consistent summary for the imputed 
  demographic education data ;
  
%macro combchi(df=,chi=);
  proc iml;
    df=&df;
    g2={&chi};
    m=ncol(g2);
    g=sqrt(g2);
    mg2=sum(g2)/m;
    r=(1+1/m)*(ssq(g)-(sum(g)**2)/m)/(m-1);
    f=(mg2/df - r*(m-1)/(m+1))/(1+r);
    ddf=(m-1)*(1+1/r)**2/df**(3/m);
    p=1-probf(f,df,ddf);
    print f df ddf;
    print p;
  run;
%mend combchi;

%combchi(df=3, chi=3.6893866104 3.6007538764 4.0383824827 3.9080845132 4.4584131157 3.6002960997 2.6140400572 2.4509739477 3.2945178648 4.9943560221)

* Here we calculate the same demographic information as reported above for inclusion in table 1.
  Both SD and SE are reported in Table 1 ;

proc sort data=missing_groups; by _imputation_; run;

proc univariate data=missing_groups noprint;
var age age_onset age_attempt education numbatmp numberdx;
by _imputation_;
output out=SfSLMeans mean=mean_age mean_age_onset mean_age_attempt mean_education mean_numbatmp mean_numberdx
                     stderr=se_age se_age_onset se_age_attempt se_education se_numbatmp se_numberdx;
	title '162 participants - insufficient data for inclusion';
run;

proc mianalyze data=SfSLMeans;
modeleffects mean_age mean_age_onset mean_age_attempt mean_education mean_numbatmp mean_numberdx;
stderr se_age se_age_onset se_age_attempt se_education se_numbatmp se_numberdx;
run;

* Here we create a numeric dummy variable for analyzed versus omitted ;

data SfSL_331_Comp;
set SfSL_331_Comp;
if comparison_group = 'missing' then comp_group=1; else comp_group=0;
run;

* Here we create a macro to generate a test of difference between the analysed 
  and omitted data using a regression model with comp_group as the predictor, 
  and apply the appropriate multiple imputation correction ;
  
%macro mi_sfsl_reg_base(var,out,edf);
  *ensure data is sorted by imputation variable;
  proc sort data=SfSL_331_Comp; by _Imputation_; run;
  
  *output mean and standard error estimates for each variable;
  proc reg data=SfSL_331_Comp outest=&out covout noprint;
  model &var=comp_group;
  by _Imputation_;
  run;
  
  * now I combine the results with mianalyze;
  proc mianalyze data=&out edf=&edf;
  modeleffects Intercept comp_group;
  run;
%mend;

* Now we run this regression model on each of the continuous variables in Table 1 ;

%mi_sfsl_reg_base(numbatmp,outNumbAtmpB,329);
%mi_sfsl_reg_base(numberdx,outNumbDxBase,329);
%mi_sfsl_reg_base(age_onset,outAgeOnsetBase,329);
%mi_sfsl_reg_base(age_attempt,outAgeAttemptBase,329);

* For age (no missing data) the first imputation set from the data was used and
  no correction for multiple imputation is needed ;
  
proc reg data=SfSL_331_Comp covout;
model age=comp_group;
where _Imputation_=1;
run;


/**************************************
*  Main analyses (TAS outcomes)
***************************************/

* Here we create a macro to make a comparison between the 209 instances with 
  pre-post pairs of data to determine if changes are present due to group ;
  
%macro mi_sfsl_ttest(varPre,varPost,out,edf);
  proc sort data=SfSL_All; by _Imputation_; run;
  
  ods output Statistics=&out;
  proc ttest data=SfSL_All;
  paired &varPre * &varPost;
  by _Imputation_;
  run;
  
  proc mianalyze data=&out edf=&edf;
  modeleffects mean;
  stderr StdErr;
  run;
%mend;

* Now we run this macro on all outcome variables of interest ;

%mi_sfsl_ttest(TAS_mean,TAS_post,ttest_out,208);
%mi_sfsl_ttest(TASEF_mean,TASEF_post,ttest_EF_out,208);
%mi_sfsl_ttest(TASDF_mean,TASDF_post,ttest_DF_out,208);
%mi_sfsl_ttest(TASIF_mean,TASIF_post,ttest_IF_out,208);
%mi_sfsl_ttest(SWL_mean,SWL_post,ttest_SWL_out,208);
%mi_sfsl_ttest(PSI_mean,PSI_post,ttest_PSI_out,208);
%mi_sfsl_ttest(PSIAppAv_mean,PSIAppAv_post,ttest_PSIA_out,208);
%mi_sfsl_ttest(PSIPersCont_mean,PSIPersCont_post,ttest_PSIPC_out,208);
%mi_sfsl_ttest(PSIConf_mean,PSIConf_post,ttest_PSIC_out,208);
%mi_sfsl_ttest(BDI_mean,BDI_post,ttest_BDI_out,208);
%mi_sfsl_ttest(BHS_mean,BHS_post,ttest_BHS_out,208);
%mi_sfsl_ttest(BIS_mean,BIS_post,ttest_BIS_out,208);
%mi_sfsl_ttest(BISMotor_mean,BISMotor_post,ttest_BISM_out,208);
%mi_sfsl_ttest(BISAttention_mean,BISAttention_post,ttest_BISA_out,208);
%mi_sfsl_ttest(BISNonPlanning_mean,BISNonPlanning_post,ttest_BISN_out,208);

/* Regression analysis - univariate approach 
*  In this section a macro is written to test whether the final TAS
*   score is predicted by another variable, adjusting for each individual's
*   initial (pre-group) TAS score.
*/

* We define the macro ;

%macro mi_sfsl_reg_uni(var,out,edf);
  * ensure data is sorted by imputation variable;
  proc sort data=SfSL_All2; by _Imputation_; run;
  
  * output mean and standard error estimates for each variable;
  proc reg data=SfSL_All2 outest=&out covout noprint;
  model tas_post=TAS_mean &var;
  by _Imputation_;
  run;
  
  * combine the results with mianalyze;
  proc mianalyze data=&out edf=&edf;
  modeleffects Intercept TAS_mean &var;
  run;
%mend;

* Now we run this macro using various predictors of interest ;

%mi_sfsl_reg_uni(age,outAge,208);
%mi_sfsl_reg_uni(genderM,outGenderM,208);
%mi_sfsl_reg_uni(genderT,outGenderT,208);
%mi_sfsl_reg_uni(age_onset,outAge_Onset,208);
%mi_sfsl_reg_uni(age_attempt,outAge_Attempt,208);
%mi_sfsl_reg_uni(numberdx,outNumDx,208);
%mi_sfsl_reg_uni(numbatmp,outNumAtmp,208);
%mi_sfsl_reg_uni(BDI_mean,outBDI,208);
%mi_sfsl_reg_uni(BHS_mean,outBHS,208);
%mi_sfsl_reg_uni(SWL_mean,outSWL,208);
%mi_sfsl_reg_uni(PSI_mean,outPSI,208);
%mi_sfsl_reg_uni(BIS_mean,outBIS,208);
%mi_sfsl_reg_uni(group2,outGroup2,208);
%mi_sfsl_reg_uni(group3,outGroup3,208);
* The education variables listed below are testing if someone achieving
  this level of education (OR HIGHER) had a difference in TAS scores ;
%mi_sfsl_reg_uni(HSPlus,outHSPlus,208);
%mi_sfsl_reg_uni(GTHS,outGTHS,208);
%mi_sfsl_reg_uni(PS,outPS,208);


/* Regression analysis - all variables */
proc reg data=SfSL_All2 outest=outreg covout noprint;
model tas_post=tas_mean age genderM genderT age_onset age_attempt numberdx numbatmp HSPlus GTHS PS BDI_mean BHS_mean SWL_mean PSI_mean BIS_mean group2 group3;
by _Imputation_;
run;

proc mianalyze data=outreg edf=208;
modeleffects Intercept tas_mean age genderM genderT age_onset age_attempt numberdx numbatmp HSPlus GTHS PS BDI_mean BHS_mean SWL_mean PSI_mean BIS_mean group2 group3;
run;

/* Regression analysis - select variables: variables that were significant (p<0.05) in the univariate models */
proc reg data=SfSL_All2 outest=outregsmall covout noprint;
model tas_post=tas_mean age_onset GTHS;
by _Imputation_;
run;

proc mianalyze data=outregsmall edf=208;
modeleffects Intercept tas_mean age_onset GTHS;
run;


/******************************************
*  Path analyses (based on change scores) *
******************************************/

*Sort data for imputations;
proc sort data=sfSL_all; by _Imputation_; run;

* Output estimates for the mean change of each covariate;
proc univariate data=SfSL_All noprint;
var SWL_change BDI_change BHS_change PSIConf_change TASDF_change TASIF_change;
by _Imputation_;
output out=SfSLMeans mean=SWL_change BDI_change BHS_change PSIConf_change TASDF_change TASIF_change
                     stderr=se_SWL_change se_BDI_change se_BHS_change se_PSIConf_change se_TASDF_change se_TASIF_change;
run;

proc mianalyze data=SfSLMeans;
modeleffects SWL_change BDI_change BHS_change PSIConf_change TASDF_change TASIF_change;
stderr se_SWL_change se_BDI_change se_BHS_change se_PSIConf_change se_TASDF_change se_TASIF_change;
run;

* Create path model ;

proc sort data=sfSL_all; by _Imputation_; run;
proc calis data=sfSL_all outest=est;
path
 SWL_change <- BDI_change,
 SWL_change <- BHS_change,
 SWL_change <- PSIConf_change,
 BHS_change <- TASIF_change,
 BHS_change <- BDI_change,
 BHS_change <- PSIConf_change,
 BDI_change <- TASIF_change,
 PSIConf_change <- TASDF_change,
 TASDF_change <- TASIF_change;
by _Imputation_;
run;

proc mianalyze data=est;
modeleffects _parm1 _parm2 _parm3 _parm4 _parm5 _parm6 _parm7 _parm8 _parm9;
run;

* Create pruned path model - take out relationships that were statistically insignificant ;

proc calis data=sfSL_all outest=est_pruned noprint;
path
 SWL_change <- BHS_change,
 SWL_change <- PSIConf_change,
 BHS_change <- BDI_change,
 BHS_change <- PSIConf_change,
 BDI_change <- TASIF_change,
 PSIConf_change <- TASDF_change,
 TASDF_change <- TASIF_change;
by _Imputation_;
run;

proc mianalyze data=est_pruned;
modeleffects _parm1 _parm2 _parm3 _parm4 _parm5 _parm6 _parm7;
run;


