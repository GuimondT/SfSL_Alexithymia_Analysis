/**********************************************************
*
* SfSL_Imputation.SAS
*   Written by: Tim Guimond & Kristen O'Brien
*   Date: 27 December 2017
* This file reads in the raw data file (SfSL_Sparse) which
* was prepared in R in long format. It contains 2 identifiers
* followed by 14 variables which have a score value (_mean),
* and a minimum (_min) and maximum (_max) for interval
* imputation.
*
***********************************************************/

options set = SRCLIB 'C:/Program Files (x86)/Srclib/sas' sasautos = ('!SRCLIB' sasautos) mautosource;

/* iveware examples - sas version */

/* import the input datasets */

libname SFSL 'C:/Users/Tim/';

proc import datafile='C:/Users/Tim/SfSL_Long_PreImpute.csv' out=SfSL_Data dbms=csv replace;
getnames=yes;
run;

/* run iveware */

/* multiple imputation 
*
* Note that within the bounds section we make an entry for each
*  subscale and specify the variable where the maximum possible 
*  observed value is stored (for this subscale) and the minimum
*  possible score (based on a participant's partial responses).
*
* For example, the BISMotor subscale minimum is stored in the 
*   BISMotor_min variable, and is specified like this in the bounds section:
*
*        BISMotor_mean(>=BISMotor_min,<=BISMotor_max)
*
*/

%impute(name=impute,dir=.,setup=new)
title Skills for Safer Living Multiple imputation;
datain SfSL_Data;
dataout SfSL_Impute ALL;
continuous
        BISMotor_mean BISAttention_mean BisNonPlanning_Mean
        BDI_mean PSIConf_mean PSIAppAv_mean PSIPersCont_mean
        BHS_mean SWL_mean TASIF_mean TASDF_mean TASEF_mean
        age age_onset age_attempt education numberdx;
categorical
        gender;
count numbatmp;
transfer ParticipantID timepoint BISMotor_min
        BISAttention_min BisNonPlanning_min
        BDI_min PSIConf_min PSIAppAv_min PSIPersCont_min
        BHS_min SWL_min TASIF_min TASDF_min TASEF_min
        BISMotor_max BISAttention_max BisNonPlanning_Max
        BDI_max PSIConf_max PSIAppAv_max PSIPersCont_max
        BHS_max SWL_max TASIF_max TASDF_max TASEF_max;
bounds numbatmp(>1) numberdx(>=0,<=8)
        BISMotor_mean(>=BISMotor_min,<=BISMotor_max)
        BISAttention_mean(>=BISAttention_min,<=BISAttention_max)
        BisNonPlanning_Mean(>=BisNonPlanning_min,<=BisNonPlanning_max)
        BDI_mean(>=BDI_min,<=BDI_max)
        PSIConf_mean(>=PSIConf_min,<=PSIConf_max)
        PSIAppAv_mean(>=PSIAppAv_min,<=PSIAppAv_max)
        PSIPersCont_mean(>=PSIPersCont_min,<=PSIPersCont_max)
        BHS_mean(>=BHS_min,<=BHS_max)
        SWL_mean(>=SWL_min,<=SWL_max)
        TASIF_mean(>=TASIF_min,<=TASIF_max)
        TASDF_mean(>=TASDF_min,<=TASDF_max)
        TASEF_mean(>=TASEF_min,<=TASEF_max)
        age(>=16,<=100)
        age_onset(>=2,<=age)
        age_attempt(>=age_onset,<=age);
iterations 5;
multiples 10;
seed 2018;
run;

data SFSL.SfSL_Imputed;
set SfSL_Impute;
run;
