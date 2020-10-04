################################################################################
# File: SfSL_Post_Imputation.r
# Created: Feb 7, 2018
#
# Read in the imputation SfSL dataset from the SAS output and create a data
# set for analysis by selecting complete cases with pre and post measures.
# Baseline assessment data (demographics) was merged with the outcome measure 
# data.
#
################################################################################

library(tidyverse)
library(sas7bdat)
srclib<<-"C:/Program Files (x86)/Srclib/R"
source(file.path(srclib,"init.R",fsep=.Platform$file.sep))

# Set the local working directory containing the data files
#setwd("C:/Users/Tim/")

# Now we read in the output sas datafile and convert it to a tibble to use the 
# tidyverse functions for data selection.
SfSL_Imputed<-read.sas7bdat("sfsl_imputed.sas7bdat")
SfSL_Imputed<-as.tibble(SfSL_Imputed)

# The following steps are needed to select the appropriate data
# 1) Need code to remove all the duplicate imputed demographic info, must 
#    select one at random for each participant
# 2) Need to remove all interim (10-week) data points
# 3) Need to calculate total scores from subscale values
# 4) Need to match pre-post observations and decide which pairs are to be retained

# 1) Need code to remove all the duplicate imputed demographic info, must 
#    select one at random for each participant
SfSL_Demo_Imputed<-SfSL_Imputed%>%
  select(ParticipantID,timepoint,X_mult_,age,gender,
         age_onset,age_attempt,numberdx,numbatmp,education)
SfSL_Scales<-SfSL_Imputed%>%
  select(ParticipantID,timepoint,X_mult_,BISMotor_mean,BISAttention_mean,
         BISNonPlanning_mean,BDI_mean,PSIConf_mean,PSIAppAv_mean,PSIPersCont_mean,
         BHS_mean,SWL_mean,TASIF_mean,TASDF_mean,TASEF_mean)
SfSL_Demo_One_Imputed<-SfSL_Demo_Imputed%>%
  group_by(X_mult_,ParticipantID)%>%
  sample_n(1)
SfSL_Demo_One_Imputed<-SfSL_Demo_One_Imputed%>%ungroup()%>%
  select(-timepoint)
SfSL_Merge_Imputed<-SfSL_Demo_One_Imputed%>%
  right_join(SfSL_Scales)

# The steps above create a dataset of all individuals who contributed any amount
# of scale data to the imputation and represent our starting point. We calculate
# demographics for all these participants to use for comparison with the
# cases that will ultimately be included (have pre-post data for at least one group)
# versus those cases that ultimately must be discarded.

# Need to calculate the average age, age_onset, etc
#  for each person of the 331 in SfSL_Demo_One_Imputed, this dataset is stored for
#  analysis in SAS to account for the multiply imputed records.
write_csv(SfSL_Demo_One_Imputed,"SfSL_Demo331_PostImpute.csv",na=".",col_names=T)

# Given that gender was completely observed for all participants, we now create
# demographic estimates for this variable to include in Table 1.
Analysis_Gender_331_count<-SfSL_Demo_One_Imputed%>%filter(X_mult_==1)%>%
  group_by(gender)%>%summarise(n=n())%>%mutate(freq=n/sum(n))

# 2) Need to remove all interim (10-week) data points
SfSL_Small_Imputed<-SfSL_Merge_Imputed%>%
  filter(timepoint!=2 & timepoint!=5)

# 3) Need to calculate total scores from subscale values
SfSL_Small_Imputed<-SfSL_Small_Imputed%>%
  mutate(TAS_mean=TASEF_mean+TASDF_mean+TASIF_mean,
         PSI_mean=PSIConf_mean+PSIAppAv_mean+PSIPersCont_mean,
         BIS_mean=BISMotor_mean+BISAttention_mean+BISNonPlanning_mean)%>%
  arrange(X_mult_,ParticipantID,timepoint)

# 4) Need to match pre-post observations and decide which pairs are to be retained
#  a) Delete all data from ID's <omitted list> these participants had no pre or post data
#    only interim data that had been included in the analysis
SfSL_Keepers<-SfSL_Small_Imputed%>%
  filter(!(ParticipantID %in% c())) #omitted list#

#  b) Copy data between timepoints that can act as proxies for non-observed timepoints.

# In situations where people had a post group 1 but no pre group 2, given these often occured within
# 2-4 weeks, their post group 1 data was copied and used as pre group 2 data as well
# A list of pairs was created by hand to be read in for this purpose (hand checked against 
#  attendance records). The actual data has been removed from this file, previously on 
#  separate lines pairs of ParticipantID with the timepoint=3 were listed.
# for example a line could be 1000001,3 
# This would signify that participant 10000001 at timepoint 3 data needs to be copied.
copy3to4_List <- read.csv(text = "
  ParticipantID,timepoint
")  
Copy3to4<-SfSL_Keepers%>%
  right_join(copy3to4_List)%>%
  select(-timepoint)%>%
  mutate(timepoint=4)
SfSL_Keepers<-SfSL_Keepers%>%bind_rows(Copy3to4)

# An identical process was conducted in reverse:
#  when pre group 2 data was present but no post group 1, these were substituted.
#  The pre group 2 data was copied and used as post group 1 observations.
copy4to3_List <- read.csv(text = "
  ParticipantID,timepoint
")  
Copy4to3<-SfSL_Keepers%>%
  right_join(copy4to3_List)%>%
  select(-timepoint)%>%
  mutate(timepoint=3)
SfSL_Keepers<-SfSL_Keepers%>%bind_rows(Copy4to3)

# The same process is now carried out for post group 2 data and pre group 3 data.
#  Timepoint 6=post group 2 and 7=pre group 3
copy6to7_List <- read.csv(text = "
  ParticipantID,timepoint
")
Copy6to7<-SfSL_Keepers%>%
  right_join(copy6to7_List)%>%
  select(-timepoint)%>%
  mutate(timepoint=7)
SfSL_Keepers<-SfSL_Keepers%>%bind_rows(Copy6to7)
exclude_List <- read.csv(text = "
  ParticipantID,timepoint
")  
SfSL_Keepers<-SfSL_Keepers%>%anti_join(exclude_List)

# c) Make datapairs for timepoints (1,3),(4,6) and (7,8) (pre-post pairs of data)
SFSL_Keep1<-SfSL_Keepers%>%filter(timepoint==1)%>%select(-timepoint)
SFSL_Keep3<-SfSL_Keepers%>%filter(timepoint==3)%>%
  select(ParticipantID,X_mult_,BISMotor_mean,BISAttention_mean,BISNonPlanning_mean,
         BIS_mean,BDI_mean,PSIConf_mean,PSIAppAv_mean,PSIPersCont_mean,PSI_mean,
         BHS_mean,SWL_mean,TASIF_mean,TASDF_mean,TASEF_mean,TAS_mean)%>%
  rename(BISMotor_post=BISMotor_mean,
         BISAttention_post=BISAttention_mean,
         BISNonPlanning_post=BISNonPlanning_mean,
         BIS_post=BIS_mean,
         BDI_post=BDI_mean,
         PSIConf_post=PSIConf_mean,
         PSIAppAv_post=PSIAppAv_mean,
         PSIPersCont_post=PSIPersCont_mean,
         PSI_post=PSI_mean,
         BHS_post=BHS_mean,
         SWL_post=SWL_mean,
         TASIF_post=TASIF_mean,
         TASDF_post=TASDF_mean,
         TASEF_post=TASEF_mean,
         TAS_post=TAS_mean)
SFSL_Keep4<-SfSL_Keepers%>%filter(timepoint==4)%>%select(-timepoint)
SFSL_Keep6<-SfSL_Keepers%>%filter(timepoint==6)%>%
  select(ParticipantID,X_mult_,BISMotor_mean,BISAttention_mean,BISNonPlanning_mean,
         BIS_mean,BDI_mean,PSIConf_mean,PSIAppAv_mean,PSIPersCont_mean,PSI_mean,
         BHS_mean,SWL_mean,TASIF_mean,TASDF_mean,TASEF_mean,TAS_mean)%>%
  rename(BISMotor_post=BISMotor_mean,
         BISAttention_post=BISAttention_mean,
         BISNonPlanning_post=BISNonPlanning_mean,
         BIS_post=BIS_mean,
         BDI_post=BDI_mean,
         PSIConf_post=PSIConf_mean,
         PSIAppAv_post=PSIAppAv_mean,
         PSIPersCont_post=PSIPersCont_mean,
         PSI_post=PSI_mean,
         BHS_post=BHS_mean,
         SWL_post=SWL_mean,
         TASIF_post=TASIF_mean,
         TASDF_post=TASDF_mean,
         TASEF_post=TASEF_mean,
         TAS_post=TAS_mean)
SFSL_Keep7<-SfSL_Keepers%>%filter(timepoint==7)%>%select(-timepoint)
SFSL_Keep8<-SfSL_Keepers%>%filter(timepoint==8)%>%
  select(ParticipantID,X_mult_,BISMotor_mean,BISAttention_mean,BISNonPlanning_mean,
         BIS_mean,BDI_mean,PSIConf_mean,PSIAppAv_mean,PSIPersCont_mean,PSI_mean,
         BHS_mean,SWL_mean,TASIF_mean,TASDF_mean,TASEF_mean,TAS_mean)%>%
  rename(BISMotor_post=BISMotor_mean,
         BISAttention_post=BISAttention_mean,
         BISNonPlanning_post=BISNonPlanning_mean,
         BIS_post=BIS_mean,
         BDI_post=BDI_mean,
         PSIConf_post=PSIConf_mean,
         PSIAppAv_post=PSIAppAv_mean,
         PSIPersCont_post=PSIPersCont_mean,
         PSI_post=PSI_mean,
         BHS_post=BHS_mean,
         SWL_post=SWL_mean,
         TASIF_post=TASIF_mean,
         TASDF_post=TASDF_mean,
         TASEF_post=TASEF_mean,
         TAS_post=TAS_mean)

# discard any that are missing one of the pair
SFSL_First<-SFSL_Keep1%>%inner_join(SFSL_Keep3)%>%
  mutate(Group=1)
SFSL_Second<-SFSL_Keep4%>%inner_join(SFSL_Keep6)%>%
  mutate(Group=2)
SFSL_Third<-SFSL_Keep7%>%inner_join(SFSL_Keep8)%>%
  mutate(Group=3)
# Join them all together
SfSL_All<-bind_rows(SFSL_First,SFSL_Second,SFSL_Third)%>%ungroup()
# Here we save an analysis dataset to be imported for analysis in SAS
write_csv(SfSL_All,"SfSL_All_PostImpute.csv",na=".",col_names=T)

# Identify the unique Participant IDs and the number of timepoint pairs
SFSL_Final_Count<-SfSL_All%>%filter(X_mult_==1)%>%count(ParticipantID)%>%count(n)
SFSL_First_Count<-SFSL_First%>%filter(X_mult_==1)%>%count(ParticipantID)
SFSL_Second_Count<-SFSL_Second%>%filter(X_mult_==1)%>%count(ParticipantID)
SFSL_Third_Count<-SFSL_Third%>%filter(X_mult_==1)%>%count(ParticipantID)

# Now we create a demographic data and an analysis dataset to be imported for analysis in SAS
# Demographic (Age, Gender, Number of attempts,
#   number of diagnoses, level of education, Age_onset, Age_Attempt)
SfSL_Demo_Final<-SfSL_All%>%
  select(ParticipantID,X_mult_,age,gender,
         age_onset,age_attempt,numberdx,numbatmp,education)%>%distinct()
write_csv(SfSL_Demo_Final,"SfSL_Demo169_PostImpute.csv",na=".",col_names=T)

# Given that Gender is completely observed, we calculate the percent of 
#  each gender in this analysed subset to include in Table 1.
Analysis_Gender_169_count<-SfSL_Demo_Final%>%filter(X_mult_==1)%>%
  group_by(gender)%>%summarise(n=n())%>%mutate(freq=n/sum(n))
