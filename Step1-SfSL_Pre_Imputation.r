################################################################################
# File: SfSL_Imputation.r
# Created: Dec 16, 2017
# Updated: Jan 21, 2018
#
# Generate a dataset that is in a format that will allow for imputation 
#  especially interval imputation for the SfSL data
#
#  Tasks: 1) read in the data
#         2) merge demographic with longitudinal data
# Update: merge in the baseline assessment data (demographics)
#
################################################################################

library(tidyverse)
library(readxl)
library(psych)

# Setting the working directory for the data
#setwd("C:/Users/Tim/") # tablet

# First, I read in the longitudinal dataset
# This data was stored in wide format, with the response to each question within
# a questionnaire at a specific timepoint in a separate column locations described below
SfSL<-read_excel("tor.merge.raw.Oct.02.17_Corrected.xlsx") #

# Second, I read in the demographic data, contained in 3 separate files by region
SfSLBaseT<-read_excel("assessments.toronto.up to June.2017_Corrected21Jan2018.xlsx") # Toronto
SfSLBaseK<-read_excel("kw.assess.aug.28.17_Corrected.xlsx") # Kitchener-Waterloo
SfSLBaseM<-read_excel("missing.folks_Corrected.xlsx") # Mississauga

# Creating variables for each of the start and end locations of each
# questionnaire's responses at each time point (there were 8 timepoints
#  - a pre and post gorup for each of )
#  1=pre-group1, 2=10-week group 1, 3=post-group1, 4=pre-group2, 5=10-week group 2,
#  6=post group2, 7=pre-group3, 8=post-group3
PSI_Start<-c(2,73,37,108,178,143,213,248)
PSI_End<-c(36,107,71,142,212,177,247,282)
TAS_Start<-c(284,329,304,354,404,379,424,444)
TAS_End<-c(303,348,323,373,423,398,443,463)
BHS_Start<-c(465,505,485,525,565,545,585,605)
BHS_End<-c(484,524,504,544,584,564,604,624)
BDI_Start<-c(646,732,668,689,753,710)
BDI_End<-c(666,752,688,709,773,730)
BIS_Start<-c(775,835,805,865,925,895,955,985)
BIS_End<-c(804,864,834,894,954,924,984,1014)
SWL_Start<-c(1016,1021,1026,1031,1041,1036,1046)
SWL_End<-c(1020,1025,1030,1035,1045,1040,1050)

# We will eventually store each total score value, but to 
# generate these and the minimum and maximum possible scores
# for those who failed to respond to specific questions, we
# divide the data into individual datasets for each questionnaire
# Here we create column labels for these new datasets
psiName<-c("ParticipantID",paste("psi",1:35,sep=""))
tasName<-c("ParticipantID",paste("tas",1:20,sep=""))
bhsName<-c("ParticipantID",paste("bhs",1:20,sep=""))
bdiName<-c("ParticipantID",paste("bdi",1:21,sep=""))
bisName<-c("ParticipantID",paste("bis",1:30,sep=""))
swlName<-c("ParticipantID",paste("swl",1:5,sep=""))

# Now for each questionnaire (i.e. each outcome of interest), we extract
# the responses to this questionnaire, name the columns, count the missing
# responses for each timepoint and store them in a list of dataframes
# Then we bind all the dataframes together to create the same outcome dataset in 
# long format.

# PSI dataset creation
psiData<-list()
for(i in 1:length(PSI_Start)){
  psiData[[i]]<-SfSL[,c(1,PSI_Start[i]:PSI_End[i])]
  names(psiData[[i]])<-psiName
  MissCount<-rowSums(is.na(psiData[[i]]))
  psiData[[i]]<-cbind(psiData[[i]],MissCount)
  psiData[[i]]<-psiData[[i]]%>%filter(!is.na(ParticipantID))%>%filter(MissCount<35)%>%
    mutate(timepoint=i,TimeID=ParticipantID+(0.1*i))%>%select(-MissCount)
}

psiFull<-bind_rows(psiData[[1]],psiData[[2]],psiData[[3]],psiData[[4]],psiData[[5]],psiData[[6]],psiData[[7]],psiData[[8]])

# TAS dataset creation
tasData<-list()
for(i in 1:length(TAS_Start)){
  tasData[[i]]<-SfSL[,c(283,TAS_Start[i]:TAS_End[i])]
  names(tasData[[i]])<-tasName
  MissCount<-rowSums(is.na(tasData[[i]]))
tasData[[i]]<-cbind(tasData[[i]],MissCount)
  tasData[[i]]<-tasData[[i]]%>%filter(!is.na(ParticipantID))%>%filter(MissCount<20)%>%
    mutate(timepoint=i,TimeID=ParticipantID+(0.1*i))%>%select(-MissCount)
}

tasFull<-bind_rows(tasData[[1]],tasData[[2]],tasData[[3]],tasData[[4]],tasData[[5]],tasData[[6]],tasData[[7]],tasData[[8]])

# BHS dataset creation
bhsData<-list()
for(i in 1:length(BHS_Start)){
  bhsData[[i]]<-SfSL[,c(464,BHS_Start[i]:BHS_End[i])]
  names(bhsData[[i]])<-bhsName
  MissCount<-rowSums(is.na(bhsData[[i]]))
  bhsData[[i]]<-cbind(bhsData[[i]],MissCount)
  bhsData[[i]]<-bhsData[[i]]%>%filter(!is.na(ParticipantID))%>%filter(MissCount<20)%>%
    mutate(timepoint=i,TimeID=ParticipantID+(0.1*i))%>%select(-MissCount)
}

bhsFull<-bind_rows(bhsData[[1]],bhsData[[2]],bhsData[[3]],bhsData[[4]],bhsData[[5]],bhsData[[6]],bhsData[[7]],bhsData[[8]])
# Since the BHS results were incorrectly coded, and some were reverse coded, the following
# code corrects these elements to allow for correct calculation of a total score
BHS_Subtract1List<-paste("bhs",c(1,3,5,6,8,10,11,13,15,19),sep="")
bhsFull[,BHS_Subtract1List]<-(bhsFull[,BHS_Subtract1List]-1)
BHS_Change2To0List<-paste("bhs",c(2,4,7,9,12,14,16,17,18,20),sep="")
bhsFull[,BHS_Change2To0List]<-ifelse(bhsFull[,BHS_Change2To0List]==2,0,1)

# BDI dataset creation
bdiData<-list()
for(i in 1:length(BDI_Start)){
  bdiData[[i]]<-SfSL[,c(645,BDI_Start[i]:BDI_End[i])]
  names(bdiData[[i]])<-bdiName
  MissCount<-rowSums(is.na(bdiData[[i]]))
  bdiData[[i]]<-cbind(bdiData[[i]],MissCount)
  bdiData[[i]]<-bdiData[[i]]%>%filter(!is.na(ParticipantID))%>%filter(MissCount<21)%>%
    mutate(timepoint=i,TimeID=ParticipantID+(0.1*i))%>%select(-MissCount)
}

bdiFull<-bind_rows(bdiData[[1]],bdiData[[2]],bdiData[[3]],bdiData[[4]],bdiData[[5]],bdiData[[6]])

# BIS dataset creation
bisData<-list()
for(i in 1:length(BIS_Start)){
  bisData[[i]]<-SfSL[,c(774,BIS_Start[i]:BIS_End[i])]
  names(bisData[[i]])<-bisName
  MissCount<-rowSums(is.na(bisData[[i]]))
  bisData[[i]]<-cbind(bisData[[i]],MissCount)
  bisData[[i]]<-bisData[[i]]%>%filter(!is.na(ParticipantID))%>%filter(MissCount<30)%>%
    mutate(timepoint=i,TimeID=ParticipantID+(0.1*i))%>%select(-MissCount)
}

bisFull<-bind_rows(bisData[[1]],bisData[[2]],bisData[[3]],bisData[[4]],bisData[[5]],bisData[[6]],bisData[[7]],bisData[[8]])

# SWL dataset creation
swlData<-list()
for(i in 1:length(SWL_Start)){
  swlData[[i]]<-SfSL[,c(1015,SWL_Start[i]:SWL_End[i])]
  names(swlData[[i]])<-swlName
  MissCount<-rowSums(is.na(swlData[[i]]))
  swlData[[i]]<-cbind(swlData[[i]],MissCount)
  swlData[[i]]<-swlData[[i]]%>%filter(!is.na(ParticipantID))%>%filter(MissCount<5)%>%
    mutate(timepoint=i,TimeID=ParticipantID+(0.1*i))%>%select(-MissCount)
}

swlFull<-bind_rows(swlData[[1]],swlData[[2]],swlData[[3]],swlData[[4]],swlData[[5]],swlData[[6]],swlData[[7]])

# Temporary variables are now removed, to retain only the new long format datasets
rm(i,MissCount,psiData,tasData,bhsData,bdiData,bisData,swlData) # rm(psiName,tasName,bhsName,bdiName,bisName,swlName)
rm(PSI_Start,PSI_End,TAS_Start,TAS_End,BHS_Start,BHS_End,BDI_Start,BDI_End,BIS_Start,BIS_End,SWL_Start,SWL_End)

# Here we merge together all the questionnaire data by participantID and timepoint to create
# a single long format dataset
SfSLFull<-full_join(psiFull,tasFull)
SfSLFull<-SfSLFull%>%full_join(bhsFull)%>%full_join(bdiFull)%>%full_join(bisFull)%>%full_join(swlFull)

# We use a function we created to determine the lower bound and upper bound to the possible
# values of a questionnaire with some missing values. This function is stored in the file
# "measureSumSpan" and is commented throughout to explain the process. For values with no 
# missing items the lower and upper bounds will be equal to the observed sum; for those
# questionnaires with all responses missing the lower bound will be the lowest possible 
# observed score on this outcome, and the upper bound the highest possible score.
source("measureSumSpan.r")

# The summed scores along with this min and max are stored in a new dataset called 
# SfSL_Sparse. This dataset will not have the question level response data within it.
SfSL_Sparse<-SfSLFull%>%select(ParticipantID,timepoint)

# In order to use this function we first enumerate all the items in each subscale
# of a questionnaire and then identify by position which items must be reverse coded
# e.g. BIS Motor is comprised of items: 2, 3, 4, 16, 17, 19, 21, 22, 23, 25, 30*
# and the reverse coded item is bis25, which is in position 11 of this sequence
BISMotorList<-paste("bis",c(2,3,4,16,17,19,21,22,23,25,30),sep="")
BISMotorRev<-c(11)
BISMotor<-measureSumSpan(SfSLFull,BISMotorList,BISMotorRev,minVal=1,maxVal=4,varName="BISMotor")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(BISMotor)

BISAttentionList<-paste("bis",c(5,6,9,11,20,24,26,28),sep="")
BISAttentionRev<-c(3,5)
BISAttention<-measureSumSpan(SfSLFull,BISAttentionList,BISAttentionRev,minVal=1,maxVal=4,varName="BISAttention")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(BISAttention)

BISNonPlanningList<-paste("bis",c(1,7,8,10,12,13,14,15,18,27,29),sep="")
BISNonPlanningRev<-c(1,2,3,4,5,6,8,11)
BISNonPlanning<-measureSumSpan(SfSLFull,BISNonPlanningList,BISNonPlanningRev,minVal=1,maxVal=4,varName="BISNonPlanning")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(BISNonPlanning)

BDIList<-bdiName[-1]
BDIRev<-c()
BDI<-measureSumSpan(SfSLFull,BDIList,BDIRev,minVal=0,maxVal=3,varName="BDI")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(BDI)

PSIConfList<-paste("psi",c(5,10,11,12,19,23,24,27,33,34,35),sep="")
PSIConfRev<-c(3,10)
PSIConf<-measureSumSpan(SfSLFull,PSIConfList,PSIConfRev,minVal=1,maxVal=6,varName="PSIConf")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(PSIConf)

PSIAppAvList<-paste("psi",c(1,2,4,6,7,8,13,15,16,17,18,20,21,28,30,31),sep="")
PSIAppAvRev<-c(1,2,3,7,8,10,13)
PSIAppAv<-measureSumSpan(SfSLFull,PSIAppAvList,PSIAppAvRev,minVal=1,maxVal=6,varName="PSIAppAv")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(PSIAppAv)

PSIPersContList<-paste("psi",c(3,14,25,26,32),sep="")
PSIPersContRev<-1:5
PSIPersCont<-measureSumSpan(SfSLFull,PSIPersContList,PSIPersContRev,minVal=1,maxVal=6,varName="PSIPersCont")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(PSIPersCont)

BHSList<-bhsName[-1]
BHSRev<-c()
BHS<-measureSumSpan(SfSLFull,BHSList,BHSRev,minVal=0,maxVal=1,varName="BHS")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(BHS)

SWLList<-paste("swl",1:5,sep="")
SWLRev<-c()
SWL<-measureSumSpan(SfSLFull,SWLList,SWLRev,minVal=1,maxVal=7,varName="SWL")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(SWL)

TASIFList<-paste("tas",c(1,3,6,7,9,13,14),sep="")
TASIFRev<-c()
TASIF<-measureSumSpan(SfSLFull,TASIFList,TASIFRev,minVal=1,maxVal=5,varName="TASIF")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(TASIF)

TASDFList<-paste("tas",c(2,4,11,12,17),sep="")
TASDFRev<-c(2)
TASDF<-measureSumSpan(SfSLFull,TASDFList,TASDFRev,minVal=1,maxVal=5,varName="TASDF")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(TASDF)

TASEFList<-paste("tas",c(5,8,10,15,16,18,19,20),sep="")
TASEFRev<-c(1,3,6,7)
TASEF<-measureSumSpan(SfSLFull,TASEFList,TASEFRev,minVal=1,maxVal=5,varName="TASEF")
SfSL_Sparse<-SfSL_Sparse%>%bind_cols(TASEF)

# Here we make a version of the dataset that excludes the min and max scores
#  called SfSL_ScoresStart
SfSL_ScoresStart<-SfSL_Sparse%>%select(-ends_with("_min"))%>%select(-ends_with("_max"))

# Here we remove all the temporary items that constructed each of these subscales
rm(measureSumSpan)
rm(bisName,BISMotorList,BISMotorRev,BISMotor,BISAttentionList,BISAttentionRev,BISAttention,
   BISNonPlanningList,BISNonPlanningRev,BISNonPlanning,bisFull)
rm(psiName,PSIAppAvList,PSIAppAvRev,PSIAppAv,PSIConfList,PSIConfRev,PSIConf,
   PSIPersContList,PSIPersContRev,PSIPersCont,psiFull)
rm(bdiName,BDIList,BDIRev,BDI,bdiFull,bhsName,BHSList,BHSRev,BHS,BHS_Change2To0List,BHS_Subtract1List,bhsFull)
rm(tasName,TASDFList,TASEFList,TASIFList,TASDFRev,TASEFRev,TASIFRev,TASDF,TASEF,TASIF,tasFull)
rm(swlName,SWLList,SWLRev,SWL,swlFull)

# Now we create datasets to help count the number of unique participants with 
# their demographic data by region and them merge them together
SfSLParticipants<-SfSL_Sparse%>%distinct(ParticipantID)

# Toronto demographic data
SfSLBaseKeepT<-SfSLBaseT%>%select(c(idassess,gender,age,age_onset,age_attempt,numberdx,numbatmp,education)) %>%
  mutate(education=ifelse(education==0,NA,education)) %>% mutate(age_onset=ifelse(age_onset==0,NA,age_onset)) %>%
  mutate(age_attempt=ifelse(age_attempt==0,NA,age_attempt)) %>% mutate(gender=ifelse(gender>2,3,gender)) %>%
  mutate(numbatmp=ifelse(numbatmp==0.02,NA,numbatmp)) %>% mutate(numbatmp=ifelse(numbatmp==0,NA,numbatmp))

# Kitchener-Waterloo demographic data
SfSLBaseKeepK<-SfSLBaseK%>%select(c(idassess,gender,age,age_onset,age_attempt,numberdx,numbatmp,education)) %>%
  mutate(education=ifelse(education==0,NA,education)) %>% mutate(age_onset=ifelse(age_onset==0,NA,age_onset)) %>%
  mutate(age_attempt=ifelse(age_attempt==0,NA,age_attempt)) %>% mutate(gender=ifelse(gender>2,3,gender)) %>%
  mutate(numbatmp=ifelse(numbatmp==0.02,NA,numbatmp)) %>% mutate(numbatmp=ifelse(numbatmp==0,NA,numbatmp))
SfSLBaseKeepK<-SfSLBaseKeepK%>%filter(idassess!=1558)

# Mississauga demographic data
SfSLBaseKeepM<-SfSLBaseM%>%select(c(idassess,gender,age,age_onset,age_attempt,numberdx,numbatmp,education)) %>%
  mutate(education=ifelse(education==0,NA,education)) %>% mutate(age_onset=ifelse(age_onset==0,NA,age_onset)) %>%
  mutate(age_attempt=ifelse(age_attempt==0,NA,age_attempt)) %>% mutate(gender=ifelse(gender>2,3,gender)) %>%
  mutate(numbatmp=ifelse(numbatmp==0.02,NA,numbatmp)) %>% mutate(numbatmp=ifelse(numbatmp==0,NA,numbatmp))

# Merged demographic data
SfSLBaseKeep<-bind_rows(SfSLBaseKeepT,SfSLBaseKeepK,SfSLBaseKeepM)

# Removing temporary files needed for the merge
rm(SfSLBaseKeepK,SfSLBaseKeepT,SfSLBaseKeepM)

# Here we merge the demographic data with the longitudinal questionnaires, store the result
# in SfSL_Impute_Base, export to a .csv file which will be used for imputation in IVEware within SAS
SfSL_Impute_Base<-SfSL_Sparse%>%left_join(SfSLBaseKeep,by=c("ParticipantID"="idassess"))

write_csv(SfSL_Impute_Base,"SfSL_Long_PreImpute.csv",na=".",col_names=T)

# All objects created through this process were then saved for future reference in SfSLPreImpute.Rdata
save.image("SfSLPreImpute.Rdata")


###############################################################################
# Finally, in order to determine the properties of the TAS in this study, 
# Cronbach's alpha was calculated.

# I begin a new sparse dataset with only participantID and timepoint
tas_Sparse<-tasFull%>%select(ParticipantID,timepoint)

# Now add columns for each variable that we want a summary for
#  We want to calculate the Cronbach's alpha for each subscale
#  and will will keep the raw data, hence we rerun the tas scoring portions

TASIFList<-paste("tas",c(1,3,6,7,9,13,14),sep="")
TASIFRev<-c()
TASIF<-measureSumSpan(tasFull,TASIFList,TASIFRev,minVal=1,maxVal=5,varName="TASIF",keepRawData = TRUE)
tas_Sparse<-tas_Sparse%>%bind_cols(TASIF)

TASDFList<-paste("tas",c(2,4,11,12,17),sep="")
TASDFRev<-c(2)
TASDF<-measureSumSpan(tasFull,TASDFList,TASDFRev,minVal=1,maxVal=5,varName="TASDF",keepRawData = TRUE)
tas_Sparse<-tas_Sparse%>%bind_cols(TASDF)

TASEFList<-paste("tas",c(5,8,10,15,16,18,19,20),sep="")
TASEFRev<-c(1,3,6,7)
TASEF<-measureSumSpan(tasFull,TASEFList,TASEFRev,minVal=1,maxVal=5,varName="TASEF",keepRawData = TRUE)
tas_Sparse<-tas_Sparse%>%bind_cols(TASEF)

# Here we calculate the alpha for each subscale
alpha(tas_Sparse[,TASIFList],cumulative=TRUE)
alpha(tas_Sparse[,TASDFList],cumulative=TRUE)
alpha(tas_Sparse[,TASEFList],cumulative=TRUE)

# Now we calculate the OVERALL alpha
TASALLList<-paste("tas",1:20,sep="")
alpha(tas_Sparse[,TASALLList],cumulative=TRUE)



