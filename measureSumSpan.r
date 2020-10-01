# measureSumSpan Function (creating a sum of responses and an interval of 
#                           possible values when missing responses are present)
# this function takes inputs of:
#  a dataset [dataIn] (could contain responses from several questionnaires),
#  a vector [varList] with the variable names (as named in the dataIn),
#  a vector [revList] of indices from varList that need to be reverse coded,
#  the minimum response value for each question [minVal],
#  the maximum response value for each question [maxVal].
#    (these are assumed to be identical for all questions - this would need
#    to be changed for use with scales with different response values  
#    that differ by question)
#  a name [varname] to call the sum of the items from varList to save, AND
#  a flag [keepRawData] for whether you want the output to include 
#    the original raw data (default is false - returns only summary values).
# this function returns:
#  a dataframe that includes the sum of items (accounting for reverse coding)
#  and where there is missing values, they are replaced by the minimum response 
#  value and then subsequently by the maximum response value, to construct an  
#  interval with the minimum possible value for the total scale and a maximum 
#  value for the total scale to prepare for interval imputation.
# If there are no missing values then varname_sum=varname_min=varname_max

measureSumSpan<-function(dataIn,varList,revList,minVal,maxVal,varName,
                         keepRawData=FALSE){
  require(tidyverse)
  # The number of missing responses within each row for this measure is 
  #  counted by row summing the is.na() counts from the relevant responses
  MissCount<-rowSums(is.na(dataIn[,varList]))
  
  # First I generate a sum of the recorded non-reverse coded items (fwdSum)

  # Must deal with three different possibilities to create the sum, min and 
  #   max of the measure sum:
  #  1) no reverse coding of any responses i.e. is.null(revList)
  #  2) the non-reversed responses include no items
  #  3) the non-reversed responses include only one item
  #  4) when more than one variable will be left as non-reversed
  
  if(is.null(revList)){ # no reverse coding at all
    fwdSum<-rowSums(dataIn[,varList],na.rm=T)
  } else if ((length(varList)==length(revList))){
    fwdSum<-0
  } else if ((length(varList)-length(revList))==1){
    fwdSum<-dataIn[,varList[-revList]]
    fwdSum[is.na(fwdSum)]<-0
  } else {
    fwdSum<-rowSums(dataIn[,varList[-revList]],na.rm = T)
  }
  
  # Second I generate a sum of the recorded reverse coded items (revSum)
  
  if(is.null(revList)){
    revSum<-0
  } else if(length(revList)==1){
    revSum<-maxVal-dataIn[,varList[revList]]+minVal
    revSum[is.na(revSum)]<-0
  } else {
    revSum<-rowSums(maxVal-dataIn[,varList[revList]]+minVal,na.rm = T)
  }
  
  # Here we calculate the sum of the observed items and add create the minimum
  # and maximum total scale values by adding (min/max)Val times the number of
  # missing responses for this individual
  AvailableSum<-fwdSum+revSum
  MinSum<-MissCount*minVal+AvailableSum
  MaxSum<-MissCount*maxVal+AvailableSum
  
  # Now we calculate the actual sum (returning NA if any missing) using the
  # same logic as before for both fwdSum and revSum
  if(is.null(revList)){
    fwdSum<-rowSums(dataIn[,varList],na.rm=F)
  } else if ((length(varList)==length(revList))){
    fwdSum<-0
  } else if((length(varList)-length(revList))==1){
    fwdSum<-dataIn[,varList[-revList]]
  } else {
    fwdSum<-rowSums(dataIn[,varList[-revList]],na.rm = F)
  }
  if(is.null(revList)){
    revSum<-0
  } else if(length(revList)==1){
    revSum<-maxVal-dataIn[,varList[revList]]+minVal
  } else {
    revSum<-rowSums(maxVal-dataIn[,varList[revList]]+minVal,na.rm = F)
  }
  CurrentSum<-fwdSum+revSum
  
  # Now we create a results matrix to return (keeping responses if indicated)
  resultMat<-tibble(CurrentSum,MinSum,MaxSum) # consider testing with tibble() instead of data_frame
  dimnames(resultMat)[[2]]<-paste(varName,c("sum","min","max"),sep="_") #may want to use rename
  revScore<-function(x) maxVal-x+minVal
  if(keepRawData) {
    # First I create data to retain (using the same logic as before)
    if(is.null(revList)){
      dataKeep<-dataIn[,varList]
    } else if ((length(varList)==length(revList))){
      revKeep<-dataIn%>%select(varList[revList])%>%mutate_all(revScore)
      resultMat<-revKeep
    } else {
      revKeep<-dataIn%>%select(varList[revList])%>%mutate_all(revScore)
      dataKeep<-dataIn[,varList[-revList]]%>% bind_cols(revKeep)
    }
    resultMat<-dataKeep %>% bind_cols(resultMat)
  }
  return(resultMat)
}
