################################################################################
# File: SfSL_Figure1.r
# Created: Feb 7, 2018
# Updated: April 11, 2020
#
# Read in the imputation SfSL dataset and create a figure 1 (clinically 
#   relevant/significant change)
#
################################################################################

library(tidyverse)
library(ggpubr)

setwd("C:/Users/Tim/Dropbox/Data/yvonne/Imputation/DATA/")
SfSL_All<-read_csv("SfSL_All_PostImpute.csv",col_names=T) %>%
  mutate(Group=factor(Group))

SD_PRE_TAS_MI<-SfSL_All%>%group_by(X_mult_)%>%summarize(SD_PRE_TAS=sd(TAS_mean))
SD_PRE_TAS<-mean(SD_PRE_TAS_MI$SD_PRE_TAS)
SE_Diff<-SD_PRE_TAS*sqrt(1-0.85)*sqrt(2)
DIFF_Margin<-1.96*SE_Diff

SfSL_All <- SfSL_All %>%
  mutate(Diff=TAS_mean-TAS_post,ReliableImprove=if_else(TAS_mean-TAS_post>DIFF_Margin,1,0),
         ReliableDiff=if_else(ReliableImprove==1,1,if_else(TAS_mean-TAS_post<(-DIFF_Margin),-1,0)),
         Reliable=factor(ReliableDiff,levels=c(-1,0,1),labels=c("Reliable worsening","Non-reliable change","Reliable Improvement")))

# Here we calculate the proportion of each imputation which shows reliable
#  improvement and then creates a multiply imputed correction to report an 
#  overall proportion.
ReliableChangeProp_MI<-SfSL_All%>%group_by(X_mult_)%>%summarize(number=sum(ReliableImprove),percent=mean(ReliableImprove))
ReliableChangeProp<-colMeans(ReliableChangeProp_MI)
ReliableChanges_MI<-SfSL_All%>%
  group_by(X_mult_,Reliable)%>%
  summarize(number=n())%>%
  spread(key=Reliable, value=number)
ReliableChanges<-colMeans(ReliableChanges_MI)

All_Imputation_plots <- ggplot(SfSL_All,aes(x=TAS_mean,y=TAS_post,col=Reliable)) +
  geom_point(size=0.5) +
  geom_abline(slope=1,intercept=c(0,0)) +
  geom_abline(slope=1,intercept=c(0,DIFF_Margin)) +
  geom_abline(slope=1,intercept=c(0,-DIFF_Margin)) +
  geom_hline(yintercept=61,col="blue") +
  geom_vline(xintercept=61,col="blue") +
#  geom_hline(yintercept=51,col="darkgreen") +
#  geom_vline(xintercept=51,col="darkgreen") +
  facet_wrap(~X_mult_) +
  xlim(0,90) +
  ylim(0,90) +
  xlab("Pre Intervention TAS Score") +
  ylab("Post Intervention TAS Score") +
  theme_pubclean()
All_Imputation_plots

SfSL_MI1<-SfSL_All%>%filter(X_mult_==1)
Single_Imputation_plot <- ggplot(SfSL_MI1,aes(x=TAS_mean,y=TAS_post,shape=Reliable)) +
  geom_point(size=2,col="blue") +
  geom_abline(slope=1,intercept=c(0,0)) +
  geom_abline(slope=1,intercept=c(0,DIFF_Margin)) +
  geom_abline(slope=1,intercept=c(0,-DIFF_Margin)) +
  geom_hline(yintercept=61,col="blue") +
  geom_vline(xintercept=61,col="blue") +
  scale_shape_manual(values=c(1,16)) +
  annotate("text", x = 87, y = 90, angle=45, label = "y = x") +
  annotate("text", x = 74, y = 90, angle=45, label = "y = x + 13.53") +
  annotate("text", x = 87, y = 76, angle=45, label = "y = x - 13.53") +
  annotate("text", x = 1, y = 62, label = "Cut-off = 61",col="blue") +
  annotate("text", x = 60, y = 1, angle=90, label = "Cut-off = 61",col="blue") +
  #  geom_hline(yintercept=51,col="darkgreen") +
  #  geom_vline(xintercept=51,col="darkgreen") +
  #  geom_smooth(method="lm") + # to use this need to remove the colour option col=Reliable in aes
  xlim(0,90) +
  ylim(0,90) +
  xlab("Pre Intervention TAS Score") +
  ylab("Post Intervention TAS Score") +
  theme_pubclean() +
  theme(panel.background = element_rect(colour="black",size=1),
        legend.title=element_blank())
Single_Imputation_plot

endlabel1=paste0("=",ReliableChanges[2],")")
endlabel2=paste0("=",ReliableChanges[3],")")
endlabel3=paste0("=",ReliableChanges[4],")")

Single_Imputation_plot_BW <- ggplot(SfSL_MI1,aes(x=TAS_mean,y=TAS_post,shape=Reliable,col=Reliable)) +
  geom_abline(slope=1,intercept=c(0,0)) +
  geom_abline(slope=1,intercept=c(0,DIFF_Margin)) +
  geom_abline(slope=1,intercept=c(0,-DIFF_Margin)) +
  geom_hline(yintercept=61,col="darkgrey") +
  geom_vline(xintercept=61,col="darkgrey") +
  scale_shape_manual(labels=c(
    substitute(paste("Reliable worsening (",bar(n),x,sep=""),list(x=endlabel1)),
    substitute(paste("Non-reliable change (",bar(n),x,sep=""),list(x=endlabel2)),
    substitute(paste("Reliable improvement (",bar(n),x,sep=""),list(x=endlabel3))),
    values=c(2,1,16)) +
  scale_colour_manual(labels=c(
    substitute(paste("Reliable worsening (",bar(n),x,sep=""),list(x=endlabel1)),
    substitute(paste("Non-reliable change (",bar(n),x,sep=""),list(x=endlabel2)),
    substitute(paste("Reliable improvement (",bar(n),x,sep=""),list(x=endlabel3))),
    values=c("grey50","grey50","black")) +
  annotate("text", x = 87, y = 90, angle=45, label = "y = x") +
  annotate("text", x = 72, y = 88, angle=45, label = "y = x + 13.53") +
  annotate("text", x = 87, y = 76, angle=45, label = "y = x - 13.53") +
  annotate("text", x = 5, y = 62.5, label = "Cut-off = 61",col="darkgrey") +
  annotate("text", x = 59.5, y = 5, angle=90, label = "Cut-off = 61",col="darkgrey") +
  geom_point(size=2) +
  xlim(0,90) +
  ylim(0,90) +
  xlab("Pre Intervention TAS Score") +
  ylab("Post Intervention TAS Score") +
  theme(panel.background = element_rect(fill="white",
                                        colour="black",
                                        size=1,
                                        linetype="solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey95"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey95"),
        legend.title=element_blank(),
        legend.position = "top")
Single_Imputation_plot_BW

pdf("ReliableChangePlot.pdf")
Single_Imputation_plot_BW
dev.off()
