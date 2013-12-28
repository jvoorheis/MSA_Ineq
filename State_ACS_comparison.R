library(data.table)
library(ggplot2)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/NatlGB2")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/StateGB2_724.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_State_NatlGB2_73_cellmeanfit.rda")
ACS_NatlGB2.df[,1:2]<-sapply(ACS_NatlGB2.df[,1:2], as.character)
ACS_NatlGB2.df[,2:2]<-sapply(ACS_NatlGB2.df[,2:2], as.numeric)
ACS_Natl_cellmean<-ACS_NatlGB2.df
colnames(ACS_Natl_cellmean)<-c("State", "year", "ACS_Natl_cellmean_gini",	"ACS_Natl_cellmean_theil", 	"ACS_Natl_cellmean_9010", "ACS_Natl_cellmean_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACSState__NatlGB2_73_topcodefit.rda")
ACS_NatlGB2.df[,1:2]<-sapply(ACS_NatlGB2.df[,1:2], as.character)
ACS_NatlGB2.df[,2:2]<-sapply(ACS_NatlGB2.df[,2:2], as.numeric)
ACS_Natl_topcode<-ACS_NatlGB2.df
colnames(ACS_Natl_topcode)<-c("State", "year", "ACS_Natl_topcode_gini",  "ACS_Natl_topcode_theil", 	"ACS_Natl_topcode_9010", "ACS_Natl_topcode_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_StateGB2_73_cellmeanfit.rda")
ACS_StateGB2.df[,1:2]<-sapply(ACS_StateGB2.df[,1:2], as.character)
ACS_StateGB2.df[,2:2]<-sapply(ACS_StateGB2.df[,2:2], as.numeric)
ACS_State_cellmean<-ACS_StateGB2.df
colnames(ACS_State_cellmean)<-c("State", "year", "ACS_State_cellmean_gini",  "ACS_State_cellmean_theil",   "ACS_State_cellmean_9010", "ACS_State_cellmean_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_StateGB2_73_topcodefit.rda")
ACS_StateGB2.df[,1:2]<-sapply(ACS_StateGB2.df[,1:2], as.character)
ACS_StateGB2.df[,2:2]<-sapply(ACS_StateGB2.df[,2:2], as.numeric)
ACS_State_topcode<-ACS_StateGB2.df
colnames(ACS_State_topcode)<-c("State", "year", "ACS_State_topcode_gini",  "ACS_State_topcode_theil",   "ACS_State_topcode_9010", "ACS_State_topcode_top1")


ACS_Natl_cellmean<-data.table(ACS_Natl_cellmean)
ACS_Natl_topcode<-data.table(ACS_Natl_topcode)
ACS_State_cellmean<-data.table(ACS_State_cellmean)
ACS_State_topcode<-data.table(ACS_State_topcode)
setkey(ACS_Natl_cellmean, State, year)
setkey(ACS_Natl_topcode, State, year)
setkey(ACS_State_cellmean, State, year)
setkey(ACS_State_topcode, State, year)
ACS_joint<-ACS_Natl_cellmean[ACS_Natl_topcode]
ACS_joint<-ACS_joint[ACS_State_topcode]
ACS_joint<-ACS_joint[ACS_State_cellmean]
ACS_joint$year<-as.character(ACS_joint$year)
ACS_joint$year<-as.numeric(ACS_joint$year)
#Graph the 4 ACS series together
# MSA_unique<-unique(ACS_joint$MSA)
# setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/ACS_compare")
# for (i in MSA_unique){
#   temp.df<-subset(ACS_joint, ACS_joint$MSA==i)
#   temp.MSA<-unlist(strsplit(i, "/"))[1]
#   filename<-paste(temp.MSA, "_", "gini", ".png", sep="")
#   png(file=filename)
#   print(ggplot(temp.df, aes(year)) + 
#           geom_line(aes(y=Natl_cellmean_gini, colour="Natl_cellmean_gini"))+
#           geom_line(aes(y=Natl_topcode_gini, colour="Natl_topcode_gini"))+
#           geom_line(aes(y=MSA_topcode_gini, colour="MSA_topcode_gini"))+
#           geom_line(aes(y=MSA_cellmean_gini, colour="MSA_cellmean_gini"))+
#           opts(title=i))
#   dev.off()
#   filename<-paste(temp.MSA, "_", "top1", ".png", sep="")
#   png(file=filename)
#   print(ggplot(temp.df, aes(year)) + 
#           geom_line(aes(y=Natl_cellmean_top1, colour="Natl_cellmean_top1"))+
#           geom_line(aes(y=Natl_topcode_top1, colour="Natl_topcode_top1"))+
#           geom_line(aes(y=MSA_topcode_top1, colour="MSA_topcode_top1"))+
#           geom_line(aes(y=MSA_cellmean_top1, colour="MSA_cellmean_top1"))+
#           opts(title=i))
#   dev.off()
# }

#Comparing ACS+CPS
StateGB2.df<-data.table(StateGB2.df)
setkey(StateGB2.df, State, year)
ACS_joint<-data.table(ACS_joint)
setkey(ACS_joint, State, year)
#try2<-NatlGB2.df[ACS_joint]

try3<-ACS_joint[try2]

summary(lm(Gini_StateGB2~ACS_Natl_topcode_gini, data=try3))
summary(lm(Gini_StateGB2~ACS_Natl_cellmean_gini, data=try3))
summary(lm(Gini_StateGB2~ACS_State_topcode_gini, data=try3))
summary(lm(Gini_StateGB2~ACS_State_cellmean_gini, data=try3))
summary(lm(Top1Share~ACS_Natl_topcode_top1, data=try3))
summary(lm(Top1Share~ACS_Natl_cellmean_top1, data=try3))
summary(lm(Top1Share~ACS_State_topcode_top1, data=try3))
summary(lm(Top1Share~ACS_State_cellmean_top1, data=try3))
summary(lm(Theil~ACS_Natl_topcode_theil, data=try3))
summary(lm(Theil~ACS_Natl_cellmean_theil, data=try3))
summary(lm(Theil~ACS_State_topcode_theil, data=try3))
summary(lm(Theil~ACS_State_cellmean_theil, data=try3))
summary(lm(X9010_ratio~ACS_Natl_topcode_9010, data=try3))
summary(lm(X9010_ratio~ACS_Natl_cellmean_9010, data=try3))
summary(lm(X9010_ratio~ACS_State_topcode_9010, data=try3))
summary(lm(X9010_ratio~ACS_State_cellmean_9010, data=try3))




State_unique<-unique(try3$State)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/ACS_compare")
for (i in State_unique){
  temp.df<-subset(try3, try3$State==i & try3$year>=2005)
  temp.State<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.State, "_", "gini", ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Natl_cellmean_gini, colour="Natl_cellmean_gini"))+
          geom_line(aes(y=ACS_Natl_cellmean_gini, colour="ACS_Natl_cellmean_gini"))+
          geom_line(aes(y=ACS_Natl_topcode_gini, colour="ACS_Natl_topcode_gini"))+
          geom_line(aes(y=ACS_State_topcode_gini, colour="ACS_State_topcode_gini"))+
          geom_line(aes(y=ACS_State_cellmean_gini, colour="ACS_State_cellmean_gini"))+
          geom_line(aes(y=Gini_StateGB2, colour="Gini_StateGB2"))+
          opts(title=i))
  dev.off()
  filename<-paste(temp.State, "_", "top1", ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Natl_cellmean_top1, colour="Natl_cellmean_top1"))+
          geom_line(aes(y=ACS_Natl_cellmean_top1, colour="ACS_Natl_cellmean_top1"))+
          geom_line(aes(y=ACS_Natl_topcode_top1, colour="ACS_Natl_topcode_top1"))+
          geom_line(aes(y=ACS_State_topcode_top1, colour="ACS_State_topcode_top1"))+
          geom_line(aes(y=ACS_State_cellmean_top1, colour="ACS_State_cellmean_top1"))+
          geom_line(aes(y=Top1Share, colour="Top1Share"))+
          opts(title=i))
  dev.off()
}

Gini_combine<-data.frame("Census_gini"=try3$Natl_cellmean_gini, "ACS_gini"=try3$ACS_Natl_cellmean_gini, "CPS_gini"=try3$Gini_StateGB2)
Gini_combine$weighted_gini <- rowMeans(Gini_combine, na.rm=T)
# Gini_combine$year<-try3$year
# Gini_combine$State<-try3$State
Top1_combine<-data.frame("Census_top1"=try3$Natl_cellmean_top1, "ACS_top1"=try3$ACS_Natl_cellmean_top1, "CPS_top1"=try3$Top1Share)
Top1_combine$weighted_top1 <- rowMeans(Top1_combine, na.rm=T)
#Theil
Theil_combine<-data.frame("Census_theil"=try3$Natl_cellmean_theil, "ACS_theil"=try3$ACS_Natl_cellmean_theil, "CPS_theil"=try3$Theil)
Theil_combine$weighted_theil <- rowMeans(Theil_combine, na.rm=T)
#90-10 ratio
ratio9010_combine<-data.frame("Census_9010"=try3$Natl_cellmean_9010, "ACS_9010"=try3$ACS_Natl_cellmean_9010, "CPS_9010"=try3$X9010_ratio)
ratio9010_combine$weighted_9010 <- rowMeans(ratio9010_combine, na.rm=T)
# Top1_combine$year<-try3$year
# Top1_combine$State<-try3$State

Combined_series<-cbind(Gini_combine, Top1_combine, Theil_combine, ratio9010_combine)
Combined_series$year<-try3$year
Combined_series$State<-try3$State

State_unique<-unique(Gini_combine$State)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/EqualWeights")
for (i in State.unique){
  temp.df<-subset(Combined_series, Combined_series$State==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, "_", "gini", ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=ACS_gini, colour="ACS_gini"))+
          geom_line(aes(y=CPS_gini, colour="CPS_gini"))+
          geom_line(aes(y=Census_gini, colour="Census_gini"))+
          geom_line(aes(y=weighted_gini, colour="weighted_gini"))+
          opts(title=i))
  dev.off()
}

save(Combined_series, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Combined_series_state.rda")

# Gini_combine$State<-vapply(Gini_combine$MSA, FUN=state_strip, FUN.VALUE="")
# Gini_combine<-data.table(Gini_combine)
# setkey(Gini_combine, State, MSA, year)
# Gini_combine<-subset(Gini_combine, Gini_combine$year>=1990)
# Gini_combine<-data.table(Gini_combine)
# setkey(Gini_combine, State, MSA, year)
# write.csv(Gini_combine, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Gini_combine.csv")
#save(Gini_combine, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Gini_combine.rda")