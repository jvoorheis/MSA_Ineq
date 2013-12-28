#Generate Miscellaneous State Gini Plots
library(foreign)
library(data.table)
library(reldist)
library(ineq)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/StateGB2_73.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_State_hh.rda")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")

#Generate The GB2 imputed Gini alone
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/State_CPS")
State_unique<-unique(StateGB2.df$State)
generate_gini_plots_state(State_unique, StateGB2.df, type="top1")

#Merge with the Frank (2009) State Top income share dataset
Frank_ineq<-read.dta("/home/john/Dropbox/econ stuff/PhD/Fields/Taxation (Winter 2013)/State Taxation and top incomes/Frank_All.dta")
Frank_ineq<-subset(Frank_ineq, Frank_ineq$year>=1977)
colnames(Frank_ineq)<-c("year", "State", "Frank_atkin05", "Frank_gini", "Frank_rmeandev", "Frank_theil", "Frank_top10", "Frank_top1")
Frank_ineq<-data.table(Frank_ineq)
StateGB2.df<-data.table(StateGB2.df)
setkey(Frank_ineq, State, year)
setkey(StateGB2.df, State, year)
try1<-Frank_ineq[StateGB2.df]

#Plot the Frank top incomes vs. the CPS GB2 top incomes
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Frank_GB2")
for (i in State_unique){
  temp.df<-subset(try1, try1$State==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Frank_top1, colour="Frank top 1"))+
          geom_line(aes(y=Top1Share, colour="GB2 top 1"))+
          opts(title=i))
  dev.off()
}

#Calculate cell mean gini and top 1% share and merge onto the GB2 imputed data
cellmean_gini<-aggregate(cellmean_equivinc~State+year, data=CPS.work.hh, FUN=gini)
colnames(cellmean_gini)[3]<-"Cellmean_gini"
cellmean_top1<-aggregate(cellmean_equivinc~State+year, data=CPS.work.hh, FUN=top1share)
colnames(cellmean_top1)[3]<-"Cellmean_top1"
cellmean_gini<-data.table(cellmean_gini)
cellmean_top1<-data.table(cellmean_top1)
setkey(cellmean_gini, State, year)
setkey(cellmean_top1, State, year)
try2<-cellmean_gini[StateGB2.df]
try2<-try2[cellmean_top1]

#Plot the cellmean and GB2 inquelity measures together
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/State_combined")
for (i in State_unique){
  temp.df<-subset(try2, try2$State==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, "_", "gini", ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Gini_StateGB2, colour="GB2_gini"))+
          geom_line(aes(y=Cellmean_gini, colour="Cellmean_gini"))+
          opts(title=i))
  dev.off()
  filename<-paste(temp.MSA, "_", "top1", ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Top1Share, colour="GB2_top1"))+
          geom_line(aes(y=Cellmean_top1, colour="Cellmean_top1"))+
          opts(title=i))
  dev.off()
}


