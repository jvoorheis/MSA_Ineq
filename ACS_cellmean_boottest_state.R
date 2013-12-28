setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
#setwd("/ibrix/home8/jlv/MSA_Ineq")
load("ACS_State_topcode_hh1.rda")
load("Census_State_topcode_hh1.rda")

ACS.work.hh<-subset(ACS.work.hh,  
                    ACS.work.hh$year==2005 | ACS.work.hh$year==2011)
Census.work.hh<-subset(Census.work.hh, Census.work.hh$year==2000)

ACS.work.hh<-data.frame("year"=ACS.work.hh$year, "State" = ACS.work.hh$State, "cellmean_equivinc" = ACS.work.hh$cellmean_equivinc)
Census.work.hh<-data.frame("year"=Census.work.hh$year, "State" = Census.work.hh$State, "cellmean_equivinc" = Census.work.hh$cellmean_equivinc)

ACS.work.hh<-rbind(ACS.work.hh, Census.work.hh)
rm(Census.work.hh)
library(GB2)
library(ineq)
library(reldist)
library(doMC)
library(parallel)
library(plyr)
library(reshape)
library(data.table)
library(xtable)

registerDoMC()
options(cores=detectCores())
#source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/functions.r")

Gini_diff_boot<-function(inc, replications, GB2_reps, year_1, year_2){
  boot_reps<-c()
  y1.df<-subset(inc, inc$year==year_1)
  y2.df<-subset(inc, inc$year==year_2)
  boot_reps<-numeric(replications*GB2_reps)
  for (i in 1:replications*GB2_reps){
    boot_reps[i]<-gini(sample(y1.df$cellmean_equivinc, replace=T))-gini(sample(y2.df$cellmean_equivinc, replace=T))
  }
  return(ecdf(boot_reps)(0))
}




State2005<-as.character(unique(subset(ACS.work.hh, ACS.work.hh$year==2000)$State))
result_temp<-ddply(ACS.work.hh, .variables=c("State"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2000, 2005)), .parallel=T)
result_temp_1<-ddply(ACS.work.hh, .variables=c("State"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2005, 2011)), .parallel=T)
result_temp_2<-ddply(ACS.work.hh, .variables=c("State"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2000, 2011)), .parallel=T)
result_temp<-cbind(result_temp, result_temp_1$P_val, result_temp_2$P_val)

colnames(result_temp)<-c("State", "p<0, 2000-2005", "p<0, 2005-2011", "p<0, 2000-2011")
addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste("\\hline \n",
                             "\\endhead \n",
                             "\\hline \n",
                             "{\\footnotesize Continued on next page} \n",
                             "\\endfoot \n",
                             "\\endlastfoot \n",sep=""))
print(xtable(result_temp), tabular.environment = "longtable", floating = FALSE,
      include.rownames = FALSE,  # because addtorow will substitute the default row names 
      add.to.row = addtorow,     # this is where you actually make the substitution
      hline.after=c(-1))

print(xtable(result_temp), tabular.environment = "longtable", floating = FALSE,
      include.rownames = FALSE,  # because addtorow will substitute the default row names 
      add.to.row = addtorow,     # this is where you actually make the substitution
      hline.after=c(-1), file="State_ACS_giniboot.tex")

save(result_temp, file="State_ACS_giniboot.rda")


ACS.work.hh<-count<-1
attempt1<-aggregate(count~State, data=ACS.work.hh, FUN==sum)