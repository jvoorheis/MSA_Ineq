setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
#setwd("/ibrix/home8/jlv/MSA_Ineq")
load("ACS_topcode_hh1.rda")
load("Census_topcode_hh1.rda")

ACS.work.hh<-subset(ACS.work.hh,  
                    ACS.work.hh$year==2005 | ACS.work.hh$year==2011)
Census.work.hh<-subset(Census.work.hh, Census.work.hh$year==2000)

ACS.work.hh<-data.frame("year"=ACS.work.hh$year, "MSA" = ACS.work.hh$MSA, "cellmean_equivinc" = ACS.work.hh$cellmean_equivinc)
Census.work.hh<-data.frame("year"=Census.work.hh$year, "MSA" = Census.work.hh$MSA, "cellmean_equivinc" = Census.work.hh$cellmean_equivinc)

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

registerDoMC()
options(cores=detectCores())
#source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")

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


# MSA2005<-as.character(unique(subset(Gini_try, Gini_try$year==2005)$MSA))
# MSA2005<-sort(MSA2005)
# P_val<-foreach (k=MSA2005, .combine=c)%dopar%{
#   x1<-subset(Gini_try, Gini_try$MSA==k)
#   Gini_diff_boot(x1, 100, 100, 2005, 2011) 
# }
# MSA_results<-data.frame("MSA"=MSA2005, "P_val"=P_val)

result_temp<-ddply(ACS.work.hh, .variables=c("MSA"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2000, 2005)), .parallel=T)
result_temp1<-ddply(ACS.work.hh, .variables=c("MSA"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2005, 2011)), .parallel=T)
result_temp2<-ddply(ACS.work.hh, .variables=c("MSA"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2000, 2011)), .parallel=T)
#result_temp<-ddply(Gini_try, .variables=c("State"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2005, 2011)), .parallel=T)
result_temp<-cbind(result_temp, result_temp1$P_val, result_temp2$P_val)

save(result_temp, file = "MSA_ACS_cellmean_bootresults.rda")
