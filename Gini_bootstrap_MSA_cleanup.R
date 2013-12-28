#Faster GB2 Imuptation code (using plyr)
library(GB2)
library(ineq)
library(reldist)
library(doMC)
library(parallel)
library(plyr)
library(reshape)

setwd("/ibrix/home8/jlv/MSA_Ineq")
#setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
# source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
# load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_State_hh.rda")

ratio9010_diff_boot<-function(inc, replications, GB2_reps, year_1, year_2){
  boot_reps<-c()
  y1.df<-subset(inc, inc$year==year_1)
  y2.df<-subset(inc, inc$year==year_2)
  boot_reps<-numeric(replications*GB2_reps)
  for (i in 1:replications){
    
    for (j in 1:GB2_reps){
      boot_reps[(i-1)*GB2_reps+j]<-ratio9010f(sample(y1.df[,j], replace=T))-ratio9010f(sample(y2.df[,j], replace=T))
    }
  }
  return(ecdf(boot_reps)(0))
}

top1_diff_boot<-function(inc, replications, GB2_reps, year_1, year_2){
  boot_reps<-c()
  y1.df<-subset(inc, inc$year==year_1)
  y2.df<-subset(inc, inc$year==year_2)
  boot_reps<-numeric(replications*GB2_reps)
  for (i in 1:replications){
    
    for (j in 1:GB2_reps){
      boot_reps[(i-1)*GB2_reps+j]<-top1share(sample(y1.df[,j], replace=T))-top1share(sample(y2.df[,j], replace=T))
    }
  }
  return(ecdf(boot_reps)(0))
}

Gini_diff_boot<-function(inc, replications, GB2_reps, year_1, year_2){
  boot_reps<-c()
  y1.df<-subset(inc, inc$year==year_1)
  y2.df<-subset(inc, inc$year==year_2)
  boot_reps<-numeric(replications*GB2_reps)
  for (i in 1:replications){
    
    for (j in 1:GB2_reps){
      boot_reps[(i-1)*GB2_reps+j]<-gini(sample(y1.df[,j], replace=T))-gini(sample(y2.df[,j], replace=T))
    }
  }
  return(ecdf(boot_reps)(0))
}

theil_diff_boot<-function(inc, replications, GB2_reps, year_1, year_2){
  boot_reps<-c()
  y1.df<-subset(inc, inc$year==year_1)
  y2.df<-subset(inc, inc$year==year_2)
  boot_reps<-numeric(replications*GB2_reps)
  for (i in 1:replications){
    
    for (j in 1:GB2_reps){
      boot_reps[(i-1)*GB2_reps+j]<-Theil(sample(y1.df[,j], replace=T))-Theil(sample(y2.df[,j], replace=T))
    }
  }
  return(ecdf(boot_reps)(0))
}





registerDoMC()
options(cores=detectCores())

#ptm1<-proc.time()
#Natl_fit<-foreach (i=1986:2012, .combine=c)%dopar%{
#  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
#  bottom_cutoff<-quantile(temp.year$cellmean_equivinc, probs=0.3)
#  temp.year<-subset(temp.year, temp.year$cellmean_equivinc>bottom_cutoff)
#  ml.gb2(temp.year$cellmean_equivinc)
#}

#Gini_try<-data.frame()
#counter=0
#for (i in c(1986:2012)){
#counter=counter+1
#tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i)
#virtual_inc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
#for (j in 1:100){
#  temp.data.replace<-vapply(tempyear$topcoded_equivinc, FUN=topcode_sub, Natl_fit[counter], FUN.VALUE=0.0)
#  virtual_inc[,j]<-temp.data.replace+tempyear$bottom_equivinc
#}
#virtual_inc<-data.frame(virtual_inc)
#virtual_inc$State<-tempyear$State
##virtual_inc$hwtsupp<-tempyear$hwtsupp
#virtual_inc$year<-rep(i, length(tempyear$topcoded_equivinc))
#Gini_try<-rbind(Gini_try, virtual_inc)
#}
#save(Gini_try, file="Full_GB2_virtinc.rda")
load("Full_CPS_State_GB2_virtinc.rda")
Gini_try<-subset(Gini_try, Gini_try$year==1991 | Gini_try$year %in% 2006:2012)
#Gini_try<-subset(Gini_try, Gini_try$State %in% State_list)
#Gini_try<-subset(Gini_try, Gini_try$year>=1986)

ptm1<-proc.time()
test_results<-data.frame()
years_left<-2006:2012
for (i in 1991:1991){
  year_temp<-subset(years_left, years_left>i)
  for (j in year_temp){
    temp.df <- subset(Gini_try, Gini_try$year==i | Gini_try$year==j)
    result_temp<-ddply(temp.df, .variables=c("State"), function(x) c("year_1"=i, "year_2"=j, "Pval_gini"=Gini_diff_boot(x, 500, 100, i, j), "Pval_top1"=top1_diff_boot(x, 500, 100, i, j), "P_val_theil"=theil_diff_boot(x, 500, 100, i, j), "Pval_9010"=ratio9010_diff_boot(x, 500, 100, i, j)), .parallel=T, .progress="text")
    test_results<-rbind(test_results, result_temp)
    tempfile <- paste("tempfile", as.character(i), as.character(j), ".rda", sep="")
    save(test_results, file=tempfile)
  }
}
proc.time()-ptm1
save(test_results, file="multi_bootstrap_test_1999.rda")

