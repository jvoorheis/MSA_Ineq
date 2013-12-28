setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
#setwd("/ibrix/home8/jlv/MSA_Ineq")
load("CPS_topcode_hh1.rda")

CPS.work.hh<-subset(CPS.work.hh,  CPS.work.hh$year==2000 |
                    CPS.work.hh$year==2005 | CPS.work.hh$year==2011)

CPS.work.hh<-data.frame("year"=CPS.work.hh$year, "MSA" = CPS.work.hh$MSA, "cellmean_equivinc" = CPS.work.hh$cellmean_equivinc)


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
#source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")

Gini_diff_boot<-function(inc, replications, GB2_reps, year_1, year_2){
  boot_reps<-c()
  y1.df<-subset(inc, inc$year==year_1)
  y2.df<-subset(inc, inc$year==year_2)
  boot_reps<-numeric(replications)
  for (i in 1:replications){
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

result_temp<-ddply(CPS.work.hh, .variables=c("MSA"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2000, 2005)), .parallel=T)
result_temp1<-ddply(CPS.work.hh, .variables=c("MSA"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2005, 2011)), .parallel=T)
result_temp2<-ddply(CPS.work.hh, .variables=c("MSA"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2000, 2011)), .parallel=T)
#result_temp<-ddply(Gini_try, .variables=c("State"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2005, 2011)), .parallel=T)
result_temp<-cbind(result_temp, result_temp1$P_val, result_temp2$P_val)


save(result_temp, file = "MSA_CPS_cellmean_bootresults.rda")
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
      hline.after=c(-1), file="State_CPS_cellmean_giniboot.tex")

save(result_temp, file="State_CPS_cellmean_giniboot.rda")

NY<-subset(CPS.work.hh, CPS.work.hh$MSA=="New York-Northern New Jersey-Long Island")
Gini_diff_boot_full<-function(inc, replications, GB2_reps, year_1, year_2){
  boot_reps<-c()
  y1.df<-subset(inc, inc$year==year_1)
  y2.df<-subset(inc, inc$year==year_2)
  boot_reps<-numeric(replications*GB2_reps)
  for (i in 1:(replications*GB2_reps)){
    boot_reps[i]<-gini(sample(y1.df$cellmean_equivinc, replace=T))-gini(sample(y2.df$cellmean_equivinc, replace=T))
  }
  return(boot_reps)
}

x1<-Gini_diff_boot_full(NY, 100, 100, 2000, 2005)
hist(x1)
ecdf(x1)(0)