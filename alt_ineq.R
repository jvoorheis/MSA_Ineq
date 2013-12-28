#Faster GB2 Imuptation code (using plyr)
library(GB2)
library(ineq)
library(reldist)
library(doMC)
library(parallel)
library(plyr)
library(reshape)


setwd("/home8/jlv/MSA_Ineq")
source("functions.r")
load("CPS_topcode_hh1.rda")
CPS.work.hh<-data.frame("year"=CPS.work.hh$year, "MSA" = CPS.work.hh$MSA, "cellmean_equivinc" = CPS.work.hh$cellmean_equivinc,
                           "topcoded_equivinc" = CPS.work.hh$topcoded_equivinc, "bottom_equivinc" = CPS.work.hh$bottom_equivinc)
                           
registerDoMC()
options(cores=detectCores())
                           
ptm1<-proc.time()
Natl_fit<-foreach (i=1968:2012, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$cellmean_equivinc, probs=0.3)
  temp.year<-subset(temp.year, temp.year$cellmean_equivinc>bottom_cutoff)
  ml.gb2(temp.year$cellmean_equivinc)
}

Gini_try<-data.frame()
counter=0
for (i in c(1968:2012)){
counter=counter+1
tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i)
virtual_inc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
for (j in 1:100){
  temp.data.replace<-vapply(tempyear$topcoded_equivinc, FUN=topcode_sub, Natl_fit[counter], FUN.VALUE=0.0)
  virtual_inc[,j]<-temp.data.replace+tempyear$bottom_equivinc
}
virtual_inc<-data.frame(virtual_inc)
virtual_inc$MSA<-tempyear$MSA
#virtual_inc$hwtsupp<-tempyear$hwtsupp
virtual_inc$year<-rep(i, length(tempyear$topcoded_equivinc))
Gini_try<-rbind(Gini_try, virtual_inc)
}
proc.time()-ptm1
save(Gini_try, file="Full_CPS_MSA_GB2_virtinc_1968.rda")


ptm<-proc.time()
alternate_ineq<-ddply(Gini_try, .variables=c("MSA", "year"), function(x) c("CPS_gini"=mean(apply(x[1:100], 2, gini)), "CPS_theil"=mean(apply(x[1:100], 2, Theil)), "CPS_atkin"=mean(apply(x[1:100], 2, Atkinson)), "CPS_schutz" = mean(apply(x[1:100], 2, RS)), "CPS_entropy05" = mean(apply(x[1:100], 2, ineq, type="entropy")), "CPS_entropy1"=mean(apply(x[1:100], 2, ineq, type="entropy", 1)), "CPS_entropy2"=mean(apply(x[1:100], 2, ineq, type="entropy", 2)), "CPS_entropy3"=mean(apply(x[1:100], 2, ineq, type="entropy", 3)), "CPS_coefvar" = mean(apply(x[1:100], 2, var.coeff)), "CPS_9010" = mean(apply(x[1:100], 2, ratio9010f)), "CPS_top1" = mean(apply(x[1:100], 2, top1share)), "CPS_95med"=mean(apply(x[1:100], 2, ratio95medf)), "CPS_99med"=mean(apply(x[1:100], 2, ratio99medf)), "CPS_8020"=mean(apply(x[1:100], 2, ratio8020f)) ), .parallel=T)


proc.time()-ptm

save(alternate_ineq, file="CPS_full_ineq_MSA.rda")

