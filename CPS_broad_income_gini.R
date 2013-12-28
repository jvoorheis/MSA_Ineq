library(GB2)
library(ineq)
library(reldist)
library(plyr)
library(ggplot2)
library(reshape)
library(parallel)
library(doMC)

registerDoMC()
options(cores=detectCores())

setwd("/home8/jlv/MSA_Ineq")
source("functions.r")
load("CPS_broadinc_hh.rda")

CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year>1991 & CPS.work.hh$year<2013)

years_unique<-unique(CPS.work.hh$year)                           
ptm1<-proc.time()
Natl_fit_prebroad<-foreach (i=years_unique, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$equivinc_pretax_broad, probs=0.3)
  temp.year<-subset(temp.year, temp.year$equivinc_pretax_broad>bottom_cutoff)
  ml.gb2(temp.year$equivinc_pretax_broad)
}
Natl_fit_postbroad<-foreach (i=years_unique, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$equivinc_posttax_broad, probs=0.3)
  temp.year<-subset(temp.year, temp.year$equivinc_posttax_broad>bottom_cutoff)
  ml.gb2(temp.year$equivinc_posttax_broad)
}
Natl_fit_preequity<-foreach (i=years_unique, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$equivinc_pretax_equity, probs=0.3)
  temp.year<-subset(temp.year, temp.year$equivinc_pretax_equity>bottom_cutoff)
  ml.gb2(temp.year$equivinc_pretax_equity)
}
Natl_fit_postequity<-foreach (i=years_unique, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$equivinc_posttax_equity, probs=0.3)
  temp.year<-subset(temp.year, temp.year$equivinc_posttax_equity>bottom_cutoff)
  ml.gb2(temp.year$equivinc_posttax_equity)
}
Natl_fit_baseline<-foreach (i=years_unique, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$equivinc_pretax_broad, probs=0.3)
  temp.year<-subset(temp.year, temp.year$equivinc_pretax_broad>bottom_cutoff)
  ml.gb2(temp.year$equivinc_pretax_broad)
}

Gini_try_postbroad<-data.frame()
Gini_try_preequity<-data.frame()
Gini_try_postequity<-data.frame()
Gini_try_prebroad<-data.frame()
counter=0
for (i in years_unique){
counter=counter+1
tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i)
virtual_inc_postbroad<-matrix(0, length(tempyear$topcoded_equivinc_postequity), 100)
virtual_inc_preequity<-matrix(0, length(tempyear$topcoded_equivinc_postequity), 100)
virtual_inc_postequity<-matrix(0, length(tempyear$topcoded_equivinc_postequity), 100)
virtual_inc_prebroad<-matrix(0, length(tempyear$topcoded_equivinc_postequity), 100)
for (j in 1:100){
  temp.data.replace<-vapply(tempyear$topcoded_equivinc_prebroad, FUN=topcode_sub, Natl_fit_prebroad[counter], FUN.VALUE=0.0)
  virtual_inc_prebroad[,j]<-temp.data.replace+tempyear$bottom_equivinc_prebroad
  temp.data.replace<-vapply(tempyear$topcoded_equivinc_postbroad, FUN=topcode_sub, Natl_fit_postbroad[counter], FUN.VALUE=0.0)
  virtual_inc_postbroad[,j]<-temp.data.replace+tempyear$bottom_equivinc_postbroad
  temp.data.replace<-vapply(tempyear$topcoded_equivinc_preequity, FUN=topcode_sub, Natl_fit_preequity[counter], FUN.VALUE=0.0)
  virtual_inc_preequity[,j]<-temp.data.replace+tempyear$bottom_equivinc_preequity
  temp.data.replace<-vapply(tempyear$topcoded_equivinc_postequity, FUN=topcode_sub, Natl_fit_postequity[counter], FUN.VALUE=0.0)
  virtual_inc_postequity[,j]<-temp.data.replace+tempyear$bottom_equivinc_postequity
}
virtual_inc_prebroad<-data.frame(virtual_inc_prebroad)
virtual_inc_postbroad<-data.frame(virtual_inc_postbroad)
virtual_inc_preequity<-data.frame(virtual_inc_preequity)
virtual_inc_postequity<-data.frame(virtual_inc_postequity)
virtual_inc_prebroad$State<-tempyear$State
virtual_inc_preequity$State<-tempyear$State
virtual_inc_postbroad$State<-tempyear$State
virtual_inc_postequity$State<-tempyear$State
virtual_inc_prebroad$hwtsupp<-tempyear$hwtsupp
virtual_inc_preequity$hwtsupp<-tempyear$hwtsupp
virtual_inc_postbroad$hwtsupp<-tempyear$hwtsupp
virtual_inc_postequity$hwtsupp<-tempyear$hwtsupp
virtual_inc_prebroad$year<-rep(i, length(tempyear$topcoded_equivinc))
virtual_inc_preequity$year<-rep(i, length(tempyear$topcoded_equivinc))
virtual_inc_postbroad$year<-rep(i, length(tempyear$topcoded_equivinc))
virtual_inc_postequity$year<-rep(i, length(tempyear$topcoded_equivinc))
Gini_try_preequity<-rbind(Gini_try_preequity, virtual_inc_preequity)
Gini_try_postbroad<-rbind(Gini_try_postbroad, virtual_inc_postbroad)
Gini_try_postequity<-rbind(Gini_try_postequity, virtual_inc_postequity)
Gini_try_prebroad<-rbind(Gini_try_prebroad, virtual_inc_prebroad)
}
proc.time()-ptm1
save(Gini_try_prebroad, file="State_GB2_virtinc_prebroad.rda")
save(Gini_try_preequity, file="State_GB2_virtinc_preequity.rda")
save(Gini_try_postbroad, file="State_GB2_virtinc_postbroad.rda")
save(Gini_try_postequity, file="State_GB2_virtinc_postequity.rda")

ptm<-proc.time()
Multi_ineq_prebroad<-ddply(Gini_try_prebroad, .variables=c("State", "year"), function(x) c("CPS_gini"=mean(apply(x[1:100], 2, gini)), "CPS_theil"=mean(apply(x[1:100], 2, Theil)), "CPS_atkin"=mean(apply(x[1:100], 2, Atkinson)), "CPS_schutz" = mean(apply(x[1:100], 2, RS)), "CPS_entropy05" = mean(apply(x[1:100], 2, ineq, type="entropy")), "CPS_entropy1"=mean(apply(x[1:100], 2, ineq, type="entropy", 1)), "CPS_entropy2"=mean(apply(x[1:100], 2, ineq, type="entropy", 2)), "CPS_entropy3"=mean(apply(x[1:100], 2, ineq, type="entropy", 3)), "CPS_coefvar" = mean(apply(x[1:100], 2, var.coeff)), "CPS_9010" = mean(apply(x[1:100], 2, ratio9010f)), "CPS_top1" = mean(apply(x[1:100], 2, top1share)), "CPS_95med"=mean(apply(x[1:100], 2, ratio95medf)), "CPS_99med"=mean(apply(x[1:100], 2, ratio99medf)), "CPS_8020"=mean(apply(x[1:100], 2, ratio8020f)) ), .parallel=T)

Multi_ineq_postbroad<-ddply(Gini_try_postbroad, .variables=c("State", "year"), function(x) c("CPS_gini"=mean(apply(x[1:100], 2, gini)), "CPS_theil"=mean(apply(x[1:100], 2, Theil)), "CPS_atkin"=mean(apply(x[1:100], 2, Atkinson)), "CPS_schutz" = mean(apply(x[1:100], 2, RS)), "CPS_entropy05" = mean(apply(x[1:100], 2, ineq, type="entropy")), "CPS_entropy1"=mean(apply(x[1:100], 2, ineq, type="entropy", 1)), "CPS_entropy2"=mean(apply(x[1:100], 2, ineq, type="entropy", 2)), "CPS_entropy3"=mean(apply(x[1:100], 2, ineq, type="entropy", 3)), "CPS_coefvar" = mean(apply(x[1:100], 2, var.coeff)), "CPS_9010" = mean(apply(x[1:100], 2, ratio9010f)), "CPS_top1" = mean(apply(x[1:100], 2, top1share)), "CPS_95med"=mean(apply(x[1:100], 2, ratio95medf)), "CPS_99med"=mean(apply(x[1:100], 2, ratio99medf)), "CPS_8020"=mean(apply(x[1:100], 2, ratio8020f)) ), .parallel=T)

Multi_ineq_preequity<-ddply(Gini_try_preequity, .variables=c("State", "year"), function(x) c("CPS_gini"=mean(apply(x[1:100], 2, gini)), "CPS_theil"=mean(apply(x[1:100], 2, Theil)), "CPS_atkin"=mean(apply(x[1:100], 2, Atkinson)), "CPS_schutz" = mean(apply(x[1:100], 2, RS)), "CPS_entropy05" = mean(apply(x[1:100], 2, ineq, type="entropy")), "CPS_entropy1"=mean(apply(x[1:100], 2, ineq, type="entropy", 1)), "CPS_entropy2"=mean(apply(x[1:100], 2, ineq, type="entropy", 2)), "CPS_entropy3"=mean(apply(x[1:100], 2, ineq, type="entropy", 3)), "CPS_coefvar" = mean(apply(x[1:100], 2, var.coeff)), "CPS_9010" = mean(apply(x[1:100], 2, ratio9010f)), "CPS_top1" = mean(apply(x[1:100], 2, top1share)), "CPS_95med"=mean(apply(x[1:100], 2, ratio95medf)), "CPS_99med"=mean(apply(x[1:100], 2, ratio99medf)), "CPS_8020"=mean(apply(x[1:100], 2, ratio8020f)) ), .parallel=T)

Multi_ineq_postequity<-ddply(Gini_try_postequity, .variables=c("State", "year"), function(x) c("CPS_gini"=mean(apply(x[1:100], 2, gini)), "CPS_theil"=mean(apply(x[1:100], 2, Theil)), "CPS_atkin"=mean(apply(x[1:100], 2, Atkinson)), "CPS_schutz" = mean(apply(x[1:100], 2, RS)), "CPS_entropy05" = mean(apply(x[1:100], 2, ineq, type="entropy")), "CPS_entropy1"=mean(apply(x[1:100], 2, ineq, type="entropy", 1)), "CPS_entropy2"=mean(apply(x[1:100], 2, ineq, type="entropy", 2)), "CPS_entropy3"=mean(apply(x[1:100], 2, ineq, type="entropy", 3)), "CPS_coefvar" = mean(apply(x[1:100], 2, var.coeff)), "CPS_9010" = mean(apply(x[1:100], 2, ratio9010f)), "CPS_top1" = mean(apply(x[1:100], 2, top1share)), "CPS_95med"=mean(apply(x[1:100], 2, ratio95medf)), "CPS_99med"=mean(apply(x[1:100], 2, ratio99medf)), "CPS_8020"=mean(apply(x[1:100], 2, ratio8020f)) ), .parallel=T)

save(Multi_ineq_prebroad, file="Multi_ineq_prebroad.rda")
save(Multi_ineq_preequity, file="Multi_ineq_preequity.rda")
save(Multi_ineq_postbroad, file="Multi_ineq_postbroad.rda")
save(Multi_ineq_postequity, file="Multi_ineq_postequity.rda")
proc.time()-ptm



