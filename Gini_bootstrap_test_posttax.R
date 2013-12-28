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
load("CPS_household_tax.rda")
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year>1991)

years<-c(1995, 2000, 2005, 2011)

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




# CPS.work.hh<-data.frame("year"=CPS.work.hh$year, "State" = CPS.work.hh$State, "cellmean_equivinc" = CPS.work.hh$cellmean_equivinc,
                           # "topcoded_equivinc" = CPS.work.hh$topcoded_equivinc, "bottom_equivinc" = CPS.work.hh$bottom_equivinc)



registerDoMC()
options(cores=detectCores())

ptm1<-proc.time()
Natl_fit<-foreach (i=1992:2013, .combine=c)%dopar%{
 temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
 bottom_cutoff<-quantile(temp.year$cellmean_equivinc, probs=0.1)
 temp.year<-subset(temp.year, temp.year$cellmean_equivinc>bottom_cutoff)
 ml.gb2(temp.year$cellmean_equivinc)
}
Natl_fit_posttax<-foreach (i=1992:2013, .combine=c)%dopar%{
 temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
 bottom_cutoff<-quantile(temp.year$cellmean_equivinc_posttax, probs=0.1)
 temp.year<-subset(temp.year, temp.year$cellmean_equivinc_posttax>bottom_cutoff)
 ml.gb2(temp.year$cellmean_equivinc_posttax)
}
Natl_fit_eitc<-foreach (i=1992:2013, .combine=c)%dopar%{
 temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
 bottom_cutoff<-quantile(temp.year$cellmean_equivinc_eitc, probs=0.1)
 temp.year<-subset(temp.year, temp.year$cellmean_equivinc_eitc>bottom_cutoff)
 ml.gb2(temp.year$cellmean_equivinc_eitc)
}
proc.time()-ptm1

pmt<-proc.time()
Gini_try<-data.frame()
Gini_try_posttax<-data.frame()
Gini_try_eitc<-data.frame()
counter=0
for (i in c(1992:2013)){
counter=counter+1
tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i)
virtual_inc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
virtual_inc_posttax<-matrix(0, length(tempyear$topcoded_equivinc), 100)
virtual_inc_eitc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
for (j in 1:100){
 temp.data.replace<-vapply(tempyear$topcoded_equivinc, FUN=topcode_sub, Natl_fit[counter], FUN.VALUE=0.0)
 virtual_inc[,j]<-temp.data.replace+tempyear$bottom_equivinc
 temp.data.replace.pt<-vapply(tempyear$topcoded_equivinc_posttax, FUN=topcode_sub, Natl_fit_posttax[counter], FUN.VALUE=0.0)
 virtual_inc_posttax[,j]<-temp.data.replace.pt+tempyear$bottom_equivinc_posttax
 temp.data.replace.ei<-vapply(tempyear$topcoded_equivinc_eitc, FUN=topcode_sub, Natl_fit_eitc[counter], FUN.VALUE=0.0)
 virtual_inc_eitc[,j]<-temp.data.replace.ei+tempyear$bottom_equivinc_eitc
}
virtual_inc<-data.frame(virtual_inc)
virtual_inc$State<-tempyear$State
virtual_inc$year<-rep(i, length(tempyear$topcoded_equivinc))
Gini_try_posttax<-rbind(Gini_try_posttax, virtual_inc_posttax)
virtual_inc_posttax<-data.frame(virtual_inc_posttax)
virtual_inc_posttax$State<-tempyear$State
virtual_inc_posttax$year<-rep(i, length(tempyear$topcoded_equivinc))
Gini_try_posttax<-rbind(Gini_try_posttax, virtual_inc_posttax)
virtual_inc_eitc<-data.frame(virtual_inc_eitc)
virtual_inc_eitc$State<-tempyear$State
virtual_inc_eitc$year<-rep(i, length(tempyear$topcoded_equivinc))
Gini_try_eitc<-rbind(Gini_try_eitc, virtual_inc_eitc)


}
save(Gini_try, file="Full_GB2_virtinc_pretax.rda")
save(Gini_try_posttax, file="Full_GB2_virtinc_posttax.rda")
save(Gini_try_eitc, file="Full_GB2_virtinc_pretax_eitc.rda")
proc.time()-ptm
#load("Full_CPS_State_GB2_virtinc.rda")


ptm<-proc.time()
alternate_ineq_pretax<-ddply(Gini_try, .variables=c("State", "year"), function(x) c("CPS_gini"=mean(apply(x[1:100], 2, gini)), "CPS_theil"=mean(apply(x[1:100], 2, Theil)), "CPS_atkin"=mean(apply(x[1:100], 2, Atkinson)), "CPS_schutz" = mean(apply(x[1:100], 2, RS)), "CPS_entropy05" = mean(apply(x[1:100], 2, ineq, type="entropy")), "CPS_entropy1"=mean(apply(x[1:100], 2, ineq, type="entropy", 1)), "CPS_entropy2"=mean(apply(x[1:100], 2, ineq, type="entropy", 2)), "CPS_entropy3"=mean(apply(x[1:100], 2, ineq, type="entropy", 3)), "CPS_coefvar" = mean(apply(x[1:100], 2, var.coeff)), "CPS_9010" = mean(apply(x[1:100], 2, ratio9010f)), "CPS_top1" = mean(apply(x[1:100], 2, top1share)), "CPS_95med"=mean(apply(x[1:100], 2, ratio95medf)), "CPS_99med"=mean(apply(x[1:100], 2, ratio99medf)), "CPS_8020"=mean(apply(x[1:100], 2, ratio8020f)) ), .parallel=T)

alternate_ineq_posttax<-ddply(Gini_try_posttax, .variables=c("State", "year"), function(x) c("CPS_gini"=mean(apply(x[1:100], 2, gini)), "CPS_theil"=mean(apply(x[1:100], 2, Theil)), "CPS_atkin"=mean(apply(x[1:100], 2, Atkinson)), "CPS_schutz" = mean(apply(x[1:100], 2, RS)), "CPS_entropy05" = mean(apply(x[1:100], 2, ineq, type="entropy")), "CPS_entropy1"=mean(apply(x[1:100], 2, ineq, type="entropy", 1)), "CPS_entropy2"=mean(apply(x[1:100], 2, ineq, type="entropy", 2)), "CPS_entropy3"=mean(apply(x[1:100], 2, ineq, type="entropy", 3)), "CPS_coefvar" = mean(apply(x[1:100], 2, var.coeff)), "CPS_9010" = mean(apply(x[1:100], 2, ratio9010f)), "CPS_top1" = mean(apply(x[1:100], 2, top1share)), "CPS_95med"=mean(apply(x[1:100], 2, ratio95medf)), "CPS_99med"=mean(apply(x[1:100], 2, ratio99medf)), "CPS_8020"=mean(apply(x[1:100], 2, ratio8020f)) ), .parallel=T)

alternate_ineq_eitc<-ddply(Gini_try_eitc, .variables=c("State", "year"), function(x) c("CPS_gini"=mean(apply(x[1:100], 2, gini)), "CPS_theil"=mean(apply(x[1:100], 2, Theil)), "CPS_atkin"=mean(apply(x[1:100], 2, Atkinson)), "CPS_schutz" = mean(apply(x[1:100], 2, RS)), "CPS_entropy05" = mean(apply(x[1:100], 2, ineq, type="entropy")), "CPS_entropy1"=mean(apply(x[1:100], 2, ineq, type="entropy", 1)), "CPS_entropy2"=mean(apply(x[1:100], 2, ineq, type="entropy", 2)), "CPS_entropy3"=mean(apply(x[1:100], 2, ineq, type="entropy", 3)), "CPS_coefvar" = mean(apply(x[1:100], 2, var.coeff)), "CPS_9010" = mean(apply(x[1:100], 2, ratio9010f)), "CPS_top1" = mean(apply(x[1:100], 2, top1share)), "CPS_95med"=mean(apply(x[1:100], 2, ratio95medf)), "CPS_99med"=mean(apply(x[1:100], 2, ratio99medf)), "CPS_8020"=mean(apply(x[1:100], 2, ratio8020f)) ), .parallel=T)
save(alternate_ineq, file="CPS_alt_ineq_pretax.rda")
save(alternate_ineq_posttax, file="CPS_alt_ineq_posttax.rda")
save(alternate_ineq_eitc, file="CPS_alt_ineq_eitc.rda")
proc.time()-ptm

ptm1<-proc.time()
Gini_try<-subset(Gini_try, Gini_try$year %in% years)
Gini_try_posttax<-subset(Gini_try_posttax, Gini_try_posttax$year %in% years)
Gini_try_eitc<-subset(Gini_try_eitc, Gini_try_eitc$year %in% years)
test_results_pretax<-data.frame()
test_results_posttax<-data.frame()
test_results_eitc<-data.frame()
years_left<-years
for (i in years){
  year_temp<-subset(years_left, years_left>i)
  for (j in year_temp){
    temp.df <- subset(Gini_try, Gini_try$year==i | Gini_try$year==j)
    result_temp<-ddply(temp.df, .variables=c("State"), function(x) c("year_1"=i, "year_2"=j, "Pval_gini"=Gini_diff_boot(x, 500, 100, i, j), "Pval_top1"=top1_diff_boot(x, 500, 100, i, j), "P_val_theil"=theil_diff_boot(x, 500, 100, i, j), "Pval_9010"=ratio9010_diff_boot(x, 500, 100, i, j)), .parallel=T)
    test_results<-rbind(test_results, result_temp)
    temp.df <- subset(Gini_try_posttax, Gini_try_posttax$year==i | Gini_try_posttax$year==j)
    result_temp<-ddply(temp.df, .variables=c("State"), function(x) c("year_1"=i, "year_2"=j, "Pval_gini"=Gini_diff_boot(x, 500, 100, i, j), "Pval_top1"=top1_diff_boot(x, 500, 100, i, j), "P_val_theil"=theil_diff_boot(x, 500, 100, i, j), "Pval_9010"=ratio9010_diff_boot(x, 500, 100, i, j)), .parallel=T)
    test_results_posttax<-rbind(test_results_posttax, result_temp)
    temp.df <- subset(Gini_try_eitc, Gini_try_eitc$year==i | Gini_try_eitc$year==j)
    result_temp<-ddply(temp.df, .variables=c("State"), function(x) c("year_1"=i, "year_2"=j, "Pval_gini"=Gini_diff_boot(x, 500, 100, i, j), "Pval_top1"=top1_diff_boot(x, 500, 100, i, j), "P_val_theil"=theil_diff_boot(x, 500, 100, i, j), "Pval_9010"=ratio9010_diff_boot(x, 500, 100, i, j)), .parallel=T)
    test_results_eitc<-rbind(test_results_eitc, result_temp)
  }
}
proc.time()-ptm1
save(test_results, file="CPS_Gini_bootstrap_pretax.rda")
save(test_results_posttax, file="CPS_Gini_bootstrap_posttax.rda")
save(test_results_eitc, file="CPS_Gini_bootstrap_eitc.rda")
ptm<-proc.time()


