library(GB2)
library(ineq)
library(reldist)
library(doMC)
library(parallel)

#setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
#source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
#Manually setting the ACISS directory for now. Can probably set global vars to get around this. 
setwd("/home8/jlv/MSA_Ineq")
source("functions.r")
load("ACS_topcode_hh1.rda")

registerDoMC()
options(cores=detectCores())


#Drawing from National Income Distribution
GB2_gini<-c()
year<-c()
MSA<-c()
top1<-c()
#GB2_gini_w<-c()
theilcoef<-c()
ratio9010<-c()
#Since each year has a potentially different set of MSAs, 
#we define the unique MSA vector on a per year basis
ptm <- proc.time()
for (i in 1968:2012){
  MSA_unique<-unique(subset(ACS.work.hh, ACS.work.hh$year==i)$MSA)
  temp.year<-subset(ACS.work.hh, ACS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$sqrt_equivinc, probs=0.3)
  temp.year<-subset(temp.year, temp.year$sqrt_equivinc>bottom_cutoff)
  Natl_fit<-ml.gb2(temp.year$cellmean_equivinc)
  gini_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(ACS.work.hh, ACS.work.hh$year==i & ACS.work.hh$MSA==j)
    gini_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
      gini_try_MSA[k]<-gini(temp.data.final)
    }
    mean(gini_try_MSA)
  }
#  gini_w_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
#    temp.MSA<-subset(ACS.work.hh, ACS.work.hh$year==i & ACS.work.hh$MSA==j)
#    gini_try_MSA<-c(rep(0, 200))
#    for (k in 1:200){
#      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
#      temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
#      gini_try_MSA[k]<-gini(temp.data.final, weights=temp.MSA$hwtsupp)
#    }
#    mean(gini_try_MSA)
#  }
  theil_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(ACS.work.hh, ACS.work.hh$year==i & ACS.work.hh$MSA==j)
    theil_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
      theil_try_MSA[k]<-Theil(temp.data.final)
    }
    mean(theil_try_MSA)
  }
  ratio9010_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(ACS.work.hh, ACS.work.hh$year==i & ACS.work.hh$MSA==j)
    ratio9010_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
      ratio9010_try_MSA[k]<-ratio9010f(temp.data.final)
    }
    mean(ratio9010_try_MSA)
  }
  top1_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(ACS.work.hh, ACS.work.hh$year==i & ACS.work.hh$MSA==j)
    top1_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
      top1_try_MSA[k]<-top1share(temp.data.final)
    }
    mean(top1_try_MSA)
  }
  GB2_gini<-append(GB2_gini, gini_MSA)
  year<-append(year, rep(i, length(MSA_unique)))
  MSA<-append(MSA, MSA_unique)
  top1<-append(top1, top1_MSA)
  #GB2_gini_w<-append(GB2_gini_w, gini_w_MSA)
  theilcoef<-append(theilcoef, theil_MSA)
  ratio9010<-append(ratio9010, ratio9010_MSA)
}
proc.time() - ptm
NatlGB2.df<-data.frame("MSA"=MSA, "year"=year, "Gini_NatlGB2"=GB2_gini, "Theil"=theilcoef, "9010_ratio"=ratio9010, "Top1Share"=top1)
save(NatlGB2.df, file="ACS_NatlGB2_627.rda")
