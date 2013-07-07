library(GB2)
library(ineq)
library(reldist)
library(doMC)
library(parallel)

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("CPS_topcode_hh.rda")

registerDoMC()
options(cores=detectCores())


#Drawing from Individual MSA's Income Distribution
GB2_gini<-c()
year<-c()
MSA<-c()
top1<-c()
GB2_gini_w<-c()
theilcoef<-c()
ratio9010<-c()
#Since each year has a potentially different set of MSAs, 
#we define the unique MSA vector on a per year basis
ptm <- proc.time()
for (i in 1968:2012){
  MSA_unique<-unique(subset(CPS.work.hh, CPS.work.hh$year==i)$MSA)
  gini_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(CPS.work.hh, CPS.work.hh$year==i & CPS.work.hh$MSA==j)
    bottom_cutoff<-quantile(temp.MSA$sqrt_equivinc, probs=0.2)
    temp.year<-subset(temp.MSA, temp.MSA$sqrt_equivinc>bottom_cutoff)
    MSA_fit<-ml.gb2(temp.year$sqrt_equivinc, method=1)
    gini_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, MSA_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.year$bottom_equivinc
      if (gini(temp.data.final)!=NaN){
	gini_try_MSA[k]<-gini(temp.data.final)
      }
      else{gini_try_MSA[k]=NA}
    }
    mean(gini_try_MSA, na.rm=T)
  }
  gini_w_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(CPS.work.hh, CPS.work.hh$year==i & CPS.work.hh$MSA==j)
    bottom_cutoff<-quantile(temp.MSA$sqrt_equivinc, probs=0.2)
    temp.year<-subset(temp.MSA, temp.MSA$sqrt_equivinc>bottom_cutoff)
    MSA_fit<-ml.gb2(temp.year$sqrt_equivinc, method=1)
    gini_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, MSA_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.year$bottom_equivinc
      if (gini(temp.data.final, weights=temp.year$hwtsupp)!=NaN){
	gini_try_MSA[k]<-gini(temp.data.final, weights=temp.year$hwtsupp)
      }
      else{gini_try_MSA[k]=NA}
    }
    mean(gini_try_MSA, na.rm=T)
  }
  theil_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(CPS.work.hh, CPS.work.hh$year==i & CPS.work.hh$MSA==j)
    bottom_cutoff<-quantile(temp.MSA$sqrt_equivinc, probs=0.2)
    temp.year<-subset(temp.MSA, temp.MSA$sqrt_equivinc>bottom_cutoff)
    MSA_fit<-ml.gb2(temp.year$sqrt_equivinc, method=1)
    theil_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, MSA_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.year$bottom_equivinc
      if (Theil(temp.data.final)!=NaN){
	theil_try_MSA[k]<-Theil(temp.data.final)
      }
      else{theil_try_MSA[k]=NA}
    }
    mean(theil_try_MSA, na.rm=T)
  }
  top1_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(CPS.work.hh, CPS.work.hh$year==i & CPS.work.hh$MSA==j)
    bottom_cutoff<-quantile(temp.MSA$sqrt_equivinc, probs=0.2)
    temp.year<-subset(temp.MSA, temp.MSA$sqrt_equivinc>bottom_cutoff)
    MSA_fit<-ml.gb2(temp.year$sqrt_equivinc, method=1)
    top1_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, MSA_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.year$bottom_equivinc
      if (top1share(temp.data.final)!=NaN){
	top1_try_MSA[k]<-top1share(temp.data.final)
      }
      else{top1_try_MSA[k]=NA}
    }
    mean(top1_try_MSA, na.rm=T)
  }
  ratio9010_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(CPS.work.hh, CPS.work.hh$year==i & CPS.work.hh$MSA==j)
    bottom_cutoff<-quantile(temp.MSA$sqrt_equivinc, probs=0.2)
    temp.year<-subset(temp.MSA, temp.MSA$sqrt_equivinc>bottom_cutoff)
    MSA_fit<-ml.gb2(temp.year$sqrt_equivinc, method=1)
    ratio9010_try_MSA<-c(rep(0, 200))
    for (k in 1:200){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, MSA_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.year$bottom_equivinc
      if (ratio9010f(temp.data.final)!=NaN){
	ratio9010_try_MSA[k]<-ratio9010f(temp.data.final)
      }
      else{ratio9010_try_MSA[k]=NA}
    }
    mean(ratio9010_try_MSA, na.rm=T)
  }
  GB2_gini<-append(GB2_gini, gini_MSA)
  year<-append(year, rep(i, length(MSA_unique)))
  MSA<-append(MSA, MSA_unique)
  GB2_gini_w<-append(GB2_gini_w, gini_w_MSA)
  theilcoef<-append(theilcoef, theil_MSA)
  ratio9010<-append(ratio9010, )
}
proc.time() - ptm
MSAGB2_multi.df<-data.frame("MSA"=MSA, "year"=year, "Gini_MSAGB2"=GB2_gini, "Theil"=theilcoef, "9010_ratio"=ratio9010, "Top1Share"=top1)
