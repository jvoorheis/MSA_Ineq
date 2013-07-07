#Just NY
library(GB2)
library(ineq)
library(reldist)
library(doMC)
library(parallel)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
MSA_unique<-unique(subset(CPS.work.hh, CPS.work.hh$year==2011)$MSA)
temp.year<-subset(CPS.work.hh, CPS.work.hh$year==2011)
bottom_cutoff<-quantile(temp.year$sqrt_equivinc, probs=0.3)
temp.year<-subset(temp.year, temp.year$sqrt_equivinc>bottom_cutoff)
Natl_fit1<-ml.gb2(temp.year$sqrt_equivinc)
Natl_fit2<-ml.gb2(temp.year$cellmean_equivinc)
temp.MSA<-subset(CPS.work.hh, CPS.work.hh$year==2011 & CPS.work.hh$MSA_FIPS_corrected==5606)
MSA_fit<-ml.gb2(temp.MSA$cellmean_equivinc)
gini_try_MSA<-c(rep(0, 200))
for (k in 1:200){
  temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit1, FUN.VALUE=0.0)
  temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
  gini_try_MSA[k]<-gini(temp.data.final)
}
print(mean(gini_try_MSA))
gini_try_MSA<-c(rep(0, 200))
for (k in 1:200){
  temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit2, FUN.VALUE=0.0)
  temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
  gini_try_MSA[k]<-gini(temp.data.final)
}
print(mean(gini_try_MSA))
gini_try_MSA<-c(rep(0, 200))
for (k in 1:200){
  temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, MSA_fit, FUN.VALUE=0.0)
  temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
  gini_try_MSA[k]<-gini(temp.data.final)
}
print(mean(gini_try_MSA))