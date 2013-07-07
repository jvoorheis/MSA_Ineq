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


#Drawing from National Income Distribution
GB2_gini<-c()
year<-c()
MSA<-c()
#Since each year has a potentially different set of MSAs, 
#we define the unique MSA vector on a per year basis
ptm <- proc.time()
for (i in 1968:2012){
  MSA_unique<-unique(subset(CPS.work.hh, CPS.work.hh$year==i)$MSA)
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$sqrt_equivinc, probs=0.2)
  temp.year<-subset(temp.year, temp.year$sqrt_equivinc>bottom_cutoff)
  Natl_fit<-ml.gb2(temp.year$sqrt_equivinc)
  gini_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(CPS.work.hh, CPS.work.hh$year==i & CPS.work.hh$MSA==j)
    gini_try_MSA<-c(rep(0, 100))
    for (k in 1:100){
      temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
      temp.data.final<-temp.data.replace+temp.year$bottom_equivinc
      gini_try_MSA[k]<-gini(temp.data.final)
    }
    mean(gini_try_MSA)
  }
  GB2_gini<-append(GB2_gini, gini_MSA)
  year<-append(year, rep(i, length(MSA_unique)))
  MSA<-append(MSA, MSA_unique)
}
proc.time() - ptm
NatlGB2.df<-data.frame("year"=year, "Gini_NatlGB2"=GB2_gini, "MSA"=MSA)
