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
#Since each year has a potentially different set of MSAs, 
#we define the unique MSA vector on a per year basis
ptm <- proc.time()
for (i in 1968:2012){
  MSA_unique<-unique(subset(CPS.work.hh, CPS.work.hh$year==i)$MSA)
  gini_MSA<-foreach (j=MSA_unique, .combine=c)%dopar%{
    temp.MSA<-subset(CPS.work.hh, CPS.work.hh$year==i & CPS.work.hh$MSA==j)
    if temp.MSA
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
    mean(gini_try_MSA)
  }
  GB2_gini<-append(GB2_gini, gini_MSA)
  year<-append(year, rep(i, length(MSA_unique)))
  MSA<-append(MSA, MSA_unique)
}
proc.time() - ptm
MSAGB2.df<-data.frame("year"=year, "Gini_MSAGB2"=GB2_gini, "MSA"=MSA)
