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
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates_cellmean.rda")

CPS2000<-subset(CPS.work.hh, CPS.work.hh$year==2000 | CPS.work.hh$year==2011)
NY<-subset(CPS2000, CPS2000$MSA=="New York-Northern New Jersey-Long Island")

Lorenz_diff_boot<-function(inc, replications, GB2_reps, year_1, year_2, lorenz_ords){
  y1.df<-subset(inc, inc$year==year_1)
  y2.df<-subset(inc, inc$year==year_2)
  lorenz_vec<-(1:(lorenz_ords-1))/lorenz_ords
  boot_reps<-matrix(0, replications*GB2_reps, (lorenz_ords-1))
  for (i in 1:replications){
    for (j in 1:GB2_reps){
      lc1<-Lc(sample(y1.df[,j], replace=T))
      lc2<-Lc(sample(y2.df[,j], replace=T))
      temp<-apply(data.frame(lorenz_vec), 2, function(x) lc1$L[as.integer(x*length(lc1$L))]-lc2$L[as.integer(x*length(lc2$L))])
      boot_reps[(i-1)*GB2_reps+j,]<-temp
    }
  }
  results<-numeric((lorenz_ords-1))
  for (i in 1:(lorenz_ords-1)){
    results[i]<-ecdf(boot_reps[,i])(0)
  }
  return(results)
}

Natl_fit<-foreach (i=c(2000, 2011), .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$sqrt_equivinc, probs=0.3)
  temp.year<-subset(temp.year, temp.year$sqrt_equivinc>bottom_cutoff)
  ml.gb2(temp.year$cellmean_equivinc)
}

Gini_try<-data.frame()
counter=0
for (i in c(2000, 2011)){
  counter=counter+1
  tempyear<-subset(CPS2000, CPS2000$year==i)
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

NY<-subset(Gini_try, Gini_try$MSA=="New York-Northern New Jersey-Long Island")

NY_Lc_try<-ddply(Gini_try, .variables=c("MSA"), function(x) c("P_val"=Lorenz_diff_boot(x, 100, 100, 2000, 2011, 20)), .parallel=T)

top100<-unique(subset(Ineq.work5, Ineq.work5$Population>500000)$MSA)
MSA.unique<-unique(Gini_try$MSA)

top100_lc<-Gini_try
for (i in MSA.unique){
  if (i %in% top100){
    top100_lc<-top100_lc
  }
  else{
    top100_lc<-subset(top100_lc, top100_lc$MSA!=i)
  }
}

NY_Lc_try1<-ddply(top100_lc, .variables=c("MSA"), function(x) c("P_val"=Lorenz_diff_boot(x, 100, 100, 2000, 2011, 20)), .parallel=T)


CPS2000<-subset(CPS.work.hh, CPS.work.hh$year==2000 | CPS.work.hh$year==2005)
