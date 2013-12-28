#Faster GB2 Imuptation code (using plyr)
library(GB2)
library(ineq)
library(reldist)
library(doMC)
library(parallel)
library(plyr)
library(reshape)

source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")

registerDoMC()
options(cores=detectCores())
ptm1<-proc.time()
Natl_fit<-foreach (i=1968:1969, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$sqrt_equivinc, probs=0.3)
  temp.year<-subset(temp.year, temp.year$sqrt_equivinc>bottom_cutoff)
  ml.gb2(temp.year$cellmean_equivinc)
}
time_fit<-proc.time()-ptm1
ptm<-proc.time()
counter = 0
NatlGB2.df<-data.frame()
for (i in 1968:1969){
  counter = counter + 1
  #Create virtual dataset for year i
  tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i)
  virtual_inc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
  for (j in 1:100){
    temp.data.replace<-vapply(tempyear$topcoded_equivinc, FUN=topcode_sub, Natl_fit[counter], FUN.VALUE=0.0)
    virtual_inc[,j]<-temp.data.replace+tempyear$bottom_equivinc
  }
  virtual_inc<-data.frame(virtual_inc)
  virtual_inc$MSA<-tempyear$MSA
  virtual_inc$year<-rep(i, length(tempyear$topcoded_equivinc))
  virtual_inc$hwtsupp<-tempyear$hwtsupp
  virtual_inc<-melt(virtual_inc, id.vars=c("MSA", "year", "hwtsupp"))
  year_i_ineq<-ddply(virtual_inc, .variables=c("MSA", "year", "variable"), function(x) c("CPS_gini"=gini(x$value), "CPS_gini_var"=jackknife_ineq(x$value, "gini"), 
                                                                                         "CPS_gini_w"=gini(x$value, weights=x$hwtsupp), "CPS_gini_w_var"=jackknife_ineq(x$value, "gini_w", weights=x$hwtsupp),
                                                                                         "CPS_theil"=Theil(x$value), "CPS_theil_var"=jackknife_ineq(x$value, "Theil"),
                                                                                         "CPS_9010"=ratio9010f(x$value), "CPS_9010_var"=jackknife_ineq(x$value, "9010"),
                                                                                         "CPS_top1"=top1share(x$value), "CPS_top1_var"=jackknife_ineq(x$value, "top1")))
  year_i_means<-ddply(year_i_ineq, .variables=c("MSA", "year"), function(x) c("CPS_gini"=mean(x$CPS_gini, na.rm=T),
                                                                                          "CPS_gini_w"=mean(x$CPS_gini_w, na.rm=T), "CPS_theil"=mean(x$CPS_theil, na.rm=T),
                                                                                          "CPS_9010"=mean(x$CPS_9010, na.rm=T), "CPS_top1"=mean(x$CPS_top1, na.rm=T), 
                                                                              "CPS_gini_var"=mean(x$CPS_gini_var, na.rm=T),
                                                                              "CPS_gini_w_var"=mean(x$CPS_gini_w_var, na.rm=T), "CPS_theil_var"=mean(x$CPS_theil_var, na.rm=T),
                                                                              "CPS_9010_var"=mean(x$CPS_9010_var, na.rm=T), "CPS_top1_var"=mean(x$CPS_top1, na.rm=T)), .parallel=T)
  
  NatlGB2.df<-rbind(NatlGB2.df, year_i_means)
}
time_impute<-proc.time()-ptm

time_fit
time_impute