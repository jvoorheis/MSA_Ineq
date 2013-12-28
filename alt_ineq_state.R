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
load("CPS_State_hh.rda")
CPS.work.hh<-data.frame("year"=CPS.work.hh$year, "State" = CPS.work.hh$State, "cellmean_equivinc" = CPS.work.hh$cellmean_equivinc,
                           "topcoded_equivinc" = CPS.work.hh$topcoded_equivinc, "bottom_equivinc" = CPS.work.hh$bottom_equivinc)
CPS.work.hh<-subset(CPS.work.hh, is.na(CPS.work.hh$cellmean_equivinc)==F)
                           
registerDoMC()
options(cores=detectCores())
                
years_unique<-unique(CPS.work.hh$year)                           
ptm1<-proc.time()
Natl_fit<-foreach (i=years_unique, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$cellmean_equivinc, probs=0.3)
  temp.year<-subset(temp.year, temp.year$cellmean_equivinc>bottom_cutoff)
  ml.gb2(temp.year$cellmean_equivinc)
}

Gini_try<-data.frame()
counter=0
for (i in years_unique){
counter=counter+1
tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i)
virtual_inc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
for (j in 1:100){
  temp.data.replace<-vapply(tempyear$topcoded_equivinc, FUN=topcode_sub, Natl_fit[counter], FUN.VALUE=0.0)
  virtual_inc[,j]<-temp.data.replace+tempyear$bottom_equivinc
}
virtual_inc<-data.frame(virtual_inc)
virtual_inc$State<-tempyear$State
#virtual_inc$hwtsupp<-tempyear$hwtsupp
virtual_inc$year<-rep(i, length(tempyear$topcoded_equivinc))
Gini_try<-rbind(Gini_try, virtual_inc)
}
proc.time()-ptm1
save(Gini_try, file="Full_CPS_State_GB2_virtinc.rda")




