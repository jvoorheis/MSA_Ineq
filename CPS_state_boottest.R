load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_State_topcode_hh1.rda")
ACS.work.hh<-subset(ACS.work.hh, ACS.work.hh$year==2005 | ACS.work.hh$year==2011)
NY<-subset(ACS.work.hh, ACS.work.hh$MSA=="New York-Northern New Jersey-Long Island")
library(GB2)
library(ineq)
library(reldist)
library(doMC)
library(parallel)
library(plyr)
library(reshape)
library(data.table)

registerDoMC()
options(cores=detectCores())
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")

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

Natl_fit<-foreach (i=c(2005, 2011), .combine=c)%dopar%{
  temp.year<-subset(ACS.work.hh, ACS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$sqrt_equivinc, probs=0.3)
  temp.year<-subset(temp.year, temp.year$sqrt_equivinc>bottom_cutoff)
  ml.gb2(temp.year$cellmean_equivinc)
}

Gini_try<-data.frame()
counter=0
for (i in c(2005, 2011)){
  counter=counter+1
  tempyear<-subset(ACS.work.hh, ACS.work.hh$year==i)
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
#try1<-Gini_diff_boot(Gini_try, 100, 100, 2005, 2011)

droplevels(Gini_try$State)
State2005<-as.character(unique(subset(Gini_try, Gini_try$year==2005)$State))
State2005<-sort(State2005)
P_val<-foreach (k=State2005, .combine=c)%dopar%{
  x1<-subset(Gini_try, Gini_try$State==k)
  Gini_diff_boot(x1, 100, 100, 2005, 2011) 
}
State_results<-data.frame("State"=State2005, "P_val"=P_val)

result_temp<-ddply(Gini_try, .variables=c("State"), function(x) c("P_val"=Gini_diff_boot(x, 100, 100, 2005, 2011)), .parallel=T)

ACS.work.hh<-count<-1
attempt1<-aggregate(count~State, data=ACS.work.hh, FUN==sum)