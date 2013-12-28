library(doMC)
library(parallel)
library(plyr)
library(GB2)
library(ineq)
library(reldist)
library(data.table)
registerDoMC(cores=detectCores())
library(compiler)
enableJIT(1)

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
source("Code/functions.r")
load("Data/CPS_broadinc_MSA_hh.rda")
CPS.work.hh<-CPS.work.hh[which(CPS.work.hh$year>=1990),]
CPS.work.hh$cellmean_equivinc = CPS.work.hh$cellmean_equivinc+1
CPS.work.hh<-CPS.work.hh[which(is.na(CPS.work.hh$State)==F),]


topcode_sub1<-function(inc, fit2){
  bottom<-pgb2(inc, fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4])
  return(qgb2(runif(1,min=bottom, max=1), fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4]))
}

I_phi<-function(inc){
  return(mean(inc*sapply(inc, function(x) if (x>0){1}else{0})))
}

semipar_BDB_LD<-function(inc1, inc2, topcode1, topcode2, fit1, fit2, reps, func = "S", year1, year2){
  gridsize<-0.7*min(c(length(inc1), length(inc2)))
  grid1 <- seq(0,1,1/gridsize)
  grid1<-grid1[2:length(grid1)]
  inc1<-inc1[which(topcode1==0)]
  inc2<-inc2[which(topcode2==0)]
  topcode1<-topcode1[which(topcode1>0)]
  topcode2<-topcode2[which(topcode2>0)]
  bootreps.df<-data.frame()
  mireps.df<-data.frame()
  for (i in 1:reps){
    temp1<-vapply(topcode1, FUN=topcode_sub1, fit1, FUN.VALUE=0.0)
    temp2<-vapply(topcode2, FUN=topcode_sub1, fit2, FUN.VALUE=0.0)
    inc1t<-append(temp1, sample(inc1, replace=T))
    inc2t<-append(temp2, sample(inc2, replace=T))
    inc1m<-append(inc1,temp1)
    inc2m<-append(inc2,temp2)
    bootreps1<-Lc(inc1t)$L[round(grid1*length(inc1t))]-Lc(inc2t)$L[round(grid1*length(inc2t))]
    mireps1<-Lc(inc1m)$L[round(grid1*length(inc1t))]-Lc(inc2m)$L[round(grid1*length(inc2t))]
    bootreps.df<-rbind(bootreps.df, data.table("reps"=rep(i, length(grid1)), "grid1"=grid1, "bootreps1"=bootreps1))
    mireps.df<-rbind(mireps.df, data.table("reps"=rep(i, length(grid1)),"grid1"=grid1, "mireps1"=mireps1))    
  }
  mireps.df<-aggregate(mireps1~grid1, mireps.df, FUN=mean)
  bootreps.df$phi_hat<-rep(mireps.df$mireps1, reps)
  bootreps.df$phi_diff<- bootreps.df$bootreps1-bootreps.df$phi_hat
  if (func=="S"){
    p_val<-aggregate(phi_diff~reps, bootreps.df, FUN=max)$phi_diff-max(mireps.df$mireps1)
    p_val<-mean(sapply(p_val, function(x) if (x>0){1} else{0}))
  }
  else if (func == "I"){
    p_val<-aggregate(phi_diff~reps, bootreps.df, FUN=I_phi)$phi_diff-I_phi(mireps.df$mireps1)
    p_val<-mean(sapply(p_val, function(x) if (x>0){1} else{0}))
  }
  #return(c("p_val"=as.numeric(p_val), "year1"=as.integer(year1), "year2"=as.integer(year2)))
  return(p_val)
}

ptm<-proc.time()
years = c(1995,2000,2005,2011,2012)
LD_results<-data.frame()
for (i in c(1995, 2000, 2005)){
  min2005 <- quantile(CPS.work.hh[which(CPS.work.hh$year==i),]$cellmean_equivinc, 0.3)
  fit2005_a<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==i & CPS.work.hh$cellmean_equivinc>min2005),]$cellmean_equivinc, method=1)
  for (j in years[which(years>i)]){
    min2011 <- quantile(CPS.work.hh[which(CPS.work.hh$year==j),]$cellmean_equivinc, 0.3)
    fit2011<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==j & CPS.work.hh$cellmean_equivinc>min2011),]$cellmean_equivinc)
    CPS.LD<-ddply(CPS.work.hh, .variables=c("State"), function(x) data.frame("p_val"=semipar_BDB_LD(x[which(x$year==i),]$cellmean_equivinc, x[which(x$year==j),]$cellmean_equivinc, 
                                                                                   x[which(x$year==i),]$topcoded_equivinc, x[which(x$year==j),]$topcoded_equivinc, 
                                                                                   fit2005_a, fit2011, 10, func="S", year1=i, year2=j), "year1"=i, "year2"=j), .parallel=T)
    LD_results<-rbind(LD_results, CPS.LD)
  }
}
proc.time()-ptm
save(LD_results, file="Data/CPS_LD_semipar_results.rda")


i=1995
j=2000
min2005 <- quantile(CPS.work.hh[which(CPS.work.hh$year==i),]$cellmean_equivinc, 0.3)
fit2005_a<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==i & CPS.work.hh$cellmean_equivinc>min2005),]$cellmean_equivinc, method=1)
min2011 <- quantile(CPS.work.hh[which(CPS.work.hh$year==j),]$cellmean_equivinc, 0.3)
fit2011<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==j & CPS.work.hh$cellmean_equivinc>min2011),]$cellmean_equivinc)
system.time(CPS.LD<-ddply(CPS.work.hh, .variables=c("State"), function(x) data.frame("p_val"=semipar_BDB_LD(x[which(x$year==i),]$cellmean_equivinc, x[which(x$year==j),]$cellmean_equivinc, 
                                                                               x[which(x$year==i),]$topcoded_equivinc, x[which(x$year==j),]$topcoded_equivinc, 
                                                                               fit2005_a, fit2011, 10, year1=i, year2=j), "year1"=2005, "year2"=2011), .parallel=T))


AL05<-CPS.work.hh[which(CPS.work.hh$year==2005 & CPS.work.hh$State=="Alabama"),]
AL11<-CPS.work.hh[which(CPS.work.hh$year==2011 & CPS.work.hh$State=="Alabama"),]
fit05<-ml.gb2(AL05$cellmean_equivinc)
fit11<-ml.gb2(AL11$cellmean_equivinc)
try1<-semipar_BDB_LD(AL05$cellmean_equivinc, AL11$cellmean_equivinc, AL05$topcoded_equivinc, AL11$topcoded_equivinc, fit05, fit11, 500, func="I", year1=2005, year2=2011)