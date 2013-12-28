#semipar bootstrap

library(doMC)
library(parallel)
library(plyr)
library(GB2)
registerDoMC(cores=8)
library(compiler)
enableJIT(1)

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
source("Code/functions.r")

load("Data/CPS_broadinc_MSA_hh.rda")
CPS.work.hh$cellmean_equivinc = CPS.work.hh$cellmean_equivinc+1
CPS.work.hh<-CPS.work.hh[which(CPS.work.hh$year==2005 | CPS.work.hh$year==2011),]
CPS.work.hh1<-CPS.work.hh[which(CPS.work.hh$Population>500000),]

Lorenz_KB<-function(inc, weight="default", ordinate, type="mean"){
  if (weight=="default"){
    weight<-rep(1, length(inc))
  }
  N <- length(inc)
  xi_p <- quantile(inc, probs=ordinate)
  N_hat<-sum(weight)
  mu_hat <- mean(inc)
  I_vec<-apply(data.frame(inc),1,function(x) if (x<=xi_p){1} else{0})
  L_hat<-(1/(N_hat*mu_hat))*sum(weight*inc*I_vec)
  if (type=="mean"){
    return(L_hat)
  }
  else if (type == "variance"){
    u_i <- (1/(N_hat*mu_hat))*((inc-xi_p)*I_vec + ordinate*xi_p - inc*L_hat)
    var_hat <- N*var(u_i)*(sum(weight^2))
    return(var_hat)
  }
}

semipar_GB2_bootstrap<-function(inc1, inc2, topcode1, topcode2, fit1, fit2, ord, reps, alpha){
  theta_hat<-Lorenz_KB(inc1, weight="default", ord)-Lorenz_KB(inc2, weight="default", ord)
  inc1<-inc1[which(topcode1==0)]
  inc2<-inc2[which(topcode2==0)]
  topcode1<-topcode1[which(topcode1>0)]
  topcode2<-topcode2[which(topcode2>0)]
#   bootreps<-foreach(i=1:reps, .combine=rbind)%do%{
#     temp1<-vapply(topcode1, FUN=topcode_sub, fit1, FUN.VALUE=0.0)
#     temp2<-vapply(topcode2, FUN=topcode_sub, fit2, FUN.VALUE=0.0)
#     inc1t<-append(temp1, sample(inc1, replace=T))
#     inc2t<-append(temp2, sample(inc2, replace=T))
#     data.frame("boot"=Lorenz_KB(inc1t, weight="default", ord) - Lorenz_KB(inc2t, weight="default", ord), 
#                "MI" = Lorenz_KB(append(temp1, inc1), weight="default", ord) - Lorenz_KB(append(temp2, inc2), weight="default", ord))
#   }
  bootreps<-data.frame()
  for (i in 1:reps){
    temp1<-vapply(topcode1, FUN=topcode_sub1, fit1, FUN.VALUE=0.0)
    temp2<-vapply(topcode2, FUN=topcode_sub1, fit2, FUN.VALUE=0.0)
    inc1t<-append(temp1, sample(inc1, replace=T))
    inc2t<-append(temp2, sample(inc2, replace=T))
    bootreps<-rbind(bootreps, data.frame("boot"=Lorenz_KB(inc1t, weight="default", ord) - Lorenz_KB(inc2t, weight="default", ord), 
               "MI" = Lorenz_KB(append(temp1, inc1), weight="default", ord) - Lorenz_KB(append(temp2, inc2), weight="default", ord)))
  }
  theta_hat1 <-  mean(bootreps$MI)
  bootreps <-bootreps$boot
  p_val <- mean(sapply(bootreps, function(x) if ((x-theta_hat)^2 >= theta_hat^2){1} else{0}))
  p_val1 <- mean(sapply(bootreps, function(x) if ((x-theta_hat1)^2 >= theta_hat1^2){1} else{0}))
  return(c("ord" = ord,
           "Point"=theta_hat, 
           "Point1" = theta_hat1,
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2))),
           "p_val"=p_val,
           "p_val1"=p_val1))
}

percentile_boot<-function(inc1, inc2, ord, alpha, reps){
  theta_hat<-Lorenz_KB(inc1, weight="default", ord,  type="mean")-
    Lorenz_KB(inc2, weight="default", ord, type="mean")
  bootreps<-foreach(i=1:reps, .combine=c)%do%{
    Lorenz_KB(sample(inc1, replace=T), weight="default", ord, type="mean")-Lorenz_KB(sample(inc2, replace=T), weight="default",ord, type="mean")
  }
  p_val <- mean(sapply(bootreps, function(x) if ((x-theta_hat)^2 >= theta_hat^2){1} else{0}))
  return(c("ord" = ord,
           "Point"=theta_hat, 
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2))),
           "p_val"=p_val))
}

topcode_sub1<-function(inc, fit2){
  bottom<-pgb2(inc, fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4])
  return(qgb2(runif(1,min=bottom, max=1), fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4]))
}

min2005 <- quantile(CPS.work.hh[which(CPS.work.hh$year==2005),]$cellmean_equivinc, 30)
#system.time(fit2005<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==2005 & CPS.work.hh$cellmean_equivinc>min2005),]$cellmean_equivinc, method=2) )
system.time(fit2005_a<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==2005 & CPS.work.hh$cellmean_equivinc>min2005),]$cellmean_equivinc, method=1) )

plotsML.gb2(CPS.work.hh[which(CPS.work.hh$year==2005),]$cellmean_equivinc, fit2005$opt2$par[1],fit2005$opt2$par[2],fit2005$opt2$par[3],fit2005$opt2$par[4])
plotsML.gb2(CPS.work.hh[which(CPS.work.hh$year==2005),]$cellmean_equivinc, fit2005_a$opt1$par[1],fit2005_a$opt1$par[2],fit2005_a$opt1$par[3],fit2005_a$opt1$par[4])

min2011 <- quantile(CPS.work.hh[which(CPS.work.hh$year==2011),]$cellmean_equivinc, 0.3)
system.time(fit2011<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==2011 & CPS.work.hh$cellmean_equivinc>min2011),]$cellmean_equivinc))

fit2005
fit2011

CA2005<-CPS.work.hh[which(CPS.work.hh$year==2005 & CPS.work.hh$State=="Georgia"),]
CA2011<-CPS.work.hh[which(CPS.work.hh$year==2011 & CPS.work.hh$State=="Georgia"),]
system.time(try1<-semipar_GB2_bootstrap(CA2011$cellmean_equivinc, CA2005$cellmean_equivinc, 
                            CA2011$topcoded_equivinc, CA2005$topcoded_equivinc, fit2011, fit2005, 0.5, 500, 0.05))
system.time(try2<-percentile_boot(CA2011$cellmean_equivinc, CA2005$cellmean_equivinc,  0.5, 0.05, 500))

try1
try2

# Rprof("semipar.out")
# try1<-semipar_GB2_bootstrap(CA2011$cellmean_equivinc, CA2005$cellmean_equivinc, 
#                              CA2011$topcoded_equivinc, CA2005$topcoded_equivinc, fit2011, fit2005, 0.5, 500, 0.05)
# Rprof(NULL)