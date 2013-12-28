library(GB2)
library(ineq)
library(reldist)
library(plyr)
library(ggplot2)
library(reshape)
library(parallel)
library(doMC)

registerDoMC(cores=8)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")

percentile_boot<-function(inc1, inc2, ord, alpha, reps){
  theta_hat<-Lorenz_KB(inc1, weight="default", ord,  type="mean")-
    Lorenz_KB(inc2, weight="default", ord, type="mean")
  bootreps<-foreach(i=1:reps, .combine=c)%dopar%{
    Lorenz_KB(sample(inc1, replace=T), weight="default", ord, type="mean")-Lorenz_KB(sample(inc2, replace=T), weight="default",ord, type="mean")
  }
  return(c("Point"=theta_hat, 
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2)))
  ))
}

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

semipar_bootstrap<-function(inc1, inc2, fit1, fit2, p, ord, alpha, reps){
  #bottom (1-p)
  bottom_1<-inc1[which(inc1<quantile(inc1, probs=(1-p)))]
  bottom_2<-inc2[which(inc2<quantile(inc2, probs=(1-p)))]
  k1<-length(inc1)-length(bottom_1)
  k2<-length(inc2)-length(bottom_2)
  
  theta_hat<-Lorenz_KB(inc1, weight="default", ord,  type="mean")-
    Lorenz_KB(inc2, weight="default", ord, type="mean")
  bootreps<-foreach(i=1:reps, .combine=c)%dopar%{
    bottom_sample1<-sample(bottom_1, replace=T)
    bottom_sample2<-sample(bottom_2, replace=T)
    topdraws_1<-runif(k1, (1-p),1)
    topdraws_2<-runif(k2, (1-p), 1)
    top_sample1<-qgb2(topdraws_1, fit1$opt1$par[1], fit1$opt1$par[2], fit1$opt1$par[3], fit1$opt1$par[4])
    top_sample2<-qgb2(topdraws_2, fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4])
    Lorenz_KB(append(bottom_sample1, top_sample1), weight="default", ord, type="mean")-Lorenz_KB(append(bottom_sample2, top_sample2), weight="default",ord, type="mean")
  }
  return(c("Point"=theta_hat, 
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2)))
  ))
}

topcode_bootstrap<-function(inc1, inc2, fit1, fit2, topcode1, topcode2, ord, alpha, reps){
  top_1<-topcode1*inc1
  top_2<-topcode2*inc2
  top_1<-top_1[which(top_1>0)]
  top_2<-top_2[which(top_2>0)]
  bottom_1<-(1-topcode1)*inc1
  bottom_2<-(1-topcode2)*inc2
  bottom_1<-bottom_1[which(bottom_1>0)]
  bottom_2<-bottom_2[which(bottom_2>0)]
  theta_hat<-Lorenz_KB(inc1, weight="default", ord,  type="mean")-
    Lorenz_KB(inc2, weight="default", ord, type="mean")
  bootreps<-foreach(i=1:reps, .combine=c)%dopar%{
    bottom_sample1<-sample(bottom_1, replace=T)
    bottom_sample2<-sample(bottom_2, replace=T)
    top_sample1<-apply(data.frame(sample(top_1, replace=T)),1, topcode_sub, fit1)
    top_sample2<-apply(data.frame(sample(top_2, replace=T)),1, topcode_sub, fit2)
    Lorenz_KB(append(bottom_sample1, top_sample1), weight="default", ord, type="mean")-Lorenz_KB(append(bottom_sample2, top_sample2), weight="default",ord, type="mean")
  }
  return(c("Point"=theta_hat, 
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2)))
  ))
}



load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_broadinc_hh.rda")
CPS.work.hh$equivinc_pretax_broad<-CPS.work.hh$equivinc_pretax_broad+1
CPS.work.hh$threshold_pretax_broad<-CPS.work.hh$threshold_pretax_broad+1
fit2000<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==2000 & CPS.work.hh$equivinc_pretax_broad>10000),]$equivinc_pretax_broad)
fit2012<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==2012 & CPS.work.hh$equivinc_pretax_broad>10000),]$equivinc_pretax_broad)

inc1<-CPS.work.hh[which(CPS.work.hh$year==2012) ,]$threshold_pretax_broad
inc2<-CPS.work.hh[which(CPS.work.hh$year==2000),]$threshold_pretax_broad
topcode1<-CPS.work.hh[which(CPS.work.hh$year==2012) ,]$topcoded
topcode2<-CPS.work.hh[which(CPS.work.hh$year==2000) ,]$topcoded


try1<-semipar_bootstrap(inc1, inc2, fit2012, fit2000, 0.05, 0.5, 0.05, 500)
try2<-topcode_bootstrap(inc1, inc2, fit2012, fit2000,  topcode1, topcode2, 0.5, 0.05, 500)
try3<-percentile_boot(inc1, inc2, 0.5, 0.05, 500)

pretaxCI<-data.frame()
percCI<-data.frame()
for (i in seq(0.05, 0.95, 0.05)){
  pretaxCI<-rbind(pretaxCI, semipar_bootstrap(inc1, inc2, fit2012, fit2000, 0.05, i, 0.05, 500))
  percCI<-rbind(percCI, percentile_boot(inc1, inc2, i, 0.05, 500))
}

