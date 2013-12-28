library(foreach)
library(doMC)
registerDoMC(cores=detectCores())

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("Data/CPS_broadinc.rda")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
library(foreach)
work_inc<-subset(CPS.work.hh, CPS.work.hh$year==2012 & CPS.work.hh$statefip=="California")
work_inc1<-subset(CPS.work.hh, CPS.work.hh$year==2000 & CPS.work.hh$statefip=="California")


Lorenz_KB<-function(inc, weight, ordinate, type="mean"){
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

bias_correction<-function(inc1, inc2, ordinate, reps, alpha, accelerate=T){
  theta_hat<-Lorenz_KB(inc1, rep(1, length(inc1)), ordinate, type="mean")-
    Lorenz_KB(inc2, rep(1, length(inc2)), ordinate, type="mean")
  bootreps<-foreach(i=1:reps, .combine=c)%dopar%{
    Lorenz_KB(sample(inc1, replace=T), rep(1, length(inc1)), ordinate, type="mean")-Lorenz_KB(sample(inc2, replace=T), rep(1, length(inc2)), ordinate, type="mean")
  }
  samplesize<-min(length(inc1), length(inc2))
  temp_inc1<-sample(inc1, samplesize, replace=F)
  temp_inc2<-sample(inc2, samplesize, replace=F)
  if (accelerate==T){
    jackknife<-foreach(i=1:samplesize, .combine=c)%dopar%{
      Lorenz_KB(temp_inc1[-i], rep(1, (samplesize-1)), ordinate, type="mean")-Lorenz_KB(temp_inc2[-i], rep(1, (samplesize-1)), ordinate, type="mean")
    }
    theta_bar <- mean(jackknife)
    a <- sum((theta_bar-jackknife)^3)/(6*(sum((theta_bar-jackknife)^2)^1.5))
  }
  else{
    a<-0
  }
  z_num <- apply(data.frame(bootreps),1,function(x) if (x<=theta_hat){1} else{0})
  z_0 <- pnorm(sum(z_num)/reps)
  z_1 <- qnorm(1-alpha/2)
  z_2 <- qnorm(alpha/2)
  print(c(z_0, z_1))
  p_1<-pnorm(z_0+(z_0-z_1)/(1-a*(z_0-z_1)))
  p_2<-pnorm(z_0+(z_0+z_1)/(1-a*(z_0+z_1)))
  return(c("LB"=quantile(bootreps, probs=p_1), "UB"=quantile(bootreps, probs=p_2)))
}
ptm<-proc.time()
Rprof()
x<-bias_correction(work_inc$equivinc_posttax_equity, work_inc1$equivinc_posttax_equity, 0.1, 500, 0.05, accelerate=T)
proc.time()-ptm
x
summaryRprof()

