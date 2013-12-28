library(foreach)
library(doMC)
library(plyr)
registerDoMC(cores=detectCores())

setwd("/ibrix/home8/jlv/MSA_Ineq")
load("CPS_broadinc.rda")
source("functions.r")

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
  p_1<-pnorm(z_0+(z_0-z_1)/(1-a*(z_0-z_1)))
  p_2<-pnorm(z_0+(z_0+z_1)/(1-a*(z_0+z_1)))
  return(c("Point"=theta_hat, "LB"=as.numeric(quantile(bootreps, probs=p_1)), "UB"=as.numeric(quantile(bootreps, probs=p_2))))
}



broad_inc<-data.frame("State"=CPS.work.hh$statefip, "year"=CPS.work.hh$year, "cellmean_equivinc"=CPS.work.hh$cellmean_equivinc, "equivinc_pretax_broad"=CPS.work.hh$equivinc_pretax_broad, "equivinc_posttax_broad"=CPS.work.hh$equivinc_posttax_broad)

broad_inc_temp<-broad_inc[which(broad_inc$year==2000 | broad_inc$year==2012),]
st.unique<-unique(broad_inc$State)

cellmean_LC<-data.frame()
for (i in seq(0.05, 0.95, 0.05)){
cellmean_temp<-ddply(broad_inc_temp, .variables=c("State"), function(x) c("year_1"=2000, "year_2"=2012, "ord"=i, bias_correction(x[which(x$year==2012),]$cellmean_equivinc, x[which(x$year==2000),]$cellmean_equivinc, i, 500, 0.05, accelerate=F)))
cellmean_LC<-rbind(cellmean_LC, cellmean_temp)
}
save(cellmean_LC, file="cellmean_LC_ordtest.rda")
broad_pre_LC<-data.frame()
for (i in seq(0.05, 0.95, 0.05)){
cellmean_temp<-ddply(broad_inc_temp, .variables=c("State"), function(x) c("year_1"=2000, "year_2"=2012, "ord"=i, bias_correction(x[which(x$year==2012),]$equivinc_pretax_broad, x[which(x$year==2000),]$equivinc_pretax_broad, i, 500, 0.05, accelerate=F)))
broad_pre_LC<-rbind(broad_pre_LC, cellmean_temp)
}
save(broad_pre_LC, file="broad_pre_LC_ordtest.rda")

broad_post_LC<-data.frame()
for (i in seq(0.05, 0.95, 0.05)){
cellmean_temp<-ddply(broad_inc_temp, .variables=c("State"), function(x) c("year_1"=2000, "year_2"=2012, "ord"=i, bias_correction(x[which(x$year==2012),]$equivinc_posttax_broad, x[which(x$year==2000),]$equivinc_posttax_broad, i, 500, 0.05, accelerate=F)), .parallel=T)
broad_post_LC<-rbind(broad_post_LC, cellmean_temp)
}
save(broad_post_LC, file="broad_post_LC_ordtest.rda")


ptm<-proc.time()
cellmean_LC<-ddply(broad_inc_temp, .variables=c("State"), function(x) c("year_1"=2000, "year_2"=2012, bias_correction(x[which(x$year==2012),]$cellmean_equivinc, x[which(x$year==2000),]$cellmean_equivinc, 0.05, 500, 0.05, accelerate=F)), .parallel=T)
proc.time()-ptm




