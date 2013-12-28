library(reldist)
library(plyr)
library(doMC)
library(foreach)

registerDoMC(cores=detectCores())

#setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("simplified_broad_inc.rda")

broad_inc_temp<-broad_inc[which(broad_inc$year==2000 | broad_inc$year==2012),]
Lorenz_KB<-function(inc, weight, ordinate, type="mean"){
  N <- length(inc)
  xi_p <- quantile(inc, probs=ordinate)
  N_hat<-sum(weight)
  mu_hat <- mean(inc)
  I_vec<-apply(data.frame(inc),1,function(x) if (is.na(x)==T){NA} else if (x<=xi_p){1} else{0})
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

percentile_boot<-function(inc1, inc2, ord, alpha, reps){
  theta_hat<-Lorenz_KB(inc1, rep(1, length(inc1)), ord, type="mean")-
    Lorenz_KB(inc2, rep(1, length(inc2)), ord, type="mean")
  bootreps<-foreach(i=1:reps, .combine=c)%dopar%{
    Lorenz_KB(sample(inc1, replace=T), rep(1, length(inc1)), ord, type="mean")-Lorenz_KB(sample(inc2, replace=T), rep(1, length(inc2)), ord, type="mean")
  }
  return(c("Point"=theta_hat, 
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2)))
  ))
}

ptm<-proc.time()
cellmean_LC<-data.frame()
for (i in seq(0.05, 0.95, 0.05)){
  cellmean_temp<-ddply(broad_inc_temp, .variables=c("State"), function(x) c("year_1"=2000, "year_2"=2012, "ord"=i, percentile_boot(x[which(x$year==2012),]$cellmean_equivinc, x[which(x$year==2000),]$cellmean_equivinc, i, 0.05, 500)),.parallel=T)
  cellmean_LC<-rbind(cellmean_LC, cellmean_temp)
}
proc.time()-ptm
save(cellmean_LC, file="cellmean_LC_perctest.rda")
broad_pre_LC<-data.frame()
for (i in seq(0.05, 0.95, 0.05)){
  cellmean_temp<-ddply(broad_inc_temp, .variables=c("State"), function(x) c("year_1"=2000, "year_2"=2012, "ord"=i, percentile_boot(x[which(x$year==2012),]$equivinc_pretax_broad, x[which(x$year==2000),]$equivinc_pretax_broad, i, 0.05,500)),.parallel=T)
  broad_pre_LC<-rbind(broad_pre_LC, cellmean_temp)
}
save(broad_pre_LC, file="broad_pre_LC_perctest.rda")

broad_post_LC<-data.frame()
for (i in seq(0.05, 0.95, 0.05)){
  cellmean_temp<-ddply(broad_inc_temp, .variables=c("State"), function(x) c("year_1"=2000, "year_2"=2012, "ord"=i, percentile_boot(x[which(x$year==2012),]$equivinc_posttax_broad, x[which(x$year==2000),]$equivinc_posttax_broad, i,0.05, 500)), .parallel=T)
  broad_post_LC<-rbind(broad_post_LC, cellmean_temp)
}
save(broad_post_LC, file="broad_post_LC_perctest.rda")