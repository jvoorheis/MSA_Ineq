#Testing script to generate Gini coefficients, Lorenz Curves, etc. using both Jackknife and bootstrapped standard errors
library(reldist)
library(boot) 
library(ineq)
library(GB2)
library(doMC)
library(parallel)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
NewYork<-subset(CPS.work.hh, CPS.work.hh$MSA=="New York-Northern New Jersey-Long Island")
temp.year<-subset(CPS.work.hh, CPS.work.hh$year==2005)
bottom_cutoff<-quantile(temp.year$sqrt_equivinc, probs=0.3)
temp.year<-subset(temp.year, temp.year$sqrt_equivinc>bottom_cutoff)
Natl_fit<-ml.gb2(temp.year$cellmean_equivinc)
temp.MSA<-subset(NewYork, NewYork$year==2005)
gini_try_MSA<-c(rep(0, 100))
gini_var_MSA<-c(rep(0, 100))
lorenz_ords<-matrix(0,19,100)
lorenz_var<-matrix(0,19,100)
ptm <- proc.time()
for (k in 1:100){
  temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
  temp.data.final<-temp.data.replace+temp.MSA$bottom_equivinc
  temp.lorenz<-Lc(temp.data.final)
  gini_try_MSA[k]<-gini(temp.data.final)
  gini_var_MSA[k]<-var(boot(temp.data.final, gini, 100)$t)
  for (i in 1:19){
    lorenz_ords[i,k]<-temp.lorenz$L[as.integer((i/20)*length(temp.data.final))]
    lorenz_var[i,k]<-var(boot(temp.data.final, lorenz_point, 100, ordinate=(i/20))$t)
  }
}
proc.time() - ptm

virtual_inc<-matrix(0, length(temp.MSA$topcoded_equivinc), 100)
for (k in 1:100){
  temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
  virtual_inc[,k]<-temp.data.replace+temp.MSA$bottom_equivinc
}
gini_MSA<-apply(virtual_inc, 2, gini)
ptm<-proc.time()
gini_var<-apply(virtual_inc, 2, function(x) var(boot(x, gini, 100)$t))
proc.time()-ptm
ptm<-proc.time()
gini_var1<-apply(virtual_inc, 2, jackknife_gini)
proc.time()-ptm

jackknife_gini<-function(income){
  n <- length(income)
  income_temp <- matrix(income, length(income), length(income))
  income_temp1 <- matrix(0, length(income)-1, length(income))
  for (i in 1:n){
    income_temp1[,i]<-income_temp[-i,i]
  }
  
  G_nk<-apply(income_temp1, 2, gini)
  Gbar<-mean(G_nk)
  return(((n-1)/n)*sum((G_nk-Gbar)^2))
}
ptm<-proc.time()
jackknife_gini(temp.data.final)


proc.time()-ptm
jackknife_gini(c(1,2,2,3,4,99))

lorenz_point<-function(income, weights, ordinate=0.5){
  n <- length(income)
  L_temp<-Lc(income, n=weights)
  return(L_temp$L[as.integer(ordinate*n)])
}
lorenz_point(temp.data.final)
boot()



temp.MSA<-subset(NewYork, NewYork$year==2012)
gini_try_MSA<-c(rep(0, 100))
gini_var_MSA<-c(rep(0, 100))
lorenz_ords<-matrix(0,19,100)
lorenz_var<-matrix(0,19,100)
virtual_inc<-matrix(0, length(temp.MSA$topcoded_equivinc), 100)
for (k in 1:100){
  temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
  virtual_inc[,k]<-temp.data.replace+temp.MSA$bottom_equivinc
}
x<-apply(virtual_inc, 2, lorenz_point, c(rep(1, length(virtual_inc[,1]))))
for (i in 1:19){
  lorenz_ords[i,]<-apply(virtual_inc, 2, lorenz_point, c(rep(1, length(virtual_inc[,1]))), ordinate=(i/20))
  lorenz_var[i,]<-apply(virtual_inc, 2, function(x) var(boot(x, lorenz_point, 500, ordinate=(i/20))$t))
}
lorenz_mean2012<-c(rep(0,19))
lorenz_variance2012<-c(rep(0,19))
for (i in 1:19){
  lorenz_mean2012[i]<-mean(lorenz_ords[i,])
}
for (i in 1:19){
  lorenz_variance2012[i]<-(1/99)*sum((lorenz_ords[i,]-lorenz_mean2012[i])^2)+mean(lorenz_var[i,])
}

temp.MSA<-subset(NewYork, NewYork$year==1995)
gini_try_MSA<-c(rep(0, 100))
gini_var_MSA<-c(rep(0, 100))
lorenz_ords<-matrix(0,19,100)
lorenz_var<-matrix(0,19,100)
virtual_inc<-matrix(0, length(temp.MSA$topcoded_equivinc), 100)
for (k in 1:100){
  temp.data.replace<-vapply(temp.MSA$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
  virtual_inc[,k]<-temp.data.replace+temp.MSA$bottom_equivinc
}
x<-apply(virtual_inc, 2, lorenz_point, c(rep(1, length(virtual_inc[,1]))))
for (i in 1:19){
  lorenz_ords[i,]<-apply(virtual_inc, 2, lorenz_point, c(rep(1, length(virtual_inc[,1]))), ordinate=(i/20))
  lorenz_var[i,]<-apply(virtual_inc, 2, function(x) var(boot(x, lorenz_point, 500, ordinate=(i/20))$t))
}
lorenz_mean1995<-c(rep(0,19))
lorenz_variance1995<-c(rep(0,19))
for (i in 1:19){
  lorenz_mean1995[i]<-mean(lorenz_ords[i,])
}
for (i in 1:19){
  lorenz_variance1995[i]<-(1/99)*sum((lorenz_ords[i,]-lorenz_mean1995[i])^2)+mean(lorenz_var[i,])
}
test_stat<-(lorenz_mean2012 - lorenz_mean1995)/sqrt(lorenz_variance2012+lorenz_variance1995)
test_stat