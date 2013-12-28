library(foreign)
library(sandwich)
library(lmtest)
library(reldist)
library(ineq)
library(ggplot2)

rif_gini<-function(x, incvar, f, options="se"){
  incvar<-sort(incvar)
  mu_F<-mean(incvar)
  G_F<-gini(incvar)
  cdf_Y<-ecdf(incvar)(incvar)
  d_Y<-c(0,(incvar[2:length(incvar)]-incvar[1:(length(incvar)-1)]))
  int_y<-apply(data.frame(1:length(incvar)), 1, function(x) sum(cdf_Y[1:x]*d_Y[1:x]))
  x$rif_y<- -(incvar/mu_F)*G_F + 1 - (incvar/mu_F) +(2/mu_F)*int_y
  fm <- as.formula(paste("rif_y ~ ", as.character(f)))
  if (options=="coef"){
    return(lm(fm, x)$coef)
  }
  else if (options=="se"){
    return(coeftest(lm(fm, x), vcovHC(lm(fm, x)))[,2])
  }
  else if (options=="names"){
    return(names(lm(fm, x)$coef))
  }
  else{
    return(lm(fm, x))
  }
}

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
#CPS.work<-read.dta("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_MSA_demo_cleaned.dta")
load("Data/CPS_broadinc.rda")
CA2011<-subset(CPS.work.hh, CPS.work.hh$year==2011 & CPS.work.hh$statefip=="California")

try1<-rif_gini(CA2011, CA2011$cellmean_equivinc, "union_dens")

incvar<-CA2011$cellmean_equivinc+0.01
incvar<-sort(incvar)
mu_F<-mean(incvar)
G_F<-gini(incvar)
u_incvar<-unique(incvar)
cdf_Y<-ecdf(incvar)(incvar)
cdf_Y1<-unique(cdf_Y)
int_y<-apply(data.frame(incvar),1,function(x) integrate(splinefun(incvar, cdf_Y), 0, x, subdivisions=400, stop.on.error=F)$value)
rif_Y<- -(incvar/mu_F)*G_F + 1 - (incvar/mu_F) +(2/mu_F)*int_y
lm(rif_Y~CA2011$union_dens)

for (i in incvar){
  print(i)
  x<-integrate(splinefun(incvar, cdf_Y), 0, i, subdivisions=200, stop.on.error=F)
}