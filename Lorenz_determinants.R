library(AER)
library(plyr)
library(parallel)
library(doMC)

registerDoMC(cores=8)

load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Lorenz_state_covariates.rda")

fw<-function(x){
  weighted.reg<-lm(LC_ord~union_cov+college_prop+UR+State_rate_wages+factor(State)+factor(year), 
                   data=x, weight=as.numeric(Ineq.work4[which(Ineq.work4$ord==0.5),]$Population))
  return(coeftest(weighted.reg, vcovHAC(weighted.reg))[2:5,])
  
}
fuw<-function(x){
  unweighted.reg<-lm(LC_ord~union_cov+college_prop+UR+State_rate_wages+factor(State)+factor(year), data=x)
  return(coeftest(unweighted.reg, vcovHAC(unweighted.reg))[2:5,])
}

try_models<-ddply(Ineq.work4, .variables=c("ord"), function(x) data.frame("var"=c("union_cov","college_prop","UR","State_rate_wages"), fw(x)), .parallel=T)
