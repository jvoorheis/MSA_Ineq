#Probably clunky way of generating lorenz ordinates, lorenz variances for each MSA
library(reldist)
library(boot) 
library(ineq)
library(GB2)
library(doMC)
library(parallel)
library(reshape)
library(data.table)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")

registerDoMC()
options(cores=detectCores())

Natl_fit<-foreach (i=c(1986, 1995, 2000, 2005, 2012), .combine=c)%dopar%{
temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
bottom_cutoff<-quantile(temp.year$sqrt_equivinc, probs=0.3)
temp.year<-subset(temp.year, temp.year$sqrt_equivinc>bottom_cutoff)
ml.gb2(temp.year$cellmean_equivinc)
}

virtual_inc_year<-data.frame()
for(i in c(1986, 1995, 2000, 2005, 2012)){
  tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i)
  virtual_inc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
  for (j in 1:100){
    temp.data.replace<-vapply(tempyear$topcoded_equivinc, FUN=topcode_sub, Natl_fit[i-1967], FUN.VALUE=0.0)
    virtual_inc[,j]<-temp.data.replace+tempyear$bottom_equivinc
  }
  virtual_inc<-data.frame(virtual_inc)
  virtual_inc$MSA<-tempyear$MSA
  virtual_inc$year<-rep(i, length(tempyear$topcoded_equivinc))
  virtual_inc_year<-rbind(virtual_inc_year, virtual_inc)
}

virtual_inc_year<-melt(virtual_inc_year, id.vars=c("MSA", "year"))


lorenz_means<-data.frame()
lorenz_vm<-data.frame()
lorenz_qi<-data.frame()
for (i in 1:19){
  lorenz_attempt1<-aggregate(value~MSA+year+variable, data=virtual_inc_year, FUN=lorenz_point, ordinate=(i/20))
  lorenz_attempt<-aggregate(value~MSA+year, data=lorenz_attempt1, FUN=mean)
  lorenz_attempt$lorenz_ordinate<-c(rep(i, length(lorenz_attempt$value)))
  lorenz_attempt1$lorenz_ordinate<-c(rep(i, length(lorenz_attempt1$value)))
  lorenz_means<-rbind(lorenz_means, lorenz_attempt)
  lorenz_qi<-rbind(lorenz_qi, lorenz_attempt1)
  lorenz_v1<-aggregate(value~MSA+year+variable, data=virtual_inc_year, FUN=function(x) var(boot(x, lorenz_point, 500, ordinate=(i/20))$t))
  lorenz_v1<-aggregate(value~MSA+year, data=lorenz_v1, FUN=mean)
  lorenz_vm$lorenz_ordinate<-c(rep(i, length(lorenz_vm$value)))
}

ptm<-proc.time()
lorenz_attempt1<-ddply(virtual_inc_year, .variables=c('MSA', 'year', 'variable'), function(x) c("lorenz_ord"=0.05, "lorenz_val"=lorenz_point(x$value, ordinate=0.05)), .parallel=T)
proc.time()-ptm
ptm<-proc.time()
lorenz_attempt1<-aggregate(value~MSA+year+variable, data=virtual_inc_year, FUN=lorenz_point, ordinate=(1/20))
proc.time()-ptm
ptm<-proc.time()
lorenz_v1<-ddply(virtual_inc_year, .variables=c('MSA', 'year', 'variable'), function(x) c("ord"=1/20, "lorenz_val"=lorenz_point(x$value, ordinate=0.05), "boot_var"=var(boot(x$value, lorenz_point, 500, ordinate=(1/20))$t)), .parallel=T)
proc.time()-ptm
ptm<-proc.time()
lorenz_means<-ddply(lorenz_v1, .variables=c('MSA', 'year', 'ord'), function(x) c("vm"=mean(x$boot_var), "qm"=mean(x$lorenz_val)))
proc.time()-ptm

colnames(lorenz_qi)[3]<-"qi"
colnames(lorenz_means)[3]<-"qm"
colnames(lorenz_vm)[3]<-"vm"
#To calculate T_p, per Jenkins, et al 2011
lorenz_qi<-data.table(lorenz_qi)
lorenz_means<-data.table(lorenz_means)
lorenz_vm<-data.table(lorenz_vm)
setkey(lorenz_qi, MSA, year, lorenz_ordinate)
setkey(lorenz_means, MSA, year, lorenz_ordinate)
setkey(lorenz_vm, MSA, year, lorenz_ordinate)
lorenz_bm<-lorenz_qi[lorenz_qm, allow.cartesian=T]
lorenz_bm$bm<-((lorenz_bm$qi-lorenz_bm$qm)^2)/99
lorenz_bm<-aggregate(bm~MSA+year+lorenz_ordinate+qm, FUN=sum, data=lorenz_bm)
lorenz_bm<-data.table(lorenz_bm)
setkey(lorenz_bm, MSA, year, lorenz_ordinate)
lorenz_var<-lorenz_bm[lorenz_vm]
lorenz_var$Tp<-lorenz_var$bm/99 + lorenz_var$vm