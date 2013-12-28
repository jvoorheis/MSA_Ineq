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
counter=0
for(i in c(1986, 1995, 2000, 2005, 2012)){
  counter=counter+1
  tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i & MSA=="New York-Northern New Jersey-Long Island")
  virtual_inc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
  for (j in 1:100){
    temp.data.replace<-vapply(tempyear$topcoded_equivinc, FUN=topcode_sub, Natl_fit[counter], FUN.VALUE=0.0)
    virtual_inc[,j]<-temp.data.replace+tempyear$bottom_equivinc
  }
  virtual_inc<-data.frame(virtual_inc)
  virtual_inc$MSA<-tempyear$MSA
  virtual_inc$year<-rep(i, length(tempyear$topcoded_equivinc))
  virtual_inc_year<-rbind(virtual_inc_year, virtual_inc)
}
virtual_inc_year<-melt(virtual_inc_year, id.vars=c("MSA", "year"))
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
lorenz_qm<-data.frame()
lorenz_qi<-data.frame()
for (i in 1:19){
  filename1<-paste("NY_lorenz_", as.character(i), ".rda", sep="")
  filename2<-paste("NY_lorenz_means_", as.character(i), ".rda", sep="")
  lorenz_v1<-ddply(virtual_inc_year, .variables=c('MSA', 'year', 'variable'), function(x) c("ord"=i, "lorenz_val"=lorenz_point(x$value, ordinate=(i/20)), "boot_var"=var(boot(x$value, lorenz_point, 500, ordinate=(i/20))$t)), .parallel=T)
  lorenz_means<-ddply(lorenz_v1, .variables=c('MSA', 'year', 'ord'), function(x) c("vm"=mean(x$boot_var), "qm"=mean(x$lorenz_val)))
  lorenz_qm<-rbind(lorenz_qm, lorenz_means)
  lorenz_qi<-rbind(lorenz_qi, lorenz_v1)
  #save(lorenz_qi, file=filename1)
  #save(lorenz_qm, file=filename2)
}
lorenz_qm<-data.table(lorenz_qm)
lorenz_qi<-data.table(lorenz_qi)
setkey(lorenz_qi, MSA, year, ord)
setkey(lorenz_qm, MSA, year, ord)
lorenz_vm<-lorenz_qi[lorenz_qm, allow.cartesian=T]
lorenz_vm$bm<-((lorenz_vm$lorenz_val-lorenz_vm$qm)^2)/99
lorenz_vm<-aggregate(bm~MSA+year+ord+qm+vm, FUN=sum, data=lorenz_vm)
lorenz_vm$Tp<-lorenz_vm$bm/99 + lorenz_vm$vm
save(lorenz_vm, file="NY_lorenz.rda")

year.unique<-data.frame("uni"=c(1986, 1995, 2000, 2005, 2012))
Test_stats<-data.frame()
for (i in c(1986, 1995, 2000, 2005)){
  temp.unique<-subset(year.unique, year.unique$uni>i)
  for (j in temp.unique$uni){
    year_A <- subset(lorenz_vm, lorenz_vm$year==i)
    year_B <- subset(lorenz_vm, lorenz_vm$year==j)
    Delta_k <- (year_A$qm - year_B$qm)/(sqrt(year_A$Tp + year_B$Tp))
    Test_stats<-rbind(Test_stats, data.frame("year_A"=c(rep(i, 19)), "year_B"=c(rep(j, 19)),"ord"=1:19, "Delta_k"=Delta_k))
  }
}

Lorenz_test_result<-function(test_stat){
  if (max(test_stat>=3.01)&min(test_stat>=-3.01)){
    return("A dominates B")
  }
  if (min(test_stat<=-3.01)&max(test_stat<=3.01)){
    return("B dominates A")
  }
  else if (max(test_stat<3.01)&min(test_stat>-3.01)){
    return("No dominance")
  }
  if (min(test_stat<=-3.01)&max(test_stat>=3.01)){
    return("Lorenz curves cross")
  }
}
NY_results<-aggregate(Delta_k~year_A+year_B, data=Test_stats, FUN=Lorenz_test_result)

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

Gini_qm<-ddply(virtual_inc_year, .variables=c('MSA', 'year', 'variable'), function(x) c("gini"=gini(x$value), "gini_var"=jackknife_gini(x$value)), .parallel=T)
Gini_qi<-Gini_qm
Gini_qm<-ddply(Gini_qi, .variables=c('MSA', 'year'), function(x) c("qm"=mean(x$gini), "vm"=mean(x$gini_var)), .parallel=T)
Gini_qm<-data.table(Gini_qm)
Gini_qi<-data.table(Gini_qi)
setkey(Gini_qi, MSA, year)
setkey(Gini_qm, MSA, year)
Gini_vm<-Gini_qi[Gini_qm, allow.cartesian=T]
Gini_vm$bm<-((Gini_vm$gini-Gini_vm$qm)^2)/99
Gini_vm<-aggregate(bm~MSA+year+qm+vm, FUN=sum, data=Gini_vm)
Gini_vm$Tp<-Gini_vm$bm/99 + Gini_vm$vm

year.unique<-data.frame("uni"=c(1986, 1995, 2000, 2005, 2012))
Test_stats_gini<-data.frame()
for (i in c(1986, 1995, 2000, 2005)){
  temp.unique<-subset(year.unique, year.unique$uni>i)
  for (j in temp.unique$uni){
    year_A <- subset(Gini_vm, Gini_vm$year==i)
    year_B <- subset(Gini_vm, Gini_vm$year==j)
    Delta_k <- (year_A$qm - year_B$qm)/(sqrt(year_A$Tp + year_B$Tp))
    Test_stats_gini<-rbind(Test_stats_gini, data.frame("year_A"=i, "year_B"=j, "Delta_k"=Delta_k))
  }
}