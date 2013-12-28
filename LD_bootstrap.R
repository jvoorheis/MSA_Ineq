library(reldist)
library(ineq)
library(foreach)
library(doMC)
library(reshape)
library(plyr)
library(parallel)

registerDoMC()
options(cores=detectCores())

years = c(1986, 1995, 2000, 2005, 2011)
load("/ibrix/home8/jlv/MSA_Ineq/CPS_State_hh.rda")
source("/ibrix/home8/jlv/MSA_Ineq/functions.r")


# NY1986<-subset(CPS.work.hh, CPS.work.hh$MSA=="New York" & CPS.work.hh$year==years[1])
# NY2012<-subset(CPS.work.hh, CPS.work.hh$MSA=="New York" & CPS.work.hh$year==years[2])
# 
# LC1986<-Lc(NY1986$cellmean_equivinc)
# LC2012<-Lc(NY2012$cellmean_equivinc)

Natl_fit<-foreach (i=years, .combine=c)%dopar%{
  temp.year<-subset(CPS.work.hh, CPS.work.hh$year==i)
  bottom_cutoff<-quantile(temp.year$cellmean_equivinc, probs=0.3)
  temp.year<-subset(temp.year, temp.year$cellmean_equivinc>bottom_cutoff)
  ml.gb2(temp.year$cellmean_equivinc)
}
Gini_try<-data.frame()
counter=0
for (i in years){
  counter=counter+1
  tempyear<-subset(CPS.work.hh, CPS.work.hh$year==i)
  virtual_inc<-matrix(0, length(tempyear$topcoded_equivinc), 100)
  for (j in 1:100){
    temp.data.replace<-vapply(tempyear$topcoded_equivinc, FUN=topcode_sub, Natl_fit[counter], FUN.VALUE=0.0)
    virtual_inc[,j]<-temp.data.replace+tempyear$bottom_equivinc
  }
  virtual_inc<-data.frame(virtual_inc)
  virtual_inc$State<-tempyear$State  
  #virtual_inc$hwtsupp<-tempyear$hwtsupp
  virtual_inc$year<-rep(i, length(tempyear$topcoded_equivinc))
  Gini_try<-rbind(Gini_try, virtual_inc)
}
gini_try1<-melt(Gini_try, id.vars=c("State", "year"))


I_phi <- function(Lc1, Lc2, max_n){
  #Use a grid of 0.1 times the largest LC vector (500 for now)
  #max_size<-max(c(length(Lc1$p), length(Lc2$p)))
  max_size<-max_n
  grid <- seq(0+1/(max_size), 1-1/(max_size), 1/(max_size))
  phi <- numeric(length(grid))
  for (i in 1:length(grid)){
    phi[i]<-Lc2$L[as.integer(grid[i]*length(Lc2$L))]-Lc1$L[as.integer(grid[i]*length(Lc1$L))]
  }
  return(phi)
}
Is_positive<-function(num){
  if (num>0){
    return(1)
  }
  else{
    return(0)
  }
}
phi_bootstrap<-function(pop1, pop2, reps){
  max_n<-0.3*max(c(length(pop1),length(pop2)))
  pop_phi<-I_phi(Lc(pop1), Lc(pop2), max_n)
  T_n <- ((length(pop1)*length(pop2))/(length(pop1)+length(pop2)))
  phi_hat <- foreach(i=1:reps, .combine=c)%do%{
    T_n * max(I_phi(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), max_n)-pop_phi)-T_n*max(pop_phi)
  }
  phi_hat<-apply(data.frame(phi_hat), 1, Is_positive)
  #   phi_hat<-numeric(reps)
  #   for (i in 1:reps){
  #     phi_hat[i]<-T_n * max(I_phi(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), max_n)-pop_phi)
  #   }
  return(sum(phi_hat))
  #return(phi_hat)
  #return(ecdf(phi_hat)(T_n*max(pop_phi)))  
}



ptm<-proc.time()
p_vals_count<-ddply(gini_try1, .variables=c("State", "variable"), function(x) c("P_val86"=phi_bootstrap(subset(x, x$year==years[5])$value, subset(x, x$year==years[1])$value, 500) , "P_val95"=phi_bootstrap(subset(x, x$year==years[5])$value, subset(x, x$year==years[2])$value, 500), "P_val00"=phi_bootstrap(subset(x, x$year==years[5])$value, subset(x, x$year==years[3])$value, 500), "P_val05"=phi_bootstrap(subset(x, x$year==years[5])$value, subset(x, x$year==years[2])$value, 500)), .parallel=T)
save(p_vals_count, file="/ibrix/home8/jlv/MSA_Ineq/State_bootstrap_LD_raw05.rda")
p_vals1995<-ddply(p_vals_count, .variables=c("State"), function(x) c("P_val86"=1/(500*100)*sum(x$P_val86) , "P_val95"= 1/(500*100) *sum(x$P_val95), "P_val00"=1/(500*100)*sum(x$P_val00) , "P_val05" = 1/(500*100)*sum(x$P_val05)))
save(p_vals1995, file="/ibrix/home8/jlv/MSA_Ineq/State_bootstrap_LD_pval05.rda")
proc.time()-ptm






