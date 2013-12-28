library(reldist)
library(ineq)
library(foreach)
library(doMC)
registerDoMC()
options(cores=detectCores())

load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_State_hh.rda")

NY1986<-subset(CPS.work.hh, CPS.work.hh$State=="New York" & CPS.work.hh$year==2000)
NY2012<-subset(CPS.work.hh, CPS.work.hh$State=="New York" & CPS.work.hh$year==2011)

LC1986<-Lc(NY1986$cellmean_equivinc)
LC2012<-Lc(NY2012$cellmean_equivinc)

LC1986$L[as.integer(length(LC1986$p)*0.9)]

Is_positive<-function(num){
  if (num>0){
    return(1)
  }
  else{
    return(0)
  }
}
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

phi_bootstrap<-function(pop1, pop2, reps){
  max_n<-0.5*max(c(length(pop1),length(pop2)))
  pop_phi<-I_phi(Lc(pop1), Lc(pop2), max_n)
  T_n <- ((length(pop1)*length(pop2))/(length(pop1)+length(pop2)))
  phi_hat <- foreach(i=1:reps, .combine=c)%dopar%{
    T_n * max(I_phi(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), max_n)-pop_phi)-T_n*max(pop_phi)
  }
  phi_hat<-apply(data.frame(phi_hat), 1, Is_positive)
#   phi_hat<-numeric(reps)
#   for (i in 1:reps){
#     phi_hat[i]<-T_n * max(I_phi(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), max_n)-pop_phi)
#   }
  return((1/reps)*sum(phi_hat))
  #return(phi_hat)
  #return(ecdf(phi_hat)(T_n*max(pop_phi)))  
}




ptm<-proc.time()
#pop_phi<-I_phi(LC1986, LC2012, max_n)
phi_hat<-phi_bootstrap( NY2012$cellmean_equivinc, NY1986$cellmean_equivinc, 500)
phi_hat
proc.time()-ptm
