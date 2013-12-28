library(reldist)
library(ineq)
library(parallel)
library(doMC)
library(foreach)

#source("/ibrix/home8/jlv/MSA_Ineq/functions.r")
#load("/ibrix/home8/jlv/MSA_Ineq/ACS/ACS_State_topcode_hh1.rda")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
#load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_State_topcode_hh1.rda")
ACS.work.hh<-subset(ACS.work.hh, ACS.work.hh$year==2005 | ACS.work.hh$year==2011)
ACS.work.hh<-subset(ACS.work.hh, ACS.work.hh$State=="California" | ACS.work.hh$State=="Alabama")
CA2005<-subset(ACS.work.hh, ACS.work.hh$year==2005 & ACS.work.hh$State=="California")
CA2011<-subset(ACS.work.hh, ACS.work.hh$year==2011 & ACS.work.hh$State=="California")
AL2005<-subset(ACS.work.hh, ACS.work.hh$year==2005 & ACS.work.hh$State=="Alabama")
AL2011<-subset(ACS.work.hh, ACS.work.hh$year==2011 & ACS.work.hh$State=="Alabama")

attempt2<-numeric(100)
for (i in 1:100){
  attempt2[i]<-I_phi_bootstrap(CA2011$cellmean_equivinc, CA2005$cellmean_equivinc, 100, 1, 10000)
}

ptm<-proc.time()
phi_star_try_AL1<-I_phi_bootstrap(AL2011$cellmean_equivinc, AL2005$cellmean_equivinc, 100, 1, 20000)
proc.time()-ptm
ptm<-proc.time()
phi_star_try_AL2<-I_phi_bootstrap(AL2011$cellmean_equivinc, AL2005$cellmean_equivinc, 100, 0.8)
proc.time()-ptm
ptm<-proc.time()
phi_star_try_AL3<-I_phi_bootstrap(AL2011$cellmean_equivinc, AL2005$cellmean_equivinc, 100, 0.6)
proc.time()-ptm

ptm<-proc.time()
S_phi_star<-S_phi_bootstrap(CA2011$cellmean_equivinc, CA2005$cellmean_equivinc, 100)

ptm<-proc.time()
S_phi_star_try_AL<-S_phi_bootstrap(AL2011$cellmean_equivinc, AL2005$cellmean_equivinc, 100, 0.5)
proc.time()-ptm
ptm<-proc.time()
phi_star_try_AL1<-I_phi_bootstrap(AL2011$cellmean_equivinc, AL2005$cellmean_equivinc, 100, 1, 200000)
proc.time()-ptm


# max_n <- length(CA2005$cellmean_equivinc)
# pop_phi<-S_phi(Lc(CA2011$cellmean_equivinc), Lc(CA2005$cellmean_equivinc), max_n)

# registerDoMC()
# options(cores=detectCores())
# #try1<-numeric(1000)
# try1<-foreach (i = 1:1000, .combine=c)%dopar%{
# max(S_phi(Lc(sample(CA2011$cellmean_equivinc, replace=T)), Lc(sample(CA2005$cellmean_equivinc, replace=T)), max_n)-pop_phi)-max(pop_phi)
# }

