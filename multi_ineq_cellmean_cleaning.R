library(GB2)
library(ineq)
library(reldist)
library(plyr)
library(ggplot2)
library(reshape)
library(parallel)
library(doMC)

registerDoMC(cores=detectCores())
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
source("Code/functions.r")
load("Data/Multi_ineq_posttax.rda")
load("Data/Multi_ineq_pretax.rda")
load("Data/Multi_ineq_postpre.rda")
load("Data/Multi_ineq_baseline.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_household_tax.rda")
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year>1991 &CPS.work.hh$year<2013 & is.na(CPS.work.hh$year)==F)

for (i in 3:16){
  colnames(Multi_ineq_baseline)[i]<-paste(colnames(Multi_ineq_baseline)[i], "_base", sep="")
  colnames(Multi_ineq_posttax)[i]<-paste(colnames(Multi_ineq_posttax)[i], "_post", sep="")
  colnames(Multi_ineq_pretax)[i]<-paste(colnames(Multi_ineq_pretax)[i], "_pre", sep="")
  colnames(Multi_ineq_postpre)[i]<-paste(colnames(Multi_ineq_postpre)[i], "_IRS", sep="")
}

multi_ineq<-cbind(Multi_ineq_baseline, Multi_ineq_posttax[,3:16], 
                  Multi_ineq_pretax[,3:16],Multi_ineq_postpre[,3:16])

cellmean_posttax_ineq<-ddply(CPS.work.hh, .variables=c("State", "year"), function(x) 
  c("cellmean_gini_post"=gini(x$cellmean_equivinc_posttax), "cellmean_theil_post"=Theil(x$cellmean_equivinc_posttax),
    "cellmean_schutz_post"=RS(x$cellmean_equivinc_posttax), 
    "cellmean_top1_post"=top1share(x$cellmean_equivinc_posttax), "cellmean_9010_post"=ratio9010f(x$cellmean_equivinc_posttax),
    "cellmean_8020_post"=ratio8020f(x$cellmean_equivinc_posttax),
    "cellmean_gini_pre"=gini(x$cellmean_equivinc_pretrans), 
    "cellmean_theil_pre"=Theil(x$cellmean_equivinc_pretrans), "cellmean_schutz_pre"=RS(x$cellmean_equivinc_pretrans), 
    "cellmean_top1_pre"=top1share(x$cellmean_equivinc_pretrans), 
    "cellmean_9010_pre"=ratio9010f(x$cellmean_equivinc_pretrans), "cellmean_8020_pre"=ratio8020f(x$cellmean_equivinc_pretrans),
    "cellmean_gini_base"=gini(x$cellmean_equivinc), "cellmean_theil_base"=Theil(x$cellmean_equivinc),
    "cellmean_schutz_base"=RS(x$cellmean_equivinc),
    "cellmean_top1_base"=top1share(x$cellmean_equivinc), "cellmean_9010_base"=ratio9010f(x$cellmean_equivinc),
    "cellmean_8020_base"=ratio8020f(x$cellmean_equivinc), "cellmean_gini_postpre"=gini(x$cellmean_equivinc_postpre), 
    "cellmean_theil_postpre"=Theil(x$cellmean_equivinc_postpre),"cellmean_schutz_postpre"=RS(x$cellmean_equivinc_postpre), 
    "cellmean_top1_postpre"=top1share(x$cellmean_equivinc_postpre), 
    "cellmean_9010_postpre"=ratio9010f(x$cellmean_equivinc_postpre),"cellmean_8020_postpre"=ratio8020f(x$cellmean_equivinc_postpre)), .parallel=T)

multi_ineq<-cbind(multi_ineq, cellmean_posttax_ineq[3:26])
save(multi_ineq, file="Data/multi_ineq_post_pre.rda")