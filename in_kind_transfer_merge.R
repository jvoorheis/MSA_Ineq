library(GB2)
library(ineq)
library(reldist)
library(plyr)
library(ggplot2)
library(reshape)
library(parallel)
library(doMC)
library(data.table)

source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
registerDoMC(cores=detectCores())
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("Data/CPS_individual_tax.rda")
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year>1991 & is.na(CPS.work.hh$year)==F)
load("Docs/WIC_NSLP_benefits.rda")
load("Docs/HUD_State_rent.rda")
CPI <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPI.csv")
CPI<-CPI[46:66,]
CPI<-data.table(CPI)
WIC_states<-data.table(WIC_states)
statelevel<-data.table(statelevel)
CPS.work.hh<-data.table(CPS.work.hh)
colnames(WIC_states)[2]<-"statefip"
colnames(statelevel)[1]<-"State"
setkey(CPS.work.hh, statefip, year)
setkey(WIC_states, statefip, year)
CPS.work.hh<-CPS.work.hh[WIC_states, allow.cartesian=T]
setkey(CPS.work.hh, State, year, eligible_rooms)
setkey(statelevel, State, year, eligible_rooms)
CPS.work.hh<-CPS.work.hh[statelevel, allow.cartesian=T]
setkey(CPI, year)
setkey(CPS.work.hh, year)
CPS.work.hh<-CPS.work.hh[CPI, allow.cartesian=T]

CPS.work.hh$rental_sub<-(12*CPS.work.hh$rent - CPS.work.hh$rent_contrib)*CPS.work.hh$hh_rentsub
CPS.work.hh$WIC_val<-CPS.work.hh$WIC*CPS.work.hh$hh_wic
CPS.work.hh$frelunch<-as.numeric(CPS.work.hh$frelunch)
CPS.work.hh$lunch_val<-CPS.work.hh$frelunch * CPS.work.hh$NSLP_Benefit
CPS.work.hh$rental_sub<-apply(data.frame(CPS.work.hh$rental_sub), 1, function(x) if(is.na(x)==T){NA} else if (x<0){0} else{x})

CPS.work.hh$in_kind_vals <- CPS.work.hh$WIC_val + CPS.work.hh$lunch_val + CPS.work.hh$rental_sub
CPS.work.hh$hhincome_posttax_broad<-CPS.work.hh$hhincome_posttax_broad+CPS.work.hh$in_kind_vals
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$eligible_rooms!=0)
CPS.work.hh<-subset(CPS.work.hh, is.na(CPS.work.hh$statefip)==F)
gini(subset(CPS.work.hh, CPS.work.hh$year==1992)$hhincome_pretax_broad)
CPS.work.hh$equivinc_posttax_broad<-CPS.work.hh$hhincome_posttax_broad/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$equivinc_pretax_broad<-CPS.work.hh$hhincome_pretax_broad/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$equivinc_posttax_broad<-apply(data.frame(CPS.work.hh$equivinc_posttax_broad),1, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$equivinc_pretax_broad<-apply(data.frame(CPS.work.hh$equivinc_pretax_broad),1, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
gini(CPS.work.hh$equivinc_posttax_broad)
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year>1991)
CPS.work.hh<-subset(CPS.work.hh, is.na(CPS.work.hh$State)==F)

CPS.work.hh$equivinc_pretax_equity<-(CPS.work.hh$hhincome_pretax_broad+CPS.work.hh$housret + CPS.work.hh$proptax)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$equivinc_posttax_equity<-(CPS.work.hh$hhincome_posttax_broad+CPS.work.hh$housret)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$equivinc_posttax_equity<-apply(data.frame(CPS.work.hh$equivinc_posttax_equity),1, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$equivinc_pretax_equity<-apply(data.frame(CPS.work.hh$equivinc_pretax_equity),1, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})

CPS.work.hh$threshold_pretax_broad <- (CPS.work.hh$threshold_pretax_broad + CPS.work.hh$in_kind_vals)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$threshold_posttax_broad <- (CPS.work.hh$threshold_posttax_broad + CPS.work.hh$in_kind_vals)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$threshold_pretax_equity <- (CPS.work.hh$threshold_pretax_broad + CPS.work.hh$in_kind_vals+CPS.work.hh$housret+CPS.work.hh$proptax)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$threshold_posttax_equity <- (CPS.work.hh$threshold_posttax_broad + CPS.work.hh$in_kind_vals+CPS.work.hh$housret)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$threshold_pretax_broad<-apply(data.frame(CPS.work.hh$threshold_pretax_broad),1, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$threshold_posttax_broad<-apply(data.frame(CPS.work.hh$threshold_posttax_broad),1, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$threshold_pretax_equity<-apply(data.frame(CPS.work.hh$threshold_pretax_equity),1, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$threshold_posttax_equity<-apply(data.frame(CPS.work.hh$threshold_posttax_equity),1, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})

CPS.work.hh$topcoded_equivinc_postequity<-CPS.work.hh$threshold_posttax_equity*CPS.work.hh$topcoded
CPS.work.hh$topcoded_equivinc_preequity<-CPS.work.hh$threshold_pretax_equity*CPS.work.hh$topcoded
CPS.work.hh$bottom_equivinc_postequity<-CPS.work.hh$threshold_posttax_equity*(1-CPS.work.hh$topcoded)
CPS.work.hh$bottom_equivinc_preequity<-CPS.work.hh$threshold_pretax_equity*(1-CPS.work.hh$topcoded)
CPS.work.hh$topcoded_equivinc_postbroad<-CPS.work.hh$threshold_posttax_broad*CPS.work.hh$topcoded
CPS.work.hh$topcoded_equivinc_prebroad<-CPS.work.hh$threshold_pretax_broad*CPS.work.hh$topcoded
CPS.work.hh$bottom_equivinc_postbroad<-CPS.work.hh$threshold_posttax_broad*(1-CPS.work.hh$topcoded)
CPS.work.hh$bottom_equivinc_prebroad<-CPS.work.hh$threshold_pretax_broad*(1-CPS.work.hh$topcoded)



save(CPS.work.hh, file="Data/CPS_broadinc.rda")


broad_redist<-ddply(CPS.work.hh, .variables=c("State", "year"), function(x) c("Gini_posttax_broad"=gini(x$equivinc_posttax_broad),
                                                                              "Gini_pretax_broad"=gini(x$equivinc_pretax_broad),
                                                                              "Gini_pretax"=gini(x$cellmean_equivinc_pretrans),
                                                                              "Gini_posttax"=gini(x$cellmean_equivinc_posttax)), 
                    .parallel=T)
save(broad_redist, file="Data/Broad_income_gini.rda")
