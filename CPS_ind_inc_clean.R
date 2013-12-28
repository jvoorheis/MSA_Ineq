library(foreign)
library(data.table)
library(plyr)
library(multicore)
library(GB2)
library(ineq)
library(reldist)
library(plyr)
library(ggplot2)
library(reshape)
library(parallel)
library(doMC)
library(data.table)
registerDoMC(cores=detectCores())



setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
CPS_FIPS_crosswalk<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/State_FIPS_crosswalk.csv")
CPS_MSA_crosswalk <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/CPS_MSA_crosswalk.csv")
CPS.work.hh<-read.dta("Data/CPS_individual_tax.dta")
# CPS.work.hh$MSA_FIPS<-as.numeric(CPS.work.hh$metarea)
CPS.work.hh$State<-as.character(CPS.work.hh$statefip)
CPS.work.hh$statefip<-NULL
CPS.work.hh<-data.table(CPS.work.hh)
CPS_FIPS_crosswalk<-data.table(CPS_FIPS_crosswalk)
CPS_MSA_crosswalk<-data.table(CPS_MSA_crosswalk)
setkey(CPS.work.hh, MSA_FIPS)
setkey(CPS_MSA_crosswalk, MSA_FIPS)
CPS.work.hh<-CPS.work.hh[CPS_MSA_crosswalk, allow.cartesian=T]
setkey(CPS.work.hh, State)
setkey(CPS_FIPS_crosswalk, State)
CPS.work.hh<-CPS.work.hh[CPS_FIPS_crosswalk, allow.cartesian=T]
head(CPS.work.hh)
#save(CPS.work.hh, file="Data/CPS_ind_tax.rda")


CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year>1991 & is.na(CPS.work.hh$year)==F)
load("Docs/WIC_NSLP_benefits.rda")
load("Docs/HUD_State_rent.rda")
statelevel<-statelevel[which(statelevel$eligible_rooms>=1),]
CPI <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPI.csv")
CPI<-CPI[46:66,]
CPI<-data.table(CPI)
WIC_states<-data.table(WIC_states)
statelevel<-data.table(statelevel)
CPS.work.hh<-data.table(CPS.work.hh)
setkey(CPS.work.hh, State, year)
setkey(WIC_states, State, year)
CPS.work.hh<-CPS.work.hh[WIC_states, allow.cartesian=T]
setkey(CPS.work.hh, statefip, year, eligible_rooms)
setkey(statelevel, statefip, year, eligible_rooms)
CPS.work.hh<-CPS.work.hh[statelevel, allow.cartesian=T]
setkey(CPI, year)
setkey(CPS.work.hh, year)
CPS.work.hh<-CPS.work.hh[CPI, allow.cartesian=T]

CPS.work.hh$rental_sub<-(12*CPS.work.hh$rent - CPS.work.hh$rent_contrib)*CPS.work.hh$hh_rentsub
CPS.work.hh$WIC_val<-CPS.work.hh$WIC*CPS.work.hh$hh_wic
CPS.work.hh$frelunch<-as.numeric(CPS.work.hh$frelunch)
CPS.work.hh$lunch_val<-CPS.work.hh$frelunch * CPS.work.hh$NSLP_Benefit
CPS.work.hh$rental_sub<-sapply(CPS.work.hh$rental_sub, function(x) if(is.na(x)==T){NA} else if (x<0){0} else{x})
save(CPS.work.hh, file="Data/CPS_broadinc_ind.rda")

load("Data/CPS_broadinc_ind.rda")
CPS.work.hh$in_kind_vals <- CPS.work.hh$WIC_val + CPS.work.hh$lunch_val + CPS.work.hh$rental_sub
CPS.work.hh$hhincome_posttax_broad<-CPS.work.hh$hhincome_posttax_broad+CPS.work.hh$in_kind_vals
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$eligible_rooms!=0)
CPS.work.hh<-subset(CPS.work.hh, is.na(CPS.work.hh$statefip)==F)
gini(subset(CPS.work.hh, CPS.work.hh$year==1992)$hhincome_pretax_broad)
CPS.work.hh$equivinc_posttax_broad<-CPS.work.hh$hhincome_posttax_broad/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$equivinc_pretax_broad<-CPS.work.hh$hhincome_pretax_broad/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$equivinc_posttax_broad<-sapply(CPS.work.hh$equivinc_posttax_broad, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$equivinc_pretax_broad<-sapply(CPS.work.hh$equivinc_pretax_broad, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
gini(CPS.work.hh$equivinc_posttax_broad)
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year>1991)
CPS.work.hh<-subset(CPS.work.hh, is.na(CPS.work.hh$State)==F)

CPS.work.hh$equivinc_pretax_equity<-(CPS.work.hh$hhincome_pretax_broad+CPS.work.hh$housret + CPS.work.hh$proptax)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$equivinc_posttax_equity<-(CPS.work.hh$hhincome_posttax_broad+CPS.work.hh$housret)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$equivinc_posttax_equity<-sapply(CPS.work.hh$equivinc_posttax_equity, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$equivinc_pretax_equity<-sapply(CPS.work.hh$equivinc_pretax_equity, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})

CPS.work.hh$threshold_pretax_broad <- (CPS.work.hh$threshold_pretax_broad + CPS.work.hh$in_kind_vals)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$threshold_posttax_broad <- (CPS.work.hh$threshold_posttax_broad + CPS.work.hh$in_kind_vals)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$threshold_pretax_equity <- (CPS.work.hh$threshold_pretax_broad + CPS.work.hh$in_kind_vals+CPS.work.hh$housret+CPS.work.hh$proptax)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$threshold_posttax_equity <- (CPS.work.hh$threshold_posttax_broad + CPS.work.hh$in_kind_vals+CPS.work.hh$housret)/sqrt(CPS.work.hh$hhsize)
CPS.work.hh$threshold_pretax_broad<-sapply(CPS.work.hh$threshold_pretax_broad, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$threshold_posttax_broad<-sapply(CPS.work.hh$threshold_posttax_broad, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$threshold_pretax_equity<-sapply(CPS.work.hh$threshold_pretax_equity, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})
CPS.work.hh$threshold_posttax_equity<-sapply(CPS.work.hh$threshold_posttax_equity, function(x) if (is.na(x)==T){NA} else if (x<0){1} else{x})

CPS.work.hh$topcoded_equivinc_postequity<-CPS.work.hh$threshold_posttax_equity*CPS.work.hh$topcoded
CPS.work.hh$topcoded_equivinc_preequity<-CPS.work.hh$threshold_pretax_equity*CPS.work.hh$topcoded
CPS.work.hh$bottom_equivinc_postequity<-CPS.work.hh$threshold_posttax_equity*(1-CPS.work.hh$topcoded)
CPS.work.hh$bottom_equivinc_preequity<-CPS.work.hh$threshold_pretax_equity*(1-CPS.work.hh$topcoded)
CPS.work.hh$topcoded_equivinc_postbroad<-CPS.work.hh$threshold_posttax_broad*CPS.work.hh$topcoded
CPS.work.hh$topcoded_equivinc_prebroad<-CPS.work.hh$threshold_pretax_broad*CPS.work.hh$topcoded
CPS.work.hh$bottom_equivinc_postbroad<-CPS.work.hh$threshold_posttax_broad*(1-CPS.work.hh$topcoded)
CPS.work.hh$bottom_equivinc_prebroad<-CPS.work.hh$threshold_pretax_broad*(1-CPS.work.hh$topcoded)
save(CPS.work.hh, file="Data/CPS_broadinc_ind.rda")



load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/PersInc.rda")
#load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_PersInc.rda")

MSA_max_pop<-aggregate(Population~MSA, PersIncPC, FUN=max)
colnames(MSA_max_pop)[2]<-"MSA_pop"
MSA_max_pop<-data.table(MSA_max_pop)
MSA_max_pop$MSA<-as.character(MSA_max_pop$MSA)
CPS.work.hh$MSA<-as.character(CPS.work.hh$MSA)
setkey(CPS.work.hh, MSA)
CPS.unique<-unique(CPS.work.hh$MSA)
pop.unique<-unique(MSA_max_pop$MSA)
CPS.unique<-CPS.unique[!(CPS.unique %in% pop.unique)]
max2<-data.frame("MSA"=CPS.unique, "MSA_pop"=rep(0, length(CPS.unique)))
MSA_max_pop<-rbind(MSA_max_pop, max2)
setkey(MSA_max_pop, MSA)

CPS.work.hh<-CPS.work.hh[MSA_max_pop, allow.cartesian=T]

save(CPS.work.hh, file="Data/CPS_broadinc_ind_MSA.rda")

CPS.work.hh.baseline<-data.frame("State"=CPS.work.hh$State, "year"=CPS.work.hh$year, "MSA"=CPS.work.hh$MSA,
                                 "cellmean_equivinc"=CPS.work.hh$cellmean_equivinc, "topcoded_equivinc"=CPS.work.hh$topcoded_equivinc,
                                 "topcoded"=CPS.work.hh$topcoded, "wtsupp"=CPS.work.hh$wtsupp)
save(CPS.work.hh.baseline, file="Data/CPS_ind_baseline.rda")
