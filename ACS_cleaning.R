setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
library(foreign)
library(data.table)
#Read in data
CPS_MSA_crosswalk <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/CPS_MSA_crosswalk.csv")
ACS.work<-read.dta("ACS_MSA_micro.dta")

#Merge crosswalk data into ACS/Census data
ACS.work<-data.table(ACS.work)
CPS_MSA_crosswalk<-data.table(CPS_MSA_crosswalk)
setkey(ACS.work, MSA_FIPS)
setkey(CPS_MSA_crosswalk, MSA_FIPS)
ACS.work<-ACS.work[CPS_MSA_crosswalk, allow.cartesian=T]
ACS.work<-subset(ACS.work, is.na(ACS.work$hhincome_topcode)==F)
ACS.work$topcode1<-ACS.work$hhincome_corr-ACS.work$hhincome_topcode
ACS.work$topcoded<-vapply(ACS.work$topcode1, FUN=ACS_trunc, FUN.VALUE=0.0)

ACS.work.hh<-aggregate(hhsize~MSA+MSA_FIPS_corrected+year+hhincome_corr+hhincome_topcode+topcoded, data=ACS.work, FUN=sqrt_eq)

ACS.work.hh$sqrt_equivinc<-ACS.work.hh$hhincome_topcode/ACS.work.hh$hhsize
ACS.work.hh$cellmean_equivinc<-ACS.work.hh$hhincome_corr/ACS.work.hh$hhsize

save(ACS.work.hh, file="ACS_topcode_hh1.rda")