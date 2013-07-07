setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
library(foreign)
library(data.table)
#Read in data
CPS_MSA_crosswalk <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/CPS_MSA_crosswalk.csv")
Census.work<-read.dta("Census_MSA_micro.dta")

#Merge crosswalk data into Census/Census data
Census.work<-data.table(Census.work)
CPS_MSA_crosswalk<-data.table(CPS_MSA_crosswalk)
setkey(Census.work, MSA_FIPS)
setkey(CPS_MSA_crosswalk, MSA_FIPS)
Census.work<-Census.work[CPS_MSA_crosswalk, allow.cartesian=T]
Census.work<-subset(Census.work, is.na(Census.work$hhincome_topcode)==F)
Census.work$topcode1<-Census.work$hhincome_corr-Census.work$hhincome_topcode
Census.work$topcoded<-vapply(Census.work$topcode1, FUN=Census_trunc, FUN.VALUE=0.0)

Census.work.hh<-aggregate(hhsize~MSA+MSA_FIPS_corrected+year+hhincome_corr+hhincome_topcode+topcoded, data=Census.work, FUN=sqrt_eq)

Census.work.hh$sqrt_equivinc<-Census.work.hh$hhincome_topcode/Census.work.hh$hhsize
Census.work.hh$cellmean_equivinc<-Census.work.hh$hhincome_corr/Census.work.hh$hhsize
save(Census.work.hh, file="Census_topcode_hh1.rda")