setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
library(foreign)
library(data.table)
CPS_FIPS_crosswalk<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/State_FIPS_crosswalk.csv")
CPS_FIPS_crosswalk<-CPS_FIPS_crosswalk[1:51,]
CPS.work.hh<-read.dta("CPS_household_tax.dta")
# CPS.work.hh$MSA_FIPS<-as.numeric(CPS.work.hh$metarea)

CPS.work.hh<-data.table(CPS.work.hh)
CPS_FIPS_crosswalk<-data.table(CPS_FIPS_crosswalk)
setkey(CPS.work.hh, statefip)
setkey(CPS_FIPS_crosswalk, statefip)
CPS.work.hh<-CPS.work.hh[CPS_FIPS_crosswalk, allow.cartesian=T]

CPS.work.hh$State<-as.character(CPS.work.hh$State)
save(CPS.work.hh, file="CPS_household_tax.rda")

