setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
library(foreign)
library(data.table)
CPS_FIPS_crosswalk<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/State_FIPS_crosswalk.csv")
CPS_work<-read.dta("CPS_topcode_State.dta")
#CPS_work$MSA_FIPS<-as.numeric(CPS_work$metarea)

CPS_work<-data.table(CPS_work)
CPS_FIPS_crosswalk<-data.table(CPS_FIPS_crosswalk)
setkey(CPS_work, statefip)
setkey(CPS_FIPS_crosswalk, statefip)
CPS.work.hh<-CPS_work[CPS_FIPS_crosswalk, allow.cartesian=T]

CPS.work.hh$State<-as.character(CPS.work.hh$State)
save(CPS.work.hh, file="CPS_State_hh.rda")

