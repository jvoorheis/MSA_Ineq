setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
library(foreign)
library(data.table)
MORG.work<-read.dta("/media/john/Shared Linux_Windows Files/MORG/CPS_union/morg_college.dta")
State_crosswalk<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/MORG_state_crosswalk.csv")
MORG.work$msafips<-NULL
MORG.work$cbsafips<-NULL

MORG.work<-data.table(MORG.work)
State_crosswalk<-data.table(State_crosswalk)
setkey(MORG.work, state)
setkey(State_crosswalk, state)
try1<-MORG.work[State_crosswalk, allow.cartesian=T]
State_emp<-aggregate(employed~State+year, data=try1, FUN=sum)
State_mem<-aggregate(union_mem~State+year, data=try1, FUN=sum)
State_cov<-aggregate(union_cove~State+year, data=try1, FUN=sum)
State_emp<-data.table(State_emp)
State_mem<-data.table(State_mem)
State_cov<-data.table(State_cov)
setkey(State_emp, State, year)
setkey(State_mem, State, year)
setkey(State_cov, State, year)
try2<-State_emp[State_mem]
setkey(try2, State, year)
try3<-try2[State_cov]
try3$union_membership<-try3$union_mem/try3$employed
try3$union_coverage<-(try3$union_mem+try3$union_cove)/try3$employed
State_union<-data.frame("State"=try3$State, "year"=try3$year, "union_mem"=try3$union_membership, "union_cov"=try3$union_coverage)
save(State_union, file="State_union.rda")
