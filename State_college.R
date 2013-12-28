setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
library(foreign)
library(data.table)
library(plyr)
MORG.work<-read.dta("/media/john/Shared Linux_Windows Files/MORG/CPS_demo/morg_college.dta")
State_crosswalk<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/MORG_state_crosswalk.csv")
MORG.work$msafips<-NULL
MORG.work$cbsafips<-NULL

MORG.work<-data.table(MORG.work)
State_crosswalk<-data.table(State_crosswalk)
setkey(MORG.work, state)
setkey(State_crosswalk, state)
try1<-MORG.work[State_crosswalk, allow.cartesian=T]

college_prop<-aggregate(college~State+year, data=try1, FUN=mean)
try1.w<-subset(try1, is.na(try1$weight)==F)
college_prow<-ddply(try1.w, .(State, year), function(x) data.frame(college_propw=weighted.mean(x$college, x$weight, na.rm=T)))
college_prop<-data.table(college_prop)
college_prow<-data.table(college_prow)
setkey(college_prop, State, year)
setkey(college_prow, State, year)
college_prow$college_prop<-college_prop$college
State_college<-college_prow
save(State_college, file="State_college.rda")