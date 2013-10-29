setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
library(foreign)
library(data.table)
library(plyr)
library(multicore)
CPS_MSA_crosswalk <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/CPS_MSA_crosswalk.csv")
CPS_work<-read.dta("CPS_topcode_MSA.dta")
#CPS_work$MSA_FIPS<-as.numeric(CPS_work$metarea)

CPS_work<-data.table(CPS_work)
CPS_MSA_crosswalk<-data.table(CPS_MSA_crosswalk)
setkey(CPS_work, MSA_FIPS)
setkey(CPS_MSA_crosswalk, MSA_FIPS)
CPS.work<-CPS_work[CPS_MSA_crosswalk, allow.cartesian=T]

#Checking to see which MSAs we have complete-ish time series for.
MSA_unique<-unique(CPS.work$MSA)
startyear<-aggregate(year~MSA, data=CPS.work, FUN=min)
endyear<-aggregate(year~MSA, data=CPS.work, FUN=max)
uniqueyear<-aggregate(year~MSA, data=CPS.work, FUN=function(x) length(unique(x)))
colnames(startyear)[2]<-"startyear"
colnames(endyear)[2]<-"endyear"
colnames(uniqueyear)[2]<-"uniqueyear"
uniqueyear<-data.table(uniqueyear)
startyear<-data.table(startyear)
endyear<-data.table(endyear)
setkey(CPS.work, MSA)
setkey(startyear, MSA)
setkey(endyear, MSA)
setkey(uniqueyear, MSA)



CPS.work<-CPS.work[startyear]
CPS.work<-CPS.work[endyear]
CPS.work<-CPS.work[uniqueyear]
CPS.work$yearlength<-CPS.work$endyear-CPS.work$startyear+1
CPS.work$gaps_ind<-CPS.work$yearlength-CPS.work$uniqueyear
# CPS.work<-subset(CPS.work, CPS.work$startyear<=1990)
# CPS.work<-subset(CPS.work, CPS.work$endyear>2010)

complete_data<-subset(CPS.work, CPS.work$gaps_ind==0 & CPS.work$yearlength>=11)
gaps_MSA<-subset(CPS.work, CPS.work$gaps_ind>0 & CPS.work$yearlength<37 & CPS.work$gaps_ind>=9)
toolow_MSA<-subset(CPS.work, CPS.work$yearlength<=10)
exceptions<-subset(CPS.work, CPS.work$gaps_ind>0 & CPS.work$yearlength>=37 & CPS.work$gaps_ind<=9)
gaps_MSA$MSA<-c(rep("NIU, household not in a metropolitan ar", length(gaps_MSA$year)))
toolow_MSA$MSA<-c(rep("NIU, household not in a metropolitan ar", length(toolow_MSA$year)))
CPS.work.hh<-rbind(complete_data, gaps_MSA, toolow_MSA, exceptions)
CPS.work.hh$MSA<-as.character(CPS.work.hh$MSA)
save(CPS.work.hh, file="CPS_topcode_hh1.rda")
write.dta(CPS.work.hh, file="CPS_topcode_hh1.dta")


#Need to do a little cleaning of these rapscallions.
CPS.work.hh$counter<-1
n_obs<-aggregate(counter~MSA+year, data=CPS.work.hh, FUN=sum)
min_obs<-aggregate(counter~MSA, data=n_obs, FUN=min)
mean_obs<-aggregate(counter~MSA, data=n_obs, FUN=mean)
colnames(min_obs)[2]<-"min_obs"
colnames(mean_obs)[2]<-"mean_obs"

CPS.work.hh<-data.table(CPS.work.hh)
mean_obs<-data.table(mean_obs)
min_obs<-data.table(min_obs)
setkey(CPS.work.hh, MSA)
setkey(mean_obs, MSA)
setkey(min_obs, MSA)
new_data<-CPS.work.hh[mean_obs]
new_data<-new_data[min_obs]
toofew_MSA<-subset(new_data, new_data$min_obs<=30)
new_data<-subset(new_data, new_data$min_obs>30)
toofew_MSA$MSA<-c(rep("NIU, household not in a metropolitan ar", length(toofew_MSA$year)))
CPS.work.hh<-rbind(new_data, toofew_MSA)
save(CPS.work.hh, file="CPS_topcode_hh1.rda")
