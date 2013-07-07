setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
library(foreign)
library(data.table)
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
colnames(startyear)[2]<-"startyear"
colnames(endyear)[2]<-"endyear"
startyear<-data.table(startyear)
endyear<-data.table(endyear)
setkey(CPS.work, MSA)
setkey(startyear, MSA)
setkey(endyear, MSA)

CPS.work<-CPS.work[startyear]
CPS.work<-CPS.work[endyear]

CPS.work<-subset(CPS.work, CPS.work$startyear<=1990)
CPS.work<-subset(CPS.work, CPS.work$endyear>2010)
CPS.work.hh<-CPS.work
CPS.work.hh$MSA<-as.character(CPS.work.hh$MSA)
save(CPS.work.hh, file="CPS_topcode_hh.rda")
write.dta(CPS.work.hh, file="CPS_topcode_hh.dta")


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

new_data<-subset(new_data, new_data$min_obs>20)
CPS.work.hh<-new_data
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$MSA!="1650")
save(CPS.work.hh, file="CPS_topcode_hh.rda")
