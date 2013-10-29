setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
library(foreign)
library(data.table)
MORG.work<-read.dta("/media/john/Shared Linux_Windows Files/MORG/CPS_demo/morg_college.dta")
#load("CBSA_FIPS_crosswalk")
#CBSA_FIPS_crosswalk_1 <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/CBSA_FIPS_crosswalk_1.csv")
#CBSA_FIPS_crosswalk_1<-subset(CBSA_FIPS_crosswalk_1, is.na(CBSA_FIPS_crosswalk_1$cbsafips)==F)
#CBSA_FIPS_crosswalk_1<-subset(CBSA_FIPS_crosswalk_1, CBSA_FIPS_crosswalk_1$MSA_FIPS>60)
#CBSA.unique<-unique(MORG.work$cbsafips)
CBSA_FIPS_crosswalk_1<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MORG_CBSA_FIPS.csv")
CPS_MSA_crosswalk <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/CPS_MSA_crosswalk.csv")
MORG.2004<-subset(MORG.work, MORG.work$year<=2005 & is.na(MORG.work$msafips)==F)
MORG.2005<-subset(MORG.work, MORG.work$year>=2005 & is.na(MORG.work$cbsafips)==F)
#CBSA.unique<-data.table("cbsafips"=CBSA.unique)
#merging crosswalk file onto pre-2005 MSA subset
MORG.2004<-data.table(MORG.2004)
CPS_MSA_crosswalk<-data.table(CPS_MSA_crosswalk)
colnames(MORG.2004)[2]<-"MSA_FIPS"
setkey(MORG.2004, MSA_FIPS)
setkey(CPS_MSA_crosswalk, MSA_FIPS)
MORG_MSA_2004<-MORG.2004[CPS_MSA_crosswalk, allow.cartesian=T]

#merging crosswalk files (cbsa->FIPS and FIPS->new MSA)
MORG.2005<-data.table(MORG.2005)
CBSA_FIPS_crosswalk_1<-data.table(CBSA_FIPS_crosswalk_1)
CPS_MSA_crosswalk<-data.table(CPS_MSA_crosswalk)
setkey(MORG.2005, cbsafips)
setkey(CBSA_FIPS_crosswalk_1, cbsafips)

try1<-MORG.2005[CBSA_FIPS_crosswalk_1, allow.cartesian=T]
try1<-data.table(try1)
setkey(try1, MSA_FIPS)
setkey(CPS_MSA_crosswalk, MSA_FIPS)
MORG_MSA_2005<-try1[CPS_MSA_crosswalk, allow.cartesian=T]

#Append the two back together
MORG_MSA_2004<-data.table(MORG_MSA_2004)
MORG_MSA_2005<-data.table(MORG_MSA_2005)
setkey(MORG_MSA_2004, state, year, MSA, MSA_FIPS_corrected, college, weight)
setkey(MORG_MSA_2005, state, year, MSA, MSA_FIPS_corrected, college, weight)
MORG_MSA_2004$cbsafips<-NULL
MORG_MSA_2005$cbsafips<-NULL
MORG_MSA_2005$msafips<-NULL
MORG_MSA_2005$X<-NULL
MORG_MSA_2005<-data.table(MORG_MSA_2005)
MORG_MSA_2004<-data.table(MORG_MSA_2004)
MORG_final<-rbind(MORG_MSA_2004, MORG_MSA_2005)
MORG_final<-subset(MORG_final, is.na(MORG_final$college)==F)
MORG_final$MSA<-as.character(MORG_final$MSA)
MSA.unique<-unique(MORG_final$MSA)
save(MORG_final, file="MORG_college_final.rda")
