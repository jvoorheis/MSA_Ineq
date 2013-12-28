setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
library(foreign)
library(data.table)
MORG.work<-read.dta("/media/john/Shared Linux_Windows Files/MORG/CPS_union/morg_college.dta")

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
#check for duplicates

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

MORG_MSA_2004<-data.table(MORG_MSA_2004)
MORG_MSA_2005<-data.table(MORG_MSA_2005)
setkey(MORG_MSA_2004, state, year, MSA, MSA_FIPS_corrected,  weight, union_mem, union_cove, employed, union_members, union_covered, employment)
setkey(MORG_MSA_2005, state, year, MSA, MSA_FIPS_corrected,  weight, union_mem, union_cove, employed, union_members, union_covered, employment)
MORG_MSA_2004$cbsafips<-NULL
MORG_MSA_2005$cbsafips<-NULL
MORG_MSA_2005$msafips<-NULL
MORG_MSA_2005$X<-NULL
MORG_MSA_2005<-data.table(MORG_MSA_2005)
MORG_MSA_2004<-data.table(MORG_MSA_2004)
MORG_final<-rbind(MORG_MSA_2004, MORG_MSA_2005)
# MORG_final<-subset(MORG_final, is.na(MORG_final$college)==F)
# MORG_final$MSA<-as.character(MORG_final$MSA)
MSA.unique<-unique(MORG_final$MSA)
save(MORG_final, file="MORG_union_final.rda")

unique(subset(MORG_final, MORG_final$MSA=="Akron, OH" & MORG_final$year==2004)$employment)

non2005<-subset(MORG_final, MORG_final$year!=2005)
only2005<-subset(MORG_final, MORG_final$year==2005)

non2005.work<-aggregate(employment~union_members+union_covered+MSA+year, data=non2005, FUN=mean)
only2005_emp<-aggregate(employed~MSA+year, data=only2005, FUN=sum)
only2005_mem<-aggregate(union_mem~MSA+year, data=only2005, FUN=sum)
only2005_cov<-aggregate(union_cove~MSA+year, data=only2005, FUN=sum)

only2005_emp<-data.table(only2005_emp)
only2005_mem<-data.table(only2005_mem)
only2005_cov<-data.table(only2005_cov)
setkey(only2005_cov, MSA, year)
setkey(only2005_mem, MSA, year)
setkey(only2005_emp, MSA, year)
only2005_work<-data.frame("union_members"=only2005_mem$union_mem, "union_covered"=only2005_cov$union_cove,
                          "MSA"=only2005_cov$MSA,	"year"=only2005_cov$year,	"employment"=only2005_emp$employed)
MORG_unions<-rbind(non2005.work, only2005_work)
MORG_unions$union_cov<-MORG_unions$union_covered/MORG_unions$employment
MORG_unions$union_mem<-MORG_unions$union_members/MORG_unions$employment


MORG_unions_emp<-aggregate(employment~MSA+year, data=MORG_unions, FUN=sum)
MORG_unions_mem<-aggregate(union_members~MSA+year, data=MORG_unions, FUN=sum)
MORG_unions_cov<-aggregate(union_covered~MSA+year, data=MORG_unions, FUN=sum)
MORG_unions_emp<-data.table(MORG_unions_emp)
MORG_unions_mem<-data.table(MORG_unions_mem)
MORG_unions_cov<-data.table(MORG_unions_cov)
setkey(MORG_unions_emp, MSA, year)
setkey(MORG_unions_mem, MSA, year)
setkey(MORG_unions_cov, MSA, year)
MORG_unions<-data.frame("union_members"=MORG_unions_mem$union_members, "union_covered"=MORG_unions_cov$union_covered,
                          "MSA"=MORG_unions_cov$MSA,  "year"=MORG_unions_cov$year,	"employment"=MORG_unions_emp$employment)
MORG_unions$union_cov<-MORG_unions$union_covered/MORG_unions$employment
MORG_unions$union_mem<-MORG_unions$union_members/MORG_unions$employment
save(MORG_unions, file="MSA_unions.rda")

uniq2000<-unique(subset(MORG_unions, MORG_unions$year==2000)$MSA)
uniq2005<-unique(subset(MORG_unions, MORG_unions$year==2005)$MSA)

MSA<-c()
Missing<-c()
for (i in uniq2005){
  if (i %in% uniq2000){
    MSA<-append(MSA, i)
    Missing<-append(Missing, 1)
  }
  else{
    MSA<-append(MSA, i)
    Missing<-append(Missing, 0)
  }
}
MissingMSA<-data.frame(MSA, Missing)
troubles<-subset(MissingMSA, MissingMSA$Missing==0)