library(reshape)
library(data.table)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
PersIncPC<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/PersIncPC.csv")
PersInc<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/PersInc.csv")
PersIncPop<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/PersIncPop.csv")
CPI<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPI.csv")

PersIncPC<-melt(PersIncPC, id=c("Fips",  "Area",	"Description"))
colnames(PersIncPC)<-c("cbsafips", "MSA_BEA", "Desc", "year", "PersIncPC")
PersIncPC$Desc<-NULL
PersIncPC$year<-as.character(PersIncPC$year)
PersIncPC$year<-as.numeric(vapply(PersIncPC$year, FUN=X_split, FUN.VALUE=""))

PersInc<-melt(PersInc, id=c("Fips",  "Area",  "Description"))
colnames(PersInc)<-c("cbsafips", "MSA_BEA", "Desc", "year", "PersInc")
PersInc$Desc<-NULL
PersInc$year<-as.character(PersInc$year)
PersInc$year<-as.numeric(vapply(PersInc$year, FUN=X_split, FUN.VALUE=""))

PersIncPop<-melt(PersIncPop, id=c("Fips",  "Area",  "Description"))
colnames(PersIncPop)<-c("cbsafips", "MSA_BEA", "Desc", "year", "Population")
PersIncPop$Desc<-NULL
PersIncPop$year<-as.character(PersIncPop$year)
PersIncPop$year<-as.numeric(vapply(PersIncPop$year, FUN=X_split, FUN.VALUE=""))

CBSA_FIPS_crosswalk_1<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MORG_CBSA_FIPS.csv")
CPS_MSA_crosswalk <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/CPS_MSA_crosswalk.csv")
CBSA_FIPS_crosswalk_1<-data.table(CBSA_FIPS_crosswalk_1)
CPS_MSA_crosswalk<-data.table(CPS_MSA_crosswalk)
PersIncPop<-data.table(PersIncPop)
setkey(PersIncPop, cbsafips)
setkey(CBSA_FIPS_crosswalk_1, cbsafips)
try1<-PersIncPop[CBSA_FIPS_crosswalk_1, allow.cartesian=T]
setkey(try1, MSA_FIPS)
setkey(CPS_MSA_crosswalk, MSA_FIPS)
try2<-try1[CPS_MSA_crosswalk, allow.cartesian=T]
try2<-aggregate(Population~MSA+year+cbsafips, FUN=mean, data=try2)
Pop<-aggregate(Population~MSA+year, FUN=sum, data=try2)

PersInc<-data.table(PersInc)
setkey(PersInc, cbsafips)
setkey(CBSA_FIPS_crosswalk_1, cbsafips)
try1<-PersInc[CBSA_FIPS_crosswalk_1, allow.cartesian=T]
setkey(try1, MSA_FIPS)
setkey(CPS_MSA_crosswalk, MSA_FIPS)
try2<-try1[CPS_MSA_crosswalk, allow.cartesian=T]
try2<-aggregate(PersInc~MSA+year+cbsafips, FUN=mean, data=try2)
PersInc<-aggregate(PersInc~MSA+year, FUN=sum, data=try2)
PersInc<-data.table(PersInc)
Pop<-data.table(Pop)
setkey(PersInc, MSA, year)
setkey(Pop, MSA, year)
PersIncPC<-Pop[PersInc]
PersIncPC$PercIncPC<-1000*(PersIncPC$PersInc/PersIncPC$Pop)
PersIncPC<-data.table(PersIncPC)
CPI<-data.table(CPI)
setkey(CPI, year)
setkey(PersIncPC, year)
PersIncPC<-PersIncPC[CPI, allow.cartesian=T]
PersIncPC<-subset(PersIncPC, PersIncPC$year>=1989)
PersIncPC$Real_PersIncPC<-229.604*(PersIncPC$PercIncPC/PersIncPC$CPI)
save(PersIncPC, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/PersInc.rda")