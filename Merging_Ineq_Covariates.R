#Combining MSA inequality series with education, tax rates, unemployment, personal income
library(data.table)
library(foreign)
library(XML)
library(ineq)
library(reldist)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Combined_series.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_unions.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/PersInc.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_humancap.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
regions<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/MSA_Regions.csv")

cellmean_gini<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=gini)
cellmean_theil<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=Theil)
cellmean_top1<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=top1share)
cellmean_9010<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=ratio9010f)
colnames(cellmean_gini)[3]<-"cellmean_gini"
colnames(cellmean_theil)[3]<-"cellmean_theil"
colnames(cellmean_9010)[3]<-"cellmean_9010"
colnames(cellmean_top1)[3]<-"cellmean_top1"
cellmean_gini<-data.table(cellmean_gini)
cellmean_theil<-data.table(cellmean_theil)
cellmean_top1<-data.table(cellmean_top1)
cellmean_9010<-data.table(cellmean_9010)
setkey(cellmean_gini, MSA, year)
setkey(cellmean_theil, MSA, year)
setkey(cellmean_9010, MSA, year)
setkey(cellmean_top1, MSA, year)
cellmean_ineq<-cellmean_gini[cellmean_theil]
setkey(cellmean_ineq, MSA, year)
cellmean_ineq<-cellmean_ineq[cellmean_top1]
setkey(cellmean_ineq, MSA, year)
cellmean_ineq<-cellmean_ineq[cellmean_9010]
setkey(cellmean_ineq, MSA, year)
MSA_UR<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/BLS_LAU_UR.csv")
MSA_state_crosswalk<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_MSA_unique.csv")
url<-"http://users.nber.org/~taxsim/state-rates/maxrate.html"
State_tax<-readHTMLTable(url, as.data.frame = TRUE)
State_tax<-data.frame(State_tax)
colnames(State_tax)<-c("year", "statefips",  "Fed_rate_wages",	"State_rate_wages",	"Total_rate_wages",	"Fed_rate_capgains",	"State_rate_capgains",	"Total_rate_capgains",	"Fed_mortgage",	"State_mortgage",	"Total_mortgage",	"State")
State_tax$statefips<-NULL
State_tax[,1:11]<-sapply(State_tax[,1:11], as.character)
State_tax[,1:10]<-sapply(State_tax[,1:10], as.numeric)
State_tax$State<-vapply(State_tax$State, FUN=DC_switch, FUN.VALUE="")


Combined_series<-data.table(Combined_series)
MORG_unions<-data.table(MORG_unions)
MSA_UR<-data.table(MSA_UR)
MSA_state_crosswalk<-data.table(MSA_state_crosswalk)
State_tax<-data.table(State_tax)
college_educ<-data.table(college_educ)
#Merge states back onto main inequality file
setkey(MSA_state_crosswalk, MSA)
setkey(Combined_series, MSA)
Ineq.work<-Combined_series[MSA_state_crosswalk, allow.cartesian=T]
setkey(Ineq.work, State, year)
setkey(State_tax, State, year)
Ineq.work<-Ineq.work[State_tax, allow.cartesian=T]

setkey(Ineq.work, MSA, year)
setkey(MORG_unions, MSA, year)
Ineq.work1<-Ineq.work[MORG_unions]

setkey(Ineq.work1, MSA, year)
setkey(MSA_UR, MSA, year)
Ineq.work2<-Ineq.work1[MSA_UR]

setkey(Ineq.work2, MSA, year)
setkey(PersIncPC, MSA, year)
Ineq.work3<-PersIncPC[Ineq.work2]

setkey(Ineq.work3, MSA, year)
setkey(college_educ, MSA, year)
Ineq.work4<-college_educ[Ineq.work3]
setkey(Ineq.work4, MSA, year)
Ineq.work5<-Ineq.work4[cellmean_ineq]
Ineq.work5<-subset(Ineq.work5, Ineq.work5$year>1989)

regions<-data.table(regions)
setkey(regions, MSA)
setkey(Ineq.work5, MSA)
Ineq.work5<-Ineq.work5[regions, allow.cartesian=T]
Ineq.work5$MSA<-as.factor(Ineq.work5$MSA)

save(Ineq.work4, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates.rda")
write.dta(Ineq.work4, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates.dta")

save(Ineq.work5, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates_cellmean.rda")
write.dta(Ineq.work5, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates_cellmean.dta")