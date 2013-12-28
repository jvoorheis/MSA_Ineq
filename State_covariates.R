#Combining MSA inequality series with education, tax rates, unemployment, personal income
library(data.table)
library(foreign)
library(XML)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Combined_series_state.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_union.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_PersInc.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_college.rda")
State_UR<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_UR.csv")
regions<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/State_Regions.csv")

url<-"http://users.nber.org/~taxsim/state-rates/maxrate.html"
State_tax<-readHTMLTable(url, as.data.frame = TRUE)
State_tax<-data.frame(State_tax)
colnames(State_tax)<-c("year", "statefips",  "Fed_rate_wages",  "State_rate_wages",	"Total_rate_wages",	"Fed_rate_capgains",	"State_rate_capgains",	"Total_rate_capgains",	"Fed_mortgage",	"State_mortgage",	"Total_mortgage",	"State")
State_tax$statefips<-NULL
State_tax[,1:11]<-sapply(State_tax[,1:11], as.character)
State_tax[,1:10]<-sapply(State_tax[,1:10], as.numeric)
State_tax$State<-vapply(State_tax$State, FUN=DC_switch, FUN.VALUE="")


Combined_series<-data.table(Combined_series)
State_union<-data.table(State_union)
State_UR<-data.table(State_UR)
State_tax<-data.table(State_tax)
State_college<-data.table(State_college)
#Merge states back onto main inequality file
Ineq.work<-Combined_series
setkey(Ineq.work, State, year)
setkey(State_tax, State, year)
Ineq.work<-Ineq.work[State_tax, allow.cartesian=T]

setkey(Ineq.work, State, year)
setkey(State_union, State, year)
Ineq.work1<-Ineq.work[State_union]

setkey(Ineq.work1, State, year)
setkey(State_UR, State, year)
Ineq.work2<-Ineq.work1[State_UR]

setkey(Ineq.work2, State, year)
setkey(PersIncPC, State, year)
Ineq.work3<-PersIncPC[Ineq.work2]

setkey(Ineq.work3, State, year)
setkey(State_college, State, year)
Ineq.work4<-State_college[Ineq.work3]

regions<-data.table(regions)
setkey(regions, State)
setkey(Ineq.work4, State)
Ineq.work4<-Ineq.work4[regions, allow.cartesian=T]
Ineq.work4$State<-as.factor(Ineq.work4$State)


save(Ineq.work4, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_covariates.rda")
write.dta(Ineq.work4, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_covariates.dta")
