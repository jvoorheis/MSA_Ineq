#Combining MSA inequality series with education, tax rates, unemployment, personal income
library(data.table)
library(foreign)
library(XML)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
#load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/LC_ordinates_state.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/LC_ordinates_state_GB2.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_union.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_PersInc.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_college.rda")
State_UR<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_UR.csv")
regions<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/State_Regions.csv")
State_Area <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_Area.csv", sep=";")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/median_education.rda")
state_medianed$stfips<-NULL
RD_State_total <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/RD_State_total.csv", sep=";")
colnames(RD_State_total)[c(1,3)]<-c("year","RD_total")
RD_State_total<-RD_State_total[which(RD_State_total$year>1991 & RD_State_total$year<2013),]
territories<-c("Puerto Rico", "Guam", "Virgin Islands")
RD_State_total<-RD_State_total[!(RD_State_total$State %in% territories),]

url<-"http://users.nber.org/~taxsim/state-rates/maxrate.html"
State_tax<-readHTMLTable(url, as.data.frame = TRUE)
State_tax<-data.frame(State_tax)
colnames(State_tax)<-c("year", "statefips",  "Fed_rate_wages",  "State_rate_wages",  "Total_rate_wages",	"Fed_rate_capgains",	"State_rate_capgains",	"Total_rate_capgains",	"Fed_mortgage",	"State_mortgage",	"Total_mortgage",	"State")
State_tax$statefips<-NULL
State_tax[,1:11]<-sapply(State_tax[,1:11], as.character)
State_tax[,1:10]<-sapply(State_tax[,1:10], as.numeric)
State_tax$State<-vapply(State_tax$State, FUN=DC_switch, FUN.VALUE="")


Combined_series<-data.table(LC_state)
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

State_Area<-data.table(State_Area)
setkey(State_Area, State)
setkey(Ineq.work4, State)
Ineq.work4<-Ineq.work4[State_Area, allow.cartesian=T]
Ineq.work4$popdens<-Ineq.work4$Population/Ineq.work4$sq_mi

Ineq.work4<-data.table(Ineq.work4)
state_medianed<-data.table(state_medianed)
setkey(Ineq.work4, State, year)
setkey(state_medianed, State, year)
Ineq.work4<-Ineq.work4[state_medianed, allow.cartesian=T]

Ineq.work4<-Ineq.work4[which(is.na(Ineq.work4$ord)==F),]
RD_State_total<-data.table(RD_State_total)
setkey(RD_State_total, State, year)
Ineq.work4<-Ineq.work4[RD_State_total, allow.cartesian=T]
Ineq.work4$RD_percap<-Ineq.work4$RD_total/Ineq.work4$Population

Ineq.work4$State<-as.factor(Ineq.work4$State)

save(Ineq.work4, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Lorenz_state_covariates_GB2.rda")
write.dta(Ineq.work4, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Lorenz_state_covariates_GB2.dta")
