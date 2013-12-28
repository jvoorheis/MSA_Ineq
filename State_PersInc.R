library(reshape)
library(data.table)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
PersIncPC<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_PersIncPC.csv")
PersInc<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_PersInc.csv")
PersIncPop<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_Pop.csv")
CPI<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPI.csv")

PersIncPC<-melt(PersIncPC, id=c("Area",  "Description"))
colnames(PersIncPC)<-c("State", "Desc", "year", "PersIncPC")
PersIncPC$Desc<-NULL
PersIncPC$year<-as.character(PersIncPC$year)
PersIncPC$year<-as.numeric(vapply(PersIncPC$year, FUN=X_split, FUN.VALUE=""))

PersInc<-melt(PersInc, id=c("Area",  "Description"))
colnames(PersInc)<-c("State", "Desc", "year", "PersInc")
PersInc$Desc<-NULL
PersInc$year<-as.character(PersInc$year)
PersInc$year<-as.numeric(vapply(PersInc$year, FUN=X_split, FUN.VALUE=""))

PersIncPop<-melt(PersIncPop, id=c("Area",  "Description"))
colnames(PersIncPop)<-c("State", "Desc", "year", "Population")
PersIncPop$Desc<-NULL
PersIncPop$year<-as.character(PersIncPop$year)
PersIncPop$year<-as.numeric(vapply(PersIncPop$year, FUN=X_split, FUN.VALUE=""))

CPI<-data.table(CPI)
PersIncPC<-data.table(PersIncPC)
PersInc<-data.table(PersInc)
PersIncPop<-data.table(PersIncPop)
setkey(PersIncPop, State, year)
setkey(PersIncPC, State, year)
setkey(PersInc, State, year)
PersIncPC<-PersIncPC[PersInc]
setkey(PersIncPC, State, year)
PersIncPC<-PersIncPC[PersIncPop]

setkey(CPI, year)
setkey(PersIncPC, year)
PersIncPC<-PersIncPC[CPI, allow.cartesian=T]
PersIncPC$Real_PersIncPC<-229.604*(PersIncPC$PersIncPC/PersIncPC$CPI)
save(PersIncPC, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_PersInc.rda")