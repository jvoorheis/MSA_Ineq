library(XML)
library(data.table)
library(foreign)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/StateGB2_73.rda")
Statepop<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_population.csv")
url<-"http://users.nber.org/~taxsim/state-rates/maxrate.html"
State_tax<-readHTMLTable(url, as.data.frame = TRUE)
State_tax<-data.frame(State_tax)
colnames(State_tax)<-c("year", "statefips",	"Fed_rate_wages",	"State_rate_wages",	"Total_rate_wages",	"Fed_rate_capgains",	"State_rate_capgains",	"Total_rate_capgains",	"Fed_mortgage",	"State_mortgage",	"Total_mortgage",	"State")
State_tax$statefips<-NULL
State_tax[,1:11]<-sapply(State_tax[,1:11], as.character)
State_tax[,1:10]<-sapply(State_tax[,1:10], as.numeric)

State_tax<-data.table(State_tax)
StateGB2.df<-data.table(StateGB2.df)
Statepop<-data.table(Statepop)
setkey(State_tax, State, year)
setkey(StateGB2.df, State, year)
Tax_ineq<-StateGB2.df[State_tax]

setkey(Tax_ineq, State)
setkey(Statepop, State)
Tax_ineq<-Tax_ineq[Statepop, allow.cartesian=T]
Tax_ineq$State<-as.factor(Tax_ineq$State)
write.dta(Tax_ineq, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/State_Tax_Ineq.dta")