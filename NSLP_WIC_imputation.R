library(reshape)
library(data.table)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
NSLP_natl <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/NSLP_natl.csv")
WIC_states <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/WIC_states.csv")
WIC_natl <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/WIC_natl.csv")
for (i in 1992:2007){
  varname<-paste("X",as.character(i), sep="")
  WIC_states[,(i-1985)]<-0
  colnames(WIC_states)[(i-1985)]<-varname
}
WIC_natl$monthly<-NULL
WIC_states<-melt(WIC_states, id.vars = c("State"))
WIC_states$variable<-apply(data.frame(WIC_states$variable), 1, X_split)
colnames(WIC_states)<-c("State", "year", "WIC_state")
WIC_states$year<-as.numeric(WIC_states$year)
WIC_natl<-data.table(WIC_natl)
WIC_states<-data.table(WIC_states)
NSLP_natl<-data.table(NSLP_natl)
setkey(WIC_states, year)
setkey(WIC_natl, year)
WIC_states<-WIC_states[WIC_natl, allow.cartesian=T]
setkey(NSLP_natl, year)
WIC_states<-WIC_states[NSLP_natl]
WIC_states$WIC<-WIC_states$WIC_benefit+(12*WIC_states$WIC_state)
WIC_states$WIC_state<-NULL
WIC_states$WIC_benefit<-NULL
save(WIC_states, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/WIC_NSLP_benefits.rda")