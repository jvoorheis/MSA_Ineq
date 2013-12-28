library(reshape)
library(plyr)
library(foreign)

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
CPS.work.hh<-read.dta("Data/CPS_household_tax.dta")
HUD_FMR <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Docs/HUD_FMR.csv")
HUD_FMR$fips2000<-NULL
HUD_FMR$fips2010<-NULL
for (i in 1:233){
  if (unlist(strsplit(colnames(HUD_FMR)[i], "a"))[1]=="ms"){
    HUD_FMR[,i]<-NULL
  }
}

deletions<-seq(201, 195, -1)
for (i in deletions){
  HUD_FMR[,i]<-NULL
}
HUD_FMR$name<-NULL
HUD_FMR$cousub<-NULL
HUD_FMR$cbsasub<-NULL
HUD_FMR$areaname<-NULL

HUD_FMR<-melt(HUD_FMR, id.vars=c("state", "county", "pop2000", "pop2010"))
year_split<-function(fmr){
  temp<-unlist(strsplit(fmr, "r"))[2]
  temp2<-unlist(strsplit(temp, "_"))
  if (as.numeric(temp2[1])<15){
    return(as.numeric(temp2[1])+2000)
  }
  else{
    return(as.numeric(temp2[1])+1900)
  }
}
bedrooms_split<-function(fmr){
  temp<-unlist(strsplit(fmr, "r"))[2]
  temp2<-unlist(strsplit(temp, "_"))
  if (length(temp2)==2){
    return(as.numeric(temp2[2]))
  }
  else{
    return(0)
  }
}
HUD_FMR$year<-apply(data.frame(HUD_FMR$variable),1,year_split)
HUD_FMR$bedrooms<-apply(data.frame(HUD_FMR$variable),1,bedrooms_split)
HUD_FMR$county<-1000*HUD_FMR$state+HUD_FMR$county
HUD_FMR$variable<-NULL
HUD_FMR<-data.frame("statefip"=HUD_FMR$state, "year"=HUD_FMR$year, "eligible_rooms"=HUD_FMR$bedrooms, "county"=HUD_FMR$county, "rent" = HUD_FMR$value, "pop"=HUD_FMR$pop2010)
statelevel<-ddply(HUD_FMR, .variables=c("statefip", "year", "eligible_rooms"), function(x) c("county"=0, "rent"=mean(x$rent, na.rm=T)))
HUD_FMR<-rbind(HUD_FMR, statelevel)
HUD_FMR<-subset(HUD_FMR, HUD_FMR$year>1991 & HUD_FMR$year<2013)
statelevel<-subset(statelevel, is.na(statelevel$statefip)==F)
save(HUD_FMR, file="Docs/HUD_rent.rda")
save(statelevel, file="Docs/HUD_State_rent.rda")