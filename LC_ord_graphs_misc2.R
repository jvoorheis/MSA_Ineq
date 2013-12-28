library(ggplot2)
library(reshape)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
LorenzResults<-data.frame()
for (i in seq(10,90,10)){
  filename <- paste("Data/State_GB2_LC_boot", as.character(i), ".rda", sep="")
  load(filename)
  LorenzResults<-rbind(LorenzResults, bootresults.State.df)
}

LC0012<-LorenzResults[which(LorenzResults$year_1==2000 & LorenzResults$year_2==2012),1:7]
LC0011<-LorenzResults[which(LorenzResults$year_1==2000 & LorenzResults$year_2==2011),1:7]
LC0512<-LorenzResults[which(LorenzResults$year_1==2005 & LorenzResults$year_2==2012),1:7]
LC0511<-LorenzResults[which(LorenzResults$year_1==2005 & LorenzResults$year_2==2011),1:7]
LC0005<-LorenzResults[which(LorenzResults$year_1==2000 & LorenzResults$year_2==2005),1:7]
MostPop <- c("California", "Texas", "New York", "Florida", "Illinois", "Pennsylvania", "Ohio", "Georgia", "Michigan", "North Carolina")
#LC0012<-LC0012[which(LC0012$State %in% MostPop),]
LC0012<-melt(LC0012, id.vars=c("State", "year_1", "year_2", "ord"))

print(ggplot(LC0012, aes(x=ord, y=value, group=variable, colour=variable)) + labs(title="Change In Lorenz Curves, 2000-2012") + xlab(NULL) + ylab (NULL) + theme(axis.text.x=element_text(angle=60, size=12)) +theme_bw() +geom_line()+geom_hline(aes(yintercept=0)) + facet_wrap(~State,ncol=9))

LC0011<-melt(LC0011, id.vars=c("State", "year_1", "year_2", "ord"))

print(ggplot(LC0011, aes(x=ord, y=value, group=variable, colour=variable)) + labs(title="Change In Lorenz Curves, 2000-2012") + xlab(NULL) + ylab (NULL) + theme(axis.text.x=element_text(angle=60, size=12)) +theme_bw() +geom_line()+geom_hline(aes(yintercept=0)) + facet_wrap(~State,ncol=9))
