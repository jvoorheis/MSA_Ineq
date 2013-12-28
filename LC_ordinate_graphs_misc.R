library(ggplot2)
library(reshape)

LorenzResults<-data.frame()
for (i in seq(10,90,10)){
  filename <- paste("~/Dropbox/Python stuff/CPS_LC_0012_", as.character(i), ".csv", sep="")
  bootresults.State.df<-read.csv(filename)
  LorenzResults<-rbind(LorenzResults, bootresults.State.df)
}


LC0012<-LorenzResults[,1:5]
LC0012<-melt(LC0012, id.vars=c("MSA", "ord"))
print(ggplot(LC0012, aes(x=ord, y=value, group=variable, colour=variable)) + labs(title="Change In Lorenz Curves, 2000-2012") + xlab(NULL) + ylab (NULL) + theme(axis.text.x=element_text(angle=60, size=12)) +theme_bw() +geom_line()+geom_hline(aes(yintercept=0)) + facet_wrap(~MSA,ncol=9))

load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_cellmean_LC0511.rda")
LC0012<-ACS_results[,1:5]
LC0012<-melt(LC0012, id.vars=c("State", "ord"))
print(ggplot(LC0012, aes(x=ord, y=value, group=variable, colour=variable)) + labs(title="Change In Lorenz Curves, 2000-2012") + xlab(NULL) + ylab (NULL) + theme(axis.text.x=element_text(angle=60, size=12)) +theme_bw() +geom_line()+geom_hline(aes(yintercept=0)) + facet_wrap(~State,ncol=9))

png(file="Results/ACS0511_LC.png", width=11, height=8.5, units="in", res=300)
print(ggplot(LC0012, aes(x=ord, y=value, group=variable, colour=variable)) + labs(title="Change In Lorenz Curves, 2005-2011 (ACS)") + xlab(NULL) + ylab (NULL) + theme(axis.text.x=element_text(angle=60, size=12)) +theme_bw() +geom_line()+geom_hline(aes(yintercept=0)) + facet_wrap(~State,ncol=7))
dev.off()

local_min<-function(point, ord){
  local_min<-which(point())
}

local_min<-ddply(ACS_results, .variables=c("State"), function(x) c("local_min"=x$ord[which(x$Point==min(x$Point))]))