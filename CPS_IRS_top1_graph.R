library(ggplot2)
#This is using only the MSA residents of the US - when I get a chance I need to redo the data-cleaning to keep non-MSA residents in
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
IRS_top1<-read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/IRS_top1.csv")
CPS_top1<-aggregate(cellmean_equivinc~year, data=CPS.work.hh, FUN=top1share)
CPS_top1<-subset(CPS_top1, CPS_top1$year<=2011)

Top1_compare<-data.frame("year"=CPS_top1$year, "CPS_top1"=CPS_top1$cellmean_equivinc, "IRS_top1"=IRS_top1$IRS_Top1)


png(file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Figures/IRS_CPS_top1.png")
print(ggplot(Top1_compare, aes(year))+
  geom_line(aes(y=CPS_top1, colour="CPS_top1"))+
  geom_line(aes(y=IRS_top1, colour="IRS_top1"))+
  opts(title="IRS vs. CPS measures of top 1% Income Share"))
dev.off()