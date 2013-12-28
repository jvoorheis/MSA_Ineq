library(ineq)
library(reldist)
library(data.table)
library(ggplot2)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
Total_income<-aggregate(cellmean_equivinc~year, data=CPS.work.hh, FUN=sum)
MSA_income<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=sum)
Natl_mean<-aggregate(cellmean_equivinc~year, data=CPS.work.hh, FUN=mean)
mean_inc<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=mean)
MSA_theil<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=Theil)
colnames(MSA_theil)[3]<-"MSA_theil"
colnames(Total_income)[2]<-"Total_income"
colnames(MSA_income)[3]<-"MSA_income"
colnames(Natl_mean)[2]<-"Natl_mean"
colnames(mean_inc)[3]<-"mean_inc"
Total_income<-data.table(Total_income)
MSA_income<-data.table(MSA_income)
Natl_mean<-data.table(Natl_mean)
mean_inc<-data.table(mean_inc)
MSA_theil<-data.table(MSA_theil)

setkey(Total_income, year)
setkey(MSA_income, year)
income_shares<-Total_income[MSA_income]

setkey(Natl_mean, year)
setkey(mean_inc, year)
MSA_mean<-Natl_mean[mean_inc]
setkey(MSA_mean, MSA, year)
setkey(income_shares, MSA, year)
setkey(MSA_theil, MSA, year)
try1<-MSA_mean[income_shares]
setkey(try1, MSA, year)
try2<-try1[MSA_theil]

try2$income_share<-try2$MSA_income/try2$Total_income
try2$rel_mean<-try2$income_share*(log(try2$mean_inc/try2$Natl_mean))
try2$relative_theil<-try2$income_share*try2$MSA_theil
Between_theil<-aggregate(rel_mean~year, data=try2, FUN=sum)
Within_theil<-aggregate(relative_theil~year, data=try2, FUN=sum)
colnames(Between_theil)[2]<-"Between_theil"
colnames(Within_theil)[2]<-"Within_theil"
Between_theil<-data.table(Between_theil)
Within_theil<-data.table(Within_theil)
setkey(Between_theil, year)
setkey(Within_theil, year)
Theil_decomp<-Between_theil[Within_theil]
Between_fit<-fitted(lm(Between_theil~year, data=subset(Theil_decomp, Theil_decomp$year>=1990)))
Within_fit<-fitted(lm(Within_theil~year, data=subset(Theil_decomp, Theil_decomp$year>=1990)))
before1990<-c(rep(NA, length(1968:1989)))
Between_fit<-as.numeric(c(before1990, Between_fit))
Within_fit<-as.numeric(c(before1990, Within_fit))
Theil_decomp$Within_fit<-Within_fit
Theil_decomp$Between_fit<-Between_fit

png(file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Figures/Between_theil_MSA.png")
print(ggplot(Theil_decomp, aes(year)) +
  geom_line(aes(y=Between_theil, colour="Between_theil"))+
  geom_line(aes(y=Between_fit, colour="Between_fit"))+ 
  opts(title="Theil Decomposition, 1968-2012"))
dev.off()

png(file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Figures/Within_theil_MSA.png")
print(ggplot(Theil_decomp, aes(year)) +
  geom_line(aes(y=Within_theil, colour="Within_theil"))+ 
  geom_line(aes(y=Within_fit, colour="Within_fit"))+         
  opts(title="Theil Decomposition, 1968-2012"))
dev.off()