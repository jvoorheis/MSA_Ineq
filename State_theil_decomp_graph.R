library(ineq)
library(reldist)
library(data.table)
library(ggplot2)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_State_hh.rda")
Total_income<-aggregate(cellmean_equivinc~year, data=CPS.work.hh, FUN=sum)
State_income<-aggregate(cellmean_equivinc~State+year, data=CPS.work.hh, FUN=sum)
Natl_mean<-aggregate(cellmean_equivinc~year, data=CPS.work.hh, FUN=mean)
mean_inc<-aggregate(cellmean_equivinc~State+year, data=CPS.work.hh, FUN=mean)
State_theil<-aggregate(cellmean_equivinc~State+year, data=CPS.work.hh, FUN=Theil)
colnames(State_theil)[3]<-"State_theil"
colnames(Total_income)[2]<-"Total_income"
colnames(State_income)[3]<-"State_income"
colnames(Natl_mean)[2]<-"Natl_mean"
colnames(mean_inc)[3]<-"mean_inc"
Total_income<-data.table(Total_income)
State_income<-data.table(State_income)
Natl_mean<-data.table(Natl_mean)
mean_inc<-data.table(mean_inc)
State_theil<-data.table(State_theil)

setkey(Total_income, year)
setkey(State_income, year)
income_shares<-Total_income[State_income]

setkey(Natl_mean, year)
setkey(mean_inc, year)
State_mean<-Natl_mean[mean_inc]
setkey(State_mean, State, year)
setkey(income_shares, State, year)
setkey(State_theil, State, year)
try1<-State_mean[income_shares]
setkey(try1, State, year)
try2<-try1[State_theil]

try2$income_share<-try2$State_income/try2$Total_income
try2$rel_mean<-try2$income_share*(log(try2$mean_inc/try2$Natl_mean))
try2$relative_theil<-try2$income_share*try2$State_theil
Between_theil<-aggregate(rel_mean~year, data=try2, FUN=sum)
Within_theil<-aggregate(relative_theil~year, data=try2, FUN=sum)
colnames(Between_theil)[2]<-"Between_theil"
colnames(Within_theil)[2]<-"Within_theil"
Between_theil<-data.table(Between_theil)
Within_theil<-data.table(Within_theil)
setkey(Between_theil, year)
setkey(Within_theil, year)
Theil_decomp<-Between_theil[Within_theil]
png(file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Figures/Between_theil_State.png")
print(ggplot(Theil_decomp, aes(year)) +
        geom_line(aes(y=Between_theil, colour="Between_theil"))+
        opts(title="Theil Decomposition, 1968-2012"))
dev.off()

png(file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Figures/Within_theil_State.png")
print(ggplot(Theil_decomp, aes(year)) +
        geom_line(aes(y=Within_theil, colour="Within_theil"))+  
        opts(title="Theil Decomposition, 1968-2012"))
dev.off()