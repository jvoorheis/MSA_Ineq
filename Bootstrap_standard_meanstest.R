library(doMC)
library(plyr)
library(data.table)
library(xtable)

registerDoMC()
options(cores=detectCores())

load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/small_pairwise_point_boot.rda")
colnames(Ineq_means)[8]<-"CPS_9010"
Ineq_attempt<-ddply(Ineq_means, .variables=c("MSA", "year"), function(x) c("qm_gini"=mean(x$CPS_gini), 
                                                                         "Tp_gini"=(0.01/99)*sum((x$CPS_gini-mean(x$CPS_gini))^2)+mean(x$CPS_gini_var),
                                                                         "qm_top1"=mean(x$CPS_top1), 
                                                                         "Tp_top1"=(0.01/99)*sum((x$CPS_top1-mean(x$CPS_top1))^2)+mean(x$CPS_top1_var),
                                                                         "qm_9010"=mean(x$CPS_9010), 
                                                                         "Tp_9010"=(0.01/99)*sum((x$CPS_9010-mean(x$CPS_9010))^2)+mean(x$CPS_9010_var)), .parallel=T)

CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year==1986 | CPS.work.hh$year==1995 |CPS.work.hh$year==2000 | CPS.work.hh$year==2005 | CPS.work.hh$year==2011)
CPS.work.hh$samplesize<-1
samplesize<-aggregate(samplesize~MSA+year, data=CPS.work.hh, FUN=sum)

Ineq_attempt<-data.table(Ineq_attempt)
samplesize<-data.table(samplesize)
setkey(Ineq_attempt, MSA, year)
setkey(samplesize, MSA, year)
Pairwise_means<-Ineq_attempt[samplesize, allow.cartesian=T]


MSA.unique<-unique(Pairwise_means$MSA)
test_results<-data.frame()
years_left<-c(1995, 2000, 2005, 2011)
for (k in MSA.unique){
for (i in c(1986, 1995, 2000, 2005)){
  year_temp<-subset(years_left, years_left>i)
  for (j in year_temp){
    temp.df1<-subset(Pairwise_means, Pairwise_means$MSA==k & Pairwise_means$year==i)
    temp.df2<-subset(Pairwise_means, Pairwise_means$MSA==k & Pairwise_means$year==j)
    temp.results<-data.frame("MSA"=k, "year_1"=i, "year_2"=j, "Test_stat_gini"=(temp.df1$qm_gini[1]-temp.df2$qm_gini[1])/sqrt(temp.df1$Tp_gini[1]/temp.df1$samplesize[1]+temp.df2$Tp_gini[1]/temp.df2$samplesize[1]),
                             "Test_stat_top1"=(temp.df1$qm_top1[1]-temp.df2$qm_top1[1])/sqrt(temp.df1$Tp_top1[1]/temp.df1$samplesize[1]+temp.df2$Tp_top1[1]/temp.df2$samplesize[1]),
                             "Test_stat_9010"=(temp.df1$qm_9010[1]-temp.df2$qm_9010[1])/sqrt(temp.df1$Tp_9010[1]/temp.df1$samplesize[1]+temp.df2$Tp_9010[1]/temp.df2$samplesize[1]))
    test_results<-rbind(test_results, temp.results)
  }
}
}