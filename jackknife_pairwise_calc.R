library(plyr)
library(data.table)
library(xtable)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
Ineq_means.df<-data.frame()
for (i in c(1986, 1995, 2000, 2005,2011)){
  filename1<-paste("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Ineq_mean_small_",as.character(i),".rda", sep="")
  filename2<-paste("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Ineq_point_small_",as.character(i),".rda", sep="")
  load(filename1)
  load(filename2)
  Ineq_means.df<-rbind(Ineq_means.df, Ineq_means)
}
colnames(Ineq_means.df)[9]<-"CPS_9010"
CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$year==1986 | CPS.work.hh$year==1995 |CPS.work.hh$year==2000 | CPS.work.hh$year==2005 | CPS.work.hh$year==2011)
CPS.work.hh$samplesize<-1
samplesize<-aggregate(samplesize~MSA+year, data=CPS.work.hh, FUN=sum)

Ineq_bm<-ddply(Ineq_means.df, .variables=c("MSA", "year"), function(x) c("gini_qm"=mean(x$CPS_gini, na.rm=T), "gini_vm"=mean(x$CPS_gini_var, na.rm=T),
                                                                         "qm_9010"=mean(x$CPS_9010, na.rm=T), "vm_9010"=mean(x$CPS_9010_var, na.rm=T), 
                                                                         "qm_top1"=mean(x$CPS_top1, na.rm=T), "vm_top1"=mean(x$CPS_top1_var, na.rm=T)))
Ineq_bm<-data.table(Ineq_bm)
Ineq_means.df<-data.table(Ineq_means.df)
samplesize<-data.table(samplesize)
setkey(Ineq_means.df, MSA, year)
setkey(Ineq_bm, MSA, year)
setkey(samplesize, MSA, year)
Pairwise_means<-Ineq_means.df[Ineq_bm, allow.cartesian=T]
setkey(Pairwise_means, MSA, year)
Pairwise_means<-Pairwise_means[samplesize, allow.cartesian=T]
Pairwise_means$bm_gini<-((Pairwise_means$CPS_gini-Pairwise_means$gini_qm)^2)/99
Pairwise_means$bm_9010<-((Pairwise_means$CPS_9010-Pairwise_means$qm_9010)^2)/99
Pairwise_means$bm_top1<-((Pairwise_means$CPS_top1-Pairwise_means$qm_top1)^2)/99
Pairwise_means<-ddply(Pairwise_means, .variables=c("MSA", "year"), function(x) c("gini_qm"=mean(x$CPS_gini, na.rm=T), 
                                                                                 "qm_9010"=mean(x$CPS_9010, na.rm=T),  
                                                                                 "qm_top1"=mean(x$CPS_top1, na.rm=T), 
                                                                                 "Tp_gini"=sum(x$bm_gini)/100+mean(x$gini_vm, na.rm=T),
                                                                                 "Tp_9010"=sum(x$bm_9010)/100+mean(x$vm_9010, na.rm=T),
                                                                                 "Tp_top1"=sum(x$bm_top1)/100+mean(x$vm_top1, na.rm=T),
                                                                                 "samplesize"=mean(x$samplesize)))

#Pairwise means tests for each unique combination of years (Z=(ybar_1 - ybar_2)/(sqrt(sigma2_1/n1+sigma2_2/n2)))
#Gini
test_results_gini<-data.frame("MSA"=NA, "year_1"=NA, "year_2"=NA, "Test_stat"=NA)
MSA_unique<-unique(Pairwise_means$MSA)
year_unique<-unique(Pairwise_means$year)
years<-c(1986, 1995, 2000, 2005)
for (k in MSA_unique){
for (i in years){
 years_left<-subset(year_unique, year_unique>i)
 for (j in years_left){
   df1<-subset(Pairwise_means, Pairwise_means$year==i & Pairwise_means$MSA==k)
   df2<-subset(Pairwise_means, Pairwise_means$year==j & Pairwise_means$MSA==k)
   test_stat<-(df1$gini_qm[1]-df2$gini_qm[1])/sqrt(df1$Tp_gini[1]/df1$samplesize[1]+df2$Tp_gini[1]/df2$samplesize[1])
   result<-c(df1$MSA[1], df1$year[1], df2$year[1], test_stat)
   test_results_gini<-rbind(test_results_gini, result)
 }
  
}
}
test_results_gini$Test_stat<-as.numeric(test_results_gini$Test_stat)
test_results_gini$year_1<-as.numeric(test_results_gini$year_1)
test_results_gini$year_2<-as.numeric(test_results_gini$year_2)
test_results_gini<-subset(test_results_gini, is.na(test_results_gini$Test_stat)==F)
less_than_05<-function(x){
  if (x<= -1.96){
    return(1)
  } 
  else{
    return(0)
  }

} 
test_results_gini$result<-vapply(test_results_gini$Test_stat,  FUN=less_than_05, FUN.VALUE=0)

all_MSAs_gini<-data.frame()
#result_type<-c("A Dominates B", "A Dominates B", "A Dominates B", "A Dominates B", "B Dominates A", "B Dominates A","B Dominates A", "Lorenz Curves Cross", "Lorenz Curves Cross", "Lorenz Curves Cross")
year_1<-c(2000,2000,1995,1995, 1986, 1986)
year_2<-c(2011,2005,2011,2005, 2005, 2011)
test_means_gini<-c(mean(subset(test_results_gini, test_results_gini$year_1==2000 & test_results_gini$year_2==2011)$result),
              mean(subset(test_results_gini, test_results_gini$year_1==2000 & test_results_gini$year_2==2005)$result),
              mean(subset(test_results_gini, test_results_gini$year_1==1995 & test_results_gini$year_2==2011)$result),
              mean(subset(test_results_gini, test_results_gini$year_1==1995 & test_results_gini$year_2==2005)$result),
              mean(subset(test_results_gini, test_results_gini$year_1==1986 & test_results_gini$year_2==2005)$result),
              mean(subset(test_results_gini, test_results_gini$year_1==1986 & test_results_gini$year_2==2011)$result))

all_MSAs_gini<-data.frame(year_1, year_2, "Proportion of MSAs"=test_means_gini)
all_MSAs_gini
print(xtable(all_MSAs_gini, display=c("d","d", "d", "f")), include.rownames=F)

test_results_top1<-data.frame("MSA"=NA, "year_1"=NA, "year_2"=NA, "Test_stat"=NA)
MSA_unique<-unique(Pairwise_means$MSA)
year_unique<-unique(Pairwise_means$year)
years<-c(1986, 1995, 2000, 2005)
for (k in MSA_unique){
  for (i in years){
    years_left<-subset(year_unique, year_unique>i)
    for (j in years_left){
      df1<-subset(Pairwise_means, Pairwise_means$year==i & Pairwise_means$MSA==k)
      df2<-subset(Pairwise_means, Pairwise_means$year==j & Pairwise_means$MSA==k)
      test_stat<-(df1$qm_top1[1]-df2$qm_top1[1])/sqrt(df1$Tp_top1[1]/df1$samplesize[1]+df2$Tp_top1[1]/df2$samplesize[1])
      result<-c(df1$MSA[1], df1$year[1], df2$year[1], test_stat)
      test_results_top1<-rbind(test_results_top1, result)
    }
    
  }
}
test_results_top1$Test_stat<-as.numeric(test_results_top1$Test_stat)
test_results_top1$year_1<-as.numeric(test_results_top1$year_1)
test_results_top1$year_2<-as.numeric(test_results_top1$year_2)
test_results_top1<-subset(test_results_top1, is.na(test_results_top1$Test_stat)==F)
less_than_05<-function(x){
  if (x<= -1.96){
    return(1)
  } 
  else{
    return(0)
  }
  
} 
test_results_top1$result<-vapply(test_results_top1$Test_stat,  FUN=less_than_05, FUN.VALUE=0)

all_MSAs_top1<-data.frame()
#result_type<-c("A Dominates B", "A Dominates B", "A Dominates B", "A Dominates B", "B Dominates A", "B Dominates A","B Dominates A", "Lorenz Curves Cross", "Lorenz Curves Cross", "Lorenz Curves Cross")
year_1<-c(2000,2000,1995,1995, 1986, 1986)
year_2<-c(2011,2005,2011,2005, 2005, 2011)
test_means_top1<-c(mean(subset(test_results_top1, test_results_top1$year_1==2000 & test_results_top1$year_2==2011)$result),
                   mean(subset(test_results_top1, test_results_top1$year_1==2000 & test_results_top1$year_2==2005)$result),
                   mean(subset(test_results_top1, test_results_top1$year_1==1995 & test_results_top1$year_2==2011)$result),
                   mean(subset(test_results_top1, test_results_top1$year_1==1995 & test_results_top1$year_2==2005)$result),
                   mean(subset(test_results_top1, test_results_top1$year_1==1986 & test_results_top1$year_2==2005)$result),
                   mean(subset(test_results_top1, test_results_top1$year_1==1986 & test_results_top1$year_2==2011)$result))

all_MSAs_top1<-data.frame(year_1, year_2, "Proportion of MSAs"=test_means_top1)
all_MSAs_top1
print(xtable(all_MSAs_top1, display=c("d","d", "d", "f")), include.rownames=F)


