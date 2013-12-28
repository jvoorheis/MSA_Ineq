library(data.table)
library(ineq)
library(reldist)
library(xtable)
library(plyr)
# load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Combined_series.rda")
# load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates.rda")
pop<-aggregate(Population~MSA, FUN=max, data=Ineq.work4)
pop_top<-subset(pop, pop$Population>5000000)


load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Gini_bootstrap_MSA/tempfile19942012.rda")
boot_results<-test_results
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Gini_bootstrap_MSA/tempfile20112012.rda")
boot_results<-rbind(boot_results, test_results)
colnames(boot_results)<-c("MSA",  "year_1",	"year_2",	"Gini",	"Top1",	"Theil",	"9010")
boot_results$year_1<-as.integer(boot_results$year_1)
boot_results$year_2<-as.integer(boot_results$year_2)


start_year<-c(1995, 2000, 2005)
end_year<-c(2011)

boot_pres<-subset(boot_results, boot_results$year_1 %in% start_year & boot_results$year_2 %in% end_year)
boot_pres_top <- subset(boot_pres, boot_pres$MSA %in% pop_top$MSA)


prop_reject05<-function(inc){
  rej<-apply(data.frame(inc),1,function(x) if (x>=0.95){1} else{0})
  return(mean(rej))
}
prop_reject10<-function(inc){
  rej<-apply(data.frame(inc),1,function(x) if (x>=0.9){1} else{0})
  return(mean(rej))
}
simplify_MSA<-function(x){
  temp.str<-unlist(strsplit(x, "-"))[1]
  temp.str<-unlist(strsplit(temp.str, "/"))[1]
  return(temp.str)
}
boot_pres_top$MSA<-apply(data.frame(boot_pres_top$MSA), 1, simplify_MSA)
boot_pres_top$row.names<-NULL

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Gini_Lorenz_tables")
for (i in start_year){
  for (j in end_year){
    temp.data<-subset(boot_pres_top, boot_pres_top$year_1 == i & boot_pres_top$year_2==j)
    filename <- paste("CPS_MSA_gini_", as.character(i), as.character(j), ".tex", sep="")
    print(xtable(temp.data), file =filename, include.rownames=F)
  }
}

load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_tempfile20052011.rda")
boot_results_ACS<-test_results
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_tempfile20002011.rda")
boot_results_ACS<-rbind(boot_results_ACS, test_results)
colnames(boot_results_ACS)<-c("MSA",  "year_1",  "year_2",	"Gini",	"Top1",	"Theil",	"9010")
boot_results_ACS$year_1<-as.integer(boot_results_ACS$year_1)
boot_results_ACS$year_2<-as.integer(boot_results_ACS$year_2)

boot_pres_ACS<-subset(boot_results_ACS, boot_results_ACS$year_1 %in% start_year & boot_results_ACS$year_2 %in% end_year)
boot_pres_top_ACS <- subset(boot_pres_ACS, boot_pres_ACS$MSA %in% pop_top$MSA)
boot_pres_top_ACS$MSA<-apply(data.frame(boot_pres_top_ACS$MSA), 1, simplify_MSA)

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Gini_Lorenz_tables")
for (i in start_year){
  for (j in end_year){
    temp.data<-subset(boot_pres_top_ACS, boot_pres_top_ACS$year_1 == i & boot_pres_top_ACS$year_2==j)
    filename <- paste("ACS_MSA_gini_", as.character(i), as.character(j), ".tex", sep="")
    print(xtable(temp.data), file =filename, include.rownames=F)
  }
}

prop_MSA05<-ddply(boot_pres, .variables=c("year_1", "year_2"), function(x) c("Gini "=prop_reject05(x$Gini), 
                                                                           "Theil "=prop_reject05(x$Theil),
                                                                           "Top 1% "=prop_reject05(x$Top1),
                                                                           "90-10 "=prop_reject05(x[,7])))
                                                                           
prop_MSA10<-ddply(boot_pres, .variables=c("year_1", "year_2"), function(x) c("Gini "=prop_reject10(x$Gini), 
                                                                           "Theil "=prop_reject10(x$Theil),
                                                                           "Top 1% "=prop_reject10(x$Top1),
                                                                           "90-10 "=prop_reject10(x[,7])))

prop_MSA_ACS10<-ddply(boot_pres_ACS, .variables=c("year_1", "year_2"), function(x) c("Gini "=prop_reject10(x$Gini), 
                                                                               "Theil "=prop_reject10(x$Theil),
                                                                               "Top 1% "=prop_reject10(x$Top1),
                                                                               "90-10 "=prop_reject10(x[,7])))
prop_MSA_ACS05<-ddply(boot_pres_ACS, .variables=c("year_1", "year_2"), function(x) c("Gini "=prop_reject05(x$Gini), 
                                                                                 "Theil "=prop_reject05(x$Theil),
                                                                                 "Top 1% "=prop_reject05(x$Top1),
                                                                                 "90-10 "=prop_reject05(x[,7])))


print(xtable(prop_MSA05), file = "CPS_MSA_gini_prop05.tex", include.rownames=F, tabular.environment="tabular")
print(xtable(prop_MSA10), file = "CPS_MSA_gini_prop10.tex", include.rownames=F, tabular.environment="tabular")
print(xtable(prop_MSA_ACS05), file = "ACS_MSA_gini_prop05.tex", include.rownames=F, tabular.environment="tabular")
print(xtable(prop_MSA_ACS10), file = "ACS_MSA_gini_prop10.tex", include.rownames=F, tabular.environment="tabular")