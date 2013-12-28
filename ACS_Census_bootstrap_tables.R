library(data.table)
library(xtable)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/PersInc.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_gini_bootstrap.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_state_gini_boottest.rda")
state_ACS<-result_temp
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_ACS_boot_gini_test.rda")
MSA_census<-result_temp
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_ACS_boot_gini_test2005.rda")
MSA_census2005<-result_temp
colnames()

MSA_results$test_result<-apply(data.frame(MSA_results$P_val),1, FUN=function(x) if (x>=0.95){1} else{0})
MSA_census$test_result<-apply(data.frame(MSA_census$P_val),1, FUN=function(x) if (x>=0.95){1} else{0})
MSA_census2005$test_result<-apply(data.frame(MSA_census2005$P_val),1, FUN=function(x) if (x>=0.95){1} else{0})


mean_pop<-aggregate(Population~MSA, data=PersIncPC, FUN=mean)
MSA_results<-data.table(MSA_results)
MSA_census<-data.table(MSA_census)
MSA_census2005<-data.table(MSA_census2005)

mean_pop<-data.table(mean_pop)
setkey(mean_pop, MSA)
setkey(MSA_results, MSA)
setkey(MSA_census, MSA)
MSA_results<-mean_pop[MSA_results]
MSA_census<-mean_pop[MSA_census]
MSA_census2005<-mean_pop[MSA_census2005]
MSA_census2005<-subset(MSA_census2005, is.na(MSA_census2005$Population)==F)

MSA_results_big<-subset(MSA_results, MSA_results$Population>=500000)
MSA_census_big<-subset(MSA_census, MSA_census$Population>=500000)
MSA_census_big2005<-subset(MSA_census2005, MSA_census2005$Population>=500000)


mean(MSA_results$test_result)
mean(MSA_results_big$test_result)
mean(MSA_census$test_result)
mean(MSA_census_big$test_result)
mean(MSA_census2005$test_result)
mean(MSA_census_big2005$test_result)
MSA_results_top<-subset(MSA_results, MSA_results$MSA=="New York-Northern New Jersey-Long Island" | 
                           MSA_results$MSA=="Los Angeles-Long Beach-Santa Ana, CA" | 
                           MSA_results$MSA=="Chicago-Naperville-Joliet, IL-IN-WI"  |
                           MSA_results$MSA=="Dallas-Fort Worth-Arlington, TX"  |
                           MSA_results$MSA=="Houston-Baytown-Sugar Land, TX" |
                           MSA_results$MSA=="Philadelphia-Camden-Wilmington, PA/NJ/D" |
                           MSA_results$MSA=="Washington, DC/MD/VA" | 
                           MSA_results$MSA=="Miami-Fort Lauderdale-Miami Beach, FL"   |
                           MSA_results$MSA=="Atlanta-Sandy Springs-Marietta, GA" |
                           MSA_results$MSA== "Boston-Cambridge-Quincy, MA-NH")
MSA_census_top<-subset(MSA_census, MSA_census$MSA=="New York-Northern New Jersey-Long Island" | 
                          MSA_census$MSA=="Los Angeles-Long Beach-Santa Ana, CA" | 
                          MSA_census$MSA=="Chicago-Naperville-Joliet, IL-IN-WI"  |
                          MSA_census$MSA=="Dallas-Fort Worth-Arlington, TX"  |
                          MSA_census$MSA=="Houston-Baytown-Sugar Land, TX" |
                          MSA_census$MSA=="Philadelphia-Camden-Wilmington, PA/NJ/D" |
                          MSA_census$MSA=="Washington, DC/MD/VA" | 
                          MSA_census$MSA=="Miami-Fort Lauderdale-Miami Beach, FL"   |
                          MSA_census$MSA=="Atlanta-Sandy Springs-Marietta, GA" |
                          MSA_census$MSA== "Boston-Cambridge-Quincy, MA-NH")
MSA_census2005_top<-subset(MSA_census2005, MSA_census2005$MSA=="New York-Northern New Jersey-Long Island" | 
                         MSA_census2005$MSA=="Los Angeles-Long Beach-Santa Ana, CA" | 
                         MSA_census2005$MSA=="Chicago-Naperville-Joliet, IL-IN-WI"  |
                         MSA_census2005$MSA=="Dallas-Fort Worth-Arlington, TX"  |
                         MSA_census2005$MSA=="Houston-Baytown-Sugar Land, TX" |
                         MSA_census2005$MSA=="Philadelphia-Camden-Wilmington, PA/NJ/D" |
                         MSA_census2005$MSA=="Washington, DC/MD/VA" | 
                         MSA_census2005$MSA=="Miami-Fort Lauderdale-Miami Beach, FL"   |
                         MSA_census2005$MSA=="Atlanta-Sandy Springs-Marietta, GA" |
                         MSA_census2005$MSA== "Boston-Cambridge-Quincy, MA-NH")
MSA_results_top$test_result<-NULL
MSA_results_top$Population<-NULL

colnames(MSA_results_top)[2]<-"p<0, 2005-2011"
MSA_census_top$test_result<-NULL
MSA_census_top$Population<-NULL

colnames(MSA_census_top)[2]<-"p<0, 2000-2011"
MSA_census2005_top$test_result<-NULL
MSA_census2005_top$Population<-NULL

colnames(MSA_census2005_top)[2]<-"p<0, 2000-2005"
MSA_results_top<-data.table(MSA_results_top)
MSA_census_top<-data.table(MSA_census_top)
MSA_census2005_top<-data.table(MSA_census2005_top)
setkey(MSA_census2005_top, MSA)
setkey(MSA_census_top, MSA)
setkey(MSA_results_top, MSA)
MSA_combined<-MSA_census2005_top[MSA_census_top]
setkey(MSA_combined, MSA)
MSA_combined<-MSA_combined[MSA_results_top]
print(xtable(MSA_combined), include.rownames=F)