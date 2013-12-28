#load lorenz_data
library(data.table)
library(xtable)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/lorenz_stats.rda")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/PersInc.rda")
library(plyr)
#Clunky nested for-loop way of doing things
mean_pop<-aggregate(Population~MSA, data=PersIncPC, FUN=mean)

MSA.unique<-unique(lorenz_vm$MSA)
year.unique<-data.frame("uni"=c(1986, 1995, 2000, 2005, 2010))
MSA<-c()
year_A<-c()
year_B<-c()
ord<-c()
Delta_k<-c()
for (k in MSA.unique){
for (i in c(1986, 1995, 2000, 2005)){
  temp.unique<-subset(year.unique, year.unique$uni>i)
  for (j in temp.unique$uni){
    year_A1 <- subset(lorenz_vm, lorenz_vm$year==i & lorenz_vm$MSA==k)
    year_B1 <- subset(lorenz_vm, lorenz_vm$year==j & lorenz_vm$MSA==k)
    if (length(year_B1$MSA)==length(year_A1$MSA) & length(year_B1$MSA)!=0 & length(year_B1$MSA)!=0){
    if (year_A1$Tp[1]>0 & year_B1$Tp[1]>0){
    Delta_k1 <- (year_A1$qm - year_B1$qm)/(sqrt(year_A1$Tp + year_B1$Tp))
    }
    else{
      Delta_k1 <- c(0, ((year_A1$qm[2:19] - year_B1$qm[2:19])/(sqrt(year_A1$Tp[2:19] + year_B1$Tp[2:19]))))
    }
    MSA<-append(MSA,c(rep(k, 19)))
    year_A<-append(year_A, rep(i, 19))
    year_B<-append(year_B, rep(j, 19))
    ord<-append(ord,1:19)
    Delta_k<-append(Delta_k, Delta_k1)
    }
  }
}
}
Test_stats<-data.frame(MSA, year_A, year_B, ord, Delta_k)
test_results<-ddply(Test_stats, .variables=c("MSA", "year_A", "year_B"), function(x) c("MSA"=x$MSA, "year_A"=x$year_A, "year_B"=x$year_B, "Test_result"=Lorenz_test_result(x$Delta_k), "Lorenz_dom"=A_dom_B(x$Delta_k)))
Test_stats<-data.table(Test_stats)
mean_pop<-data.table(mean_pop)
setkey(mean_pop, MSA)
setkey(Test_stats, MSA)
Test_stats<-Test_stats[mean_pop, allow.cartesian=T]
test_results_1<-aggregate(Delta_k~MSA+year_A+year_B+Population, data=Test_stats, FUN=Lorenz_test_result)
test_results_2<-aggregate(Delta_k~MSA+year_A+year_B+Population, data=Test_stats, FUN=A_dom_B)
test_results_3<-aggregate(Delta_k~MSA+year_A+year_B+Population, data=Test_stats, FUN=B_dom_A)
test_results_4<-aggregate(Delta_k~MSA+year_A+year_B+Population, data=Test_stats, FUN=Lorenz_cross)

all_MSAs<-data.frame()
 result_type<-c("A Dominates B", "A Dominates B", "A Dominates B", "A Dominates B", "B Dominates A", "B Dominates A","B Dominates A", "Lorenz Curves Cross", "Lorenz Curves Cross", "Lorenz Curves Cross")
year_A<-c(2000,2000,1995,1995,2000,2000,1995,2000,2000,1995)
year_B<-c(2010,2005,2010,2005,2010,2005,2010,2010,2005,2010)
test_means<-c(mean(subset(test_results_2, test_results_2$year_A==2000 & test_results_2$year_B==2010)$Delta_k),
mean(subset(test_results_2, test_results_2$year_A==2000 & test_results_2$year_B==2005)$Delta_k),
mean(subset(test_results_2, test_results_2$year_A==1995 & test_results_2$year_B==2010)$Delta_k),
mean(subset(test_results_2, test_results_2$year_A==1995 & test_results_2$year_B==2005)$Delta_k),
mean(subset(test_results_3, test_results_3$year_A==2000 & test_results_3$year_B==2010)$Delta_k),
mean(subset(test_results_3, test_results_3$year_A==2000 & test_results_3$year_B==2005)$Delta_k),
mean(subset(test_results_3, test_results_3$year_A==1995 & test_results_3$year_B==2010)$Delta_k),
mean(subset(test_results_4, test_results_4$year_A==2000 & test_results_4$year_B==2010)$Delta_k),
mean(subset(test_results_4, test_results_4$year_A==2000 & test_results_4$year_B==2005)$Delta_k),
mean(subset(test_results_4, test_results_4$year_A==1995 & test_results_4$year_B==2010)$Delta_k))
all_MSAs<-data.frame("Test Result"=result_type, year_A, year_B, "Proportion of MSAs"=test_means)
print(xtable(all_MSAs, display=c("d","s", "d", "d", "f")), include.rownames=F)

#For only large MSAs
test_results_2<-subset(test_results_2, test_results_2$Population>=1000000)
test_results_3<-subset(test_results_3, test_results_3$Population>=1000000)
test_results_4<-subset(test_results_4, test_results_4$Population>=1000000)
all_MSAs<-data.frame()
result_type<-c("A Dominates B", "A Dominates B", "A Dominates B", "A Dominates B", "B Dominates A", "B Dominates A","B Dominates A", "Lorenz Curves Cross", "Lorenz Curves Cross", "Lorenz Curves Cross")
year_A<-c(2000,2000,1995,1995,2000,2000,1995,2000,2000,1995)
year_B<-c(2010,2005,2010,2005,2010,2005,2010,2010,2005,2010)
test_means<-c(mean(subset(test_results_2, test_results_2$year_A==2000 & test_results_2$year_B==2010)$Delta_k),
              mean(subset(test_results_2, test_results_2$year_A==2000 & test_results_2$year_B==2005)$Delta_k),
              mean(subset(test_results_2, test_results_2$year_A==1995 & test_results_2$year_B==2010)$Delta_k),
              mean(subset(test_results_2, test_results_2$year_A==1995 & test_results_2$year_B==2005)$Delta_k),
              mean(subset(test_results_3, test_results_3$year_A==2000 & test_results_3$year_B==2010)$Delta_k),
              mean(subset(test_results_3, test_results_3$year_A==2000 & test_results_3$year_B==2005)$Delta_k),
              mean(subset(test_results_3, test_results_3$year_A==1995 & test_results_3$year_B==2010)$Delta_k),
              mean(subset(test_results_4, test_results_4$year_A==2000 & test_results_4$year_B==2010)$Delta_k),
              mean(subset(test_results_4, test_results_4$year_A==2000 & test_results_4$year_B==2005)$Delta_k),
              mean(subset(test_results_4, test_results_4$year_A==1995 & test_results_4$year_B==2010)$Delta_k))
all_MSAs<-data.frame("Test Result"=result_type, year_A, year_B, "Proportion of MSAs"=test_means)
print(xtable(all_MSAs, display=c("d","s", "d", "d", "f")), include.rownames=F)



unique(subset(test_results_2, test_results_2$Delta_k==1 & test_results_2$year_A==1995 & year_B==2012)$MSA)

test_results_top<-subset(test_results_1, test_results_1$MSA=="New York-Northern New Jersey-Long Island" | 
                           test_results_1$MSA=="Los Angeles-Long Beach-Santa Ana, CA" | 
                           test_results_1$MSA=="Chicago-Naperville-Joliet, IL-IN-WI"  |
                           test_results_1$MSA=="Dallas-Fort Worth-Arlington, TX"  |
                           test_results_1$MSA=="Houston-Baytown-Sugar Land, TX" |
                           test_results_1$MSA=="Philadelphia-Camden-Wilmington, PA/NJ/D" |
                           test_results_1$MSA=="Washington, DC/MD/VA" | 
                           test_results_1$MSA=="Miami-Fort Lauderdale-Miami Beach, FL")
x1<-test_results_top[,c(1:3,5)]
x1<-subset(x1, x1$year_A!=1986 & x1$year_B!=2000)
print(xtable(x1, display=c("d","s", "d", "d", "s")),include.rownames=FALSE)
test_results_top<-subset(test_results_2, test_results_2$MSA=="New York-Northern New Jersey-Long Island" | 
                           test_results_2$MSA=="Los Angeles-Long Beach-Santa Ana, CA" | 
                           test_results_2$MSA=="Chicago-Naperville-Joliet, IL-IN-WI"  |
                           test_results_2$MSA=="Dallas-Fort Worth-Arlington, TX"  |
                           test_results_2$MSA=="Houston-Baytown-Sugar Land, TX" |
                           test_results_2$MSA=="Philadelphia-Camden-Wilmington, PA/NJ/D" |
                           test_results_2$MSA=="Washington, DC/MD/VA" | 
                           test_results_2$MSA=="Miami-Fort Lauderdale-Miami Beach, FL"   |
                           test_results_2$MSA=="Atlanta-Sandy Springs-Marietta, GA" |
                           test_results_2$MSA== "Boston-Cambridge-Quincy, MA-NH")
