library(MADAM)
library(plyr)

test_results <- data.frame()
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Lorenz_dom_bootstrap/I_phi")
for (i in 5:9){
  filename = paste("State_ACS_bootstrap_LD_pval0", as.character(i), ".rda", sep="")
  load(filename)
  year1 = 2000+i
  p_vals1995$year1<-rep(year1, 51)
  p_vals1995$year2<-rep(2011, 51)
  test_results<-rbind(test_results, p_vals1995)
}
filename = paste("State_ACS_bootstrap_LD_pval", as.character(10), ".rda", sep="")
load(filename)
year1 = 2000+10
p_vals1995$year1<-rep(year1, 51)
p_vals1995$year2<-rep(2011, 51)
test_results<-rbind(test_results, p_vals1995)
for (i in 5:9){
  filename = paste("State_ACS_bootstrap_LD_pval0", as.character(i), "10.rda", sep="")
  load(filename)
  year1 = 2000+i
  p_vals1995$year1<-rep(year1, 51)
  p_vals1995$year2<-rep(2010, 51)
  test_results<-rbind(test_results, p_vals1995)
}
for (i in 5:8){
  filename = paste("State_ACS_bootstrap_LD_pval0", as.character(i), "09.rda", sep="")
  load(filename)
  year1 = 2000+i
  p_vals1995$year1<-rep(year1, 51)
  p_vals1995$year2<-rep(2009, 51)
  test_results<-rbind(test_results, p_vals1995)
}
for (i in 5:7){
  filename = paste("State_ACS_bootstrap_LD_pval0", as.character(i), "08.rda", sep="")
  load(filename)
  year1 = 2000+i
  p_vals1995$year1<-rep(year1, 51)
  p_vals1995$year2<-rep(2008, 51)
  test_results<-rbind(test_results, p_vals1995)
}
for (i in 5:6){
  filename = paste("State_ACS_bootstrap_LD_pval0", as.character(i), "07.rda", sep="")
  load(filename)
  year1 = 2000+i
  p_vals1995$year1<-rep(year1, 51)
  p_vals1995$year2<-rep(2007, 51)
  test_results<-rbind(test_results, p_vals1995)
}
for (i in 5:5){
  filename = paste("State_ACS_bootstrap_LD_pval0", as.character(i), "06.rda", sep="")
  load(filename)
  year1 = 2000+i
  p_vals1995$year1<-rep(year1, 51)
  p_vals1995$year2<-rep(2006, 51)
  test_results<-rbind(test_results, p_vals1995)
}

sum_results<-data.frame()
for (i in unique(test_results$year1)){
  for (j in unique(test_results$year2)){
    if (i<j){
      frac05<-length(subset(test_results, test_results$year1==i & test_results$year2==j&P_val00<=0.05)$State)/50
      frac10<-length(subset(test_results, test_results$year1==i & test_results$year2==j&P_val00<=0.1)$State)/50
      res1 <- data.frame("year1" = i, "year2" = j, "frac05" = frac05, "frac10"=frac10)
      sum_results<-rbind(sum_results, res1)
    }
  }
}

fisher_results<-ddply(test_results, .variables=c("year1", "year2"), function(x) c("fisher_S"=fisher.method(t(x$P_val00))$S, "fisher_p"=fisher.method(t(x$P_val00))$p.value))