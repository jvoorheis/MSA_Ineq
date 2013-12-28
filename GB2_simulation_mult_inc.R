library(foreign)
library(GB2)
library(plyr)
library(reshape)
library(ineq)
library(reldist)
ACS.try<-read.dta("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_wage2011.dta")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
# ACS.try$incwage = ACS.try$incwage+1
# ACS.try$incbus00 = ACS.try$incbus00+1
# ACS.try$incinvst = ACS.try$incinvst+1

incwage2011<-subset(ACS.try, is.na(ACS.try$incwage)==F & ACS.try$incwage!=0)$incwage
incbus2011<-subset(ACS.try, is.na(ACS.try$incbus00)==F & ACS.try$incbus00!=0)$incbus00
incinvst2011<-subset(ACS.try, is.na(ACS.try$incinvst)==F & ACS.try$incinvst!=0)$incinvst

Is_positive<-function(x){
  if (is.na(x)==T){
    return(NA)
  }
  else if (x<=0){
    return(1)
  }
  else{
    return(x)
  }
}
hard_censor<-function(x){
  if (is.na(x)==T){
    return(NA)
  }
  else if (x>=1000000){
    return(1000000)
  }
  else{
    return(x)
  }
}

wage_fit<-ml.gb2(incwage2011)
bus_fit<-ml.gb2(incbus2011)
invst_fit<-ml.gb2(incinvst2011)

States<-c()
for (i in 1:10){
  States<-append(States, rep(i, rpois(1,5)))
}
State<-sample(States, 10000, replace=T)


c_err<-numeric(10)
t_err<-numeric(10)
cm_err<-numeric(10)
for (k in 1:2){ 
incwage1_true <- rgb2(10000, wage_fit$opt1$par[1], wage_fit$opt1$par[2], wage_fit$opt1$par[3], wage_fit$opt1$par[4]) + rnorm(10000, 0, 10000)
incwage1_true <-apply(data.frame(incwage1_true),1,Is_positive)
incwage_q <- apply(data.frame(incwage1_true), 1, pgb2, wage_fit$opt1$par[1], wage_fit$opt1$par[2], wage_fit$opt1$par[3], wage_fit$opt1$par[4])
incbus1_true<-numeric(10000)
incinvst1_true<-numeric(10000)
for (i in 1:10000){
  factor1 = 20
  factor2 = 20
  while (factor1>=1 | factor2>=1){
    factor1<-runif(1, 0.2, 1.7)*incwage_q[i]
    factor2<-runif(1, 0.5, 1.5)*incwage_q[i]
  }
  incbus1_true[i]<-qgb2(factor1, bus_fit$opt1$par[1], bus_fit$opt1$par[2], bus_fit$opt1$par[3], bus_fit$opt1$par[4]) + rnorm(1,  0, 1500)
  incinvst1_true[i]<-qgb2(factor2, invst_fit$opt1$par[1], invst_fit$opt1$par[2], invst_fit$opt1$par[3], invst_fit$opt1$par[4]) + rnorm(1,  0, 1800)
}
incwage1 <- apply(data.frame(incwage1_true), 1, hard_censor)
incbus1 <- apply(data.frame(incbus1_true), 1, hard_censor)
incinvst1 <- apply(data.frame(incinvst1_true), 1, hard_censor)




topcoded <-rep(0, 10000)
topcoded_w <-rep(0, 10000)
topcoded_b <-rep(0, 10000)
topcoded_i <-rep(0, 10000)


incwage_t <- incwage1
incbus_t <- incbus1
incinvst_t <- incinvst1
incwage_c <- incwage1
incbus_c <- incbus1
incinvst_c <- incinvst1
State.df<-data.frame(State, incwage1, incbus1, incinvst1)
State.df<-ddply(State.df, .variables = c("State"), function(x) c("incwage"=mean(subset(x, x$incwage1>=quantile(x$incwage1, probs=0.975))$incwage1),
                                                                 "incbus"=mean(subset(x, x$incbus1>=quantile(x$incbus1, probs=0.975))$incbus1),
                                                                 "incinvst"=mean(subset(x, x$incinvst1>=quantile(x$incinvst1, probs=0.975))$incinvst1)))

for (i in 1:10000){
  if (incwage1[i]>=quantile(incwage1, probs=0.975)){
    s1 <- State[i]
    topcoded[i]<-1
    topcoded_w[i]<-1
    incwage_t[i] <- quantile(incwage1, probs=0.975)
    incwage_c[i] <- subset(State.df, State.df$State==s1)$incwage[1]
  }
  if (incbus1[i]>=quantile(incbus1, probs=0.975)){
    s1 <- State[i]
    topcoded[i]<-1
    topcoded_b[i]<-1
    incbus_t[i] <- quantile(incbus1, probs=0.975)
    incbus_c[i] <- subset(State.df, State.df$State==s1)$incbus[1]
  } 
  if (incinvst1[i]>=quantile(incinvst1, probs=0.975)){
    topcoded[i]<-1
    topcoded_i[i]<-1
    s1 <- State[i]
    incinvst_t[i] <- quantile(incinvst1, probs=0.975)
    incinvst_c[i] <- subset(State.df, State.df$State==s1)$incinvst[1]
  }
}
totinc <- incwage1_true + incbus1_true + incinvst1_true
totinc_c<-incwage_c+incbus_c+incinvst_c
totinc_t<-incwage_t + incbus_t + incinvst_t
totinc<-apply(data.frame(totinc), 1, Is_positive)
totinc_t<-apply(data.frame(totinc_t), 1, Is_positive)
totinc_c<-apply(data.frame(totinc_c), 1, Is_positive)
totinc<-apply(data.frame(totinc), 1, Is_positive)
dataset_x <- data.frame("State"=State, "inc"=totinc, totinc_t, totinc_c)


c_topcodedinc<-(topcoded)*totinc_c
t_topcodedinc<-(topcoded)*totinc_t
c_bottom_inc<-(1-topcoded)*totinc_c
t_bottom_inc<-(1-topcoded)*totinc_t

bottom_cutoff_c<-quantile(totinc_c, probs=0.3)
bottom_cutoff_t<-quantile(totinc_t, probs=0.3)
temp_c<-subset(totinc_c, totinc_c>bottom_cutoff_c)
temp_t<-subset(totinc_t, totinc_t>bottom_cutoff_t)
Natl_fit_c<-ml.gb2(temp_c)
Natl_fit_t<-ml.gb2(temp_t)

virtual_inc_c<-matrix(0, length(c_topcodedinc), 100)
virtual_inc_t<-matrix(0, length(t_topcodedinc), 100)


for (j in 1:100){
  temp.data.replace_c<-vapply(t_topcodedinc, FUN=topcode_sub, Natl_fit_c, FUN.VALUE=0.0)
  temp.data.replace_t<-vapply(t_topcodedinc, FUN=topcode_sub, Natl_fit_t, FUN.VALUE=0.0)
  virtual_inc_c[,j]<-temp.data.replace_c+c_bottom_inc
  virtual_inc_t[,j]<-temp.data.replace_t+t_bottom_inc
}
virtual_inc_c<-data.frame(virtual_inc_c)
virtual_inc_c$State<-State
virtual_inc_c<-melt(virtual_inc_c, id.vars=c("State"))
virtual_inc_t<-data.frame(virtual_inc_t)
virtual_inc_t$State<-State
virtual_inc_t<-melt(virtual_inc_t, id.vars=c("State"))

States_gini_c<-ddply(virtual_inc_c, .variables=c("State", "variable"), function(x) c("c_gini"=gini(x$value), "c_theil"=Theil(x$value)))
States_gini_t<-ddply(virtual_inc_t, .variables=c("State", "variable"), function(x) c("t_gini"=gini(x$value), "t_theil"=Theil(x$value)))
States_gini_truf<-ddply(dataset_x, .variables=c("State"), function(x) c("true_gini"=gini(x$inc), "true_theil"=Theil(x$inc)))

States_gini_c<-ddply(States_gini_c, .variables=c("State"), function(x) c("c_gini"=mean(x$c_gini), "c_theil"=mean(x$c_theil)))
States_gini_t<-ddply(States_gini_t, .variables=c("State"), function(x) c("t_gini"=mean(x$t_gini), "t_theil"=mean(x$t_theil)))
States_gini_cellmean<-ddply(dataset_x, .variables=c("State"), function(x) c("cellmean_gini"=gini(x$totinc_c), "cellmean_theil"=Theil(x$totinc_c)))
# mean((States_gini_c$c_theil-States_gini_truf$true_theil)^2)
# mean((States_gini_cellmean$cellmean_theil-States_gini_truf$true_theil)^2)
t_err[k] <- mean((States_gini_t$t_gini-States_gini_truf$true_gini)^2)
c_err[k] <- mean((States_gini_c$c_gini-States_gini_truf$true_gini)^2)
cm_err[k] <- mean((States_gini_cellmean$cellmean_gini-States_gini_truf$true_gini)^2)

comb_error<-(States_gini_c$c_gini+States_gini_cellmean$cellmean_gini)/2

# (States_gini_c$c_theil-States_gini_truf$true_theil)
# (States_gini_cellmean$cellmean_theil-States_gini_truf$true_theil)
# (States_gini_c$c_gini-States_gini_truf$true_gini)
# (States_gini_t$t_gini-States_gini_truf$true_gini)
# (States_gini_cellmean$cellmean_gini-States_gini_truf$true_gini)
# p = 0.5
# avg1 <- p*States_gini_cellmean$cellmean_gini  + (1-p)*States_gini_t$t_gini
# avg2 <- p*States_gini_t$t_gini  + (1-p)*States_gini_c$c_gini
# mean((avg1-States_gini_truf$true_gini)^2)
# mean((avg2-States_gini_truf$true_gini)^2)
}

Natl_gini_c<-ddply(virtual_inc_c, .variables=c("variable"), function(x) c("c_gini"=gini(x$value), "c_theil"=Theil(x$value)))
Natl_gini_t<-ddply(virtual_inc_t, .variables=c("variable"), function(x) c("t_gini"=gini(x$value), "t_theil"=Theil(x$value)))
Natl_gini_cellmean<-gini(totinc_c)
gb2_gini_c<-gini.gb2(Natl_fit_c$opt1$par[1], Natl_fit_c$opt1$par[3], Natl_fit_c$opt1$par[4])
gb2_gini_t<-gini.gb2(Natl_fit_t$opt1$par[1], Natl_fit_t$opt1$par[3], Natl_fit_t$opt1$par[4])


