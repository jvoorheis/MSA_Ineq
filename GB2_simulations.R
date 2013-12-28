#Generate three populations from a GB2 Distribution - two calibrated to CA incomes in 2011
#and one intentionally more unequal
library(GB2)
library(data.table)
library(reldist)
library(reshape)
library(ineq)
library(reldist)

n<-2




x <- rgb2(10000, 4.36, 58000, .2204, .525) + rnorm(10000, 0, 3000)
States<-c()
for (i in 1:10){
  States<-append(States, rep(i, rpois(1,5)))
}

Is_positive<-function(x){
  if (x<=0){
    return(1)
  }
  else{
    return(x)
  }
}
Is_positive2<-function(x){
  if (x<=0){
    return(0)
  }
  else{
    return(1)
  }
}
topcode<-function(inc, threshold, cellmean, type){
  if (type=="t"){
    if (inc>=threshold){
      return(threshold)
    }
    else{
      return(inc)
    }
  }
  else if (type=="c"){
    if (inc>=threshold){
      return(cellmean)
    }
    else{
      return(inc)
    }
  }
}
topcode_sub<-function(inc, fit2){
  if (inc>0){
    bottom<-pgb2(inc, fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4])
    return(qgb2(runif(1,min=bottom, max=1), fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4]))
  }
  else{
    return(inc)
  }  
}

x <- apply(data.frame(x), 1, Is_positive)


States_x<-sample(States, length(x), replace=T)
dataset_x<-data.frame("inc"=x,"State"=States_x)
state_topcodes_x<-ddply(dataset_x, .variables=c("State"), function(x) c("threshold" = quantile(x$inc, probs=0.98), 
                                                                         "cellmean"=mean(subset(x, x$inc>=quantile(x$inc, probs=0.98)& x$inc<1000000)$inc)))
dataset_x<-data.table(dataset_x)
state_topcodes_x<-data.table(state_topcodes_x)
setkey(dataset_x, State)
setkey(state_topcodes_x, State)
dataset_x<-dataset_x[state_topcodes_x, allow.cartesian=T]


dataset_x$id<-1:length(x)
dataset_x <- ddply(dataset_x, .variables = c("State", "inc", "id"), function(x) c("topcode_inc"=topcode(x$inc, x$"threshold.98%", x$cellmean, type="t"), 
                                                                                  "cellmean_inc"=topcode(x$inc, x$"threshold.98%", x$cellmean, type="c"),
                                                                                  "threshold"=x$"threshold.98%"))
dataset_x$topcoded<-apply(data.frame(dataset_x$threshold-dataset_x$inc), 1, Is_positive2)
dataset_x$c_topcodedinc<-(1-dataset_x$topcoded)*dataset_x$cellmean_inc
dataset_x$t_topcodedinc<-(1-dataset_x$topcoded)*dataset_x$topcode_inc
dataset_x$c_bottom_inc<-dataset_x$topcoded*dataset_x$cellmean_inc
dataset_x$t_bottom_inc<-dataset_x$topcoded*dataset_x$topcode_inc

bottom_cutoff_c<-quantile(dataset_x$cellmean_inc, probs=0.3)
bottom_cutoff_t<-quantile(dataset_x$topcode_inc, probs=0.3)
temp_c<-subset(dataset_x, dataset_x$inc>bottom_cutoff_c)
temp_t<-subset(dataset_x, dataset_x$inc>bottom_cutoff_t)
Natl_fit_c<-ml.gb2(temp_c$cellmean_inc)
Natl_fit_t<-ml.gb2(temp_t$topcode_inc)

virtual_inc_c<-matrix(0, length(dataset_x$c_topcodedinc), 100)
virtual_inc_t<-matrix(0, length(dataset_x$t_topcodedinc), 100)


for (j in 1:100){
  temp.data.replace_c<-vapply(dataset_x$t_topcodedinc, FUN=topcode_sub, Natl_fit_c, FUN.VALUE=0.0)
  temp.data.replace_t<-vapply(dataset_x$t_topcodedinc, FUN=topcode_sub, Natl_fit_t, FUN.VALUE=0.0)
  virtual_inc_c[,j]<-temp.data.replace_c+dataset_x$c_bottom_inc
  virtual_inc_t[,j]<-temp.data.replace_t+dataset_x$t_bottom_inc
}
virtual_inc_c<-data.frame(virtual_inc_c)
virtual_inc_c$State<-dataset_x$State
virtual_inc_c<-melt(virtual_inc_c, id.vars=c("State"))
virtual_inc_t<-data.frame(virtual_inc_t)
virtual_inc_t$State<-dataset_x$State
virtual_inc_t<-melt(virtual_inc_t, id.vars=c("State"))

States_gini_c<-ddply(virtual_inc_c, .variables=c("State", "variable"), function(x) c("c_gini"=gini(x$value), "c_theil"=Theil(x$value)))
States_gini_t<-ddply(virtual_inc_t, .variables=c("State", "variable"), function(x) c("t_gini"=gini(x$value), "t_theil"=Theil(x$value)))
States_gini_truf<-ddply(dataset_x, .variables=c("State"), function(x) c("true_gini"=gini(x$inc), "true_theil"=Theil(x$inc)))

States_gini_c<-ddply(States_gini_c, .variables=c("State"), function(x) c("c_gini"=mean(x$c_gini), "c_theil"=mean(x$c_theil)))
States_gini_t<-ddply(States_gini_t, .variables=c("State"), function(x) c("t_gini"=mean(x$t_gini), "t_theil"=mean(x$t_theil)))
States_gini_cellmean<-ddply(dataset_x, .variables=c("State"), function(x) c("cellmean_gini"=gini(x$cellmean_inc), "cellmean_theil"=Theil(x$cellmean_inc)))
mean((States_gini_c$c_theil-States_gini_truf$true_theil)^2)
mean((States_gini_cellmean$cellmean_theil-States_gini_truf$true_theil)^2)
mean((States_gini_t$t_gini-States_gini_truf$true_gini)^2)
mean((States_gini_c$c_gini-States_gini_truf$true_gini)^2)
mean((States_gini_cellmean$cellmean_gini-States_gini_truf$true_gini)^2)

(States_gini_c$c_theil-States_gini_truf$true_theil)
(States_gini_cellmean$cellmean_theil-States_gini_truf$true_theil)
(States_gini_c$c_gini-States_gini_truf$true_gini)
(States_gini_t$t_gini-States_gini_truf$true_gini)
(States_gini_cellmean$cellmean_gini-States_gini_truf$true_gini)
p = 0.5
avg1 <- p*States_gini_cellmean$cellmean_gini  + (1-p)*States_gini_t$t_gini
avg2 <- p*States_gini_cellmean$cellmean_gini  + (1-p)*States_gini_c$c_gini
mean((avg1-States_gini_truf$true_gini)^2)
mean((avg2-States_gini_truf$true_gini)^2)