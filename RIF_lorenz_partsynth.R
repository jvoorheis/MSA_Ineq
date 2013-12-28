library(foreign)
library(sandwich)
library(lmtest)
library(reldist)
library(ineq)
library(ggplot2)
library(reshape)
library(GB2)

source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
CPS.work<-read.dta("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_MSA_demo_cleaned.dta")
CPS.work<-subset(CPS.work, CPS.work$statefip=="California")
rif_glorenz<-function(y, p){
  v_p <- quantile(y, probs=p)
  rif_y <- apply(data.frame(y), 1, function(x) (if (x<v_p) {x-(1-p)*v_p} else{p*v_p}))
  return(rif_y)
}
rif_lorenz<-function(y, p){
  v_p <- quantile(y, probs=p)
  T_Lp <- Lc(y)$L[as.integer(p*length(y))]
  mu_F <- mean(y)
  rif_y <- apply(data.frame(y), 1, function(x) if (x<v_p){(x-(1-p)*v_p)/mu_F + T_Lp*(1-x/mu_F)} else{p*v_p/mu_F-T_Lp*(1-x/mu_F)})
  return(rif_y)
}
T_p<-function(q_i, v_j){
  b_m <- sum((q_i-mean(q_i))^2)/(length(q_i)-1)
  v_m <- mean(v_j)
  return(b_m/length(q_i)+v_m)
}
rifreg_lorenz<-function(x, incvar, p, formula=f){
  v_p <- quantile(y, probs=p)
  T_Lp <- Lc(y)$L[as.integer(p*length(y))]
  mu_F <- mean(y)
  x$rif_y <- apply(data.frame(y), 1, function(x) if (x<v_p){(x-(1-p)*v_p)/mu_F + T_Lp*(1-x/mu_F)} else{p*v_p/mu_F-T_Lp*(1-x/mu_F)})
  return(lm(rif_y~f, x))
}

Natl_fit<-ml.gb2(CPS.work$cellmean_equivinc)


virtual_inc<-matrix(0, length(CPS.work$cellmean_equivinc), 20)


for (j in 1:20){
  temp.data.replace<-vapply(CPS.work$topcoded_equivinc, FUN=topcode_sub, Natl_fit, FUN.VALUE=0.0)
  virtual_inc[,j]<-temp.data.replace+CPS.work$bottom_equivinc
}
virtual_inc<-data.frame(virtual_inc)
virtual_inc$State<-CPS.work$statefip
virtual_inc<-cbind(virtual_inc, CPS.work[,c(6:7,11:24)])
indepvars<-colnames(CPS.work)[c(6:7,11:24)]
virtual_inc<-melt(virtual_inc, id.vars=c(indepvars, "State"))

#virtual_inc<-ddply(virtual_inc, .variables=c("variable"), function(x) c("RIF_l"=rif_lorenz(x$value, 0.75)))
#virtual_inc$RIF_l<-rif_lorenz(virtual_inc$value, 0.75)

virtinc<-data.frame()
for (i in unique(virtual_inc$variable)){
  tempvar <- subset(virtual_inc, virtual_inc$variable==i)
  tempvar$RIF_l<-rif_lorenz(tempvar$value, 0.5)
  virtinc<-rbind(virtinc, tempvar)
}


formula_union<-RIF_l~female+black+asian+other+latino+hs+somecollege+college+grad+union_mem+union_cov+married+divorced
union_mem<-ddply(virtinc, .variables=c("variable"), function(x) data.frame("coef"=lm(formula_union, x, weights=wtsupp)$coef,
                                                                  "se"=coeftest(lm(formula_union, x, weights=wtsupp), vcovHC(lm(formula_union, x)))[,2], 
                                                                  "variable_names"=names(lm(formula_union, x, weights=wtsupp)$coef)))
union_mem1<-ddply(virtinc, .variables=c("variable"), function(x) data.frame("coef"=lm(formula_union, x)$coef,
                                                                           "se"=coeftest(lm(formula_union, x), vcovHC(lm(formula_union, x)))[,2], 
                                                                           "variable_names"=names(lm(formula_union, x)$coef)))

gini_ca<-ddply(virtinc, .variables=c("variable"), function(x) c("gini"=gini(x$value)))


union_mem <- ddply(union_mem, .variables = c("variable_names"), function(x) c("q_m"=mean(x$coef), 
                                                                              "se_T_p"=sqrt(T_p(x$coef, (x$se)^2))))
union_mem1 <- ddply(union_mem1, .variables = c("variable_names"), function(x) c("q_m"=mean(x$coef), 
                                                                              "se_T_p"=sqrt(T_p(x$coef, (x$se)^2))))
union_mem$t_stat<-union_mem$q_m/union_mem$se_T_p
union_mem1$t_stat<-union_mem1$q_m/union_mem1$se_T_p

union_mem
union_mem1
