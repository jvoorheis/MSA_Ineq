library(foreign)
library(sandwich)
library(lmtest)
library(reldist)
library(ineq)
library(ggplot2)
library(reshape)
library(GB2)
library(doMC)
library(parallel)
library(xtable)

registerDoMC(cores=detectCores())

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
rifreg_lorenz<-function(x, incvar, p, f, weights, options=""){
  y = incvar
  v_p <- quantile(y, probs=p)
  T_Lp <- Lc(y)$L[as.integer(p*length(y))]
  mu_F <- mean(y)
  #x$rif_y <- apply(data.frame(y), 1, function(x) if (x<v_p){(x-(1-p)*v_p)/mu_F + T_Lp*(1-x/mu_F)} else{p*v_p/mu_F-T_Lp*(1-x/mu_F)})
  x$rif_y <- vapply(y, FUN=function(x) if (x<v_p){(x-(1-p)*v_p)/mu_F + T_Lp*(1-x/mu_F)} else{p*v_p/mu_F-T_Lp*(1-x/mu_F)}, FUN.VALUE=0.0)
  fm <- as.formula(paste("rif_y ~ ", as.character(f)))
  if (options=="coef"){
    return(lm(fm, x, weights=x[as.character(weights)][,1])$coef)
  }
  else if (options=="se"){
    return(coeftest(lm(fm, x, weights=x[as.character(weights)][,1]), vcovHC(lm(fm, x, weights=x[as.character(weights)][,1])))[,2])
  }
  else if (options=="names"){
    return(names(lm(fm, x, weights=x[as.character(weights)][,1])$coef))
  }
  else{
    return(lm(fm, x, weights=x[as.character(weights)][,1]))
  }
}

GB2_impute<-function(data, topcodevar, bottomvar, fit, n){
    try_fun<-function(){
      return(vapply(data[topcodevar][,1], FUN=topcode_sub, fit, FUN.VALUE=0.0) +data[bottomvar][,1])
    }
    virtual_inc<-replicate(n, try_fun())
#     virtual_inc<-matrix(0, length(data[topcodevar]), n)
#     virtual_inc<-apply(virtual_inc, 2, function(x) vapply(data[topcodevar][,1], FUN=topcode_sub, fit, FUN.VALUE=0.0)+data[bottomvar][,1])
    indepvars<-colnames(data)
    virtual_inc<-cbind(data, virtual_inc)
    return(melt(virtual_inc, id.vars=indepvars))
}

Natl_fit<-ml.gb2(CPS.work$cellmean_equivinc)

ptm<-proc.time()
virtual_inc<-GB2_impute(CPS.work, "topcoded_equivinc", "bottom_equivinc", Natl_fit, 100)
proc.time()-ptm

p=0.99
f ="female+black+asian+other+latino+hs+somecollege+college+grad+union_mem+union_cov+married+divorced"
ptm<-proc.time()
union_mem<-ddply(virtual_inc, .variables=c("variable"), function(x) data.frame("coef" = rifreg_lorenz(x, x$value, p, f, weights="wtsupp", options="coef"),
                                                                               "se" = rifreg_lorenz(x, x$value, p, f, weights="wtsupp", options="se"), 
                                                                               "variable_names" = rifreg_lorenz(x, x$value, p, f, weights="wtsupp", options="names")), .parallel=T)
proc.time()-ptm
union_mem <- ddply(union_mem, .variables = c("variable_names"), function(x) c("q_m"=mean(x$coef), 
                                                                              "se_T_p"=sqrt(T_p(x$coef, (x$se)^2))))
union_mem$t_stat<-union_mem$q_m/union_mem$se_T_p
union_mem$P_val<-pt(abs(union_mem$t_stat), length(CPS.work$year)-length(union_mem$t_stat)-1, lower.tail=F)
union_mem


ptm<-proc.time()
try1 <- rifreg_lorenz(CPS.work, CPS.work$cellmean_equivinc, p, f, weights="wtsupp")
proc.time()-ptm
coeftest(try1, vcovHC(try1))