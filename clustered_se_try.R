library(foreign)
library(plm)
library(sandwich)
panel1<-read.dta("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_covariates.dta")
panel1$trend<-as.numeric(panel1$year)
x1<-plm(CPS_gini~college_prop+UR+union_mem+Real_PersIncPC+Population+State_rate_wages+trend:MSA, index=c("MSA", "year"), model="within",effect="twoways", data=panel1)
summary(x1, robust=T)
panel1$MSA<-droplevels(panel1$MSA)
x2<-lm(CPS_gini~college_prop+UR+union_mem+Real_PersIncPC+Population+factor(MSA)+factor(year) + MSA*trend, data=panel1)
robust.se <- function(model, cluster){
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}
robust.se(x2, panel1$MSA)
clx <-
  function(fm, dfcw, cluster){
  library(sandwich)
  library(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
  u <- apply(estfun(fm),2,
                 function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
  coeftest(fm, vcovCL) }
clx(x2, 1, panel1$MSA)