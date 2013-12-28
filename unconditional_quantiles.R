#Unconditional Quantile Regressions
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("Data/CPS_broadinc.rda")
CPS.work.hh$age<-as.numeric(as.character(CPS.work.hh$age))
CPS.work.hh<-subset(CPS.work, is.na(CPS.work.hh$age)==F)
#CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$age<65)
CPS.work.hh$union_dens<-CPS.work.hh$union_mem+ CPS.work.hh$union_cov
CA2011<-subset(CPS.work.hh, CPS.work.hh$year==2011 & CPS.work.hh$statefip=="California")
CA2000<-subset(CPS.work.hh, CPS.work.hh$year==2005 & CPS.work.hh$statefip=="California")
CA2000$cellmean_equivinc<-CA2000$cellmean_equivinc*(227.137/198.1)

rif_quantile<-function(x, inc, p, f, options="se"){
  n<-length(inc)
  v_p<-quantile(inc, probs=p)
  bw=bw.SJ(inc)
  f_y<-density(inc, bw=bw, n=n)
  f_vp<-f_y$y[round((ecdf(f_y$x)(v_p))*n)]
  x$rif_y<-apply(data.frame(inc),1,function(x) if(x<v_p){v_p-(1-p)/f_vp} else if(x>=v_p){v_p+p/f_vp})
  fm <- as.formula(paste("rif_y ~ ", as.character(f)))
  if (options=="coef"){
    return(lm(fm, x)$coef)
  }
  else if (options=="se"){
    return(coeftest(lm(fm, x), vcovHC(lm(fm, x))))
  }
}

f ="inschool+age+female+black+asian+otherrace+latino+highschool+somecollege+bachelors+postgrad+union_dens+married+divorced+widowed"
CA2011_rif<-rif_quantile(CA2011, CA2011$cellmean_equivinc, 0.2, f, options="coef")
CA2000_rif<-rif_quantile(CA2000, CA2000$cellmean_equivinc, 0.2, f,  options="coef")
indepvars =c("inschool","age","female","black","asian","otherrace","latino","highschool","somecollege","bachelors","postgrad","union_dens","married","divorced","widowed")

X_bar00<-c(1)
X_bar11<-c(1)
for (i in indepvars){
  X_bar00<-append(X_bar00, mean(CA2000[i][,1], na.rm=T))
  X_bar11<-append(X_bar11, mean(CA2011[i][,1], na.rm=T))
}
crossprod(matrix(X_bar11), matrix((CA2011_rif - CA2000_rif)))
crossprod(matrix(X_bar11 - X_bar00),matrix(CA2000_rif))
t(matrix(X_bar11)) %*% matrix((CA2011_rif - CA2000_rif)) + t(matrix(X_bar11 - X_bar00)) %*% matrix(CA2000_rif)
(X_bar11 - X_bar00)*CA2000_rif

ords<-c(0.01, seq(0.05,0.95,0.05), 0.99)
struct_eff<-c()
comp_eff<-c()
for (i in ords){
  gamma_2<-rif_quantile(CA2011, CA2011$equivinc_posttax_broad, i, f, options="coef")
  gamma_1<-rif_quantile(CA2000, CA2000$equivinc_posttax_broad, i, f, options="coef")
  struct_eff<-append(struct_eff, crossprod(matrix(X_bar11), matrix(gamma_2-gamma_1)))
  comp_eff<-append(comp_eff, crossprod(matrix(X_bar11 - X_bar00),matrix(gamma_1)))

}
total_eff<-struct_eff+comp_eff
quantile_effects<-data.frame(ords, struct_eff, comp_eff, total_eff)
library(ggplot2)
ggplot(quantile_effects, aes(ords)) + geom_line(aes(y=struct_eff, colour="struct_eff")) +
  geom_line(aes(y=comp_eff, colour="comp_eff")) + geom_line(aes(y=total_eff, colour="total_eff"))