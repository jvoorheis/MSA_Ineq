library(reldist)
library(ineq)
library(AER)
library(foreign)

rif_gini<-function(x, incvar, f, options="se"){
  indx<-which(colnames(x)==incvar)
  x<-x[order(x[,indx]),]
  incvar<-x[,indx]
  G_F <- gini(incvar)
  mu_F <- mean(incvar)
  d_y<-c(0,unique(incvar)[2:length(unique(incvar))]-unique(incvar)[1:(length(unique(incvar))-1)])
  u_y<-unique(incvar)
  u_Fx<-ecdf(incvar)(u_y)
  integrand<-cumsum(u_Fx*d_y)
  x.df<-data.frame(integrand, u_y)
  int_y<-apply(data.frame(incvar),1, function(x) x.df[which(x.df$u_y==x),]$integrand)
  x$rif_y<- (-incvar/mu_F)*G_F + 1 -incvar/mu_F +(2/mu_F)*int_y
  fm <- as.formula(paste("rif_y ~ ", as.character(f)))
  if (options=="coef"){
    return(lm(fm, x)$coef)
  }
  else if (options=="se"){
    return(coeftest(lm(fm, x), vcovHC(lm(fm, x))))
  }
  else if (options=="nonrobust")
    return(lm(fm, x))
}

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("Data/CPS_broadinc.rda")
CPS.work.hh$age<-as.numeric(as.character(CPS.work.hh$age))
#CPS.work.hh<-subset(CPS.work.hh, is.na(CPS.work.hh$age)==F)
CA2011<-CPS.work.hh[which(CPS.work.hh$year==2011 & CPS.work.hh$statefip=="California"),]
CA2000<-CPS.work.hh[which(CPS.work.hh$year==2000 & CPS.work.hh$statefip=="California"),]

write.dta(CA2011, file="Data/CA2011_rif.dta")
f ="inschool+age+female+black+asian+otherrace+latino+highschool+somecollege+bachelors+postgrad+union_dens+married+divorced+widowed+occ1+occ2+ind1+ind2"

f2<-"union_dens"
try_union<-rif_gini(CA2011, "cellmean_equivinc", f2)
try1<-rif_gini(CA2011, "cellmean_equivinc", f)

indepvars<-c("inschool","age","female","black","latino","highschool","somecollege","bachelors",
             "postgrad","union_dens","married","divorced","widowed", "fulltime", "ind0","ind1",
             "ind2","ind3","ind4","ind5","ind6","ind7","ind8","occ0", "occ1", "occ2", "occ3", 
             "occ4", "occ5", "occ6", "occ7", "occ8", "hh_margrate", "self_emp", "pub_sector")
f1<-"inschool"
for (i in indepvars[2:length(indepvars)]){
  f1<-paste(f1, i, sep="+")
}
try2<-rif_gini(CA2011,"cellmean_equivinc", f1)


CA2011_rif<-rif_gini(CA2011, "equivinc_pretax_broad", f1, options="coef")
CA2000_rif<-rif_gini(CA2000, "equivinc_pretax_broad", f1, options="coef")

X_bar00<-c(1)
X_bar11<-c(1)
for (i in indepvars){
  X_bar00<-append(X_bar00, mean(CA2000[i][,1], na.rm=T))
  X_bar11<-append(X_bar11, mean(CA2011[i][,1], na.rm=T))
}
gini(CA2011$cellmean_equivinc)-gini(CA2000$cellmean_equivinc)
crossprod(matrix(X_bar11), matrix((CA2011_rif - CA2000_rif)))
crossprod(matrix(X_bar11 - X_bar00),matrix(CA2000_rif))
t(matrix(X_bar11)) %*% matrix((CA2011_rif - CA2000_rif)) + t(matrix(X_bar11 - X_bar00)) %*% matrix(CA2000_rif)
(X_bar11 - X_bar00)*CA2000_rif


incvar<-CA2011$cellmean_equivinc
incvar<-sort(incvar)
G_F <- gini(incvar)
mu_F <- mean(incvar)
F_x<-ecdf(incvar)(incvar)
ptm<-proc.time()
d_y<-c(0,unique(incvar)[2:length(unique(incvar))]-unique(incvar)[1:(length(unique(incvar))-1)])
integrand<-cumsum(unique(F_x)*d_y)
u_y<-unique(incvar)
x.df<-data.frame(integrand, u_y)
int_y1<-apply(data.frame(incvar),1, function(x) x.df[which(x.df$u_y==x),]$integrand)
proc.time()-ptm
rif_y1<- (-incvar/mu_F)*G_F + 1 -incvar/mu_F +(2/mu_F)*int_y1
summary(lm(rif_y1~CA2011$union_dens))

ptm<-proc.time()
sp_int<-splinefun(unique(incvar), unique(F_x))
attempt<-integrate(sp_int, lower=0, upper = 1000)
int_y<-apply(data.frame(incvar),1, function(x) integrate(sp_int, lower=0, upper=x, subdivisions=200, stop.on.error=F)$value)
proc.time()-ptm
rif_y<- (-incvar/mu_F)*G_F + 1 -incvar/mu_F +(2/mu_F)*int_y
summary(lm(rif_y~CA2011$union_dens))

Rprof()
prof_try<-rif_gini(CA2011, "cellmean_equivinc", f1, options="coef")
