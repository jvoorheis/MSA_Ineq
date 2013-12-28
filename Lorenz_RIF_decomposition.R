library(foreign)
library(sandwich)
library(lmtest)
library(reldist)
library(ineq)
library(ggplot2)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
#CPS.work<-read.dta("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_MSA_demo_cleaned.dta")
load("Data/CPS_broadinc.rda")
indepvars<-c("inschool","age","female","black","latino","highschool","somecollege","bachelors",
             "postgrad","union_dens","married","divorced","widowed", "fulltime", "ind0","ind1",
             "ind2","ind3","ind4","ind5","ind6","ind7","ind8","occ0", "occ1", "occ2", "occ3", 
             "occ4", "occ5", "occ6", "occ7", "occ8", "hh_margrate", "self_emp", "pub_sector", "hhsize")
depvars<-c("statefip", "year", "equivinc_posttax_broad", "equivinc_pretax_broad", "equivinc_pretax_equity", "equivinc_posttax_equity","cellmean_aftertax","cellmean_eitc","cellmean_equivinc","cellmean_equivinc_pretrans", "CPI")
indices<-c()
for (i in indepvars){
  indices<-append(indices, which(colnames(CPS.work.hh)==i))
}
for (j in depvars){
  indices<-append(indices, which(colnames(CPS.work.hh)==j))
}
CPS.work.hh<-as.data.frame(CPS.work.hh)
save(CPS.work.hh, file="Data/CPS_broadinc.rda")

CPS.work.hh<-CPS.work.hh[,indices]
CPS.work.hh$age<-as.numeric(as.character(CPS.work.hh$age))
CPS.work.hh<-subset(CPS.work.hh, is.na(CPS.work.hh$age)==F)
#CPS.work.hh<-subset(CPS.work.hh, CPS.work.hh$age>17 & CPS.work.hh$age<65)
#CPS.work.hh$union_dens<-CPS.work.hh$union_mem+ CPS.work.hh$union_cov
CA2011<-subset(CPS.work.hh, CPS.work.hh$year==2011 & CPS.work.hh$statefip=="California")
CA2000<-subset(CPS.work.hh, CPS.work.hh$year==1995& CPS.work.hh$statefip=="California")
depvars1<-c("equivinc_posttax_broad", "equivinc_pretax_broad", "equivinc_pretax_equity", "equivinc_posttax_equity","cellmean_aftertax","cellmean_eitc","cellmean_equivinc","cellmean_equivinc_pretrans")
incvars<-c()
for (k in depvars1){
  incvars<-append(incvars, which(colnames(CPS.work.hh)==k))
}
for (i in incvars){
  CPS.work.hh[,i]<-CPS.work.hh[,i]*(229.604/CPS.work.hh$CPI)
}

Lorenz_KB<-function(inc, weight, ordinate, type="mean"){
  N <- length(inc)
  xi_p <- quantile(inc, probs=ordinate)
  N_hat<-sum(weight)
  mu_hat <- mean(inc)
  I_vec<-apply(data.frame(inc),1,function(x) if (x<=xi_p){1} else{0})
  L_hat<-(1/(N_hat*mu_hat))*sum(weight*inc*I_vec)
  if (type=="mean"){
    return(L_hat)
  }
  else if (type == "variance"){
    u_i <- (1/(N_hat*mu_hat))*((inc-xi_p)*I_vec + ordinate*xi_p - inc*L_hat)
    var_hat <- N*var(u_i)*(sum(weight^2))
    return(var_hat)
  }
}

rifreg_lorenz<-function(x, incvar, p, f, options=""){
  y = incvar
  v_p <- quantile(y, probs=p)
  T_Lp <- Lorenz_KB(incvar, weight=rep(1,length(incvar)), p)
  mu_F <- mean(y)
  #x$rif_y <- apply(data.frame(y), 1, function(x) if (x<v_p){(x-(1-p)*v_p)/mu_F + T_Lp*(1-x/mu_F)} else{p*v_p/mu_F-T_Lp*(1-x/mu_F)})
  x$rif_y <- vapply(y, FUN=function(x) if (x<v_p){(x-(1-p)*v_p)/mu_F + T_Lp*(1-x/mu_F)} else{p*v_p/mu_F-T_Lp*(1-x/mu_F)}, FUN.VALUE=0.0)
  fm <- as.formula(paste("rif_y ~ ", as.character(f)))
  if (options=="coef"){
    return(lm(fm, x)$coef)
  }
  else if (options=="se"){
    return(coeftest(lm(fm, x), vcovHC(lm(fm, x))))
  }
  else if (options=="names"){
    return(names(lm(fm, x)$coef))
  }
  else{
    return(lm(fm, x))
  }
}
rif_lorenz_deciles<-function(x, incvar, decile, numshares, f, options=""){
  y = incvar
  v_p1 <- quantile(y, probs=(decile/numshares))
  T_Lp1 <- Lorenz_KB(incvar, weight=rep(1,length(incvar)), (decile/numshares))
  v_p2 <- quantile(y, probs=((decile-1)/numshares))
  T_Lp2 <- Lorenz_KB(incvar, weight=rep(1,length(incvar)), ((decile-1)/numshares))
  mu_F <- mean(y)
  #x$rif_y <- apply(data.frame(y), 1, function(x) if (x<v_p){(x-(1-p)*v_p)/mu_F + T_Lp*(1-x/mu_F)} else{p*v_p/mu_F-T_Lp*(1-x/mu_F)})
  x$rif_y1 <- vapply(y, FUN=function(x) if (x<v_p1){(x-(1-(decile/numshares))*v_p1)/mu_F + T_Lp1*(1-x/mu_F)} else{ (decile/numshares)*v_p1/mu_F-T_Lp1*(1-x/mu_F)}, FUN.VALUE=0.0)
  x$rif_y2 <- vapply(y, FUN=function(x) if (x<v_p2){(x-(1-((decile-1)/numshares))*v_p2)/mu_F + T_Lp2*(1-x/mu_F)} else{ ((decile-1)/numshares)*v_p2/mu_F-T_Lp2*(1-x/mu_F)}, FUN.VALUE=0.0)
  x$rif_y<-x$rif_y1-x$rif_y2
  fm <- as.formula(paste("rif_y ~ ", as.character(f)))
  if (options=="coef"){
    return(lm(fm, x)$coef)
  }
  else if (options=="se"){
    return(coeftest(lm(fm, x), vcovHC(lm(fm, x))))
  }
}
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



#Following Fortin, Lemieux, Firpo (2010), the RIF equivalent of the OB decomposition can be written as
#Delta_O = Delta_S + Delta_X; or
#Delta_O = X_B*(gamma_B - gamma_A) + (X_B - X_A)*gamma_A
#Let B=2011, A=2000
f ="inschool+age+female+black+latino+highschool+somecollege+bachelors+postgrad+union_dens+married+divorced+widowed+fulltime"
indepvars =c("inschool","age","female","black","latino","highschool","somecollege","bachelors","postgrad","union_dens","married","divorced","widowed", "fulltime")
CA2000_rif<-rifreg_lorenz(CA2000, CA2000$equivinc_posttax_broad, 0.9, f, options="coef")
CA2011_rif<-rifreg_lorenz(CA2011, CA2011$equivinc_posttax_broad, 0.9, f, options="coef")
CA2011_decile<-rif_lorenz_deciles(CA2011, CA2011$equivinc_posttax_broad, 1, 10, f, options="coef")
CA2000_decile<-rif_lorenz_deciles(CA2000, CA2000$equivinc_posttax_broad, 1, 10, f, options="coef")

X_bar00<-c(1)
X_bar11<-c(1)
for (i in indepvars){
  print(i)
  X_bar00<-append(X_bar00, mean(CA2000[i][,1], na.rm=T))
  X_bar11<-append(X_bar11, mean(CA2011[i][,1], na.rm=T))
}
Lorenz_KB(CA2011$cellmean_equivinc, weight=rep(1, length(CA2011$cellmean_equivinc)), 0.9)-Lorenz_KB(CA2000$cellmean_equivinc,weight=rep(1, length(CA2000$cellmean_equivinc)), 0.9)
crossprod(matrix(X_bar11), matrix((CA2011_rif - CA2000_rif)))
crossprod(matrix(X_bar11 - X_bar00),matrix(CA2000_rif))
t(matrix(X_bar11)) %*% matrix((CA2011_rif - CA2000_rif)) + t(matrix(X_bar11 - X_bar00)) %*% matrix(CA2000_rif)
(X_bar11 - X_bar00)*CA2000_rif

crossprod(matrix(X_bar11), matrix((CA2011_decile - CA2000_decile)))
crossprod(matrix(X_bar11 - X_bar00),matrix(CA2000_decile))
t(matrix(X_bar11)) %*% matrix((CA2011_decile - CA2000_decile)) + t(matrix(X_bar11 - X_bar00)) %*% matrix(CA2000_decile)
(X_bar11 - X_bar00)*CA2000_decile

ords<-c(0.01, seq(0.05,0.95,0.05), 0.99)
struct_eff<-c()
comp_eff<-c()
true_eff<-c()
for (i in ords){
  gamma_2<-rifreg_lorenz(CA2011, CA2011$cellmean_equivinc, i, f, options="coef")
  gamma_1<-rifreg_lorenz(CA2000, CA2000$cellmean_equivinc, i, f, options="coef")
  struct_eff<-append(struct_eff, crossprod(matrix(X_bar11), matrix(gamma_2-gamma_1)))
  comp_eff<-append(comp_eff, crossprod(matrix(X_bar11 - X_bar00),matrix(gamma_1)))
  temp_eff<-Lorenz_KB(CA2011$cellmean_equivinc, weight=rep(1, length(CA2011$cellmean_equivinc)), i)-Lorenz_KB(CA2000$cellmean_equivinc,weight=rep(1, length(CA2000$cellmean_equivinc)), i)
  true_eff<-append(true_eff, temp_eff)
}
total_eff<-struct_eff+comp_eff
effects<-data.frame(ords, struct_eff, comp_eff, total_eff, true_eff)
library(ggplot2)
ggplot(effects, aes(ords)) + geom_line(aes(y=struct_eff, colour="struct_eff")) +
  geom_line(aes(y=comp_eff, colour="comp_eff")) + geom_line(aes(y=total_eff, colour="total_eff")) +
  geom_line(aes(y=true_eff, colour="true_eff"))


ords<-1:30
struct_eff<-c()
comp_eff<-c()
for (i in ords){
  gamma_2<-rif_lorenz_deciles(CA2011, CA2011$equivinc_pretax_broad, i, 30, f, options="coef")
  gamma_1<-rif_lorenz_deciles(CA2000, CA2000$equivinc_pretax_broad, i, 30, f, options="coef")
  struct_eff<-append(struct_eff, crossprod(matrix(X_bar11), matrix(gamma_2-gamma_1)))
  comp_eff<-append(comp_eff, crossprod(matrix(X_bar11 - X_bar00),matrix(gamma_1)))
}
total_eff<-struct_eff+comp_eff
decile_effects<-data.frame(ords, struct_eff, comp_eff, total_eff)
ggplot(decile_effects, aes(ords)) + geom_line(aes(y=struct_eff, colour="struct_eff")) +
  geom_line(aes(y=comp_eff, colour="comp_eff")) + geom_line(aes(y=total_eff, colour="total_eff"))


indepvars<-c("inschool","age","female","black","latino","highschool","somecollege","bachelors",
             "postgrad","union_dens","married","divorced","widowed", "fulltime", "ind0","ind1",
             "ind2","ind3","ind4","ind5","ind6","ind7","ind8","occ0", "occ1", "occ2", "occ3", 
             "occ4", "occ5", "occ6", "occ7", "occ8", "hh_margrate", "self_emp", "pub_sector", "hhsize")
depvars<-c("statefip", "year", "equivinc_posttax_broad", "equivinc_pretax_broad", "equivinc_pretax_equity", "equivinc_posttax_equity","cellmean_aftertax","cellmean_eitc","cellmean_equivinc","cellmean_equivinc_pretrans")
indices<-c()
for (i in indepvars){
  indices<-append(indices, which(colnames(CPS.work.hh)==i))
}
for (j in depvars){
  indices<-append(indices, which(colnames(CPS.work.hh)==j))
}
f<-"inschool"
for (i in indepvars[2:36]){
  f<-paste(f, i, sep="+")
}
CA2000_rif<-rifreg_lorenz(CA2000, CA2000$equivinc_posttax_broad, 0.9, f, options="coef")
CA2011_rif<-rifreg_lorenz(CA2011, CA2011$equivinc_posttax_broad, 0.9, f, options="coef")
CA2011_decile<-rif_lorenz_deciles(CA2011, CA2011$equivinc_posttax_broad, 1, 10, f, options="coef")
CA2000_decile<-rif_lorenz_deciles(CA2000, CA2000$equivinc_posttax_broad, 1, 10, f, options="coef")
X_bar00<-c(1)
X_bar11<-c(1)
for (i in indepvars){
  print(i)
  X_bar00<-append(X_bar00, mean(CA2000[i][,1], na.rm=T))
  X_bar11<-append(X_bar11, mean(CA2011[i][,1], na.rm=T))
}
Lorenz_KB(CA2011$cellmean_equivinc, weight=rep(1, length(CA2011$cellmean_equivinc)), 0.9)-Lorenz_KB(CA2000$cellmean_equivinc,weight=rep(1, length(CA2000$cellmean_equivinc)), 0.9)
crossprod(matrix(X_bar11), matrix((CA2011_rif - CA2000_rif)))
crossprod(matrix(X_bar11 - X_bar00),matrix(CA2000_rif))
t(matrix(X_bar11)) %*% matrix((CA2011_rif - CA2000_rif)) + t(matrix(X_bar11 - X_bar00)) %*% matrix(CA2000_rif)
(X_bar11 - X_bar00)*CA2000_rif

crossprod(matrix(X_bar11), matrix((CA2011_decile - CA2000_decile)))
crossprod(matrix(X_bar11 - X_bar00),matrix(CA2000_decile))
t(matrix(X_bar11)) %*% matrix((CA2011_decile - CA2000_decile)) + t(matrix(X_bar11 - X_bar00)) %*% matrix(CA2000_decile)
(X_bar11 - X_bar00)*CA2000_decile

ords<-1:30
struct_eff<-c()
comp_eff<-c()
union_eff<-c()
for (i in ords){
  gamma_2<-rif_lorenz_deciles(CA2011, CA2011$equivinc_pretax_broad, i, 30, f, options="coef")
  gamma_1<-rif_lorenz_deciles(CA2000, CA2000$equivinc_pretax_broad, i, 30, f, options="coef")
  struct_eff<-append(struct_eff, crossprod(matrix(X_bar11), matrix(gamma_2-gamma_1)))
  comp_eff<-append(comp_eff, crossprod(matrix(X_bar11 - X_bar00),matrix(gamma_1)))
  union_eff<-append(union_eff, (X_bar11[11]-X_bar00[11])*gamma_1[11])
}
total_eff<-struct_eff+comp_eff
decile_effects<-data.frame(ords, struct_eff, comp_eff, total_eff, union_eff)
ggplot(decile_effects, aes(ords)) + geom_line(aes(y=struct_eff, colour="struct_eff")) +
  geom_line(aes(y=comp_eff, colour="comp_eff")) + geom_line(aes(y=total_eff, colour="total_eff"))+
geom_line(aes(y=union_eff, colour="union_eff"))


ords<-c(0.01, seq(0.05,0.95,0.05), 0.99)
struct_eff<-c()
comp_eff<-c()
union_eff<-c()
union_coef<-c()
for (i in ords){
  gamma_2<-rifreg_lorenz(CA2011, log(CA2011$cellmean_equivinc+1), i, f, options="coef")
  gamma_1<-rifreg_lorenz(CA2000, log(CA2000$cellmean_equivinc+1), i, f, options="coef")
  struct_eff<-append(struct_eff, crossprod(matrix(X_bar11), matrix(gamma_2-gamma_1)))
  comp_eff<-append(comp_eff, crossprod(matrix(X_bar11 - X_bar00),matrix(gamma_1)))
  union_eff<-append(union_eff, (X_bar11[11]-X_bar00[11])*gamma_1[11])
  union_coef<-append(union_coef, gamma_1[11])
}
total_eff<-struct_eff+comp_eff
effects<-data.frame(ords, struct_eff, comp_eff, total_eff, union_eff)
ggplot(effects, aes(ords)) + geom_line(aes(y=struct_eff, colour="struct_eff")) +
  geom_line(aes(y=comp_eff, colour="comp_eff")) + geom_line(aes(y=total_eff, colour="total_eff"))+
  geom_line(aes(y=union_eff, colour="union_eff"))


ords<-c(seq(0.05,0.95,0.05))
struct_eff<-c()
comp_eff<-c()
union_eff<-c()
union_coef<-c()
for (i in ords){
  gamma_2<-rif_quantile(CA2011, CA2011$cellmean_equivinc_pretrans, i, f, options="coef")
  gamma_1<-rif_quantile(CA2000, CA2000$cellmean_equivinc_pretrans, i, f, options="coef")
  struct_eff<-append(struct_eff, crossprod(matrix(X_bar11), matrix(gamma_2-gamma_1)))
  comp_eff<-append(comp_eff, crossprod(matrix(X_bar11 - X_bar00),matrix(gamma_1)))
  union_eff<-append(union_eff, (X_bar11[11]-X_bar00[11])*gamma_1[11])
  union_coef<-append(union_coef, gamma_1[11])
}
total_eff<-struct_eff+comp_eff
quantile_effects<-data.frame(ords, struct_eff, comp_eff, total_eff, union_eff)
library(ggplot2)
ggplot(quantile_effects, aes(ords)) + geom_line(aes(y=struct_eff, colour="struct_eff")) +
  geom_line(aes(y=comp_eff, colour="comp_eff")) + geom_line(aes(y=total_eff, colour="total_eff"))+ 
  geom_line(aes(y=union_eff, colour="union_eff"))