library(reldist)
library(ineq)
library(AER)
library(foreign)
library(foreach)

registerDoMC(cores=detectCores())

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

gini_OB_decomp<-function(x1, x2, incvar, indepvars){
	f1<-indepvars[1]
	for (i in indepvars[2:length(indepvars)]){
	  f1<-paste(f1, i, sep="+")
	}
	gamma_0<-rif_gini(x1, incvar, f1, options="coef")
	gamma_1<-rif_gini(x2, incvar, f1, options="coef")
	X_bar0<-c(1)
	X_bar1<-c(1)
	for (i in indepvars){
	  X_bar0<-append(X_bar0, mean(x1[i][,1], na.rm=T))
	  X_bar1<-append(X_bar1, mean(x2[i][,1], na.rm=T))
	}
	Delta_s<-crossprod(matrix(X_bar1), matrix((gamma_1 - gamma_0)))
	Delta_X<-crossprod(matrix(X_bar1 - X_bar0),matrix(gamma_0))
	Delta_t <- Delta_s + Delta_X
	return(c("Delta_s"=Delta_s, "Delta_X"=Delta_X, "Delta_t" = Delta_t))
}

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("Data/CPS_broadinc.rda")
CPS.work.hh$age<-as.numeric(as.character(CPS.work.hh$age))
CPS.work.hh<-CPS.work.hh[which(CPS.work.hh$year==2000 | CPS.work.hh$year==2011),]

indepvars<-c("inschool","age","female","black","latino","highschool","somecollege","bachelors",
             "postgrad","union_dens","married","divorced","widowed", "fulltime", "ind0","ind1",
             "ind2","ind3","ind4","ind5","ind6","ind7","ind8","occ0", "occ1", "occ2", "occ3", 
             "occ4", "occ5", "occ6", "occ7", "occ8", "hh_margrate", "self_emp", "pub_sector")

st.unique<-unique(CPS.work.hh$statefip)

st.decomp00<-foreach(i=st.unique, .combine='rbind')%dopar%{
	st.temp<-CPS.work.hh[which(CPS.work.hh$statefip==i),]
	dec.temp<-gini_OB_decomp(st.temp[which(st.temp$year==2011),], 
		st.temp[which(st.temp$year==2000),], "cellmean_equivinc", indepvars)
	c("State"=i, dec.temp)
}
save(st.decomp00, file="state_gini_decomp2000.rda")
