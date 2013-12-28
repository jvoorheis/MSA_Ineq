library(GB2)
library(ineq)
library(reldist)

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("Data/CPS_broadinc.rda")
CPS_work<-subset(CPS_work, CPS_work$year>1990 & is.na(CPS_work$year)==F)
AL2000<-subset(CPS_work, CPS_work==2008 & CPS_work$statefip==6& is.na(CPS_work$year)==F)
AL2000<-subset(AL2000, is.na(AL2000$year)==F)
AL2013<-subset(CPS_work, CPS_work==2009 & CPS_work$statefip==6)


davidson_gini<-function(x, type="point"){
  y_i<-sort(x)
  y_args<-1:length(y_i)
  w_i <- ((2*y_args-1)*y_i)/(2*length(y_i))
  v_i <- apply(data.frame(y_args), 1, function(x) (1/length(y_i))*sum(y_i[1:x]))
  I_hat <- mean(w_i)
  G_hat <- length(y_i)*((2*I_hat/mean(y_i))-1)/(length(y_i)-1)
  if (type=="point"){
	return(G_hat)
  }
  else if (type=="var"){
	Z_hat <-  2*(w_i - v_i)-(G_hat + 1)*y_i
	var_G <- sum((Z_hat - mean(Z_hat))^2)/((length(y_i)*mean(y_i))^2)
	return(var_G)
  }
  else if (type=="both"){
	Z_hat <-  2*(w_i - v_i)-(G_hat + 1)*y_i
	var_G <- sum((Z_hat - mean(Z_hat))^2)/((length(y_i)*mean(y_i))^2)
	return(c(G_hat,var_G))
}
}

# G_hat
# gini(x)
# gini.gb2(Natl_fit$opt1$par[1], Natl_fit$opt1$par[3], Natl_fit$opt1$par[4])

a00<-davidson_gini(work_inc$equivinc_pretax_broad, type="both")
a13<-davidson_gini(work_inc1$equivinc_pretax_broad,type="both")
pnorm(abs((a00[1]-a13[1])/sqrt(a00[2]+a13[2])), lower.tail=F)
(a00[1]-a13[1])


boot_reps<-numeric(5000)
for (i in 1:5000){
  g_hat <- gini(sample(AL2000$cellmean_equivinc, replace=T))-gini(sample(AL2013$cellmean_equivinc, replace=T))
  boot_reps[i]<-g_hat
}
1-ecdf(boot_reps)(0)