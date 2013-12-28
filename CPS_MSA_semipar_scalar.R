library(doMC)
library(parallel)
library(plyr)
library(GB2)
library(ineq)
library(reldist)
registerDoMC(cores=detectCores())
library(compiler)
enableJIT(1)

setwd("/ibrix/home8/jlv/MSA_Ineq/")
source("functions.r")
load("CPS_broadinc_MSA_hh.rda")
CPS.work.hh$cellmean_equivinc = CPS.work.hh$cellmean_equivinc+1
#CPS.work.hh<-CPS.work.hh[which(CPS.work.hh$year==i | CPS.work.hh$year==i),]
unique95<-unique(CPS.work.hh[which(CPS.work.hh$year==1995),]$MSA)
unique00<-unique(CPS.work.hh[which(CPS.work.hh$year==2000),]$MSA)
unique05<-unique(CPS.work.hh[which(CPS.work.hh$year==2005),]$MSA)
unique11<-unique(CPS.work.hh[which(CPS.work.hh$year==2011),]$MSA)
unique12<-unique(CPS.work.hh[which(CPS.work.hh$year==2012),]$MSA)
CPS.work.hh<-CPS.work.hh[which(CPS.work.hh$MSA %in% unique95 & 
								CPS.work.hh$MSA %in% unique00 &
								CPS.work.hh$MSA %in% unique05 &
								CPS.work.hh$MSA %in% unique11 &
								CPS.work.hh$MSA %in% unique12),]

topcode_sub1<-function(inc, fit2){
  bottom<-pgb2(inc, fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4])
  return(qgb2(runif(1,min=bottom, max=1), fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4]))
}

semipar_GB2_boot_scalar<-function(inc1, inc2, topcode1, topcode2, fit1, fit2, reps, alpha, index="Gini", year1, year2){
  inc1<-inc1[which(topcode1==0)]
  inc2<-inc2[which(topcode2==0)]
  topcode1<-topcode1[which(topcode1>0)]
  topcode2<-topcode2[which(topcode2>0)]
  bootreps<-data.frame()
  if (index=="Gini"){
    theta_hat<-gini(inc1)-gini(inc2)
    for (i in 1:reps){
      temp1<-vapply(topcode1, FUN=topcode_sub1, fit1, FUN.VALUE=0.0)
      temp2<-vapply(topcode2, FUN=topcode_sub1, fit2, FUN.VALUE=0.0)
      inc1t<-append(temp1, sample(inc1, replace=T))
      inc2t<-append(temp2, sample(inc2, replace=T))
      
      
      bootreps<-rbind(bootreps, data.frame("boot"=gini(inc1t)-gini(inc2t), 
                                           "MI" = gini(append(temp1, inc1)) - gini(append(temp2, inc2))))
    }
  }
  else if (index=="Theil"){
    theta_hat<-Theil(inc1)-Theil(inc2)
    for (i in 1:reps){
      temp1<-vapply(topcode1, FUN=topcode_sub1, fit1, FUN.VALUE=0.0)
      temp2<-vapply(topcode2, FUN=topcode_sub1, fit2, FUN.VALUE=0.0)
      inc1t<-append(temp1, sample(inc1, replace=T))
      inc2t<-append(temp2, sample(inc2, replace=T))
      bootreps<-rbind(bootreps, data.frame("boot"=Theil(inc1t)-Theil(inc2t), 
                                           "MI" = Theil(append(temp1, inc1)) - Theil(append(temp2, inc2))))
    }
  }
  else if (index=="9010"){
    theta_hat<-ratio9010f(inc1)-ratio9010f(inc2)
    for (i in 1:reps){
      temp1<-vapply(topcode1, FUN=topcode_sub1, fit1, FUN.VALUE=0.0)
      temp2<-vapply(topcode2, FUN=topcode_sub1, fit2, FUN.VALUE=0.0)
      inc1t<-append(temp1, sample(inc1, replace=T))
      inc2t<-append(temp2, sample(inc2, replace=T))
      bootreps<-rbind(bootreps, data.frame("boot"=ratio9010f(inc1t)-ratio9010f(inc2t), 
                                           "MI" = ratio9010f(append(temp1, inc1)) - ratio9010f(append(temp2, inc2))))
    }
  }
  else if (index=="top1"){
  	theta_hat<-top1share(inc1)-top1share(inc2)
    for (i in 1:reps){
      temp1<-vapply(topcode1, FUN=topcode_sub1, fit1, FUN.VALUE=0.0)
      temp2<-vapply(topcode2, FUN=topcode_sub1, fit2, FUN.VALUE=0.0)
      inc1t<-append(temp1, sample(inc1, replace=T))
      inc2t<-append(temp2, sample(inc2, replace=T))
      bootreps<-rbind(bootreps, data.frame("boot"=top1share(inc1t)-top1share(inc2t), 
                                           "MI" = top1share(append(temp1, inc1)) - top1share(append(temp2, inc2))))
    }
  }
  theta_hat1 <-  mean(bootreps$MI)
  bootreps <-bootreps$boot
  p_val <- mean(sapply(bootreps, function(x) if ((x-theta_hat)^2 >= theta_hat^2){1} else{0}))
  p_val1 <- mean(sapply(bootreps, function(x) if ((x-theta_hat1)^2 >= theta_hat1^2){1} else{0}))
  return(data.frame("index" = index,
           "Point"=theta_hat, 
           "Point1" = theta_hat1,
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2))),
           "p_val"=p_val,
           "p_val1"=p_val1,
           "year1"=year1,
           "year2"=year2))
}

                                                                                         
years<-c(1995, 2000,2005,2011, 2012)
semiparScalarDF<-data.frame()
for (i in c(1995, 2000, 2005)){
	for (j in years[which(years>i)]){
		min2005 <- quantile(CPS.work.hh[which(CPS.work.hh$year==i),]$cellmean_equivinc, 0.3)
		fit2005_a<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==i & CPS.work.hh$cellmean_equivinc>min2005),]$cellmean_equivinc)
		min2011 <- quantile(CPS.work.hh[which(CPS.work.hh$year==j),]$cellmean_equivinc, 0.3)
		fit2011<-ml.gb2(CPS.work.hh[which(CPS.work.hh$year==j & CPS.work.hh$cellmean_equivinc>min2011),]$cellmean_equivinc)
		semipar_gini0012<-ddply(CPS.work.hh, .variables=c("MSA"), function(x) c(semipar_GB2_boot_scalar(x[which(x$year==j),]$cellmean_equivinc, 
                                 x[which(x$year==i),]$cellmean_equivinc,
                                 x[which(x$year==j),]$topcoded_equivinc,
                                 x[which(x$year==i),]$topcoded_equivinc,
                                 fit2011, fit2005_a, 500, 0.05, index="Gini", "year1"=i, "year2"=j), .parallel=T))
		semipar_theil0012<-ddply(CPS.work.hh, .variables=c("MSA"), function(x) c(semipar_GB2_boot_scalar(x[which(x$year==j),]$cellmean_equivinc, 
                                 x[which(x$year==i),]$cellmean_equivinc,
                                 x[which(x$year==j),]$topcoded_equivinc,
                                 x[which(x$year==i),]$topcoded_equivinc,
                                 fit2011, fit2005_a, 500, 0.05, index="Theil", "year1"=i, "year2"=j), .parallel=T))
		semipar_ratio0012<-ddply(CPS.work.hh, .variables=c("MSA"), function(x) c(semipar_GB2_boot_scalar(x[which(x$year==j),]$cellmean_equivinc, 
                                 x[which(x$year==i),]$cellmean_equivinc,
                                 x[which(x$year==j),]$topcoded_equivinc,
                                 x[which(x$year==i),]$topcoded_equivinc,
                                 fit2011, fit2005_a, 500, 0.05, index="9010", "year1"=i, "year2"=j), .parallel=T))
        semipar_top0012<-ddply(CPS.work.hh, .variables=c("MSA"), function(x) c(semipar_GB2_boot_scalar(x[which(x$year==j),]$cellmean_equivinc, 
                                 x[which(x$year==i),]$cellmean_equivinc,
                                 x[which(x$year==j),]$topcoded_equivinc,
                                 x[which(x$year==i),]$topcoded_equivinc,
                                 fit2011, fit2005_a, 500, 0.05, index="top1", "year1"=i, "year2"=j), .parallel=T))
		semiparScalarDF<-rbind(semiparScalarDF, semipar_gini0012, semipar_theil0012, semipar_ratio0012, semipar_top0012)
	}
}

save(semiparScalarDF, file="CPS_MSA_semiparScalar.rda")
