library(GB2)
library(ineq)
library(reldist)
library(plyr)
library(ggplot2)
library(reshape)
library(parallel)
library(doMC)

registerDoMC(cores=detectCores())
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_household_tax.rda")
CPS_work<-data.frame(CPS.work.hh)
CPS_work<-subset(CPS_work, CPS_work$year>1991 & is.na(CPS_work$year)==F)

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

lorenz_diff<-function(inc1,, inc2, ord){
  diff_l <- Lc(inc1)$L[as.integer(ord*length(inc1))] - Lc(inc2)$L[as.integer(ord*length(inc2))]
  return(diff_l)
}

pre_post<-ddply(CPS_work, .variables=c("State", "year"), function(x) 
  c("pretax_gini"=davidson_gini(x$cellmean_equivinc_pretrans, type="point"),
    "pretax_gini_var"=davidson_gini(x$cellmean_equivinc_pretrans, type="var"),
    "posttax_gini"=davidson_gini(x$cellmean_equivinc_posttax, type="point"),
    "posttax_gini_var"=davidson_gini(x$cellmean_equivinc_posttax, type="var"),
    "baseline_gini" = davidson_gini(x$cellmean_equivinc, type="point"),
    "baseline_gini_var"=davidson_gini(x$cellmean_equivinc, type="var"),
    "postpre_gini" = davidson_gini(x$cellmean_equivinc_postpre, type="point"),
    "postpre_gini_var" = davidson_gini(x$cellmean_equivinc_postpre, type="var")                          
    ), .parallel=T)
save(pre_post, file="pre_vs_post_tax_gini.rda")

curr_vars<-length(CPS_work)
num=19
varname<-rep("", num)
for (i in 1:num){
  varname[i] <- paste("ordinate", as.character(i), sep="")
  CPS_work[,curr_vars+i]<-rep(i/(num+1), length(CPS_work[,curr_vars]))
}
colnames(CPS_work)[(curr_vars+1):(curr_vars+num)]<-varname
CPS_work<-CPS_work[,c(2,5,6,10,18,22,28:37)]
CPS_work<-melt(CPS_work, id.vars=colnames(CPS_work)[1:7])
pre_post_lorenz<-ddply(CPS_work, .variables=c("State", "year", "variable"), function(x) 
  c("pretax_lorenz"=Lc(x$cellmean_equivinc_pretrans)$L[as.integer(x$value[1]*length(x$value))],
    "posttax_lorenz"=Lc(x$cellmean_equivinc_posttax)$L[as.integer(x$value[1]*length(x$value))],
    "baseline_lorenz"=Lc(x$cellmean_equivinc)$L[as.integer(x$value[1]*length(x$value))],
    "postpre_lorenz"=Lc(x$cellmean_equivinc_postpre)$L[as.integer(x$value[1]*length(x$value))],
    "ordinate"=x$value[1]
  ), .paralle=T)
save(pre_post_lorenz, file="Data/pre_post_cellmean_lorenz.rda")


pre_post$diff_test<-(pre_post$posttax_gini-pre_post$pretax_gini)/sqrt(pre_post$baseline_gini_var+pre_post$pretax_gini_var)
pre_post$diff_est<-(pre_post$pretax_gini-pre_post$baseline_gini)
pre_post$diff_est_pct<-(pre_post$pretax_gini-pre_post$posttax_gini)/pre_post$pretax_gini
pre_post$diff_est_pct_trans<-(pre_post$pretax_gini-pre_post$baseline_gini)/pre_post$pretax_gini
pre_post$diff_est_pct_tax<-(pre_post$pretax_gini-pre_post$postpre_gini)/pre_post$pretax_gini



AL2000<-subset(CPS_work, CPS_work$year==2012 & CPS_work$statefip==6& is.na(CPS_work$year)==F)
AL2000<-subset(AL2000, is.na(AL2000$year)==F)
AL2013<-subset(CPS_work, CPS_work$year==2013 & CPS_work$statefip==6)

gini(AL2000$cellmean_equivinc_posttax)
gini(AL2000$cellmean_equivinc_pretrans)
gini(AL2000$cellmean_equivinc)
apost<-davidson_gini(AL2000$cellmean_equivinc_pretax, type="both")
apre<-davidson_gini(AL2000$cellmean_equivinc, type="both")
(apost[1]-apre[1])/sqrt(apost[2]+apre[2])

(apost[1]-apre[1])/apre[1]

pre_post<-subset(pre_post, pre_post$year<2013)
#pre_post$year<-apply(data.frame(pre_post$year),1, function(x) if (x<2000){x-1900} else{x-2000})
pre_post_trunc<-pre_post[,c(1:2, 13:15)]
pre_post_rawtrunc<-pre_post[,c(1:2, 3,5,7,9)]
save(pre_post_rawtrunc, file="pre_post_gini.rda")
png(file="total.png", width=7.5, height=8.65, units="in", res=200)
print(ggplot(pre_post, aes(x=year, y=diff_est_pct)) + labs(title="Redistributiveness") + xlab(NULL) + ylab (NULL) + theme(axis.text.x=element_text(angle=60, size=12)) +  theme_bw() + geom_line() + facet_wrap(~State,ncol=6))
dev.off()


png(file="transfers.png", width=7.5, height=8.65, units="in", res=200)
print(ggplot(pre_post, aes(x=year, y=diff_est_pct_trans)) + labs(title="Redistributiveness") + xlab(NULL) + ylab (NULL) + theme(axis.text.x=element_text(angle=60, size=12)) +  theme_bw() + geom_line() + facet_wrap(~State,ncol=6))
dev.off()

png(file="taxes.png", width=7.5, height=8.65, units="in", res=200)
print(ggplot(pre_post, aes(x=year, y=diff_est_pct_tax)) + labs(title="Redistributiveness") + xlab(NULL) + ylab (NULL) + theme(axis.text.x=element_text(angle=60, size=12)) +  theme_bw() + geom_line() + facet_wrap(~State,ncol=6))
dev.off()

pre_post_trunc<-melt(pre_post_trunc, id.vars=c("State", "year"))
pre_post_rawtrunc<-melt(pre_post_rawtrunc, id.vars=c("State", "year"))

png(file="combined_pct.png", width=20.5, height=14.65, units="in", res=200)
print(ggplot(data=pre_post_trunc, aes(x=year, y = value, group=variable, colour=variable)) + 
  geom_line() + facet_wrap(~State))
dev.off()
png(file="combined_gini.png", width=20.5, height=14.65, units="in", res=200)
print(ggplot(data=pre_post_rawtrunc, aes(x=year, y = value, group=variable, colour=variable)) + 
  geom_line() + facet_wrap(~State))
dev.off()

lorenz_ord_bootstrap<-function(inc1, inc2, ord, reps, conf){
  size = min(length(inc1), length(inc2))
  boot_reps <- rep(0, reps)
  point_est <- Lc(inc1)$L[as.integer(ord*length(inc1))] - 
    Lc(inc2)$L[as.integer(ord*length(inc2))]
  boot_reps<-apply(data.frame(boot_reps),1, function(x) (Lc(sample(inc1, replace=T))$L[as.integer(ord*length(inc1))] - 
                                                           Lc(sample(inc2, replace=T))$L[as.integer(ord*length(inc2))]))
  #return(boot_reps)
  return(c(point_est, quantile(boot_reps, prob=1-(conf/2)), quantile(boot_reps, prob=conf/2)))
}
