library(GB2)
library(reldist)
library(ineq)
library(doMC)
library(foreach)
library(reshape)

registerDoMC(cores=detectCores())
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")

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
perc_boottest_pval<-function(inc1, inc2, ord, trueval){
  L_hat<-Lorenz_KB(inc1, rep(1, length(inc1)), ord, type="mean")-Lorenz_KB(inc2, rep(1, length(inc2)), ord, type="mean")
  W_star<-foreach(i=1:200, .combine=c)%dopar%{
    inc1b<-sample(inc1, 1000,replace=T)
    inc2b<-sample(inc2, 1000,replace=T)
    Lorenz_KB(inc1b, rep(1, length(inc1b)), ord, type="mean")-Lorenz_KB(inc2b, rep(1, length(inc2b)), ord, type="mean")
  }
  p_val<-(1/200)*sum(apply(data.frame(W_star),1,function(x) if((x-L_hat)^2>=(L_hat-trueval)^2){1} else{0}))
  return(p_val)
}
percentile_boot<-function(inc1, inc2, ord, alpha, reps){
  theta_hat<-Lorenz_KB(inc1, rep(1, length(inc1)), ord, type="mean")-
    Lorenz_KB(inc2, rep(1, length(inc2)), ord, type="mean")
  bootreps<-foreach(i=1:reps, .combine=c)%dopar%{
    Lorenz_KB(sample(inc1, replace=T), rep(1, length(inc1)), ord, type="mean")-Lorenz_KB(sample(inc2, replace=T), rep(1, length(inc2)), ord, type="mean")
  }
  return(c("Point"=theta_hat, 
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2)))
  ))
}
perc_replicates<-function(data, ord, reps){
  replicates<-foreach (i = 1:reps, .combine=c)%dopar%{
  Lorenz_KB(sample(data[which(data$name==2),]$value, replace=T), rep(1, length(data[which(data$name==2),]$value)), ord, type="mean")-
    Lorenz_KB(sample(data[which(data$name==1),]$value, replace=T), rep(1, length(data[which(data$name==2),]$value)), ord, type="mean")
  }
  return(replicates)
}
perc_boot_virt<-function(data, virtname, ord, reps=500){
  bootreps<-ddply(data, .variables=c("variable"), function(x) data.frame("replicates"=perc_replicates(data, ord, reps), 
                                                                        "L_hat"=rep(Lorenz_KB(data[which(data$name==2),]$value, rep(1, length(data[which(data$name==2),]$value)), ord, type="mean")-
                                                                       Lorenz_KB(data[which(data$name==1),]$value, rep(1, length(data[which(data$name==2),]$value)), ord, type="mean"), reps)),.parallel=F)
  bootreps$p_val<-(bootreps$replicates-bootreps$L_hat^2)-(bootreps$L_hat^2)
  p_val<-mean(apply(data.frame(bootreps$p_val),1, function(x) if (x>=0){1}else{0}))
  U_B<-as.numeric(quantile(bootreps$replicates, probs=0.975))
  L_B<-as.numeric(quantile(bootreps$replicates, probs=0.025))
  Point<-mean(bootreps$L_hat)
  return(c("ord"=ord, "Point"=Point, "U_B"=U_B, "L_B"=L_B, "p_val"=p_val))
}
a=2.24
b= 50000
p = 0.618
q = 1.118
x1<-rgb2(10000, a,b,p,q)
L_x1<-pgb2(qgb2(0.4,a,b,p,q), a,b,p+(1/a),q-(1/a))
a1=(a-0.25)
x2<-rgb2(10000, a1, b,p,q)
L_x2<-pgb2(qgb2(0.4,a1,b,p,q), a1,b,p+(1/a1),q-(1/a1))
pop_diff<-L_x2-L_x1
top1_1<-as.numeric(quantile(x1, probs=0.99))
cellmean_1<-mean(x1[which(x1>top1_1)])
top1_2<-as.numeric(quantile(x2, probs=0.99))
cellmean_2<-mean(x2[which(x2>top1_2)])

x1_c<-apply(data.frame(x1),1, function(x) if (x>=top1_1){cellmean_1} else{x})
x2_c<-apply(data.frame(x2),1, function(x) if (x>=top1_2){cellmean_2} else{x})
x1_t<-apply(data.frame(x1),1, function(x) if (x>=top1_1){top1_1} else{x})
x2_t<-apply(data.frame(x2),1, function(x) if (x>=top1_2){top1_2} else{x})

fit_true<-ml.gb2(x1)
fit_1<-ml.gb2(x1_c[which(x1_c>quantile(x1_c, probs=0))])
fit_2<-ml.gb2(x2_c[which(x2_c>quantile(x2_c, probs=0))])

virt_inc1<-matrix(rep(0, 10000*11), 10000, 11)
virt_inc2<-matrix(rep(0, 10000*11), 10000, 11)
for (i in 1:10){
  virt_inc1[,i]<-apply(data.frame(x1_t),1, function(x) if (x<=top1_1){x} else{topcode_sub(x, fit_1)})
  virt_inc2[,i]<-apply(data.frame(x2_t),1, function(x) if (x<=top1_2){x} else{topcode_sub(x, fit_2)} )                                        
}
virt_inc1[,11]<-rep(1, 10000)
virt_inc2[,11]<-rep(2, 10000)
virt_inc<-as.data.frame(rbind(virt_inc1, virt_inc2))
colnames(virt_inc)[11]<-"names"
virt_inc<-melt(virt_inc, id.vars=c(11))

boot_results<-data.frame()
for (i in seq(0.1, 0.9, 0.1)){
  temp.data<-perc_boot_virt(virt_inc, "variable", i )
  boot_results<-rbind(boot_results, temp.data)
}