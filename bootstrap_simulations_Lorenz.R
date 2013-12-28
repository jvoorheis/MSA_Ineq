library(GB2)
library(reldist)
library(ineq)
library(doMC)
library(foreach)

registerDoMC(cores=detectCores())

Lorenz_KB<-function(inc, weight="default", ordinate, type="mean"){
  if (weight=="default"){
    weight<-rep(1, length(inc))
  }
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

bias_correction<-function(inc1, inc2, ordinate, reps, alpha, accelerate=T){
  theta_hat<-Lorenz_KB(inc1, rep(1, length(inc1)), ordinate, type="mean")-
    Lorenz_KB(inc2, rep(1, length(inc2)), ordinate, type="mean")
  bootreps<-foreach(i=1:reps, .combine=c)%dopar%{
    Lorenz_KB(sample(inc1, replace=T), rep(1, length(inc1)), ordinate, type="mean")-Lorenz_KB(sample(inc2, replace=T), rep(1, length(inc2)), ordinate, type="mean")
  }
  samplesize<-min(length(inc1), length(inc2))
  temp_inc1<-sample(inc1, samplesize, replace=F)
  temp_inc2<-sample(inc2, samplesize, replace=F)
  if (accelerate==T){
    jackknife<-foreach(i=1:samplesize, .combine=c)%dopar%{
      Lorenz_KB(temp_inc1[-i], rep(1, (samplesize-1)), ordinate, type="mean")-Lorenz_KB(temp_inc2[-i], rep(1, (samplesize-1)), ordinate, type="mean")
    }
    theta_bar <- mean(jackknife)
    a <- sum((theta_bar-jackknife)^3)/(6*(sum((theta_bar-jackknife)^2)^1.5))
  }
  else{
    a<-0
  }
  z_num <- apply(data.frame(bootreps),1,function(x) if (x<=theta_hat){1} else{0})
  z_0 <- pnorm(sum(z_num)/reps)
  z_1 <- qnorm(1-alpha/2)
  z_2 <- qnorm(alpha/2)
  p_1<-pnorm(z_0+(z_0-z_1)/(1-a*(z_0-z_1)))
  p_2<-pnorm(z_0+(z_0+z_1)/(1-a*(z_0+z_1)))
  p_3<-pnorm(z_0+(z_0-z_1)/(1-0*(z_0-z_1)))
  p_4<-pnorm(z_0+(z_0+z_1)/(1-0*(z_0+z_1)))
  theta_hat_var<-Lorenz_KB(inc1, rep(1, length(inc1)), ordinate, type="variance")+
    Lorenz_KB(inc2, rep(1, length(inc2)), ordinate, type="variance")
  par_UB<-theta_hat+qnorm(1-alpha/2)*sqrt(theta_hat_var)
  par_LB<-theta_hat+qnorm(alpha/2)*sqrt(theta_hat_var)
  return(c("Point"=theta_hat, "BCa_LB"=as.numeric(quantile(bootreps, probs=p_1)), 
           "BCa_UB"=as.numeric(quantile(bootreps, probs=p_2)), 
           "BC_LB"=as.numeric(quantile(bootreps, probs=p_3)), 
           "BC_UB"=as.numeric(quantile(bootreps, probs=p_4)), 
           "LB"=as.numeric(quantile(bootreps, probs=alpha/2)), 
           "UB"=as.numeric(quantile(bootreps, probs=(1-alpha/2))),
           "par_LB"=par_LB,"par_UB"=par_UB
           ))
}
bootvar_LC<-function(inc1, inc2, ord){
  bootreps<-foreach(i=1:reps, .combine=c)%dopar%{
    Lorenz_KB(sample(inc1, replace=T), rep(1, length(inc1)), ord, type="mean")-Lorenz_KB(sample(inc2, replace=T), rep(1, length(inc2)), ord, type="mean")
  }
  return(var(bootreps))
}
boottest<-function(inc1, inc2, ord, trueval){
  theta_hat<-Lorenz_KB(inc1, rep(1, length(inc1)), ord, type="mean")-
    Lorenz_KB(inc2, rep(1, length(inc2)), ord, type="mean")
  theta_hat_var<-Lorenz_KB(inc1, rep(1, length(inc1)), ord, type="variance")+
    Lorenz_KB(inc2, rep(1, length(inc2)), ord, type="variance")
  W_true<-(theta_hat - trueval)/sqrt(theta_hat_var)
  W_star<-foreach(i=1:200, .combine=c)%dopar%{
    inc1b<-sample(inc1, 1000,replace=T)
    inc2b<-sample(inc2, 1000,replace=T)
    theta_hat_star<-Lorenz_KB(inc1b, rep(1, length(inc1b)), ord, type="mean")-
      Lorenz_KB(inc2b, rep(1, length(inc2b)), ord, type="mean")
    theta_hat_star_var<-Lorenz_KB(inc1b, rep(1, length(inc1)), ord, type="variance")+
      Lorenz_KB(inc2b, rep(1, length(inc2)), ord, type="variance")
    (theta_hat_star - theta_hat)/sqrt(theta_hat_star_var)
  }
  return(W_star)
  #return(mean(apply(data.frame(W_star),1,function(x) if (x^2>=W_true^2){1} else{0})))
}
perc_boottest<-function(inc1, inc2, ord, trueval){
  W_star<-foreach(i=1:200, .combine=c)%dopar%{
    inc1b<-sample(inc1, 1000,replace=T)
    inc2b<-sample(inc2, 1000,replace=T)
    Lorenz_KB(inc1b, rep(1, length(inc1b)), ord, type="mean")-Lorenz_KB(inc2b, rep(1, length(inc2b)), ord, type="mean")
  }
  if (trueval<=quantile(W_star, probs=0.975) & trueval>=quantile(W_star, probs=0.025)){
    reject<-0
  }
  else{
    reject<-1
  }
  return(reject)
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

a=2.24
b= 50000
p = 0.618
q = 1.118
x1<-rgb2(100000, a,b,p,q)
L_x1<-pgb2(qgb2(ord,a,b,p,q), a,b,p+(1/a),q-(1/a))
a1=(a-0.25)
x2<-rgb2(100000, a1, b,p,q)
L_x2<-pgb2(qgb2(ord,a1,b,p,q), a1,b,p+(1/a1),q-(1/a1))


pop_diff<-L_x2-L_x1
pop_diff1<-Lorenz_KB(x2, rep(1,length(x2)), ord0)-Lorenz_KB(x1, rep(1,length(x1)), ord0)
results_1<-matrix(rep(0,100*9), 100,9)
for (i in 1:100){
  results_1[i,]<-bias_correction(sample(x2, 1000,replace=F), sample(x1, 2000,replace=F), ord0, 500, 0.05)
}
results<-results_1
raw_results<-results
test_results<-results
results[,2]<-apply(data.frame(results[,2]),1,function(x) if (x<pop_diff){1} else{0})
results[,4]<-apply(data.frame(results[,4]),1,function(x) if (x<pop_diff){1} else{0})
results[,6]<-apply(data.frame(results[,6]),1,function(x) if (x<pop_diff){1} else{0})
results[,8]<-apply(data.frame(results[,8]),1,function(x) if (x<pop_diff){1} else{0})
results[,3]<-apply(data.frame(results[,4]),1,function(x) if (x>pop_diff){1} else{0})
results[,5]<-apply(data.frame(results[,5]),1,function(x) if (x>pop_diff){1} else{0})
results[,7]<-apply(data.frame(results[,7]),1,function(x) if (x>pop_diff){1} else{0})
results[,9]<-apply(data.frame(results[,9]),1,function(x) if (x>pop_diff){1} else{0})

test_results[,3]<-apply(data.frame(test_results[,4]),1,function(x) if (x>0){1} else{0})
test_results[,5]<-apply(data.frame(test_results[,5]),1,function(x) if (x>0){1} else{0})
test_results[,7]<-apply(data.frame(test_results[,7]),1,function(x) if (x>0){1} else{0})
test_results[,9]<-apply(data.frame(test_results[,9]),1,function(x) if (x>0){1} else{0})

true_test<-numeric(100)
boot_test<-numeric(100)
perc_test<-numeric(100)
for (i in 1:100){
  inc1<-sample(x2, 1000,replace=F)
  inc2<-sample(x1, 1000,replace=F)
  theta_hat<-Lorenz_KB(inc1, rep(1, length(inc1)), ord, type="mean")-
    Lorenz_KB(inc2, rep(1, length(inc2)), ord, type="mean")
  theta_hat_var<-Lorenz_KB(inc1, rep(1, length(inc1)), ord, type="variance")+
    Lorenz_KB(inc2, rep(1, length(inc2)), ord, type="variance")
  true_test[i]<-(theta_hat - pop_diff)/sqrt(theta_hat_var)
  boot_test[i]<-boottest(inc1, inc2, ord)
  perc_test[i]<-perc_boottest(inc1, inc2, ord, pop_diff)
}


mean(apply(data.frame(true_test),1,function(x) if (abs(x)<qt(0.975, 1000)){0} else{1}))
mean(apply(data.frame(boot_test),1,function(x) if (abs(x)<0.05){1} else{0}))
mean(perc_test)

h = 0.5
H_hat<-function(inc, k){
  inc<-sort(inc, decreasing=T)
  H_kn<-(1/k)*sum(log(inc[1:k]))-log(inc[k])
  return(H_kn)
}
k<-1:(3*as.integer(sqrt(length(inc2))))
alpha<-numeric(length(1:(3*as.integer(sqrt(length(inc2))))))
for (i in 1:(3*as.integer(sqrt(length(inc2))))){
  alpha[i]<-1/(H_hat(inc2, i))
}
plot(k, alpha,type="l") 

type1<-matrix(rep(0, 900), 100,9)
type2<-matrix(rep(0, 900), 100,9)
for (ord in seq(0.1,0.9, 0.1)){
  trueval<-pgb2(qgb2(ord,a1,b,p,q), a1,b,p+(1/a1),q-(1/a1))-pgb2(qgb2(ord,a,b,p,q), a,b,p+(1/a),q-(1/a))
  type1_temp<-numeric(100)
  type2_temp<-numeric(100)
  for (i in 1:100){
    x1<-rgb2(2000, a,b,p,q)
    x2<-rgb2(2000, a1, b,p,q)
    type1_temp[i]<-(Lorenz_KB(x2, rep(1, length(x2)), ord, type="mean")-
                      Lorenz_KB(x1, rep(1, length(x1)), ord, type="mean") - trueval)/(Lorenz_KB(x2, rep(1, length(x2)), ord, type="variance")+
                                                                                        Lorenz_KB(x1, rep(1, length(x1)), ord, type="variance"))
    type2_temp[i]<-(Lorenz_KB(x2, rep(1, length(x2)), ord, type="mean")-
                                     Lorenz_KB(x1, rep(1, length(x1)), ord, type="mean") - 0)/(Lorenz_KB(x2, rep(1, length(x2)), ord, type="variance")+
                                                                                                       Lorenz_KB(x1, rep(1, length(x1)), ord, type="variance"))
  }
  type1[,(ord*10)]<-type1_temp
  type2[,(ord*10)]<-type2_temp
}
type1_try<-sapply(type1,function(x) if (abs(x)>qt(0.975, 2000)){1} else{0}, simplify = "array")
type2<-mapply(function(x) if (abs(x)>qt(0.975, 2000)){1} else{0}, type2)
apply(type1, 2, mean)
apply(type2, 2, mean)

type1_boot<-matrix(rep(0, 900), 100,9)
type2_boot<-matrix(rep(0, 900), 100,9)
for (ord in seq(0.1,0.9, 0.1)){
  trueval<-pgb2(qgb2(ord,a1,b,p,q), a1,b,p+(1/a1),q-(1/a1))-pgb2(qgb2(ord,a,b,p,q), a,b,p+(1/a),q-(1/a))
  type1_temp<-numeric(100)
  type2_temp<-numeric(100)
  for (i in 1:100){
    x1<-rgb2(2000, a,b,p,q)
    x2<-rgb2(2000, a1, b,p,q)
    type1_temp[i]<-perc_boottest_pval(x2, x1, ord, trueval)
    type2_temp[i]<-perc_boottest_pval(x2, x1, ord, 0)
  }
  type1_boot[,(ord*10)]<-type1_temp
  type2_boot[,(ord*10)]<-type2_temp
}

trueval_ord<-numeric(9)
for (ord in seq(0.1,0.9, 0.1)){
  trueval_ord[ord*10]<-pgb2(qgb2(ord,a1,b,p,q), a1,b,p+(1/a1),q-(1/a1))-pgb2(qgb2(ord,a,b,p,q), a,b,p+(1/a),q-(1/a))
}

for (i in 1:9){
print(mean(apply(data.frame(type2_boot[,i]),1,function(x) if (x<0.1){1} else{0})))
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
x3<-x1
x3<-sort(x3)
transfer<-x3[1:1000]
PS# x3[1:1000]<-x3[1:1000]
x3[1001:2000]<-x3[1001:2000]+transfer
Lorenz_KB(x3, rep(1, length(x3)), 0.3)-Lorenz_KB(x1, rep(1, length(x1)), 0.3)
par_trans<-numeric(3)
boot_trans<-numeric(3)
for (ord in seq(0.1, 0.3, 0.1)){
par_trans[ord*10]<-(Lorenz_KB(x3, rep(1, length(x3)), ord, type="mean")-
  Lorenz_KB(x1, rep(1, length(x1)), ord, type="mean") - 0)/(Lorenz_KB(x3, rep(1, length(x3)), ord, type="variance")+
                                                                     Lorenz_KB(x1, rep(1, length(x1)), ord, type="variance"))
  boot_trans[ord*10]<-perc_boottest_pval(x3, x1, ord, 0)
}