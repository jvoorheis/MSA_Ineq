library(ggplot2)
library(GB2)
ratio9010f<-function(inc){
  return(quantile(inc, probs=0.9)/(quantile(inc,probs=0.1)+0.000001))
}
ratio95medf<-function(inc){
  return(quantile(inc, probs=0.95)/(quantile(inc,probs=0.5)+0.000001))
}
ratio99medf<-function(inc){
  return(quantile(inc, probs=0.99)/(quantile(inc,probs=0.5)+0.000001))
}
ratio8020f<-function(inc){
  return(quantile(inc, probs=0.9)/(quantile(inc,probs=0.2)+0.000001))
}
generate_gini_plots<-function(units, inc_data){
  for (i in units){
    temp.df<-subset(inc_data, inc_data$MSA==i)
    temp.MSA<-unlist(strsplit(i, "/"))[1]
    filename<-paste(temp.MSA, ".jpg", sep="")
    jpeg(file=filename)
    print(ggplot(temp.df, aes(year)) + 
            geom_line(aes(y=Gini_GB2, colour="gini"))+
            opts(title=i))
    dev.off()
  }
}
generate_gini_plots_state<-function(units, inc_data, type="gini"){
  for (i in units){
    temp.df<-subset(inc_data, inc_data$State==i)
    temp.MSA<-unlist(strsplit(i, "/"))[1]
    filename<-paste(temp.MSA, "_", type, ".png", sep="")
    if (type=="gini"){
    png(file=filename)
    print(ggplot(temp.df, aes(year)) + 
            geom_line(aes(y=Gini_StateGB2, colour="gini"))+
            opts(title=i))
    dev.off()
    }
    if (type=="top1"){
      png(file=filename)
      print(ggplot(temp.df, aes(year)) + 
              geom_line(aes(y=Top1Share, colour="Top1"))+
              opts(title=i))
      dev.off()      
    }
  }
}
sqrt_eq<-function(pernum){
  return(sqrt(max(pernum)))
}
topcode_sub<-function(inc, fit2){
  if (inc>0){
    bottom<-pgb2(inc, fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4])
    return(qgb2(runif(1,min=bottom, max=1), fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4]))
  }
  else{
    return(inc)
  }  
}
top1share<-function(inc){
    #temp.df<-subset(data.frame(inc, weights), data.frame(inc, weights)$weights==1)
    top1<-quantile(inc, probs=0.99)
    return(sum(subset(inc, inc>=top1))/sum(inc))
}


ACS_trunc<-function(inc){
  if (inc>0){
    return(1)
  }
  else{
    return(0)
  }
}
X_split<-function(year){
  return(unlist(strsplit(year, "X"))[2])
}
state_strip<-function(msa){
  return(unlist(strsplit(msa, ","))[2])
}
DC_switch<-function(state){
  if (state=="Washington DC"){
    return("District of Columbia")
  }
  else{
    return(state)
  }
}
lorenz_point<-function(income, weights=c(rep(1, length(income))), ordinate=0.5){
  n <- length(income)
  L_temp<-Lc(income, n=weights)
  if (ordinate*n %% 1 ==0){
    return(L_temp$L[as.integer(ordinate*n)])
  }
  else if (ordinate*n %% 1 != 0){
    return(mean(c(L_temp$L[as.integer(ordinate*n)], L_temp$L[as.integer(ordinate*n)+1])))
  }
}

Census_9010fix<-function(Census_NatlGB2.df){
#must load Census.work.hh first!
for (i in 1:length(Census_NatlGB2.df$year)){
  if (Census_NatlGB2.df$X9010_ratio[i]>100){
    Census_NatlGB2.df$X9010_ratio[i]<-ratio9010f(subset(Census.work.hh, Census.work.hh$cellmean_equivinc>0.01 &
                                                          Census.work.hh$MSA==Census_NatlGB2.df$MSA[i] & 
                                                          Census.work.hh$year==Census_NatlGB2.df$year[i])$cellmean_equivinc)
  }
}
return(Census_NatlGB2.df)
}

ACS_9010fix<-function(ACS_NatlGB2.df){
  #must load ACS.work.hh first!
  for (i in 1:length(ACS_NatlGB2.df$year)){
    if (ACS_NatlGB2.df$X9010_ratio[i]>100){
      ACS_NatlGB2.df$X9010_ratio[i]<-ratio9010f(subset(ACS.work.hh, ACS.work.hh$cellmean_equivinc>0.01 &
                                                            ACS.work.hh$MSA==ACS_NatlGB2.df$MSA[i] & 
                                                            ACS.work.hh$year==ACS_NatlGB2.df$year[i])$cellmean_equivinc)
    }
  }
  return(ACS_NatlGB2.df)
}
Lorenz_test_result<-function(test_stat){
  if (max(test_stat, na.rm=T)>=3.01&min(test_stat, na.rm=T)>=-3.01){
    return("A dominates B")
  }
  if (min(test_stat, na.rm=T)<=-3.01&max(test_stat, na.rm=T)<=3.01){
    return("B dominates A")
  }
  else if (max(test_stat, na.rm=T)<3.01&min(test_stat, na.rm=T)>-3.01){
    return("No dominance")
  }
  if (min(test_stat, na.rm=T)<=-3.01&max(test_stat, na.rm=T)>=3.01){
    return("Lorenz curves cross")
  }
}
A_dom_B<-function(test_stat){
  if (max(test_stat, na.rm=T)>=3.01&min(test_stat, na.rm=T)>=-3.01){
    return(1)
  }
  else{
    return(0)
  }
}
B_dom_A<-function(test_stat){
  if (min(test_stat, na.rm=T)<=-3.01&max(test_stat, na.rm=T)<=3.01){
    return(1)
  }
  else{
    return(0)
  }
}
Lorenz_cross<-function(test_stat){
  if (min(test_stat, na.rm=T)<=-3.01&max(test_stat, na.rm=T)>=3.01){
    return(1)
  }
  else{
    return(0)
  }
}
lorenz_point_vector<-function(income, weights=c(rep(1, length(income))), ordinates=19){
  n <- length(income)
  L_temp<-Lc(income, n=weights)
  lorenz_ords<-c(rep(0, ordinates))
  if ((i/(ordinates+1))*n %% 1 ==0){
    for (i in 1:ordinates){
      lorenz_ords[i]<-L_temp$L[as.integer((i/(ordinates+1))*n)]
    }
    return(lorenz_ords)
  }
  else if ((i/(ordinates+1))*n %% 1 != 0){
    for (i in 1:ordinates){
      lorenz_ords[i]<-mean(c(L_temp$L[as.integer((i/(ordinates+1))*n)], L_temp$L[as.integer((i/(ordinates+1))*n)+1]))
    }
    return(lorenz_ords)
  }
}
lorenz_var_vector<-function(income, weights=c(rep(1, length(income))), ordinates=19){
  lorenz_var<-c(rep(0, ordinates))
  for (i in 1:ordinates){
    lorenz_var[i]<-var(boot(income, lorenz_point, 200, ordinate=(i/(ordinates+1)))$t)
  }
  return(lorenz_var)
}
library(ggplot2)
library(GB2)
ratio9010f<-function(inc){
  return(quantile(inc, probs=0.9)/(quantile(inc,probs=0.1)+0.000001))
}
ratio95medf<-function(inc){
  return(quantile(inc, probs=0.95)/(quantile(inc,probs=0.5)+0.000001))
}
ratio99medf<-function(inc){
  return(quantile(inc, probs=0.99)/(quantile(inc,probs=0.5)+0.000001))
}
generate_gini_plots<-function(units, inc_data){
  for (i in units){
    temp.df<-subset(inc_data, inc_data$MSA==i)
    temp.MSA<-unlist(strsplit(i, "/"))[1]
    filename<-paste(temp.MSA, ".jpg", sep="")
    jpeg(file=filename)
    print(ggplot(temp.df, aes(year)) + 
            geom_line(aes(y=Gini_GB2, colour="gini"))+
            opts(title=i))
    dev.off()
  }
}
generate_gini_plots_state<-function(units, inc_data, type="gini"){
  for (i in units){
    temp.df<-subset(inc_data, inc_data$State==i)
    temp.MSA<-unlist(strsplit(i, "/"))[1]
    filename<-paste(temp.MSA, "_", type, ".png", sep="")
    if (type=="gini"){
    png(file=filename)
    print(ggplot(temp.df, aes(year)) + 
            geom_line(aes(y=Gini_StateGB2, colour="gini"))+
            opts(title=i))
    dev.off()
    }
    if (type=="top1"){
      png(file=filename)
      print(ggplot(temp.df, aes(year)) + 
              geom_line(aes(y=Top1Share, colour="Top1"))+
              opts(title=i))
      dev.off()      
    }
  }
}
sqrt_eq<-function(pernum){
  return(sqrt(max(pernum)))
}
topcode_sub<-function(inc, fit2){
  if (inc>0){
    bottom<-pgb2(inc, fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4])
    return(qgb2(runif(1,min=bottom, max=1), fit2$opt1$par[1], fit2$opt1$par[2], fit2$opt1$par[3], fit2$opt1$par[4]))
  }
  else{
    return(inc)
  }  
}
top1share<-function(inc, weights=c(rep(1, length(inc)))){
    #temp.df<-subset(data.frame(inc, weights), data.frame(inc, weights)$weights==1)
    top1<-quantile(inc, probs=0.99)
    return(sum(subset(inc, inc>=top1))/sum(inc))
}
ACS_trunc<-function(inc){
  if (inc>0){
    return(1)
  }
  else{
    return(0)
  }
}
X_split<-function(year){
  return(unlist(strsplit(year, "X"))[2])
}
state_strip<-function(msa){
  return(unlist(strsplit(msa, ","))[2])
}
DC_switch<-function(state){
  if (state=="Washington DC"){
    return("District of Columbia")
  }
  else{
    return(state)
  }
}
lorenz_point<-function(income, weights=c(rep(1, length(income))), ordinate=0.5){
  n <- length(income)
  L_temp<-Lc(income, n=weights)
  if (ordinate*n %% 1 ==0){
    return(L_temp$L[as.integer(ordinate*n)])
  }
  else if (ordinate*n %% 1 != 0){
    return(mean(c(L_temp$L[as.integer(ordinate*n)], L_temp$L[as.integer(ordinate*n)+1])))
  }
}

Census_9010fix<-function(Census_NatlGB2.df){
#must load Census.work.hh first!
for (i in 1:length(Census_NatlGB2.df$year)){
  if (Census_NatlGB2.df$X9010_ratio[i]>100){
    Census_NatlGB2.df$X9010_ratio[i]<-ratio9010f(subset(Census.work.hh, Census.work.hh$cellmean_equivinc>0.01 &
                                                          Census.work.hh$MSA==Census_NatlGB2.df$MSA[i] & 
                                                          Census.work.hh$year==Census_NatlGB2.df$year[i])$cellmean_equivinc)
  }
}
return(Census_NatlGB2.df)
}

ACS_9010fix<-function(ACS_NatlGB2.df){
  #must load ACS.work.hh first!
  for (i in 1:length(ACS_NatlGB2.df$year)){
    if (ACS_NatlGB2.df$X9010_ratio[i]>100){
      ACS_NatlGB2.df$X9010_ratio[i]<-ratio9010f(subset(ACS.work.hh, ACS.work.hh$cellmean_equivinc>0.01 &
                                                            ACS.work.hh$MSA==ACS_NatlGB2.df$MSA[i] & 
                                                            ACS.work.hh$year==ACS_NatlGB2.df$year[i])$cellmean_equivinc)
    }
  }
  return(ACS_NatlGB2.df)
}
Lorenz_test_result<-function(test_stat){
  if (max(test_stat, na.rm=T)>=3.01&min(test_stat, na.rm=T)>=-3.01){
    return("A dominates B")
  }
  if (min(test_stat, na.rm=T)<=-3.01&max(test_stat, na.rm=T)<=3.01){
    return("B dominates A")
  }
  else if (max(test_stat, na.rm=T)<3.01&min(test_stat, na.rm=T)>-3.01){
    return("No dominance")
  }
  if (min(test_stat, na.rm=T)<=-3.01&max(test_stat, na.rm=T)>=3.01){
    return("Lorenz curves cross")
  }
}
A_dom_B<-function(test_stat){
  if (max(test_stat, na.rm=T)>=3.01&min(test_stat, na.rm=T)>=-3.01){
    return(1)
  }
  else{
    return(0)
  }
}
B_dom_A<-function(test_stat){
  if (min(test_stat, na.rm=T)<=-3.01&max(test_stat, na.rm=T)<=3.01){
    return(1)
  }
  else{
    return(0)
  }
}
Lorenz_cross<-function(test_stat){
  if (min(test_stat, na.rm=T)<=-3.01&max(test_stat, na.rm=T)>=3.01){
    return(1)
  }
  else{
    return(0)
  }
}
lorenz_point_vector<-function(income, weights=c(rep(1, length(income))), ordinates=19){
  n <- length(income)
  L_temp<-Lc(income, n=weights)
  lorenz_ords<-c(rep(0, ordinates))
  if ((i/(ordinates+1))*n %% 1 ==0){
    for (i in 1:ordinates){
      lorenz_ords[i]<-L_temp$L[as.integer((i/(ordinates+1))*n)]
    }
    return(lorenz_ords)
  }
  else if ((i/(ordinates+1))*n %% 1 != 0){
    for (i in 1:ordinates){
      lorenz_ords[i]<-mean(c(L_temp$L[as.integer((i/(ordinates+1))*n)], L_temp$L[as.integer((i/(ordinates+1))*n)+1]))
    }
    return(lorenz_ords)
  }
}
lorenz_var_vector<-function(income, weights=c(rep(1, length(income))), ordinates=19){
  lorenz_var<-c(rep(0, ordinates))
  for (i in 1:ordinates){
    lorenz_var[i]<-var(boot(income, lorenz_point, 200, ordinate=(i/(ordinates+1)))$t)
  }
  return(lorenz_var)
}
jackknife_ineq<-function(inc, statistic, weights=NULL){
  #Note: this will produce jackknife estimates of variance for the inequality statistics used in the GB2 imputation. 
  #Requires a parallel backend to be registered first
  jack <- numeric(length(inc)-1)
  pseudo <- numeric(length(inc)) 
  if (statistic=="gini"){
    pseudo <- foreach (i=1:(length(inc)), .combine=c)%dopar%{
      for (j in 1:(length(inc))){
        if (j<i){
          jack[j]<-inc[j]
        }
        else if (j>i){
          jack[j-1]<-inc[j]
        }
      }
      length(inc)*gini(inc) - (length(inc)-1)*gini(jack)
    }
    return(var(pseudo))
  }
  if (statistic=="theil"){
    pseudo <- foreach (i=1:length(inc), .combine=c)%dopar%{
      for (j in 1:(length(inc))){
        if (j<i){
          jack[j]<-inc[j]
        }
        else if (j>i){
          jack[j-1]<-inc[j]
        }
      }
      length(inc)*Theil(inc) - (length(inc)-1)*Theil(jack)
    }
    return(var(pseudo))
  }
  if (statistic=="9010"){
    pseudo <- foreach (i=1:length(inc), .combine=c)%dopar%{
      for (j in 1:(length(inc))){
        if (j<i){
          jack[j]<-inc[j]
        }
        else if (j>i){
          jack[j-1]<-inc[j]
        }
      }
      length(inc)*ratio9010f(inc) - (length(inc)-1)*ratio9010f(jack)
    }
    return(var(pseudo))
  }
  if (statistic=="top1"){
    pseudo <- foreach (i=1:length(inc), .combine=c)%dopar%{
      for (j in 1:(length(inc))){
        if (j<i){
          jack[j]<-inc[j]
        }
        else if (j>i){
          jack[j-1]<-inc[j]
        }
      }
      length(inc)*top1share(inc) - (length(inc)-1)*top1share(jack)
    }
    return(var(pseudo))
  }
  if (statistic=="gini_w"){
    weight_j<-numeric(length(inc)-1)
    pseudo <- foreach (i=1:(length(inc)), .combine=c)%dopar%{
      for (j in 1:(length(inc))){
        if (j<i){
          jack[j]<-inc[j]
          weight_j[j]<-weights[j]
        }
        else if (j>i){
          jack[j-1]<-inc[j]
          weight_j[j-1]<-weights[j]
        }
      }
      length(inc)*gini(inc, w=weights) - (length(inc)-1)*gini(jack, w=weight_j)
    }
    return(var(pseudo))
  }
}

boot9010<-function(data, indices){
  d<-data[indices]
  return(quantile(d, probs=0.9)/quantile(d, probs=0.1))
}
boottop1<-function(data, indices){
   d<-data[indices]
   perc99<-quantile(d, probs=0.99)
   return(sum(subset(d, d>=perc99))/sum(d))
}

Is_positive<-function(num){
  if (is.na(num)==T){
    return(NA)
  }
  else if (num>0){
    return(1)
  }
  else{
    return(0)
  }
}

S_phi <- function(Lc1, Lc2, max_n){
  #Use a grid of 0.1 times the largest LC vector (500 for now)
  #max_size<-max(c(length(Lc1$p), length(Lc2$p)))
  max_size<-max_n
  grid <- seq(0+1/(max_size), 1-1/(max_size), 1/(max_size))
  phi <- numeric(length(grid))
  for (i in 1:length(grid)){
    phi[i]<-Lc2$L[as.integer(grid[i]*length(Lc2$L))]-Lc1$L[as.integer(grid[i]*length(Lc1$L))]
  }
  return(phi)
}
I_phi <-  function(Lc1, Lc2, max_n, min_fallback){
  max_size<-min(max_n, min_fallback)
  grid <- seq(0+1/(max_size), 1-1/(max_size), 1/(max_size))
  phi <- numeric(length(grid))
  phi<-Lc2$L[as.integer(grid*length(Lc2$L))]-Lc1$L[as.integer(grid*length(Lc1$L))]
  phi<- apply(data.frame(phi), 1, Is_positive)
  return((1/(max_size))*sum(phi))
}
I_phi_star<-function(Lc1_star, Lc2_star, Lc1, Lc2, max_n, min_fallback){
  max_size<-min(max_n, min_fallback)
  
  grid <- seq(0+1/(max_size), 1-1/(max_size), 1/(max_size))
  phi<-(Lc2_star$L[as.integer(grid*length(Lc2_star$L))]-Lc1_star$L[as.integer(grid*length(Lc1_star$L))]) - (Lc2$L[as.integer(grid*length(Lc2$L))]-Lc1$L[as.integer(grid*length(Lc1$L))])
  phi<- apply(data.frame(phi), 1, Is_positive)
  return((1/(max_size))*sum(phi))
}
S_phi_bootstrap<-function(pop1, pop2, reps, gridfactor){
  max_n<-gridfactor*min(c(length(pop1),length(pop2)))
  pop_phi<-S_phi(Lc(pop1), Lc(pop2), max_n)
  T_n <- (as.numeric(length(pop1))*as.numeric(length(pop2)))/(as.numeric(length(pop1))+as.numeric(length(pop2)))
  phi_hat <- foreach(i=1:reps, .combine=c)%do%{
    T_n * max(S_phi(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), max_n)-pop_phi)-T_n*max(pop_phi)
  }
  phi_hat<-apply(data.frame(phi_hat), 1, Is_positive)
  #   phi_hat<-numeric(reps)
  #   for (i in 1:reps){
  #     phi_hat[i]<-T_n * max(S_phi(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), max_n)-pop_phi)
  #   }
  return(sum(phi_hat, na.rm=T))
  #return(phi_hat)
  #return(ecdf(phi_hat)(T_n*max(pop_phi)))  
}
I_phi_bootstrap<-function(pop1, pop2, reps, gridfactor, min_fallback){
  max_n<-gridfactor*min(c(length(pop1),length(pop2)))
  pop_phi<-I_phi(Lc(pop1), Lc(pop2), max_n, min_fallback)
  T_n <- (as.numeric(length(pop1))*as.numeric(length(pop2)))/(as.numeric(length(pop1))+as.numeric(length(pop2)))
  phi_hat <- foreach(i=1:reps, .combine=c)%do%{
    T_n * I_phi_star(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), Lc(pop1), Lc(pop2), max_n, min_fallback)-T_n*pop_phi
  }
  phi_hat<-apply(data.frame(phi_hat), 1, Is_positive)
  #   phi_hat<-numeric(reps)
  #   for (i in 1:reps){
  #     phi_hat[i]<-T_n * max(S_phi(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), max_n)-pop_phi)
  #   }
  return(sum(phi_hat, na.rm=T))
  #return(phi_hat)
  #return(ecdf(phi_hat)(T_n*max(pop_phi)))  
}
phi_bootstrap<-function(pop1, pop2, reps){
  max_n<-min(c(length(pop1),length(pop2)))
  pop_phi<-I_phi(Lc(pop1), Lc(pop2), max_n)
  T_n <- (as.numeric(length(pop1))*as.numeric(length(pop2)))/(as.numeric(length(pop1))+as.numeric(length(pop2)))
  phi_hat <- foreach(i=1:reps, .combine=c)%do%{
    T_n * I_phi_star(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), Lc(pop1), Lc(pop2), max_n)-T_n*pop_phi
  }
  phi_hat<-apply(data.frame(phi_hat), 1, Is_positive)
  #   phi_hat<-numeric(reps)
  #   for (i in 1:reps){
  #     phi_hat[i]<-T_n * max(S_phi(Lc(sample(pop1, replace=T)), Lc(sample(pop2, replace=T)), max_n)-pop_phi)
  #   }
  return(sum(phi_hat, na.rm=T))
  #return(phi_hat)
  #return(ecdf(phi_hat)(T_n*max(pop_phi)))  
}

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
Lorenz_KB<-function(inc, weight="default", ordinate, type="mean"){
  if (weight=="default"){
    weight<-rep(1, length(inc))
  }
  #K&B survey sampling weights not implemented yet.
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
