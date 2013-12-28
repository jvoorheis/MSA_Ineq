library(reldist)

random_sampleF<-runif(100, 0, 10)
random_sampleG<-runif(100, 0, 10)
Lc_G<-Lc(random_sampleG)
Lc_F<-Lc(random_sampleF)
S_G<-sqrt((100*100)/(100+100))*(max(Lc_G$L-Lc_F$L))
print(S_G)

Lorenz_difference<-function(lorenz1, lorenz2){
  if (length(lorenz1$L)==length(lorenz2$L)){
    return(max(lorenz1$L-lorenz2$L))
  }
  else{
  min_length<-min(c(length(lorenz1$L), length(lorenz2$L)))
  max_length<-max(c(length(lorenz1$L), length(lorenz2$L)))
  if (length(lorenz1$L)==min_length){
    new_lorenz<-c(rep(0, min_length))
    for (i in 1:min_length){
      new_lorenz[i]<-lorenz2[as.integer((i/min_length)*max_length)]
    }
    return(max(lorenz1$L-newlorenz))
  }
  if (length(lorenz2$L)==min_length){
    new_lorenz<-c(rep(0, min_length))
    for (i in 1:min_length){
      new_lorenz[i]<-lorenz1[as.integer((i/min_length)*max_length)]
    }
    return(max(newlorenz-lorenz2$L))
  }
  }
}