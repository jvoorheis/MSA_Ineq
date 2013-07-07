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