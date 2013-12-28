quantile_effects_union <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/quantile_effects_union.csv")
quantile_effects_humancap <- read.csv("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/quantile_effects_humancap.csv")
library(ggplot2)
ineq_meas<-unique(quantile_effects_union$Inequality.Measure)
quantile_effects_union$zero<-0
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Quantile_effects")
for (i in ineq_meas){
  temp.df1<-subset(quantile_effects_union, quantile_effects_union$Inequality.Measure==i)
  temp.df2<-subset(quantile_effects_humancap, quantile_effects_humancap$Inequality.Measure==i)  
  filename1 <- paste("QPE_union_", as.character(i), ".png", sep="" )
  filename2 <- paste("QPE_humancap_", as.character(i), ".png", sep="" )
  png(file=filename1)
  print(ggplot(temp.df1, aes(Quantile)) + 
          geom_line(aes(y=Point, colour="Point Estimate"))+
          geom_line(aes(y=LB, colour="95% CI Lower Bound"))+
          geom_line(aes(y=UB, colour="95% CI Upper Bound"))+
          geom_hline(yintercept=0)+
          
          opts(title=i))
  dev.off()
  png(file=filename2)
  print(ggplot(temp.df2, aes(Quantile)) + 
          geom_line(aes(y=Point, colour="Point Estimate"))+
          geom_line(aes(y=LB, colour="95% CI Lower Bound"))+
          geom_line(aes(y=UB, colour="95% CI Upper Bound"))+
          geom_hline(yintercept=0)+
          
          opts(title=i))
  dev.off()

}