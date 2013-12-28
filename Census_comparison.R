library(data.table)
library(ggplot2)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_topcode_hh1.rda")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_GB2_newdata.rda")
colnames(NatlGB2.df)<-c("MSA", "year", "Gini_NatlGB2", "Gini_NatlGB2_w", "Theil", "X9010_ratio", "Top1Share")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_MSA_NatlGB2_729_cellmeanfit.rda")
Census_NatlGB2.df[,1:2]<-sapply(Census_NatlGB2.df[,1:2], as.character)
Census_NatlGB2.df[,2:2]<-sapply(Census_NatlGB2.df[,2:2], as.numeric)
Census_NatlGB2.df<-Census_9010fix(Census_NatlGB2.df)
Census_Natl_cellmean<-Census_NatlGB2.df
colnames(Census_Natl_cellmean)<-c("MSA", "year", "Natl_cellmean_gini", "Natl_cellmean_gini_w", "Natl_cellmean_theil", 	"Natl_cellmean_9010", "Natl_cellmean_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_MSA_NatlGB2_729_topcodefit.rda")
Census_NatlGB2.df[,1:2]<-sapply(Census_NatlGB2.df[,1:2], as.character)
Census_NatlGB2.df[,2:2]<-sapply(Census_NatlGB2.df[,2:2], as.numeric)
Census_NatlGB2.df<-Census_9010fix(Census_NatlGB2.df)
Census_Natl_topcode<-Census_NatlGB2.df
colnames(Census_Natl_topcode)<-c("MSA", "year", "Natl_topcode_gini", "Natl_topcode_gini_w", "Natl_topcode_theil", 	"Natl_topcode_9010", "Natl_topcode_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_MSAGB2_729_cellmeanfit.rda")
Census_MSAGB2.df[,1:2]<-sapply(Census_MSAGB2.df[,1:2], as.character)
Census_MSAGB2.df[,2:2]<-sapply(Census_MSAGB2.df[,2:2], as.numeric)
Census_MSAGB2.df<-Census_9010fix(Census_MSAGB2.df)
Census_MSA_cellmean<-Census_MSAGB2.df
colnames(Census_MSA_cellmean)<-c("MSA", "year", "MSA_cellmean_gini","MSA_cellmean_gini_w",  "MSA_cellmean_theil",   "MSA_cellmean_9010", "MSA_cellmean_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_MSAGB2_729_topcodefit.rda")
Census_MSAGB2.df[,1:2]<-sapply(Census_MSAGB2.df[,1:2], as.character)
Census_MSAGB2.df[,2:2]<-sapply(Census_MSAGB2.df[,2:2], as.numeric)
Census_MSAGB2.df<-Census_9010fix(Census_MSAGB2.df)
Census_MSA_topcode<-Census_MSAGB2.df
colnames(Census_MSA_topcode)<-c("MSA", "year", "MSA_topcode_gini",  "MSA_topcode_gini_w",  "MSA_topcode_theil",   "MSA_topcode_9010", "MSA_topcode_top1")

Census_Natl_cellmean<-data.table(Census_Natl_cellmean)
Census_Natl_topcode<-data.table(Census_Natl_topcode)
Census_MSA_cellmean<-data.table(Census_MSA_cellmean)
Census_MSA_topcode<-data.table(Census_MSA_topcode)
setkey(Census_Natl_cellmean, MSA, year)
setkey(Census_Natl_topcode, MSA, year)
setkey(Census_MSA_cellmean, MSA, year)
setkey(Census_MSA_topcode, MSA, year)
Census_joint<-Census_Natl_cellmean[Census_Natl_topcode]
setkey(Census_joint, MSA, year)
Census_joint<-Census_joint[Census_MSA_topcode]
setkey(Census_joint, MSA, year)
Census_joint<-Census_joint[Census_MSA_cellmean]

MSA.unique<-unique(Census_MSAGB2.df$MSA)
MSA<-c()
year<-c()
Natl_cellmean_gini<-c()
Natl_topcode_gini<-c()
MSA_cellmean_gini<-c()
MSA_topcode_gini<-c()
Natl_cellmean_gini_w<-c()
Natl_topcode_gini_w<-c()
MSA_cellmean_gini_w<-c()
MSA_topcode_gini_w<-c()
Natl_cellmean_top1<-c()
Natl_topcode_top1<-c()
MSA_cellmean_top1<-c()
MSA_topcode_top1<-c()
Natl_cellmean_theil<-c()
Natl_topcode_theil<-c()
MSA_cellmean_theil<-c()
MSA_topcode_theil<-c()
Natl_cellmean_9010<-c()
Natl_topcode_9010<-c()
MSA_cellmean_9010<-c()
MSA_topcode_9010<-c()


for (i in MSA.unique){
  temp.data<-subset(Census_joint, Census_joint$MSA==i)
  year.unique<-unique(temp.data$year)
  #gini
  temp.gini<-spline(temp.data$year, temp.data$Natl_cellmean_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_gini<-append(Natl_cellmean_gini, temp.gini$y)
  temp.gini<-spline(temp.data$year, temp.data$Natl_topcode_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_gini<-append(Natl_topcode_gini, temp.gini$y)  
  temp.gini<-spline(temp.data$year, temp.data$MSA_cellmean_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_cellmean_gini<-append(MSA_cellmean_gini, temp.gini$y)
  temp.gini<-spline(temp.data$year, temp.data$MSA_topcode_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_topcode_gini<-append(MSA_topcode_gini, temp.gini$y)
  #gini using Census weights
  temp.gini<-spline(temp.data$year, temp.data$Natl_cellmean_gini_w, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_gini_w<-append(Natl_cellmean_gini_w, temp.gini$y)
  temp.gini<-spline(temp.data$year, temp.data$Natl_topcode_gini_w, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_gini_w<-append(Natl_topcode_gini_w, temp.gini$y)  
  temp.gini<-spline(temp.data$year, temp.data$MSA_cellmean_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_cellmean_gini_w<-append(MSA_cellmean_gini_w, temp.gini$y)
  temp.gini<-spline(temp.data$year, temp.data$MSA_topcode_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_topcode_gini_w<-append(MSA_topcode_gini_w, temp.gini$y)
  #top1
  temp.top1<-spline(temp.data$year, temp.data$Natl_cellmean_top1, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_top1<-append(Natl_cellmean_top1, temp.top1$y)
  temp.top1<-spline(temp.data$year, temp.data$Natl_topcode_top1, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_top1<-append(Natl_topcode_top1, temp.top1$y)  
  temp.top1<-spline(temp.data$year, temp.data$MSA_cellmean_top1, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_cellmean_top1<-append(MSA_cellmean_top1, temp.top1$y)
  temp.top1<-spline(temp.data$year, temp.data$MSA_topcode_top1, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_topcode_top1<-append(MSA_topcode_top1, temp.top1$y)
  #Theil
  temp.theil<-spline(temp.data$year, temp.data$Natl_cellmean_theil, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_theil<-append(Natl_cellmean_theil, temp.theil$y)
  temp.theil<-spline(temp.data$year, temp.data$ Natl_topcode_theil, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_theil<-append(Natl_topcode_theil, temp.theil$y)  
  temp.theil<-spline(temp.data$year, temp.data$MSA_cellmean_theil, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_cellmean_theil<-append(MSA_cellmean_theil, temp.theil$y)
  temp.theil<-spline(temp.data$year, temp.data$MSA_topcode_theil, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_topcode_theil<-append(MSA_topcode_theil, temp.theil$y)

  #90-10 ratio
  #Theil
  temp.9010<-spline(temp.data$year, temp.data$Natl_cellmean_9010, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_9010<-append(Natl_cellmean_9010, temp.9010$y)
  temp.9010<-spline(temp.data$year, temp.data$ Natl_topcode_9010, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_9010<-append(Natl_topcode_9010, temp.9010$y)  
  temp.9010<-spline(temp.data$year, temp.data$MSA_cellmean_9010, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_cellmean_9010<-append(MSA_cellmean_9010, temp.9010$y)
  temp.9010<-spline(temp.data$year, temp.data$MSA_topcode_9010, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA_topcode_9010<-append(MSA_topcode_9010, temp.9010$y)
  #MSA, year identifiers
  MSA<-append(MSA, rep(i, length(c(min(temp.data$year):max(temp.data$year)))))
  year<-append(year, c(min(temp.data$year):max(temp.data$year)))
}
Census_interpolated<-data.frame(MSA, year, Natl_cellmean_gini, Natl_topcode_gini, MSA_cellmean_gini, MSA_topcode_gini,
                                Natl_cellmean_gini_w, Natl_topcode_gini_w, MSA_cellmean_gini_w, MSA_topcode_gini_w,
                                Natl_cellmean_top1, Natl_topcode_top1, MSA_cellmean_top1, MSA_topcode_top1,
                                Natl_cellmean_theil, Natl_topcode_theil, MSA_cellmean_theil, MSA_topcode_theil,
                                Natl_cellmean_9010, Natl_topcode_9010, MSA_cellmean_9010, MSA_topcode_9010)
save(Census_interpolated, file="Census_interpolated_MSA.rda")

NatlGB2.df<-data.table(NatlGB2.df)
setkey(NatlGB2.df, MSA, year)
Census_interpolated<-data.table(Census_interpolated)
setkey(Census_interpolated, MSA, year)
#try2<-NatlGB2.df[Census_interpolated]
try2<-Census_interpolated[NatlGB2.df]

summary(lm(Gini_NatlGB2~Natl_topcode_gini, data=try2))
summary(lm(Gini_NatlGB2~Natl_cellmean_gini, data=try2))
summary(lm(Gini_NatlGB2~MSA_topcode_gini, data=try2))
summary(lm(Gini_NatlGB2~MSA_cellmean_gini, data=try2))
summary(lm(Gini_NatlGB2_w~Natl_topcode_gini_w, data=try2))
summary(lm(Gini_NatlGB2_w~Natl_cellmean_gini_w, data=try2))
summary(lm(Gini_NatlGB2_w~MSA_topcode_gini_w, data=try2))
summary(lm(Gini_NatlGB2_w~MSA_cellmean_gini_w, data=try2))
summary(lm(Top1Share~Natl_topcode_top1, data=try2))
summary(lm(Top1Share~Natl_cellmean_top1, data=try2))
summary(lm(Top1Share~MSA_topcode_top1, data=try2))
summary(lm(Top1Share~MSA_cellmean_top1, data=try2))
summary(lm(Theil~Natl_topcode_theil, data=try2))
summary(lm(Theil~Natl_cellmean_theil, data=try2))
summary(lm(Theil~MSA_topcode_theil, data=try2))
summary(lm(Theil~MSA_cellmean_theil, data=try2))
summary(lm(X9010_ratio~Natl_topcode_9010, data=try2))
summary(lm(X9010_ratio~Natl_cellmean_9010, data=try2))
summary(lm(X9010_ratio~MSA_topcode_9010, data=try2))
summary(lm(X9010_ratio~MSA_cellmean_9010, data=try2))


setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Census_compare/")
MSA.unique<-unique(try2$MSA)
for (i in MSA.unique){
  temp.df<-subset(try2, try2$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, "_top1", ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Top1Share, colour="Top1Share"))+
          geom_line(aes(y=Natl_topcode_top1, colour="Natl_topcode_top1"))+
          geom_line(aes(y=Natl_cellmean_top1, colour="Natl_cellmean_top1"))+
          geom_line(aes(y=MSA_topcode_top1, colour="MSA_topcode_top1"))+
          geom_line(aes(y=MSA_cellmean_top1, colour="MSA_cellmean_top1"))+
          opts(title=i))
  dev.off()
}
for (i in MSA.unique){
  temp.df<-subset(try2, try2$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA,  ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Gini_NatlGB2_w, colour="Gini_NatlGB2"))+
          geom_line(aes(y=Natl_topcode_gini_w, colour="Natl_topcode_gini"))+
          geom_line(aes(y=Natl_cellmean_gini_w, colour="Natl_cellmean_gini"))+
          geom_line(aes(y=MSA_topcode_gini_w, colour="MSA_topcode_gini"))+
          geom_line(aes(y=MSA_cellmean_gini_w, colour="MSA_cellmean_gini"))+
          opts(title=i))
  dev.off()
}
for (i in MSA.unique){
  temp.df<-subset(try2, try2$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, "_9010", ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=X9010_ratio, colour="X9010_ratio"))+
          geom_line(aes(y=Natl_topcode_9010, colour="Natl_topcode_9010"))+
          geom_line(aes(y=Natl_cellmean_9010, colour="Natl_cellmean_9010"))+
          geom_line(aes(y=MSA_topcode_9010, colour="MSA_topcode_9010"))+
          geom_line(aes(y=MSA_cellmean_9010, colour="MSA_cellmean_9010"))+
          opts(title=i))
  dev.off()
}

