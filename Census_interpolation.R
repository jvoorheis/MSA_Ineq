setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/")
load("ACS_NatlGB2_73_topcodedfit.rda")
load("Census_NatlGB2_73_topcodedfit.rda")
library(data.table)
library(ggplot2)

MSA.unique<-unique(Census_NatlGB2.df$MSA)
ACS_NatlGB2.df<-data.table(ACS_NatlGB2.df)
Census_NatlGB2.df<-data.table(Census_NatlGB2.df)
Census_NatlGB2.df$year<-as.character(Census_NatlGB2.df$year)
Census_NatlGB2.df$year<-as.numeric(Census_NatlGB2.df$year)
ACS_NatlGB2.df$year<-as.character(ACS_NatlGB2.df$year)
ACS_NatlGB2.df$year<-as.numeric(ACS_NatlGB2.df$year)
setkey(ACS_NatlGB2.df, MSA, year)
setkey(Census_NatlGB2.df, MSA, year)

MSA<-c()
year<-c()
Census_gini<-c()

for (i in MSA.unique){
  temp.data<-subset(Census_NatlGB2.df, Census_NatlGB2.df$MSA==i)
  year.unique<-unique(temp.data$year)
  temp.gini<-spline(temp.data$year, temp.data$Gini_NatlGB2, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA<-append(MSA, rep(i, length(c(min(temp.data$year):max(temp.data$year)))))
  year<-append(year, c(min(temp.data$year):max(temp.data$year)))
  Census_gini<-append(Census_gini, temp.gini$y)
}
Census_interpolated<-data.frame(MSA, year, Census_gini)
save(Census_interpolated, file="Census_interpolated_MSA.rda")


setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Census/")
for (i in MSA.unique){
  temp.df<-subset(Census_interpolated, Census_interpolated$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, ".jpg", sep="")
  jpeg(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Census_gini, colour="Census_gini"))+
          opts(title=i))
  dev.off()
}

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_cellmean_gini.rda")
Census_cellmean<-data.table(Census_cellmean)
Census_cellmean$year<-as.character(Census_cellmean$year)
Census_cellmean$year<-as.numeric(Census_cellmean$year)
setkey(Census_cellmean, MSA, year)
MSA<-c()
year<-c()
cellmean_gini<-c()
for (i in MSA.unique){
  temp.data<-subset(Census_cellmean, Census_cellmean$MSA==i)
  year.unique<-unique(temp.data$year)
  temp.gini<-spline(temp.data$year, temp.data$cellmean_equivinc, n=length(c(min(temp.data$year):max(temp.data$year))))
  MSA<-append(MSA, rep(i, length(c(min(temp.data$year):max(temp.data$year)))))
  year<-append(year, c(min(temp.data$year):max(temp.data$year)))
  cellmean_gini<-append(cellmean_gini, temp.gini$y)
}
cellmean_interpolated<-data.frame(MSA, year, cellmean_gini)
save(cellmean_interpolated, file="cellmean_interpolated_MSA.rda")

load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/NatlGB2_72.rda")
NatlGB2.df<-data.table(NatlGB2.df)
Census_interpolated<-data.table(Census_interpolated)
setkey(NatlGB2.df, MSA, year)
setkey(Census_interpolated, MSA, year)
try1<-NatlGB2.df[Census_interpolated]
#try1<-try1[cellmean_interpolated]
try1<-subset(try1, is.na(try1$Gini_NatlGB2)==F)
try1$Avg_Gini<-(try1$Gini_NatlGB2+try1$Census_gini)/2
try1$Avg_cellmean<-(try1$Gini_NatlGB2+try1$cellmean_gini)/2



setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/EqualWeights/")
MSA.unique<-unique(try1$MSA)
for (i in MSA.unique){
  temp.df<-subset(try1, try1$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Avg_Gini, colour="Avg_Gini"))+
          geom_line(aes(y=Gini_NatlGB2, colour="Gini_NatlGB2"))+
          geom_line(aes(y=Census_gini, colour="Census_gini"))+
          opts(title=i))
  dev.off()
}

ACS_NatlGB2.df<-data.table(ACS_NatlGB2.df)
ACS_NatlGB2.df$year<-as.character(ACS_NatlGB2.df$year)
ACS_NatlGB2.df$year<-as.numeric(ACS_NatlGB2.df$year)
setkey(ACS_NatlGB2.df, MSA, year)
try2<-ACS_NatlGB2.df[try1]
colnames(try2)[3]<-"ACS_gini"



setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/EqualWeights/")
MSA.unique<-unique(try2$MSA)
for (i in MSA.unique){
  temp.df<-subset(try2, try1$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Avg_Gini, colour="Avg_Gini"))+
          geom_line(aes(y=Gini_NatlGB2.1, colour="Gini_NatlGB2.1"))+
          geom_line(aes(y=Census_gini, colour="Census_gini"))+
          geom_line(aes(y=ACS_gini, colour="ACS_gini"))+
          opts(title=i))
  dev.off()
}

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/EqualWeights/")
MSA.unique<-unique(try2$MSA)
for (i in MSA.unique){
  temp.df<-subset(try2, try1$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Avg_cellmean, colour="Average_gini"))+
          opts(title=i))
  dev.off()
}