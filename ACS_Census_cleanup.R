library(data.table)
library(ineq)
library(reldist)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_NatlGB2_627.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_topcode_hh1.rda")\
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/ACS_topcode_hh1.rda")

Census.MSA.unique<-unique(Census.work.hh$MSA)
Census.MSA.unique<-data.frame(Census.MSA.unique)
Census.MSA.unique$MSA<-Census.MSA.unique$Census.MSA.unique
Census.MSA.unique$MSA_label<-Census.MSA.unique$Census.MSA.unique
Census.MSA.unique$MSA<-as.numeric(Census.MSA.unique$MSA)
Census.MSA.unique$Census.MSA.unique<-NULL
Census.MSA.unique$MSA_label<-as.character(Census.MSA.unique$MSA_label)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
save(Census.MSA.unique, file="Census_MSA_unique.rda")

ACS.MSA.unique<-data.table(ACS.MSA.unique)
ACS_NatlGB2.df<-data.table(ACS_NatlGB2.df)
setkey(ACS.MSA.unique, MSA)
setkey(ACS_NatlGB2.df, MSA)
Census.MSA.unique<-data.table(Census.MSA.unique)
Census_NatlGB2.df<-data.table(Census_NatlGB2.df)
setkey(Census.MSA.unique, MSA)
setkey(Census_NatlGB2.df, MSA)
ACS_NatlGB2.df1<-ACS_NatlGB2.df[ACS.MSA.unique]
Census_NatlGB2.df1<-Census_NatlGB2.df[Census.MSA.unique]
ACS_NatlGB2.df<-ACS_NatlGB2.df1
Census_NatlGB2.df<-Census_NatlGB2.df1
ACS_NatlGB2.df$MSA<-NULL
Census_NatlGB2.df$MSA<-NULL
colnames(ACS_NatlGB2.df)[6]<-"MSA"
colnames(Census_NatlGB2.df)[6]<-"MSA"
ACS_NatlGB2.df$year<-as.character(ACS_NatlGB2.df$year)
ACS_NatlGB2.df$year<-as.numeric(ACS_NatlGB2.df$year)
Census_NatlGB2.df$year<-as.character(Census_NatlGB2.df$year)
Census_NatlGB2.df$year<-as.numeric(Census_NatlGB2.df$year)

save(ACS_NatlGB2.df, file="ACS_NatlGB2_71.rda")
save(Census_NatlGB2.df, file="Census_NatlGB2_71.rda")
ACS.work.hh$MSA<-as.character(ACS.work.hh$MSA)
ACS.cellmean<-aggregate(cellmean_equivinc~MSA+year, data=ACS.work.hh, FUN=gini)
colnames(ACS.cellmean)[3]<-"ACS_cellmean_gini"
save(ACS.cellmean, file="ACS_cellmean.rda")
Census.work.hh$MSA<-as.character(Census.work.hh$MSA)
Census_cellmean<-aggregate(cellmean_equivinc~MSA+year, data=Census.work.hh, FUN=gini)
save(Census_cellmean, file="Census_cellmean_gini.rda")