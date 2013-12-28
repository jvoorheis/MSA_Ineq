library(data.table)
library(ineq)
library(reldist)
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_MSAGB2_79_cellmean.rda")
cellmean_MSA<-MSAGB2.df
colnames(cellmean_MSA)<-c("MSA", "year", "CPS_Gini_MSA_cellmean", "CPS_Gini_w_MSA_cellmean", 
                          "CPS_Theil_MSA_cellmean", "CPS_9010_ratio_MSA_cellmean", "CPS_Top1Share_MSA_cellmean")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_NatlGB2_723_cellmean.rda")
cellmean_Natl<-NatlGB2.df
colnames(cellmean_Natl)<-c("MSA", "year", "CPS_Gini_Natl_cellmean", "CPS_Gini_w_Natl_cellmean", 
                          "CPS_Theil_Natl_cellmean", "CPS_9010_ratio_Natl_cellmean", "CPS_Top1Share_Natl_cellmean")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_NatlGB2_723_cellmean.rda")
Larrimore_Natl<-NatlGB2.df
colnames(Larrimore_Natl)<-c("MSA", "year", "CPS_Gini_Natl_Larrimore", "CPS_Gini_w_Natl_Larrimore", 
                           "CPS_Theil_Natl_Larrimore", "CPS_9010_ratio_Natl_Larrimore", "CPS_Top1Share_Natl_Larrimore")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_MSAGB2_723_cellmean.rda")
Larrimore_MSA<-MSAGB2.df
colnames(Larrimore_MSA)<-c("MSA", "year", "CPS_Gini_MSA_Larrimore", "CPS_Gini_w_MSA_Larrimore", 
                          "CPS_Theil_MSA_Larrimore", "CPS_9010_ratio_MSA_Larrimore", "CPS_Top1Share_MSA_Larrimore")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")
Cellmean_gini<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=gini)
Larrimore_gini<-aggregate(sqrt_CPScellmean~MSA+year, data=CPS.work.hh, FUN=gini)
colnames(Cellmean_gini)[3]<-"Gini_RegCellmean"
colnames(Larrimore_gini)[3]<-"Gini_ExtCellmean"

Larrimore_MSA<-data.table(Larrimore_MSA)
cellmean_MSA<-data.table(cellmean_MSA)
Larrimore_Natl<-data.table(Larrimore_Natl)
cellmean_Natl<-data.table(cellmean_Natl)
Cellmean_gini<-data.table(Cellmean_gini)
Larrimore_gini<-data.table(Larrimore_gini)
setkey(Larrimore_MSA, MSA, year)
setkey(cellmean_MSA, MSA, year)
setkey(Larrimore_Natl, MSA, year)
setkey(cellmean_Natl, MSA, year)
setkey(Cellmean_gini, MSA, year)
setkey(Larrimore_gini, MSA,  year)

merged_CPS<-Larrimore_MSA[cellmean_MSA]
setkey(merged_CPS, MSA, year)
merged_CPS<-merged_CPS[Larrimore_Natl]
setkey(merged_CPS, MSA, year)
merged_CPS<-merged_CPS[cellmean_Natl]
setkey(merged_CPS, MSA, year)
merged_CPS<-merged_CPS[Cellmean_gini]
setkey(merged_CPS, MSA, year)
merged_CPS<-merged_CPS[Larrimore_gini]

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/weighted/")
MSA.unique<-unique(merged_CPS$MSA)
for (i in MSA.unique){
  temp.df<-subset(merged_CPS, merged_CPS$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=CPS_Gini_MSA_cellmean , colour="CPS_Gini_Natl_cellmean"))+
          geom_line(aes(y=CPS_Gini_w_MSA_cellmean , colour="CPS_Gini_w_Natl_cellmean"))+
          opts(title=i))
  dev.off()
}

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/CPS_compare/")
MSA.unique<-unique(merged_CPS$MSA)
for (i in MSA.unique){
  temp.df<-subset(merged_CPS, merged_CPS$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=CPS_Gini_w_Natl_Larrimore , colour="CPS_GB2_gini"))+
          geom_line(aes(y=Gini_ExtCellmean, colour="CPS_cellmean_gini"))+
          opts(title=i))
  dev.off()
}

