library(data.table)
library(reldist)
library(ineq)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/NatlGB2")
source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/NatlGB2_72.rda")
#load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSAGB2_multi_627.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh.rda")
colnames(NatlGB2.df)[3]<-"Gini_GB2"
#colnames(MSAGB2_multi.df)[3]<-"Gini_GB2"

MSA_unique<-unique(NatlGB2.df$MSA)
generate_gini_plots(MSA_unique, NatlGB2.df)

cellmeans<-aggregate(cellmean_equivinc~MSA+year, data=CPS.work.hh, FUN=gini)
colnames(cellmeans)[3]<-"cellmeans_gini"
NatlGB2.df<-data.table(NatlGB2.df)
setkey(NatlGB2.df, MSA, year)
setkey(cellmeans, MSA, year)
Combined<-NatlGB2.df[cellmeans]
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Combined")
for (i in MSA_unique){
  temp.df<-subset(Combined, Combined$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, ".jpg", sep="")
  jpeg(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=cellmeans_gini, colour="Cell Means"))+
          geom_line(aes(y=Gini_GB2, colour="GB2 Imputation"))+
          opts(title=i))
  dev.off()
}


##I've elected to use only the national distribution for the GB2 imputation.
# setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/MSAGB2")
# MSA_unique<-unique(MSAGB2_multi.df$MSA)
# generate_gini_plots(MSA_unique, MSAGB2_multi.df)
# 
# #Merge the two datasets and compare:
# setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Combined")
# load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/NatlGB2_627.rda")
# load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSAGB2_multi_627.rda")
# NatlGB2.df<-data.table(NatlGB2.df)
# MSAGB2_multi.df<-data.table(MSAGB2_multi.df)
# setkey(NatlGB2.df, MSA, year)
# setkey(MSAGB2_multi.df, MSA, year)
# Combined<-MSAGB2_multi.df[NatlGB2.df]
# for (i in MSA_unique){
#   temp.df<-subset(Combined, Combined$MSA==i)
#   temp.MSA<-unlist(strsplit(i, "/"))[1]
#   filename<-paste(temp.MSA, ".jpg", sep="")
#   jpeg(file=filename)
#   print(ggplot(temp.df, aes(year)) + 
#           geom_line(aes(y=Gini_MSAGB2, colour="MSA distribution"))+
#           geom_line(aes(y=Gini_NatlGB2, colour="National distribution"))+
#           opts(title=i))
#   dev.off()
# }
