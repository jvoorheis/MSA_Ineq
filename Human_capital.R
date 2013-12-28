load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MORG_college_final.rda")
library(plyr)
library(doMC)
cores=detectCores()-1
registerDoMC()
options(cores=cores)

college_propw<-c()
college_prop<-c()
year<-c()
MSA<-c()
MSA_unique<-unique(MORG_final$MSA)
# for (i in MSA_unique){
#   year_unique<-unique(subset(MORG_final, MORG_final$MSA==i)$year)
#   for (j in year_unique){
#     temp.data<-subset(MORG_final, MORG_final$MSA==i & MORG_final$year==j)
#     MSA<-append(MSA, i)
#     year<-append(year, j)
#     college_propw<-append(college_propw, weighted.mean(temp.data$college, temp.data$weight, na.rm=T))
#     college_prop<-append(college_prop, mean(temp.data$college))
#   }
# }
college_educ<-ddply(MORG_final, .variables = c("MSA", "year"), function(x) c("college_propw" = weighted.mean(x$college, x$weight, na.rm=T),
                                                                             "college_prop" = mean(x$college)), .parallel=T)
#college_educ<-data.frame(MSA, year, college_propw, college_prop)
#college_educ<-aggregate(college~MSA+year, data=MORG_final, FUN=mean)
save(college_educ, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_humancap.rda")
library(ggplot2)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/HumanCap")
for (i in MSA_unique){
  temp.df<-subset(college_educ, college_educ$MSA==i)
  temp.MSA<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.MSA, "_", "college", ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=college_prop, colour="college_prop"))+
          opts(title=i))
  dev.off()
}