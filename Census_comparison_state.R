library(data.table)
library(ggplot2)
setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/StateGB2_724.rda")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_State_NatlGB2_73_cellmeanfit.rda")
Census_NatlGB2.df[,1:2]<-sapply(Census_NatlGB2.df[,1:2], as.character)
Census_NatlGB2.df[,2:2]<-sapply(Census_NatlGB2.df[,2:2], as.numeric)
Census_Natl_cellmean<-Census_NatlGB2.df
colnames(Census_Natl_cellmean)<-c("State", "year", "Natl_cellmean_gini",  "Natl_cellmean_theil", 	"Natl_cellmean_9010", "Natl_cellmean_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_State_NatlGB2_73_topcodefit.rda")
Census_NatlGB2.df[,1:2]<-sapply(Census_NatlGB2.df[,1:2], as.character)
Census_NatlGB2.df[,2:2]<-sapply(Census_NatlGB2.df[,2:2], as.numeric)
Census_Natl_topcode<-Census_NatlGB2.df
colnames(Census_Natl_topcode)<-c("State", "year", "Natl_topcode_gini",  "Natl_topcode_theil", 	"Natl_topcode_9010", "Natl_topcode_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_StateGB2_73_cellmeanfit.rda")
Census_StateGB2.df[,1:2]<-sapply(Census_StateGB2.df[,1:2], as.character)
Census_StateGB2.df[,2:2]<-sapply(Census_StateGB2.df[,2:2], as.numeric)
Census_State_cellmean<-Census_StateGB2.df
colnames(Census_State_cellmean)<-c("State", "year", "State_cellmean_gini",  "State_cellmean_theil",   "State_cellmean_9010", "State_cellmean_top1")
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Census_StateGB2_73_topcodefit.rda")
Census_StateGB2.df[,1:2]<-sapply(Census_StateGB2.df[,1:2], as.character)
Census_StateGB2.df[,2:2]<-sapply(Census_StateGB2.df[,2:2], as.numeric)
Census_State_topcode<-Census_StateGB2.df
colnames(Census_State_topcode)<-c("State", "year", "State_topcode_gini",  "State_topcode_theil",   "State_topcode_9010", "State_topcode_top1")

Census_Natl_cellmean<-data.table(Census_Natl_cellmean)
Census_Natl_topcode<-data.table(Census_Natl_topcode)
Census_State_cellmean<-data.table(Census_State_cellmean)
Census_State_topcode<-data.table(Census_State_topcode)
setkey(Census_Natl_cellmean, State, year)
setkey(Census_Natl_topcode, State, year)
setkey(Census_State_cellmean, State, year)
setkey(Census_State_topcode, State, year)
Census_joint<-Census_Natl_cellmean[Census_Natl_topcode]
setkey(Census_joint, State, year)
Census_joint<-Census_joint[Census_State_topcode]
setkey(Census_joint, State, year)
Census_joint<-Census_joint[Census_State_cellmean]

State.unique<-unique(Census_StateGB2.df$State)
State<-c()
year<-c()
Natl_cellmean_gini<-c()
Natl_topcode_gini<-c()
State_cellmean_gini<-c()
State_topcode_gini<-c()
Natl_cellmean_top1<-c()
Natl_topcode_top1<-c()
State_cellmean_top1<-c()
State_topcode_top1<-c()
Natl_cellmean_theil<-c()
Natl_topcode_theil<-c()
State_cellmean_theil<-c()
State_topcode_theil<-c()
Natl_cellmean_9010<-c()
Natl_topcode_9010<-c()
State_cellmean_9010<-c()
State_topcode_9010<-c()


for (i in State.unique){
  temp.data<-subset(Census_joint, Census_joint$State==i)
  year.unique<-unique(temp.data$year)
  #gini
  temp.gini<-spline(temp.data$year, temp.data$Natl_cellmean_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_gini<-append(Natl_cellmean_gini, temp.gini$y)
  temp.gini<-spline(temp.data$year, temp.data$Natl_topcode_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_gini<-append(Natl_topcode_gini, temp.gini$y)  
  temp.gini<-spline(temp.data$year, temp.data$State_cellmean_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  State_cellmean_gini<-append(State_cellmean_gini, temp.gini$y)
  temp.gini<-spline(temp.data$year, temp.data$State_topcode_gini, n=length(c(min(temp.data$year):max(temp.data$year))))
  State_topcode_gini<-append(State_topcode_gini, temp.gini$y)
  #top1
  temp.top1<-spline(temp.data$year, temp.data$Natl_cellmean_top1, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_top1<-append(Natl_cellmean_top1, temp.top1$y)
  temp.top1<-spline(temp.data$year, temp.data$Natl_topcode_top1, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_top1<-append(Natl_topcode_top1, temp.top1$y)  
  temp.top1<-spline(temp.data$year, temp.data$State_cellmean_top1, n=length(c(min(temp.data$year):max(temp.data$year))))
  State_cellmean_top1<-append(State_cellmean_top1, temp.top1$y)
  temp.top1<-spline(temp.data$year, temp.data$State_topcode_top1, n=length(c(min(temp.data$year):max(temp.data$year))))
  State_topcode_top1<-append(State_topcode_top1, temp.top1$y)
  #Theil
  temp.theil<-spline(temp.data$year, temp.data$Natl_cellmean_theil, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_theil<-append(Natl_cellmean_theil, temp.theil$y)
  temp.theil<-spline(temp.data$year, temp.data$ Natl_topcode_theil, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_theil<-append(Natl_topcode_theil, temp.theil$y)  
  temp.theil<-spline(temp.data$year, temp.data$Theil, n=length(c(min(temp.data$year):max(temp.data$year))))
  State_cellmean_theil<-append(State_cellmean_theil, temp.theil$y)
  temp.theil<-spline(temp.data$year, temp.data$State_topcode_theil, n=length(c(min(temp.data$year):max(temp.data$year))))
  State_topcode_theil<-append(State_topcode_theil, temp.theil$y)

  #90-10 ratio
  #Theil
  temp.9010<-spline(temp.data$year, temp.data$Natl_cellmean_9010, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_cellmean_9010<-append(Natl_cellmean_9010, temp.9010$y)
  temp.9010<-spline(temp.data$year, temp.data$ Natl_topcode_9010, n=length(c(min(temp.data$year):max(temp.data$year))))
  Natl_topcode_9010<-append(Natl_topcode_9010, temp.9010$y)  
  temp.9010<-spline(temp.data$year, temp.data$State_cellmean_9010, n=length(c(min(temp.data$year):max(temp.data$year))))
  State_cellmean_9010<-append(State_cellmean_9010, temp.9010$y)
  temp.9010<-spline(temp.data$year, temp.data$State_topcode_9010, n=length(c(min(temp.data$year):max(temp.data$year))))
  State_topcode_9010<-append(State_topcode_9010, temp.9010$y)
  #State, year identifiers
  State<-append(State, rep(i, length(c(min(temp.data$year):max(temp.data$year)))))
  year<-append(year, c(min(temp.data$year):max(temp.data$year)))
}
Census_interpolated<-data.frame(State, year, Natl_cellmean_gini, Natl_topcode_gini, State_cellmean_gini, State_topcode_gini,
                                Natl_cellmean_top1, Natl_topcode_top1, State_cellmean_top1, State_topcode_top1,
                                Natl_cellmean_theil, Natl_topcode_theil, State_cellmean_theil, State_topcode_theil,
                                Natl_cellmean_9010, Natl_topcode_9010, State_cellmean_9010, State_topcode_9010)
save(Census_interpolated, file="Census_interpolated_State.rda")

StateGB2.df<-data.table(StateGB2.df)
setkey(StateGB2.df, State, year)
Census_interpolated<-data.table(Census_interpolated)
setkey(Census_interpolated, State, year)
#try2<-NatlGB2.df[Census_interpolated]
try2<-Census_interpolated[StateGB2.df]

summary(lm(Gini_StateGB2~Natl_topcode_gini, data=try2))
summary(lm(Gini_StateGB2~Natl_cellmean_gini, data=try2))
summary(lm(Gini_StateGB2~State_topcode_gini, data=try2))
summary(lm(Gini_StateGB2~State_cellmean_gini, data=try2))
summary(lm(Top1Share~Natl_topcode_top1, data=try2))
summary(lm(Top1Share~Natl_cellmean_top1, data=try2))
summary(lm(Top1Share~State_topcode_top1, data=try2))
summary(lm(Top1Share~State_cellmean_top1, data=try2))
summary(lm(Theil~Natl_topcode_theil, data=try2))
summary(lm(Theil~Natl_cellmean_theil, data=try2))
summary(lm(Theil~State_topcode_theil, data=try2))
summary(lm(Theil~State_cellmean_theil, data=try2))
summary(lm(X9010_ratio~Natl_topcode_9010, data=try2))
summary(lm(X9010_ratio~Natl_cellmean_9010, data=try2))
summary(lm(X9010_ratio~State_topcode_9010, data=try2))
summary(lm(X9010_ratio~State_cellmean_9010, data=try2))


setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/Census_compare/")
State.unique<-unique(try2$State)
for (i in State.unique){
  temp.df<-subset(try2, try2$State==i)
  temp.State<-unlist(strsplit(i, "/"))[1]
  filename<-paste(temp.State, ".png", sep="")
  png(file=filename)
  print(ggplot(temp.df, aes(year)) + 
          geom_line(aes(y=Gini_StateGB2, colour="Gini_StateGB2"))+
          geom_line(aes(y=Natl_topcode_gini, colour="Natl_topcode_gini"))+
          geom_line(aes(y=Natl_cellmean_gini, colour="Natl_cellmean_gini"))+
          geom_line(aes(y=State_topcode_gini, colour="State_topcode_gini"))+
          geom_line(aes(y=State_cellmean_gini, colour="State_cellmean_gini"))+
          opts(title=i))
  dev.off()
}

