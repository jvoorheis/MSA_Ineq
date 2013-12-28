library(ggplot2)
library(animation)
library(foreign)
library(RColorBrewer)

load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Combined_series_state.rda")
states <- map_data("state")
Combined_series$State<-tolower(Combined_series$State)
Top1Coef<-data.frame("region"=Combined_series$State, "Top1" = Combined_series$weighted_top1, "year"=Combined_series$year)
GiniCoef<-data.frame("region"=Combined_series$State, "Gini" = Combined_series$weighted_gini, "year"=Combined_series$year)
TheilCoef<-data.frame("region"=Combined_series$State, "Theil" = Combined_series$weighted_theil, "year"=Combined_series$year)

levelify<-function(coef, dist){
  return(round(quantile(dist, probs=round(ecdf(dist)(coef), 1)), 3))
}

GiniCoef$GiniFactor<-as.factor(apply(data.frame(GiniCoef$Gini), 1, levelify, GiniCoef$Gini))
Top1Coef$Top1Factor<-as.factor(apply(data.frame(Top1Coef$Top1), 1, levelify, Top1Coef$Top1))
TheilCoef$TheilFactor<-as.factor(apply(data.frame(TheilCoef$Theil), 1, levelify, TheilCoef$Theil))

GiniColors <- brewer.pal(11, "RdYlGn")
names(GiniColors) <- sort(levels(GiniCoef$GiniFactor), decreasing=T)
GinicolScale <- scale_fill_manual(name = "Gini Coefficient", values = GiniColors)
Top1Colors <- brewer.pal(11, "RdYlGn")
names(Top1Colors) <- sort(levels(Top1Coef$Top1Factor), decreasing=T)
Top1colScale <- scale_fill_manual(name = "Top 1% Share", values = Top1Colors)
TheilColors <- brewer.pal(11, "RdYlGn")
names(TheilColors) <- sort(levels(TheilCoef$TheilFactor), decreasing=T)
TheilcolScale <- scale_fill_manual(name = "Theil Coefficient", values = TheilColors)

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Results/State_maps")
for (i in 1977:2012){
  state_gini<-subset(GiniCoef, GiniCoef$year==i)
  state_gini$year<-NULL
  state_gini$GiniFactor<-droplevels(state_gini$GiniFactor)
  choro <- merge(states, state_gini, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  filegini <- paste("Gini_states_", as.character(i), ".png", sep="")
  filetop1 <- paste("Top1_states_", as.character(i), ".png", sep="")
  filetheil <- paste("Theil_states_", as.character(i), ".png", sep="")
  png(filename=filegini, width = 900, height=480)
  print(qplot(long, lat, data = choro, group = group, fill = GiniFactor,
        geom = "polygon") + labs(title=paste("Gini Coefficient, ", as.character(i), sep="")) + 
          GinicolScale)
  dev.off()
  state_Top1<-subset(Top1Coef, Top1Coef$year==i)
  state_Top1$year<-NULL
  state_Top1$Top1Factor<-droplevels(state_Top1$Top1Factor)
  choro <- merge(states, state_Top1, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  png(filename=filetop1, width = 900, height=480)
  print(qplot(long, lat, data = choro, group = group, fill = Top1Factor,
              geom = "polygon") + labs(title=paste("Top 1% Share, ", as.character(i), sep="")) + 
          Top1colScale)
  dev.off()
  state_Theil<-subset(TheilCoef, TheilCoef$year==i)
  state_Theil$year<-NULL
  state_Theil$TheilFactor<-droplevels(state_Theil$TheilFactor)
  choro <- merge(states, state_Theil, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  png(filename=filetheil, width = 900, height=480)
  print(qplot(long, lat, data = choro, group = group, fill = TheilFactor,
              geom = "polygon") + labs(title=paste("Theil Coefficient, ", as.character(i), sep="")) + 
          TheilcolScale)
  dev.off()

}
ani.options(interval=1, outdir = getwd())
im.convert('Gini_states*.png', output = 'State_gini.gif')
im.convert('Top1_states*.png', output = 'State_top1.gif')
im.convert('Theil_states*.png', output = 'State_theil.gif')

#Frank (2009) Data
FrankData<-read.dta("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/Frank_All.dta")
FrankData<-subset(FrankData, is.na(FrankData$atkin05)==F & FrankData$year>1976)
FrankData$state <-tolower(FrankData$state)
colnames(FrankData)[2]<-"region"
FrankData$Top1Factor<-as.factor(apply(data.frame(FrankData$top1), 1, levelify, FrankData$top1))
FrankColors <- brewer.pal(11, "RdYlGn")
names(FrankColors) <- sort(levels(FrankData$Top1Factor), decreasing=T)
FrankcolScale <- scale_fill_manual(name = "Top 1% Share", values = FrankColors)

for (i in 1977:2005){
  state_frank<-subset(FrankData, FrankData$year==i)
  state_frank$year<-NULL
  state_frank$Top1Factor<-droplevels(state_frank$Top1Factor)
  choro <- merge(states, state_frank, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  filetop1 <- paste("Frank_Top1_states_", as.character(i), ".png", sep="")
  png(filename=filetop1, width = 900, height=480)
  print(qplot(long, lat, data = choro, group = group, fill = Top1Factor,
              geom = "polygon") + labs(title=paste("Top 1% Share, ", as.character(i), ", Frank (2009)", sep="")) + 
          FrankcolScale)
  dev.off()
}
im.convert('Frank_Top1_states*.png', output = 'Frank_Top1_gini.gif')
