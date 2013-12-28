load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_GB2_newdata.rda")
library(data.table)
library(xtable)
startyear<-data.table(aggregate(year~MSA, FUN=min, data=NatlGB2.df))
colnames(startyear)[2]<-"start_year"
endyear<-data.table(aggregate(year~MSA, FUN=max, data=NatlGB2.df))
colnames(endyear)[2]<-"end_year"
setkey(startyear, MSA)
setkey(endyear, MSA)
MSA_years<-startyear[endyear]

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste("\\hline \n",
                             "\\endhead \n",
                             "\\hline \n",
                             "{\\footnotesize Continued on next page} \n",
                             "\\endfoot \n",
                             "\\endlastfoot \n",sep=""))
print(xtable(MSA_years), tabular.environment = "longtable", floating = FALSE,
      include.rownames = FALSE,  # because addtorow will substitute the default row names 
      add.to.row = addtorow,     # this is where you actually make the substitution
      hline.after=c(-1))