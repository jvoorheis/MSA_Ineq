library(ineq)
library(reldist)
library(plyr)
load("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_hh1.rda")

MSA_names_fix<-function(MSA){
  if (MSA=="New York-Northern New Jersey-Long Islan"){
    return("New York-Northern New Jersey-Long Island")
  }
  else{
    return(MSA)
  }
}
CPS.work.hh$MSA<-apply(data.frame(CPS.work.hh$MSA), 1, MSA_names_fix)

source("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code/functions.r")
cellmean_ineq<-ddply(CPS.work.hh, .variables=c("MSA", "year"), function(x) c("Cellmean_gini"=gini(x$cellmean_equivinc), 
                                                                             "Cellmean_theil"=Theil(x$cellmean_equivinc),
                                                                             "Cellmean_top1"=top1share(x$cellmean_equivinc),
                                                                             "Cellmean_9010"=ratio9010f(x$cellmean_equivinc)))
colnames(cellmean_ineq)[6]<-"Cellmean_9010"
save(cellmean_ineq, file="/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/MSA_cellmean_ineq.rda")


