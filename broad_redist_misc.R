library(GB2)
library(ineq)
library(reldist)
library(plyr)
library(ggplot2)
library(reshape)
library(parallel)
library(doMC)
library(data.table)

setwd("/media/john/Shared Linux_Windows Files/MSA Level Inequality/")
load("Data/CPS_broadinc.rda")
cores=detectCores()
registerDoMC(cores)

broad_redist<-ddply(CPS.work.hh, .variables=c("statefip", "year"), function(x) c("Gini_posttax_broad"=gini(x$equivinc_posttax_broad),
                                                                              "Gini_pretax_broad"=gini(x$equivinc_pretax_broad),
                                                                              "Gini_pretax"=gini(x$cellmean_equivinc_pretrans),
                                                                              "Gini_posttax"=gini(x$cellmean_equivinc_posttax),
                                                                              "Gini_pretax_equity"=gini(x$equivinc_pretax_equity),
                                                                              "Gini_posttax_equity"=gini(x$equivinc_posttax_equity),
                                                                              "Gini_postpre"=gini(x$cellmean_posttax_pretrans),
                                                                              "Gini_base"=gini(x$cellmean_equivinc)), 
                    .parallel=T)
save(broad_redist, file="Broad_income_gini_ind.rda")