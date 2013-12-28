library(foreign)
library(sandwich)
library(lmtest)
library(reldist)
library(ineq)
library(ggplot2)

CPS.work<-read.dta("/media/john/Shared Linux_Windows Files/MSA Level Inequality/Data/CPS_topcode_MSA_demo_cleaned.dta")
CA2011<-subset(CPS.work, CPS.work$year==2011 & CPS.work$statefip=="California")
CA2000<-subset(CPS.work, CPS.work$year==2000 & CPS.work$statefip=="California")


rif_glorenz<-function(y, p, formula){
  v_p <- quantile(y, probs=p)
  rif_y <- apply(data.frame(y), 1, function(x) (if (x<v_p) {x-(1-p)*v_p} else{p*v_p}))
  return(rif_y)
}
rif_lorenz<-function(y, p){
  v_p <- quantile(y, probs=p)
  T_Lp <- Lc(y)$L[as.integer(p*length(y))]
  mu_F <- mean(y)
  rif_y <- apply(data.frame(y), 1, function(x) if (x<v_p){(x-(1-p)*v_p)/mu_F + T_Lp*(1-x/mu_F)} else{p*v_p/mu_F-T_Lp*(1-x/mu_F)})
  return(rif_y)
}

CA$RIF_inc1<-rif_glorenz(CA$cellmean_equivinc, 0.95)
try1 <- lm(RIF_inc1~female+black+asian+other+latino+hs+somecollege+college+grad+union_mem+union_cov+married+divorced, data=CA)
coeftest(try1, vcovHC(try1))

CA$RIF_inc2<-rif_lorenz(CA$cellmean_equivinc, 0.1)
try2 <- lm(RIF_inc2~female+black+asian+other+latino+hs+somecollege+college+grad+union_mem+union_cov+married+divorced, data=CA)
try3<-coeftest(try2, vcovHC(try2))
print(xtable(try2), file="Lorenz_reg_10.tex")

grid <- seq(0.05, 0.95, by=0.05)
grid<-append(grid, 0.99)
grid<-append(grid, 0.995)
fitted_vals<-c()
robust_se<-c()
for (i in grid){
  CA$RIF_inc2<-rif_lorenz(CA$cellmean_equivinc, i)
  try2 <- lm(RIF_inc2~female+black+asian+other+latino+hs+somecollege+college+grad+union_mem+union_cov+married+divorced, data=CA)
  fitted_vals<-append(fitted_vals,coeftest(try2, vcovHC(try2))[11,1])
  robust_se<-append(robust_se,coeftest(try2, vcovHC(try2))[11,2] )
  
}
data_x <- data.frame(fitted_vals, robust_se)
data_x$lower <- data_x$fitted_vals - 1.96*data_x$robust_se
data_x$upper <- data_x$fitted_vals + 1.96*data_x$robust_se
data_x$ord<-grid
png(file="LPE_unions.png", width=900, height=900, units="px", type="cairo-png")
print(ggplot(data_x, aes(grid))+
  geom_line(aes(y=upper, colour="upper CI"))+
  geom_line(aes(y=lower, colour= "lower CI"))+
  geom_line(aes(y=fitted_vals, colour= "LPE, Unions")))
dev.off()