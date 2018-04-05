## 2. Calibrate testing rates, symptom-based case find and test rate multiplier for high-risk ##

library(dfoptim)
source("calib_psi_v3.R")
psi18  = vparameters$psi[c(1:3, 7:9, 19:21, 25:27, 31:33, 37:39)]
vt.psi = seq(0, 12, 1)
vparameters$eta.m <- matrix(0, 42, 5)   #set PrEP entry rate as 0s first
nm.psi <- nmkb(par = c(psi18, vparameters$v2, vparameters$v3, vparameters$high.psi), fn =psiObj, 
               lower =c(rep(0, 20), 1), upper =c(rep(0.1, 20), 2),
               fixed = vparameters, obs = ndiag18.obs[2, ])
nm.par18 = nm.psi$par[1:18]
nm.par42 = c(nm.par18[1:3], nm.par18[1:3]*nm.psi$par[21], rep(c(nm.par18[4:6], nm.par18[4:6]*nm.psi$par[21]), 2), rep(nm.par18[7:9], 2), rep(nm.par18[10:12], 2), nm.par18[13:15], nm.par18[13:15]*nm.psi$par[21], nm.par18[16:18], nm.par18[16:18]*nm.psi$par[21])
write.excel <- function(tab, ...) write.table(tab, "clipboard", sep="\t", row.names =F)
write.excel(nm.par42)
print (nm.psi$par[19])  #v2
print (nm.psi$par[20])  #v3
print (nm.psi$par[21])  #high.psi