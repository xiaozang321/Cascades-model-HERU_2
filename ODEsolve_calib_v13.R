##################################################################################
# An R script to solve ODE's of the HIV model using deSolve package.             #
#                                                                                #
# Author:  Jeong Min, Xiao Zang, Emanuel Krebs                                   #
# updated: March 19, 2018                                                        #
##################################################################################

#A pdf document that provides very detailed information about the package "deSolve"
#vignette("deSolve")

## Module set-up ##
rm(list=ls())
#setwd("C:/Users/Xiao/Dropbox/R code/Casacades model R code/Cascades model-HERU_v2")
#setwd("C:/Users/jason/Dropbox/R code/Casacades model R code/Cascades model-HERU")
library(dfoptim) #for calibration function nmkb
source("ode_model_func_v10.R")   #ode function module
source("obj_func_v11.R")         #objective function module
# Set function to copy result to excel
write.excel <- function(tab, ...) write.table( tab, "clipboard", sep="\t", row.names=F)


## Read in data and all inputs ##
city = "NYC"
source ("Data_input_v4.R")
# output: vparameters, vt (time step), x (all initials in vector), calpar (calibration data), target data

## The GOF with point estimates ##
#print(system.time())
obj (calib.par =calpar$pe, calpar.info =calpar.info, fixed =vparameters, fixed.list =vlist,
     calib.target =calib.target, valid.target =valid.target, plot =T)


###### Model calibration ######
#Nelder-Mead should not be used for high-dimensional optimization
# maximum # of parameters for nmk is 30 
nm <- nmkb(par = calpar$pe, fn = obj,
          lower = calpar$lower, upper = calpar$upper,
          calpar.info = calpar.info, fixed = vparameters, fixed.list =vlist,
          calib.target = calib.target, valid.target = valid.target, plot=F)

obj (calib.par = nm$par, calpar.info = calpar.info, fixed = vparameters, fixed.list =vlist,
     calib.target = calib.target, valid.target = valid.target, plot=T)

write.excel(nm$par)


#### Random sampling to obtain multiple starting values ####
library(prevalence)
library(mc2d)
# save obj function for one calibration:
#sink("obj_10.txt")
#sink()

###### Latin hypercube sampling to get 1,000 simplexes for calibration######
#install.packages("FME")
library(FME)

nsample = 10            #currently set 10 to reduce computing time
npar    = nrow (calpar)

set.seed(5454)
# generates Latin hypercube samples from 90% interval
lhs = Latinhyper(matrix(c(0.05,0.95), npar, 2, byrow=T), nsample)

randsp = matrix(0, nsample, npar)
for (i in 1:npar){
  if (calpar$no.par[i] != 3){
    if (calpar$dist[i] == "beta"){
      randsp [ ,i] = qbeta(lhs[ ,i], calpar[i, ]$par1, calpar[i, ]$par2)
    }
    else if (calpar$dist[i] == "pert") {
      randsp [ ,i] = qbetagen(lhs[ ,i], calpar[i, ]$par1, calpar[i, ]$par2, calpar[i, ]$lower, calpar[i, ]$upper)
    }
    else if (calpar$dist[i] == "1/pert") {
      randsp [ ,i] = 1/(qbetagen(lhs[ ,i], calpar[i, ]$par1, calpar[i, ]$par2, 1/calpar[i, ]$upper, 1/calpar[i, ]$lower))
    }
    else if (calpar$dist[i] == "gamma") {
      randsp [ ,i] = qgamma(lhs[ ,i], calpar[i, ]$par1, 1/calpar[i, ]$par2)
    }
    else if (calpar$dist[i] == "gamma/0.21") {
      randsp [ ,i] = round(qgamma(lhs[ ,i], calpar[i, ]$par1, 1/calpar[i, ]$par2)/0.21)
    }
    else if (calpar$dist[i] == "uni") {
      randsp [ ,i] = qunif(lhs[ ,i], calpar[i, ]$lower, calpar[i, ]$upper)
    }
    else if (calpar$dist[i] == "1/uni") {
      randsp [ ,i] = 1/(qunif(lhs[ ,i], 1/calpar[i, ]$upper, 1/calpar[i, ]$lower))
    }
    else if (calpar$dist[i] == "ln") {
      randsp [ ,i] = qlnorm(lhs[ ,i], calpar[i, ]$par1, calpar[i, ]$par2)
    }
    else if (calpar$dist[i] == "1-ln") {
      randsp [ ,i] = 1- (qlnorm(lhs[ ,i], calpar[i, ]$par1, calpar[i, ]$par2))
    }
    else if (calpar$dist[i] == "1/ln") {
      randsp [ ,i] = 1/(qlnorm(lhs[ ,i], calpar[i, ]$par1, calpar[i, ]$par2))
    }
    else if (calpar$dist[i] == "poisson") {
      randsp [ ,i] = qpois(lhs[ ,i], calpar[i, ]$par1) / calpar[i, ]$par2
    }
  }
}

any(randsp - matrix(rep(calpar$lower, each=10), nrow = 10, ncol=npar) < 0 )  # random sample check
any(matrix(rep(calpar$upper, each=10), nrow = 10, ncol=npar) - randsp < 0 )  # random sample check

print(randsp)
write.excel(randsp)

objval = numeric(nsample)
for (i in 1:nsample){
  objval[i] = obj(calib.par =randsp[i, ], calpar.info = calpar.info, fixed = vparameters, fixed.list =vlist,
                  calib.target = calib.target, valid.target = valid.target, plot=F)
}

nmrs = as.list(0)
for (i in 1:nsample){
  nmrs[[i]] <- nmkb(par =randsp[i, ], fn =obj,
                    lower = calpar$lower, upper = calpar$upper,
                    calpar.info = calpar.info, fixed = vparameters, fixed.list =vlist,
                    calib.target = calib.target, valid.target = valid.target, plot=F)
}

parrs = matrix(0, npar, nsample)
objrs = numeric(nsample)
for (i in 1:nsample){
  parrs[ ,i] = nmrs[[i]]$par
  objrs[i]   = obj(calib.par =nmrs[[i]]$par, calpar.info = calpar.info, fixed = vparameters, fixed.list =vlist,
                   calib.target = calib.target, valid.target = valid.target, plot=F)
}
write.excel(parrs)
print(objrs)



#### Diagnostic calibration plots
source("calOut.R")

calOut10 = matrix(0, nsample, 164)   # ((11 groups * 3 targets) + 6 calib targets + 2 valid targets) * 4 years
for (i in 1:nsample){
  calOut10[i, ] = calOut(calib.par =nmrs[[i]]$par, calpar.info = calpar.info, fixed = vparameters, fixed.list =vlist)
}

# Target operationalization
diag.obs  = calib.target$diag18.obs [-1, ]
ndiag.obs = calib.target$ndiag18.obs[-1, ]
death.obs = calib.target$death18.obs[-1, ]
inc.all   = valid.target$obs.inc.all
inc.msm   = valid.target$obs.inc.msm

# observed total diagnosed PLHIV
diag11.obs   = group11(diag.obs)
names11 = c(names18[1:3], "MSM/PWID", "PWID", names18[13:18])

# observed new diagnosis
ndiag.all.obs= rowSums(ndiag.obs)
ndiag.b.obs  = rowSums(ndiag.obs[ ,grep("black", names18)]) # black
ndiag.m.obs  = rowSums(ndiag.obs[ ,1:3]) #msm
ndiag11.obs  = group11(ndiag.obs)

# observed mortality
death.all.obs= rowSums(death.obs)
death.b.obs  = rowSums(death.obs[ ,grep("black", names18)]) # black
death.m.obs  = rowSums(death.obs[ ,1:3]) #msm
death11.obs  = group11(death.obs)


# plots for the multiple calibrated parameter sets

# function to plot model outcomes compared with observed values
moplot = function(model, obs, title, low =0, high =0){
  ymin = min(c(model, obs), na.rm =T)*0.8
  ymax = max(c(model, obs), na.rm =T)*1.1
  # observed output
  plot(yr, obs, xlab ="Year", main =title,
       ylab = 'Number of individuals', type ='l', ylim =c(ymin,ymax), lwd =2,
       xaxt = "n") #xaxis label not shown
  if (nyr <= 4) axis(side = 1, at = yr)
  else axis(side =1)
  # from model output
  for (i in 1:nsample){
    lines(yr, model[i, ], lty =2, lwd =1.5, col="blue")
  }
  # CI for incidence
  if (any(low >0)){
    lines(yr[1:4], low,  lty=3, lwd=2)
    lines(yr[1:4], high, lty=3, lwd=2)
  }
}

####Result plots for all groups####
par(oma = c(3.5, 1, 1, 1))
par(mfrow=c(5, 7))

for (i in 1:11){
  moplot(calOut10[ ,(4*i-3):(4*i)],     diag11.obs[ ,i],  paste("Diag:", names11[i]))
}
for (i in 1:11){
  moplot(calOut10[ ,(4*i+41):(4*i+44)], ndiag11.obs[ ,i], paste("New diag:", names11[i]))
}
for (i in 1:11){
  moplot(calOut10[ ,(4*i+85):(4*i+88)], death11.obs[ ,i], paste("Death:", names11[i]))
}
#incidence
moplot(calOut10[ ,157:160], obs.inc.all$value, "Total HIV incidence",                  obs.inc.all$low, obs.inc.all$high)
moplot(calOut10[ ,161:164], obs.inc.msm$value, "HIV incidence: MSM & MSM/PWID", obs.inc.msm$low, obs.inc.msm$high)
#### legend ####
par(fig=c(0, 1, 0, 0.3), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
legend("bottom", c("Model","Obs"), lty=c(2,1), cex=1, lwd=2, col=c("blue","black"))


####Calibration/validation plots####
par(mfrow=c(5, 4))
par(oma =  c(3, 3, 3, 3))
par(mar =  c(2, 2, 2, 1))
for (i in 1:11){
  moplot(calOut10[ ,(4*i-3):(4*i)],     diag11.obs[ ,i],  paste("Diag:", names11[i]))
}
# new diagnosis
moplot(calOut10[ ,133:136], ndiag.all.obs, "New diag: total")
moplot(calOut10[ ,137:140], ndiag.b.obs,   "New diag: black")
moplot(calOut10[ ,141:144], ndiag.m.obs,   "New diag: MSM")
# death
moplot(calOut10[ ,145:148], death.all.obs, "Death: total")
moplot(calOut10[ ,149:152], death.b.obs,   "Death: black")
moplot(calOut10[ ,153:156], death.m.obs,   "Death: MSM")
#incidence
moplot(calOut10[ ,157:160], obs.inc.all$value, "Total HIV incidence",                  obs.inc.all$low, obs.inc.all$high)
moplot(calOut10[ ,161:164], obs.inc.msm$value, "HIV incidence: MSM & MSM/PWID", obs.inc.msm$low, obs.inc.msm$high)
#### legend ####
par(fig=c(0, 1, 0, 0.3), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
legend("bottom", c("Model","Obs"), lty=c(2,1), cex=1, lwd=2, col=c("blue","black"))










## Calibration with different settings ##

#### 1. Set tolerance level (minimum difference in obj for iterations), max # iterations
#print(system.time())
nm <- nmkb(par = calpar$initial, fn = obj,
           lower = calpar$low, upper = calpar$high,
           calpar.info = calpar.info, fixed = vparameters, 
           calib.target = calib.target, valid.target = valid.target, plot=F,
           control = list(tol=1e-10, maxfeval=5000))
sink()

obj (calib.par = nm$par, calpar.info = calpar.info, fixed = vparameters, 
     calib.target = calib.target, valid.target = valid.target, plot=T)

# plot of objective function over iteration:
obj.all = read.table("obj_10.txt")[ ,1]
length(obj.all)
plot(obj.all,xlab="Iteration", ylab="GoF", main="GoF vs. iteration with tol=1e-10")

write.excel(nm$par)


#### 2. Calibration withouit bounds:
# save obj function for one calibration:
sink ("obj_no_bound.txt")
nm.wo.b <- nmk(par = calpar$initial, fn=obj,
               # lower = calpar$low, upper = calpar$high,
               calpar.info = calpar.info, fixed = vparameters, 
               calib.target = calib.target, valid.target = valid.target, plot=F,
             control=list(tol=1e-06, maxfeval=10000))
sink()

# plot of objective function over iteration:
obj.nob = read.table("obj_no_bound.txt")[ ,1]
length(obj.nob) 
plot (obj.nob, xlab="Iteration", ylab="GoF",
      main="GoF vs. iteration without bounds, tol=1e-6")

obj (calib.par =nm.wo.b$par, calpar.info = calpar.info, fixed = vparameters, 
     calib.target = calib.target, valid.target = valid.target, plot=T)
write.excel(nm.wo.b$par)

