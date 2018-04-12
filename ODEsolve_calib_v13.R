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

# save obj function for one calibration:
sink("obj_10.txt")

# random sampling

nsample = 10            #currently set 10 to reduce computing time
npar    = nrow (calpar)
set.seed(5484561)







###### Latin hypercube sampling to get 1,000 simplexes for calibration######
#install.packages("FME")
library(FME)
parRange <- data.frame(min =calpar$low, max =calpar$high)

nsample = 10            #currently set 10 to reduce computing time
npar    = nrow (calpar)
set.seed(5484561)

# generates probabilities from 95% interval
lhs = Latinhyper(matrix(c(0.025,0.975), npar, 2, byrow=T), nsample)


## The distribution should be dependent on their data type and distribution parameters
## If commands should be added to automate the recognition of distribution and parameters
lhs.ln = matrix(0, nsample, npar)
ln95ci = matrix(0, npar,    2)
for (i in 1:npar){
  par.ln = parLN(m[i], s[i])
  lhs.ln[ ,i] = qlnorm(lhs[ ,i],        par.ln$mu, par.ln$sigma)   #This needs to be modified
  ln95ci[i, ] = qlnorm(c(0.025, 0.975), par.ln$mu, par.ln$sigma)   #This needs to be modified
}
write.excel(ln95ci)

#calculate the objective function (optional !!) for the derived LH samples before calibration
objval = numeric(nsample)
for (i in 1:nsample){
  objval[i] = obj(calib.par =lhs.ln[i, ], calpar.info =calpar.info, fixed =vparameters, 
                  calib.target = calib.target, valid.target = valid.target, plot=F)
}

# run the calibration for all the LH samples
nmlh = as.list(0)
for (i in 1:nsample){
  nmlh[[i]] <- nmkb(par =lh10[i, ], fn =obj,
                    lower =ln95ci[ ,1], upper =ln95ci[ ,2],
                    calpar.info = calpar.info, fixed = vparameters, 
                    calib.target = calib.target, valid.target = valid.target, plot=F)
}
parlh = matrix(0, npar, nsample)
objlh = numeric(nsample)
for (i in 1:nsample){
  parlh[ ,i] = nmlh[[i]]$par
  objlh[i]   = obj(calib.par =nmlh[[i]]$par, calpar.info =calpar.info, fixed =vparameters, 
                   calib.target = calib.target, valid.target = valid.target, plot=F)
}
write.excel(parlh)
print(objlh)


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

