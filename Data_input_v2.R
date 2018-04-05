################################################################################
## DATA INPUT                                                                 ##
################################################################################

## Module set-up ##
#install.packages("openxlsx") 
library(openxlsx)
source("ode_model_func_v10.R")

## Read in data from excel file ##
myFile.name <- c("model input")
WB <- loadWorkbook(paste0(myFile.name,".xlsx"))

par_info <- read.xlsx(WB, sheet="parameter_info")
vlist <- list()
length(vlist) = nrow(par_info)
names (vlist) = par_info$parameter
vparameters = vlist

city_sp     = grep('Y', par_info$city_specific)
non_city_sp = grep('N', par_info$city_specific)

uncert      = grep('Y', par_info$uncertain)
cert        = grep('N', par_info$uncertain)

for (i in city_sp){
  vlist[[i]] = subset(read.xlsx(WB, sheet=par_info$parameter[i]), city == city)
  vparameters[[i]] =  vlist[[i]]$pe
}

for (i in non-city_cp){
  vlist[[i]] = read.xlsx(WB, sheet=par_info$parameter[i])
  vparameters[[i]] =  vlist[[i]]$pe
}

names.gp   = rownames(par.gp)      #group names, 42 groups: full
names18    = rownames(par18)       #group names, 18 groups: gender*risk group*ethnicity
names.pwid = rownames(par9)        #group names, 9 PWID groups: no high/low, OAT
names.msm  = rownames(par18_msm)   #group names, 18 MSM and M/P groups: with high/low, OAT
names.e    = rownames(e)           #group names, 6 groups: high/low*ethnicity


param_f    = read.xlsx(WB, sheet="single parameters")     #single parameters
par9      <- read.xlsx(WB, sheet="par9_pwid", rowNames=T) #parameters for PWID (9 groups, no high/low)
par18_msm <- read.xlsx(WB, sheet="par18_msm", rowNames=T) #parameters for PWID (18 groups, w/ high/low)
par18     <- read.xlsx(WB, sheet="par18",     rowNames=T) #Compartment transitions (18 groups) 
par.gp    <- read.xlsx(WB, sheet="par42",     rowNames=T) #Behaviours and test rate (42 groups)
trans     <- read.xlsx(WB, sheet="trans",     rowNames=T) #transmissbility (4 CD4*4 routes)
mor       <- read.xlsx(WB, sheet="mor",       rowNames=T) #mortality (42 pop*8 states)
e         <- read.xlsx(WB, sheet="e",         rowNames=T) #mixing assortativeness (6 * 3)
weight    <- read.xlsx(WB, sheet="weight",    rowNames=T) #weight for calibration target
####Population name parameters
names.gp   = rownames(par.gp)      #group names, 42 groups: full
names18    = rownames(par18)       #group names, 18 groups: gender*risk group*ethnicity
names.pwid = rownames(par9)        #group names, 9 PWID groups: no high/low, OAT
names.msm  = rownames(par18_msm)   #group names, 18 MSM and M/P groups: with high/low, OAT
names.e    = rownames(e)           #group names, 6 groups: high/low*ethnicity

####Initial parameters
pop       <- read.xlsx(WB, sheet="pop",           rowNames=T) #initial population parameters
initial18 <- read.xlsx(WB, sheet="par_initial18", rowNames=T) #18 groups: initial % of S,I,D,T,O
initial42 <- read.xlsx(WB, sheet="par_initial",   rowNames=T) #42 groups: initial % of 19 states (no PrEP)
####PrEP parameters
prep      <- read.xlsx(WB, sheet="PrEP",          rowNames=T) #total # starting PrEP: 4 years
prep.dist <- read.xlsx(WB, sheet="PrEP.prop",     rowNames=T) #% of prep among each ethnical group

### Combine all in vpameters
vparameters = c(param_f, par9, par18_msm, par18, par.gp, trans, mor, e, pop, initial18, initial42, prep, prep.dist, weight,
                as.data.frame(names.gp),   as.data.frame(names18),
                as.data.frame(names.pwid), as.data.frame(names.msm),
                as.data.frame(names.e))

vparameters$state.name = c("S1", "S2", "Sp", "Ia", "I1", "I2", "I3", "Iap", "Ip",
                           "Da", "D1", "D2", "D3", "T1", "T2", "T3", "O1", "O2", "O3",
                           "inc_bo", "inc_bs", "inc_g", "diag", "death")


## Set target (calibration and validation) data ##
load("NYC_targets_v2.Rdata") 
####*diag18.obs*, *ndiag18.obs*, *death18.obs*: observed total diagnoses, new diagnoses and deaths for 18 groups 2011-2015  
####*obs.inc.all*, *obs.inc.msm*: observed incidence with range in 2012-2015  
calib.target <- list (diag18.obs = diag18.obs, ndiag18.obs = ndiag18.obs, death18.obs = death18.obs)
valid.target <- list (obs.inc.all = obs.inc.all, obs.inc.msml = obs.inc.msm)


## Set time steps ##
lyr = 2015          # the last year
nyr = lyr-2012+1    # no. of years
end_yr_ind = c(12*(1:nyr))  #indicator for year-end in month
yr  = 2012:lyr
n   = nyr*12        # from 2012 to lyr by month
vt  = seq(0, n, 1)  # time variable includes t=0



## Set model initials ##
source("model.initial_func.R")
init = model.initial(par = vparameters, diag18 = diag18.obs[1, ])  #42*19 initials
inits = cbind(init, inc_bo=0, inc_bs=0, inc_g=0, diag=0, death=0)
x=as.vector(t(inits))   #ode function requires init as vector

#### initial proportion of 42 groups;
init.group.prop = as.vector(rowSums(init)/sum(init))

# initial population for 18 groups (collapsing onOAt/offOAT, low/high))
init.tot     =numeric(18)
init.sus     =numeric(18)
init.sus.inf =numeric(18) #susceptible + infected
# change 42 group names into 18 without OAT, low, high
rname = gsub(paste(c("/OAT","/low","/high"), collapse="|"), "", names.gp)
for (i in 1:18){
  ind = which(rname %in% names18[i])
  init.tot[i] = sum(init[ind, ])
  init.sus[i] = sum(init[ind, c("S1","S2","Sp")])
  init.sus.inf[i] = init.sus[i] + sum(init[ind, c("Ia","I1","I2","I3","Iap","Ip")])
}
init.pop = as.data.frame(cbind(init.tot, init.sus, init.sus.inf))
init.group.prop = as.data.frame(init.group.prop)

vparameters = c(vparameters, init.pop, init.group.prop)

vparameters$prop.adj=FALSE; vparameters$bal=TRUE
#prop.adj:  force the risk group proportion to remain constant
#bal:       balance # of sexual parnership between males & females

## Set HIV testing rates, using 1. back calculation or 2. calibration, only need to set once ##
#!! Remember to copy the results to excel

#### 1. Calcaute HIV testing rates ##
#source('testing_back.R')

#### 2. Calibrate testing rates, symptom-based case find and test rate multiplier for high-risk ##
#source('testing_calib.R')


## Calcaute PrEP starting rates ##
vparameters$eta.m <- matrix(0, 42, 5)
source("PrEP_v2.R")
eta.m <- prepentry(x =x, vt =vt, vpar =vparameters)
vparameters$eta.m <- eta.m

## POpulation group indicators ##
source("Group_indicator.R")

## Read in free parameters ##
calpar  = read.xlsx("cali_par_rank_v9.xlsx", sheet="het2")
calpar.info = as.list(3)      #Contains information for calpar
calpar.info$names   = unique(calpar$par)
calpar.info$groups  = calpar$group
calpar.info$plength = as.numeric(table(factor(calpar$par, levels=calpar.info$names)))
