# R file to get calibration targets.
# Residents outside NYC excluded, but uknowns inlcuded

rm(list=ls())
#setwd("Y:/Bohdan/CascadeCEA/Compartmental model using R/R code")
setwd("C:/Users/Xiao/Dropbox/R code/New code")

library(openxlsx)

#install.packages("reshape")
library(reshape)
cali.File.name <- "par_all_v23"
WB.target <- loadWorkbook(paste0(cali.File.name,".xlsx"))
obs = read.xlsx(WB.target, sheet = "NYC")
outNYC = read.xlsx(WB.target, sheet = "NYC outside percent")
#outNYC.unk=readWorksheet(WB.target, sheet = "NYC outside+unknown percent",useCachedValues=T)

### get observed numbers by gender, 13-64
obs2 = obs[obs$Age=="13-64" & obs$year %in% (2011:2015),1:6]

# function to exclude outside NYC and unknown
get.obs.nyc = function(par,race){
  obs.all = cast(obs2[obs2$Parameter==par & obs2$'Race/Ethnicity'==race, 1:6],    year~Gender)
  # exclude obs from outside of NYC and unknown
  outNYC2 = cast(outNYC[outNYC$Parameter==par & outNYC$'Race/Ethnicity'==race, ], year~Gender)
  #outNYC.unk=cast(outNYC.unk[outNYC.unk$Parameter==par & outNYC.unk$'Race/Ethnicity'==race, ],
  #                year~Gender)
  return(obs.all[ ,c('Female','Male')]*(1 - outNYC2[ ,c('Female','Male')]/100))
  #return(obs.all[,c('Female','Male')]*(1-outNYC.unk[,c('Female','Male')]/100))
  #return(obs.all[,c('Female','Male')])
}
# function to get observed number by gender
get.obs.gender = function(par){
  # 13-64, all
  obs.all = get.obs.nyc(par,"All")
  # 13-64, black
  obs.b = get.obs.nyc(par,"Black")
  # 13-64, hispanic
  obs.h = get.obs.nyc(par,"Hispanic")
  # 13-64, white
  obs.w = obs.all - obs.b - obs.h
  return(list(all=obs.all, black=obs.b, hisp=obs.h, white=obs.w))
}

# observed number by gender
diag  = get.obs.gender("Diagnosed PLWHA")
ndiag = get.obs.gender("New diagnosis")
death = get.obs.gender("All-cause death among PLWHA")

#### distribute unknown risk groups
# males
obs.male = obs[obs$Age=="13+" & obs$Gender=="Male", 1:6]

# unknown risk distribution by Harrion et al.:
rsk.ukn.prop.m = c(0.64785, 0.13349, 0.04602, 0.17264)

get.rsk.prop.m = function(par, race, prop=T){
  
  dat = cast(obs.male[obs.male$Parameter== par & obs.male$'Race/Ethnicity' %in% race, ], 
             year~Risk.group, fun.aggregate=sum)
  tot = rowSums(dat[ ,-1])
  all.known = rowSums(dat[ ,c('HET','IDU','MSM','MSM-IDU')])
  #proportional to the known risk groups
  if (prop==T){
    MSM.rev  = dat$MSM *(1+ 1/all.known*dat$Unknown)
    IDU.rev  = dat$IDU *(1+ 1/all.known*dat$Unknown)
    MIDU.rev = dat$'MSM-IDU' *(1+ 1/all.known*dat$Unknown)
    HET.rev  = dat$Other+dat$HET *(1+ 1/all.known*dat$Unknown)
  }
  else {
    MSM.rev  = dat$MSM +rsk.ukn.prop.m[1]*dat$Unknown
    IDU.rev  = dat$IDU +rsk.ukn.prop.m[2]*dat$Unknown
    MIDU.rev = dat$'MSM-IDU' +rsk.ukn.prop.m[3]*dat$Unknown
    HET.rev  = dat$Other+dat$HET +rsk.ukn.prop.m[4]*dat$Unknown
  }
  out = data.frame(MSM.rev, IDU.rev, MIDU.rev, HET.rev)/tot
  return(out)
}

# femlaes
obs.female = obs[obs$Age=="13+" & obs$Gender=="Female", 1:6]

# unknown risk distribution by Harrion et al.:
rsk.ukn.prop.f = c(0.1683, 0.8317)

get.rsk.prop.f = function(par, race, prop=T){
  dat = cast(obs.female[obs.female$Parameter== par & 
                        obs.female$'Race/Ethnicity' %in% race, ],
           year~Risk.group, fun.aggregate=sum)
  ## for white and others, combine white, asian, native
  
  tot = rowSums(dat[ ,-1])
  all.known = rowSums(dat[ ,c('HET','IDU')])
  if (prop==T){
    #proportional to the known risk groups
    IDU.rev = dat$IDU* (1+ 1/all.known*dat$Unknown)
    HET.rev = dat$Other + dat$HET*(1 +1/all.known*dat$Unknown)
  }
  else {
    IDU.rev = dat$IDU   + rsk.ukn.prop.f[1] *dat$Unknown
    HET.rev = dat$Other + dat$HET + rsk.ukn.prop.f[2] *dat$Unknown
  }
  out=data.frame(IDU.rev, HET.rev)/tot
  return(out)
}

# observed PLHIV for each 18 risk groups in 2011-2015
risk18 = function(cat, overall, prop=F){
  risk18 = matrix(0, 5, 18)
  for (i in 1:5){
    # dianosed PLHIV with unknown distributed by Harrion et al.:
    # male
    b.rsk   = get.rsk.prop.m(cat, "Black", prop)*overall$black$Male
    h.rsk   = get.rsk.prop.m(cat, "Hispanic", prop)*overall$hisp$Male
    w.rsk   = get.rsk.prop.m(cat, c("White","Asian","Native"), prop)*overall$white$Male
    
    # females
    b.rsk.f = get.rsk.prop.f(cat, "Black", prop)*overall$black$Female
    h.rsk.f = get.rsk.prop.f(cat, "Hispanic", prop)*overall$hisp$Female
    w.rsk.f = get.rsk.prop.f(cat, c("White","Asian","Native"), prop)*overall$white$Female
    
    rsk.m   = rbind(w.rsk[i, ],   b.rsk[i, ],   h.rsk[i, ])
    rsk.f   = rbind(w.rsk.f[i, ], b.rsk.f[i, ], h.rsk.f[i, ])
    risk18[i, ] = c(rsk.m$MSM.rev, rsk.m$MIDU.rev, rsk.m$IDU.rev,
                 rsk.f$IDU.rev, rsk.m$HET.rev, rsk.f$HET.rev)
  }
  return(risk18)
}
diag18.obs  = risk18("Diagnosed PLWHA ", diag)
ndiag18.obs = risk18("New diagnosis", ndiag)
death18.obs = risk18("All-cause death among PLWHA", death, prop=T)

# total number of HIV tests
# tot.test=sum(vparameters$ntest) #182,144

# incidence for all, msm.all
obs.inc     = cast(obs[obs$Parameter=="Incident HIV infections", 1:6], year~Risk.group)
obs.inc.l   = cast(obs[obs$Parameter=="Incident HIV infections", 1:7], year~Risk.group)
obs.inc.u   = cast(obs[obs$Parameter=="Incident HIV infections", 1:8], year~Risk.group)
obs.inc.all = data.frame(year=obs.inc$year, value=obs.inc$All,
                       low=obs.inc.l$All, high=obs.inc.u$All)[-1, ]
obs.inc.msm = data.frame(year=obs.inc$year,value=obs.inc$"MSM, MSM-IDU",
                       low=obs.inc.l$"MSM, MSM-IDU", high=obs.inc.u$"MSM, MSM-IDU")[-1, ]


# save the targets
save(diag18.obs, ndiag18.obs, death18.obs, obs.inc.all, obs.inc.msm,
     file = "NYC_targets_v2.Rdata")

###########
# get in-migration rates

# maturation rate
par18 = read.xlsx(WB.target, sheet = "par18")
mat   = par18$mat*12 # yearly maturation rate

# calcualted diagnosed PLHIV in 2012-2105 from previous diagnosed PLHIV,
# new diagnoses, # deaths, maturation-out rate
diag.der  = diag18.obs[-5, ] + ndiag18.obs[-1, ] - death18.obs[-1, ] - diag18.obs[-5, ]*mat

# difference between the calcuated diagnosed PLHIV and observed diagnosed #
diff.diag = diag18.obs[-1, ] - diag.der # considered as in-migration among PLHIV

# prop of in-migration for 2012-2014
mig.p = colMeans((diff.diag /diag18.obs[-1, ])[-4, ]) # average over 2012-2014
mig.r = -log(1-mig.p)/12 # rate

# copy the result to excel
write.excel <- function(tab, ...) write.table(tab, "clipboard", sep="\t", row.names=F)
write.excel(mig.r)
#the variable "rho.m" in the parameter file

