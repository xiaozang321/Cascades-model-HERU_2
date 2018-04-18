################################################################################
## calOut function                                                            ##
################################################################################
# calOut function considering new diagnosis, PLHIV diagnosed, number of death.
# return model estimated full list of outcomes for plots

## Parameters ##
# calib.par is a vector of parameters to be calibrated
# leng: length of each parameter (i.e. noG has a length of 36)
# names: parameter names for calib.par
# (names of parameters for calibration should be assigned for every iteration)
# groups: specific group names for a parameter
# fixed is a list of fixed parameters (not going to be calibrated)

require(deSolve)

calOut <- function(calib.par, calpar.info, fixed, fixed.list) { 

  
  # Attain inputs for objective function
  names.gp   = as.vector(fixed$names.gp)
  names18    = as.vector(fixed$names18)
  n.gp       = length(fixed$names.gp)
  state.name = fixed$state.name
  leng   = calpar.info$plength
  names  = calpar.info$names
  
  ## Assigning parameter values for Morris method ##
  vlist = fixed.list
  vparameters = fixed
  
  par2   = as.list(leng)
  
  for(l in 1:length(leng)){
    if (l==1) {
      par2[[l]]   = calib.par[1:leng[l]]
    }
    else {
      par2[[l]]   = calib.par[(sum(leng[1:(l-1)])+1):sum(leng[1:l])]
    }
  }
  
  names(par2)   = names
  
  # update parameter list with new free parameter values and save in par3
  
  for (l in 1:length(names)){
    if (length(unlist(vparameters[names[l]])) == 1) { # single parameter
      vparameters[names[l]][[1]] = par2[names[l]][[1]]
    }
    if (par_info[par_info$parameter == names[l], ]$stratification == 3) { #parameters with 3 dimensions: gender, ethnicity, risk
      for (j in 1:leng[l]) {
        vlist[[names[l]]][vlist[[names[l]]]$gender == calpar[calpar$par == names[l], ][j, ]$gender & vlist[[names[l]]]$ethnicity == calpar[calpar$par == names[l], ][j,]$ethnicity & vlist[[names[l]]]$risk == calpar[calpar$par == names[l], ][j,]$risk, ]$pe = par2[names[l]][[1]][j]
      }}
    if (par_info[par_info$parameter == names[l], ]$stratification == 4) { #parameters with 4 dimensions: gender, ethnicity, risk, sexual intensity
      for (j in 1:leng[l]) {
        vlist[[names[l]]][vlist[[names[l]]]$gender == calpar[calpar$par == names[l], ][j, ]$gender & vlist[[names[l]]]$ethnicity == calpar[calpar$par == names[l], ][j,]$ethnicity & vlist[[names[l]]]$risk == calpar[calpar$par == names[l], ][j,]$risk & vlist[[names[l]]]$sexual.intensity == calpar[calpar$par == names[l], ][j,]$sexual.intensity, ]$pe = par2[names[l]][[1]][j]
      }}
    if (par_info[par_info$parameter == names[l], ]$stratification == 1) { #parameters with 1 dimension: CD4
      for (j in 1:leng[l]) {
        vlist[[names[l]]][vlist[[names[l]]]$CD4 == calpar[calpar$par == names[l], ][j, ]$CD4, ]$pe = par2[names[l]][[1]][j]
      }}
  }
  
  source("parameterization.R")
  if ("v.ssp" %in% names){
    vparameters$v.ssp = vparameters$v.ssp * (vparameters$init.tot[gp18.gn$all.pwid]/ sum(vparameters$init.tot[gp18.gn$all.pwid])) 
  }
  
  par3 = vparameters
  
  #parameters in "par3" overwrite ones in "fixed" if names are the same.
  out_euler <- euler(x, vt, ode_model, par3)[ ,-1]
  outa = array(out_euler[-1, ], dim = c(n, length(state.name), n.gp)) # initial value deleted
  
  
  dimnames(outa)[[2]] = state.name
  #number of individuals in each states at time t, excluding incidence
  outn = outa[ ,1:19, ] 
  
  #total diagnoses
  diag         = apply(outn[end_yr_ind, 10:19, ], c(1,3), sum)
  diag.all     = rowSums(diag)
  diag.b       = rowSums(diag[ , black])
  #print(diff(apply(outn[,10:19,msm],1,sum)))
  diag.m       = rowSums(diag[ , msm])
  diag18.model = matrix(0, 4, 18)
  for (i in 1:18){
    ind = which(rname %in% names18[i])
    diag18.model[ ,i] = rowSums(diag[ ,ind])
  }
  # total diagnoses in 11 groups (combining MSM/PWID into one; PWID into one)
  diag11.model = group11(diag18.model)
  
  # new diagnosis from model
  ndiag        = rbind(outa[12, "diag", ],  apply(outa[end_yr_ind, "diag", ], 2, diff))
  ndiag.all    = rowSums(ndiag) # total
  ndiag.b      = rowSums(ndiag[ ,black]) # black
  ndiag.m      = rowSums(ndiag[ ,msm]) #msm
  # generate results for 11 groups
  ndiag18.model = matrix(0, 4, 18)
  for (i in 1:18){
    ind = which(rname %in% names18[i])
    ndiag18.model[ ,i] = rowSums(ndiag[ ,ind])
  }
  # total diagnoses in 11 groups (combining MSM/PWID into one; PWID into one)
  ndiag11.model = group11(ndiag18.model)

  #names11 = c(names18[1:3], "MSM/PWID", "PWID", names18[13:18])
  
  
  # death from model
  death        = rbind(outa[12,"death", ],  apply(outa[end_yr_ind, "death", ], 2, diff))
  death.all    = rowSums(death) # total
  death.b      = rowSums(death[ ,black]) # black
  death.m      = rowSums(death[ ,msm]) #msm
  # print results for 11 groups
  death18.model = matrix(0, 4, 18)
  for (i in 1:18){
    ind = which(rname %in% names18[i])
    death18.model[ ,i] = rowSums(death[ ,ind])
  }
  # total diagnoses in 11 groups (combining MSM/PWID into one; PWID into one)
  death11.model = group11(death18.model)
  
  # incidence from model
  cum.inc    = apply(outa[ ,c("inc_bo","inc_bs","inc_g"), ], c(1,3), sum)
  inc        = rbind(cum.inc[12, ],  apply(cum.inc[end_yr_ind, ], 2, diff))
  tot.inc    = rowSums(inc)
  all.msm.inc= rowSums(inc[ ,all.msm])

  return (c(diag11.model, ndiag11.model, death11.model, ndiag.all, ndiag.b, ndiag.m, death.all, death.b, death.m, tot.inc, all.msm.inc))
  
  # model = c(diag11.model, ndiag.all,     ndiag.b,     ndiag.m,     death.all,     death.b,     death.m)
  # obs   = c(diag11.obs,   ndiag.all.obs, ndiag.b.obs, ndiag.m.obs, death.all.obs, death.b.obs, death.m.obs)
  # dev = abs((model-obs)/obs)
  # 
  # return(weighted.mean(dev, rep(fixed$w, each=4), na.rm=T))
  
  
  # model.est = c(diag,tdiag,death)
  # w = rep(1,length(obs))
  # abs.dev  = abs(model.est-obs)
  # prop.dev = abs((model.est-obs)/obs)
  # obj = weighted.mean(prop.dev,w,na.rm=T)
  # mean.dev   = mean(abs.dev)
  # median.dev = median(abs.dev)
  # range.dev  = range(abs.dev)
  #   
  # model.out=c(diagy.all, diagy.bm, diagy.hm, diagy.wm, diagy.het.m, diagy.het.f,
  #   tot.diag.all, tot.diag.b, tot.diag.m, death.all, tot.inc, all.msm.inc)
  # 
  # return(c(model.out, obj, mean.dev, median.dev, range.dev))
}
