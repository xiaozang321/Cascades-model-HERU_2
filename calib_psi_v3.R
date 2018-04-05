# R code to calibrate the HIV test rates by comparing 2012 new diagnoses 


psiObj = function(par, fixed, obs){
  psi18    = par[1:18]
  v2       = par[19]
  v3       = par[20]
  high.psi = par[21]
  
  # model number of diagnosed
  state.name = fixed$state.name
  
  source("Group_indicator.R")
  
  # change 42 group names without OAT, low, high
  rname = gsub(paste(c("/OAT","/low","/high"), collapse="|"), "", names.gp)
  # expand 18 groups of hiv testing rates into 42 groups
  psi = numeric(42) 
  for (i in 1:length(psi18)){
    ind = which(rname %in% names18[i])
    psi[ind] = psi18[i]
  }

  psi[high] = psi[low]*high.psi
  
  vpar = c(as.data.frame(psi), v2 =v2, v3 =v3, high.psi, fixed)

  out_euler <- euler(x, vt.psi, ode_model, vpar)[ ,-1]
  outa = array(out_euler[-1, ], dim = c(12, length(state.name), length(names.gp))) # initial value deleted
  
  dimnames(outa)[[1]] = 1:12
  dimnames(outa)[[2]] = state.name
  dimnames(outa)[[3]] = names.gp
  
  diag2012 = outa[12, "diag", ]
  
  ndiag.model.msm  = c(sum(diag2012[intersect(white, msm)]),
                       sum(diag2012[intersect(black, msm)]),
                       sum(diag2012[intersect(hisp,  msm)]))
  ndiag.model.midu = c(sum(diag2012[intersect(white, midu)]),
                       sum(diag2012[intersect(black, midu)]),
                       sum(diag2012[intersect(hisp,  midu)]))
  ndiag.model.idu.m= c(sum(diag2012[intersect(white, idu.m)]),
                       sum(diag2012[intersect(black, idu.m)]),
                       sum(diag2012[intersect(hisp,  idu.m)]))
  ndiag.model.idu.f= c(sum(diag2012[intersect(white, idu.f)]),
                       sum(diag2012[intersect(black, idu.f)]),
                       sum(diag2012[intersect(hisp,  idu.f)]))
  ndiag.model.het.m= c(sum(diag2012[intersect(white, het.m)]),
                       sum(diag2012[intersect(black, het.m)]),
                       sum(diag2012[intersect(hisp,  het.m)]))
  ndiag.model.het.f= c(sum(diag2012[intersect(white, het.f)]),
                       sum(diag2012[intersect(black, het.f)]),
                       sum(diag2012[intersect(hisp,  het.f)]))
  
  model.est = c(ndiag.model.msm,   ndiag.model.midu,  ndiag.model.idu.m,
                ndiag.model.idu.f, ndiag.model.het.m, ndiag.model.het.f)
    
  dev = abs((model.est - obs)/obs)
  return(mean(dev))
}