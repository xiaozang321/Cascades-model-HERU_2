# R function to find psi from initial values

psi = function(y, names42, v2, v3, ndiag.obs, high.p=0.2, high.p.het=1){
  # y: initial population (42x19)
  # v2: symptom-based defining rate for I2
  # v3: symptom-based defining rate for I3
  # ndiag.obs: observed new diagnoses in 2012 for 18 groups (by gender, ethnicity, risk)
  # high.p: the high risk group for MSM has higher testing rates by high.p.
  #       Default: 20% higher testing rate for high risk MSM vs. low risk MSM
  # the function returns monthly hiv testing rates for 42 groups
  names.gp = names42

  source("Group_indicator.R")
  
  find.inf = function(inf.ind, gp18){
    if (length(inf.ind)==1) inf=y[ ,inf.ind]
    else inf = rowSums(y[ ,inf.ind])

    msmL.inf   = c(sum(inf[intersect(white, msmL)]),   sum(inf[intersect(black, msmL)]),   sum(inf[intersect(hisp, msmL)]))
    msmH.inf   = c(sum(inf[intersect(white, msmH)]),   sum(inf[intersect(black, msmH)]),   sum(inf[intersect(hisp, msmH)]))
    miduL.inf  = c(sum(inf[intersect(white, miduL)]),  sum(inf[intersect(black, miduL)]),  sum(inf[intersect(hisp, miduL)]))
    miduH.inf  = c(sum(inf[intersect(white, miduH)]),  sum(inf[intersect(black, miduH)]),  sum(inf[intersect(hisp, miduH)]))
    pwid.m.inf = c(sum(inf[intersect(white, idu.m)]),  sum(inf[intersect(black, idu.m)]),  sum(inf[intersect(hisp, idu.m)]))
    pwid.f.inf = c(sum(inf[intersect(white, idu.f)]),  sum(inf[intersect(black, idu.f)]),  sum(inf[intersect(hisp, idu.f)]))
    het.mL.inf = c(sum(inf[intersect(white, het.mL)]), sum(inf[intersect(black, het.mL)]), sum(inf[intersect(hisp, het.mL)]))
    het.fL.inf = c(sum(inf[intersect(white, het.fL)]), sum(inf[intersect(black, het.fL)]), sum(inf[intersect(hisp, het.fL)]))
    het.mH.inf = c(sum(inf[intersect(white, het.mH)]), sum(inf[intersect(black, het.mH)]), sum(inf[intersect(hisp, het.mH)]))
    het.fH.inf = c(sum(inf[intersect(white, het.fH)]), sum(inf[intersect(black, het.fH)]), sum(inf[intersect(hisp, het.fH)]))
    
    inf.gp18 = c(msmL.inf +msmH.inf, miduL.inf +miduH.inf, pwid.m.inf, pwid.f.inf, het.mL.inf +het.mH.inf, het.fL.inf +het.fH.inf)
    inf.gp   = c(msmL.inf, msmH.inf, miduL.inf, miduH.inf, pwid.m.inf, pwid.f.inf, het.mL.inf, het.mH.inf, het.fL.inf, het.fH.inf)
    if (gp18==T) return(inf.gp18)
    else return(inf.gp)
  }
  
  inf.all = find.inf(c("Ia","I1","I2","I3"), gp18=F)     ###to calculate all infected (undiagnosed) in each group (6 or 10 groups * 3 races = 18 or 30) in 2012
  
  # new diagnoses by symptom-based HIV defining
  ndiag.symp = (1-exp(-v2*12)) * find.inf("I2", gp18=T) + (1-exp(-v3*12)) * find.inf("I3", gp18=T)
  # testing prob over 1 year
  psi.p=numeric(42) 
  # hiv testing prob for msm 
  # assume that high risk group has higher testing rates by high.p
  # [psi.low*inf.low + (1+high.p)*psi.low*inf.high= new diagnoses]
  psi.p.msmL   = (ndiag.obs[1:3] - ndiag.symp[1:3])/ (inf.all[1:3] + (1+high.p) * inf.all[4:6])
  psi.p[msm]   = c(psi.p.msmL,  psi.p.msmL*(1+high.p))
  # hiv testing prob for midu
  psi.p.miduL  = (ndiag.obs[4:6] - ndiag.symp[4:6])/ (inf.all[7:9] + (1+high.p) * inf.all[10:12])
  psi.p[midu]  = c(psi.p.miduL, psi.p.miduL*(1+high.p))
  # hiv testing prob for pwid male
  psi.p[idu.m] = (ndiag.obs[7:9]   -   ndiag.symp[7:9])/ inf.all[13:15]
  # hiv testing prob for pwid female
  psi.p[idu.f] = (ndiag.obs[10:12] - ndiag.symp[10:12])/ inf.all[16:18]
  # hiv testing prob for het
  psi.p.het.mL = (ndiag.obs[13:15] - ndiag.symp[13:15])/ (inf.all[19:21] +(1+high.p.het)*inf.all[22:24])
  psi.p[het.m] = c(psi.p.het.mL, psi.p.het.mL*(1+high.p.het))
  psi.p.het.fL = (ndiag.obs[16:18] - ndiag.symp[16:18])/ (inf.all[25:27] +(1+high.p.het)*inf.all[28:30])
  psi.p[het.f] = c(psi.p.het.fL, psi.p.het.fL*(1+high.p.het))
  
  # count number of testing:
  num.test = sum(psi.p*rowSums(y[ ,c("S1","S2","Ia","I1","I2","I3")]))
  
  # Convert testing rate from yearly to per month for 36 groups
  psi = -log(1-psi.p)/12 
  
  return(list(psi=psi, num.test=num.test)) 
}