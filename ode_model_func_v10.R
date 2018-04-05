##################################################################################
# An R script to solve ODE's of HIV model using deSolve package. 
# 
#  In-migration rate added into diagnosed compartments (D, T, O)
#  42 risk grouops in total (including Low/high HET)
#
# Author: Jeong Min
# Created: Oct 30, 2016
# updated: May 30, 2017
##################################################################################

require("deSolve")
source("ode_list_func_v5.R") #for a function "ode_list"
source("ode_list_func_OAT_v5.R") #for a function "ode_list_OAT"
source("ode_list_func_offOAT_v5.R") #for a function "ode_list_offOAT"
#source("FoI_JM_v9.R")
source("FoI_JM_v9_ass_low.R")


##################################################################################
# this is a function to calculates the time derivatives of states S,I,D,T, and O
# the function gets passed to the ODEsolve package
##################################################################################

#### vparameters include#### 
# epsilonS: %decrease in no. of sexual partners due to diagnosis;
#          assume that epsiolon is same for het and msm
# noG: number of opposite sexual parnters for general pop (42 groups)
# nsG: number of same sexual parnters for general pop (18 msm groups)
# sigmaO.MF: probability of transmission by M->F (acute and 3 chronic states)
# sigmaO.FM: probability of transmission by F->M (acute and 3 chronic states)
# sigmaS: probability of transmission by same sex (acute and 3 chronic states)
# delta_H: % reducion in probability of transmission by sex due to ART for het
# delta_M: % reducion in probability of transmission by sex due to ART for msm
# tau: Probability of transmission per shared injection (acute and 3 chronic states)
# delta_I: % reducion in probability of transmission by injection due to ART
# kappa_H: condom effectiveness for hetero
# kappa_M: condom effectiveness for msm
# uio: probability of condom use for opposite sex (42 groups)
# uis: probability of condom use for same sex (18 groups)
# eff.prep: % reduction in risk of infection for PrEP
# d: number of injection
# s: proportion of injections that are shared (9 groups)
# eff.oat: % reduction in # of shared injections reduced due to OAT
# ass.eO: assortative coefficient for heterosexual mixing between different ethnicity
# ass.eS: assortative coefficient for homosexual mixing between different ethnicity
# rho: the entry rate (18 groups)
# 1/ws: average duration uninfected individuals in compartment S remain 
#   identified after screening (assume same for all groups)
# 1/wp: average duration uninfected individuals in compartment Sp remain on PrEP  
# mat: maturation rates 
# mor: mortality rates
# ntest: number of tests for susceptible population (18 groups)
# prop.prep: proportion of susceptible MSM on PrEP
# psi.p: screening rates for Iap (acute HIV on PrEP)
# theta.ai: transition rate from acute states (Ia) to chronic state I1 (CD4>=500)
# theta.ad: transition rate from acute states (Da) to chronic state D1 (CD4>=500)
# phi: % infected of people receiving ART once diagnosed (42 groups)
# v2,v3: symptom-based case finding rate for I2 and I3, respectively
# alpha: ART initiation rate for D1,D2, and D3
# alpha.re: ART re-initiation rate for O1,O2, and O3
# theta.1: HIV disease progression rate for those not on ART, from I1/D1 to I2/D2
# theta.2: HIV disease progression rate for those not on ART, from I2/D2 to I3/D3
# theta.t: transition probabilities for those on ART 
# theta.o: ART dropout probability from states T1,T2,T3
# theta.t.pwid: transition probabilities for those on ART among PWID
# theta.o.pwid: ART dropout probability from states T1,T2,T3 among PWID
# oat.q: OAT dropout rate
# nOAT: number of PWID enrolled in OAT, by gender and ethnicity (9 groups)
# theta.o.oat: multiplier for ART dropout rate among PWID on OAT
# v.ssp: number of syringes distributed
# eff.ssp: effect of ssp
# names.gp: 42 group names
# names18: 18 group names (collapsing onOAT/offOAT, high/low)
# init.tot: initial total population for 18 groups
# init.sus: initial susceptible population for 18 groups
# rho.m: monthly in-migration rate
# trans.red: reduced probability of HIV transmission due to CCR5 mutation
# psi: hiv testing rates for n.gp groups####
# the time variable, t, starts from 0 to n-1 (=last month-1)
# number of opposite sexual partners

ode_model=function(t, x, vparameters){
   with(as.list(c(vparameters, x)), {

     n.gp = length(names.gp)
     
     no = matrix(0, n.gp, 19)
     noG[noG<0] = 0
     no[ , 1:9]   = noG # for susceptible/infected
     no[ , 10:19] = noG*(1-epsilonS) #for diagnosed/on ART/off ART
       
     #no. of same sexual partners
     ns = matrix(0, 18, 19)
     nsG[nsG<0] = 0
     ns[ , 1:9]   = nsG # for susceptible/infected
     ns[ , 10:19] = nsG*(1-epsilonS) #for diagnosed/on ART/off ART
     
     # probability of transmission by sex for 16 HIV+ states
     sigmaFM = c(sigmaO.FM, sigmaO.FM[1:2], sigmaO.FM, sigmaO.FM[2:4]*(1-delta_H), sigmaO.FM[2:4])
     sigmaMF = c(sigmaO.MF, sigmaO.MF[1:2], sigmaO.MF, sigmaO.MF[2:4]*(1-delta_H), sigmaO.MF[2:4])
     sigmaM  = c(sigmaS,    sigmaS[1:2],    sigmaS,    sigmaS[2:4]*(1-delta_M),    sigmaS[2:4])
     #Probability of transmission per shared injection for 16 HIV+ states
     tau.all = c(tau, tau[1:2], tau, tau[2:4]*(1-delta_I), tau[2:4])

     #condom use adjustment term
     uio[uio>1] = 1; uis[uis>1] = 1
     uoC = matrix(1-uio*kappa_H, n.gp, 19) #same across different HIV states
     usC = matrix(1-uis*kappa_M, 18,   19) #same across different HIV states
     
     # mortality
     mo = cbind(mor_S,  mor_S,  mor_S,  mor_Ia, mor_I1, mor_I2, mor_I3,
                mor_Ia, mor_I1, mor_Ia, mor_I1, mor_I2, mor_I3, mor_T1, mor_T2, mor_T3,
                mor_I1, mor_I2, mor_I3)

     #x=c(S1,S2,Sp,Ia,I1,I2,I3,Iap,Ip,Da,D1,D2,D3,T1,T2,T3,O1,O2,O3,inc,D_PLHIV,D_diag,death)
     #index for states from S1 to O3
     
     # convert x to a matrix with each row representing a group.
     y = matrix(x, nrow=n.gp, byrow=TRUE) # last column is for incidence
     y2 = y[ , 1:19]
     row.names(y2) = names.gp
     
     # change n.gp group names without OAT, low, high
     rname = gsub(paste(c("/OAT", "/low", "/high"), collapse="|"), "", names.gp)
     # pwid group names without OAT, low, high
     pwid.name = names18[grep("PWID", names18)]

     
     # hiv test rate per month
     # assign the rate into n.gp groups
     #psi=numeric(n.gp);
     phi   = matrix(0, n.gp, 3)
     alpha = matrix(0, n.gp, 3)
     alpha.re =numeric(n.gp)
     theta.t12=numeric(n.gp); theta.t13=numeric(n.gp); theta.t21=numeric(n.gp)
     theta.t23=numeric(n.gp); theta.t31=numeric(n.gp); theta.t32=numeric(n.gp)
     theta.t1O=numeric(n.gp); theta.t2O=numeric(n.gp); theta.t3O=numeric(n.gp)
     rho.m.all=numeric(n.gp) # entry rates for in-migration 
     rho.all  =numeric(n.gp) # entry rates 
     trans.red.all=numeric(n.gp) # reduced HIV transmission due to CCR5 mutation
     mu_mat   =numeric(n.gp) #  maturation-out rate
     for (i in 1:18){
       ind = which(rname %in% names18[i])
       #psi[ind]=psi18[i];
       phi[ind, ]    = c(phi1[i],   phi2[i],   phi3[i])
       alpha[ind, ]  = c(alpha1[i], alpha2[i], alpha3[i])
       alpha.re[ind] = O_T[i]
       theta.t12[ind]= T1_T2[i]; theta.t13[ind] =T1_T3[i]; theta.t21[ind] =T2_T1[i]
       theta.t23[ind]= T2_T3[i]; theta.t31[ind] =T3_T1[i]; theta.t32[ind] =T3_T2[i]
       theta.t1O[ind]= T1_O[i];  theta.t2O[ind] =T2_O[i];  theta.t3O[ind] =T3_O[i]
       rho.m.all[ind]= rho.m[i]
       rho.all[ind]  = rho[i]
       trans.red.all[ind] = trans.red[i]
       mu_mat[ind]   = mat[i]
     }
     
     
     # proportion of oat: number of OAT clients/PWID population
     pop.pwid = init.tot[grep("PWID", names18)]
     prop.oat = nOAT/pop.pwid
     # oat.e: OAT entry rate for 9 groups (collapsing onOAt/offOAT, low/high)
     oat.e = prop.oat/(1-prop.oat) *oat.q 
     
     # ssp coverage: #syringe distributed/(PWID population*d)
     cov.ssp = v.ssp/ (pop.pwid*d*12)
     # set the maximum coverage as 1
     cov.ssp[cov.ssp>1] = 1
     
     # assign oat entry rate and coverage of ssp for all n.gp groups
     oat.e.all   = numeric(n.gp) 
     cov.ssp.all = numeric(n.gp)
     s.all       = numeric(n.gp)
     for (i in 1:length(pwid.name)){
       ind = which(rname %in% pwid.name[i])
       oat.e.all[ind]   = oat.e[i]
       cov.ssp.all[ind] = cov.ssp[i]
       s.all[ind]       = s[i]
     }
     #oat.e.all=c(rep(0,6),rep(oat.e[1:3],4),rep(oat.e[4:6],2),rep(oat.e[7:9],2),rep(0,6))
     #cov.ssp.all=c(rep(0,6),rep(cov.ssp[1:3],4),rep(cov.ssp[4:6],2),rep(cov.ssp[7:9],2),
      #             rep(0,6))
     
     source("Group_indicator.R")

     # sufficient contact rate (y2=y[,1:19], excluding incidence/new diagnosis)
     foi= FoI(y=y2, no=no, uoC=uoC, ns=ns, usC=usC, eO=ass.eO, eS=ass.eS, sigmaFM=sigmaFM, sigmaMF=sigmaMF, sigmaM=sigmaM, tau=tau.all,
              eff.prep=eff.prep, d=d, s=s.all, eff.oat=eff.oat, cov.ssp=cov.ssp.all, eff.ssp=eff.ssp, s.multi=s.multi, trans.red=trans.red.all, bal)
     # the function returns matrix of dim c(n.gp,2) with row of each group. 
     # second column for PrEP
    
     #entry  & maturation rate
     #rho1=rho2=rho3=mu_mat=numeric(42)
     #rho1[m]=rho1_m; rho1[f]=rho1_f #2011-2015
     #rho2[m]=rho2_m; rho2[f]=rho2_f #2016-2040
     #rho2[m]=rho2_m; rho2[f]=rho2_f #2016-2030
     #rho3[m]=rho3_m; rho3[f]=rho3_f #2030-2040
     #if (t<=60) rho=rho1
     #else if (t<=240) rho=rho2
     #else rho=rho3
     #mu_mat[m]=mat_m;mu_mat[f]=mat_f;
     
     #eta: PrEP entry rate
     if (t<6)          eta = eta.m[ , 1]
     if (t>=6  & t<12) eta = eta.m[ , 2]
     if (t>=12 & t<24) eta = eta.m[ , 3] 
     if (t>=24 & t<36) eta = eta.m[ , 4]
     if (t>=36)        eta = eta.m[ , 5] 
     
     
     # out is the population difference between the time t and t+1
     out = matrix(0, n.gp, 24)
     
     #y[,1:19]=y2
     
     # MSM or HET
     for (i in c(msm,het)) {
       out[i, ] = ode_list(y[i, ], foi$beta_o[i, ], foi$beta_s[i, ], foi$gamma[i, ],
                          rho.all[i], ws, wp, mu_mat[i], mo[i, ], eta[i],  #eta for PrEP entry
                          psi[i], psi.p, theta.ai, theta.ad,
                          phi[i, ], v2, v3, alpha[i, ], alpha.re[i],
                          theta.1, theta.2,
                          theta.t12[i], theta.t13[i], theta.t21[i], theta.t23[i],
                          theta.t31[i], theta.t32[i],
                          theta.t1O[i], theta.t2O[i], theta.t3O[i], rho.m.all[i])
     }

     # MSM/PWID NOT on OAT (to add OAT entry/dropout)
     for (j in 1:length(oat)) {
       i=off.oat[j]
       out[i, ] = ode_list_offOAT(y[i, ], y[oat[j], ],
                          foi$beta_o[i, ], foi$beta_s[i, ], foi$gamma[i, ],
                          rho.all[i], ws, wp, mu_mat[i], mo[i, ], eta[i],  #eta for PrEP entry
                          psi[i], psi.p, theta.ai, theta.ad,
                          phi[i, ], v2, v3, alpha[i, ], alpha.re[i],
                          theta.1, theta.2,
                          theta.t12[i], theta.t13[i], theta.t21[i], theta.t23[i],
                          theta.t31[i], theta.t32[i],
                          theta.t1O[i], theta.t2O[i], theta.t3O[i],
                          oat.e.all[i], oat.q, rho.m.all[i])
     }

     # MSM/PWID on OAT (to add OAT entry/dropout)
     for (j in 1:length(oat)) {
       i=oat[j]
       out[i, ] = ode_list_OAT(y[i, ], y[off.oat[j], ],
                          foi$beta_o[i, ], foi$beta_s[i, ], foi$gamma[i, ],
                          rho.all[i], ws, wp, mu_mat[i], mo[i, ], eta[i],    #eta for PrEP entry
                          psi[i], psi.p, theta.ai, theta.ad,
                          phi[i, ], v2, v3, alpha[i, ], alpha.re[i],
                          theta.1, theta.2,
                          theta.t12[i], theta.t13[i], theta.t21[i], theta.t23[i],
                          theta.t31[i], theta.t32[i],
                          theta.t1O[i]*theta.o.oat, theta.t2O[i]*theta.o.oat, theta.t3O[i]*theta.o.oat,
                          oat.e.all[i], oat.q, rho.m.all[i])
     }
     


     if (prop.adj==T) {
       # adjust population to have the same proportion as the initial
       tot.group = rowSums(y2)
       dif.group = sum(y2)*init.group.prop -tot.group
       dif       = dif.group*y2 /tot.group 
       # if n<0 then adjustment is not applied
       dif[which(y2+dif<0, arr.ind=T)] = 0
       out = out + cbind(dif,0,0,0,0,0)
     }
     
     list(as.vector(t(out)))
   })
}
