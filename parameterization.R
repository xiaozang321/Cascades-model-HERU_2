################################################################################
## Parameterization for parameters that require manipulations                 ##
################################################################################


#### Parameter in pwid (1st 18 in gp) domain ####
pwid.gn = pwid.gn.fun (names.pwid)

##nOAT: (1) = nOAT_OTP + nOAT_Bup; (2) 6 to 9
nOAT = numeric(length(names.pwid))
nOAT[pwid.gn$M.w]  = round((with(vlist$nOAT_OTP, pe[gender == "m" & ethnicity == "w"]) + with(vlist$nOAT_Bup, pe[gender == "m" & ethnicity == "w"])) * c(with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "w"]), 1-with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "w"])))
nOAT[pwid.gn$M.b]  = round((with(vlist$nOAT_OTP, pe[gender == "m" & ethnicity == "b"]) + with(vlist$nOAT_Bup, pe[gender == "m" & ethnicity == "b"])) * c(with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "b"]), 1-with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "b"])))
nOAT[pwid.gn$M.h]  = round((with(vlist$nOAT_OTP, pe[gender == "m" & ethnicity == "h"]) + with(vlist$nOAT_Bup, pe[gender == "m" & ethnicity == "h"])) * c(with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "h"]), 1-with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "h"])))

nOAT[pwid.gn$F.w] = round(with(vlist$nOAT_OTP, pe[gender == "f" & ethnicity == "w"]) + with(vlist$nOAT_Bup, pe[gender == "f" & ethnicity == "w"]))
nOAT[pwid.gn$F.b] = round(with(vlist$nOAT_OTP, pe[gender == "f" & ethnicity == "b"]) + with(vlist$nOAT_Bup, pe[gender == "f" & ethnicity == "b"]))
nOAT[pwid.gn$F.h] = round(with(vlist$nOAT_OTP, pe[gender == "f" & ethnicity == "h"]) + with(vlist$nOAT_Bup, pe[gender == "f" & ethnicity == "h"]))
vparameters$nOAT = nOAT

##v.ssp: 1 to 9, need to be adjusted later after deriving PWID initials

##s: 6 to 9, mwid = pwid
s = numeric(length(names.pwid))
s[pwid.gn$white] = with(vlist$s, pe[ethnicity == "w"])
s[pwid.gn$black] = with(vlist$s, pe[ethnicity == "b"])
s[pwid.gn$hisp]  = with(vlist$s, pe[ethnicity == "h"])
vparameters$s = s


#### Parameter in MSM domain ####
msm.gn = msm.gn.fun (names.msm)

##nsG: 6 to 18, mwid = msm, non-OAT = OAT
nsG = numeric(length(names.msm))
nsG[msm.gn$low]  = with(vlist$nsG, pe[sexual.intensity == "low"])  #low-risk
nsG[msm.gn$high] = with(vlist$nsG, pe[sexual.intensity == "high"]) #high-risk
vparameters$nsG = nsG

##uis: 6 to 18, mwid = msm, non-OAT = OAT
uis = numeric(length(names.msm))
uis[msm.gn$low]  = with(vlist$uis, pe[sexual.intensity == "low"])  #low-risk
uis[msm.gn$high] = with(vlist$uis, pe[sexual.intensity == "high"]) #high-risk
vparameters$uis = uis


#### Parameter in pop18 (race*gender*risk) domain ####
gp18.gn = gp18.gn.fun (names18)

##T1_O: 12 to 18, m=f for pwid, mwid=pwiD
T1_O = numeric(length(names18))
T1_O[gp18.gn$all.pwid] = with(vlist$T1_O, pe[risk == "pwid"])
T1_O[gp18.gn$msm]      = with(vlist$T1_O, pe[risk == "msm"])
T1_O[gp18.gn$het.f]    = with(vlist$T1_O, pe[risk == "het" & gender == "f"])
T1_O[gp18.gn$het.m]    = with(vlist$T1_O, pe[risk == "het" & gender == "m"])
vparameters$T1_O = T1_O

##T2_O: 12 to 18, m=f for pwid, mwid=pwiD
T2_O = numeric(length(names18))
T2_O[gp18.gn$all.pwid] = with(vlist$T2_O, pe[risk == "pwid"])
T2_O[gp18.gn$msm]      = with(vlist$T2_O, pe[risk == "msm"])
T2_O[gp18.gn$het.f]    = with(vlist$T2_O, pe[risk == "het" & gender == "f"])
T2_O[gp18.gn$het.m]    = with(vlist$T2_O, pe[risk == "het" & gender == "m"])
vparameters$T2_O = T2_O


##T3_O: 12 to 18, m=f for pwid, mwid=pwiD
T3_O = numeric(length(names18))
T3_O[gp18.gn$all.pwid] = with(vlist$T3_O, pe[risk == "pwid"])
T3_O[gp18.gn$msm]      = with(vlist$T3_O, pe[risk == "msm"])
T3_O[gp18.gn$het.f]    = with(vlist$T3_O, pe[risk == "het" & gender == "f"])
T3_O[gp18.gn$het.m]    = with(vlist$T3_O, pe[risk == "het" & gender == "m"])
vparameters$T3_O = T3_O

##T1_T2: 12 to 18, m=f for pwid, mwid=pwiD
T1_T2 = numeric(length(names18))
T1_T2[gp18.gn$all.pwid] = with(vlist$T1_T2, pe[risk == "pwid"])
T1_T2[gp18.gn$msm]      = with(vlist$T1_T2, pe[risk == "msm"])
T1_T2[gp18.gn$het.f]    = with(vlist$T1_T2, pe[risk == "het" & gender == "f"])
T1_T2[gp18.gn$het.m]    = with(vlist$T1_T2, pe[risk == "het" & gender == "m"])
vparameters$T1_T2 = T1_T2

##T1_T3: 12 to 18, m=f for pwid, mwid=pwiD
T1_T3 = numeric(length(names18))
T1_T3[gp18.gn$all.pwid] = with(vlist$T1_T3, pe[risk == "pwid"])
T1_T3[gp18.gn$msm]      = with(vlist$T1_T3, pe[risk == "msm"])
T1_T3[gp18.gn$het.f]    = with(vlist$T1_T3, pe[risk == "het" & gender == "f"])
T1_T3[gp18.gn$het.m]    = with(vlist$T1_T3, pe[risk == "het" & gender == "m"])
vparameters$T1_T3 = T1_T3

##T2_T3: 12 to 18, m=f for pwid, mwid=pwiD
T2_T3 = numeric(length(names18))
T2_T3[gp18.gn$all.pwid] = with(vlist$T2_T3, pe[risk == "pwid"])
T2_T3[gp18.gn$msm]      = with(vlist$T2_T3, pe[risk == "msm"])
T2_T3[gp18.gn$het.f]    = with(vlist$T2_T3, pe[risk == "het" & gender == "f"])
T2_T3[gp18.gn$het.m]    = with(vlist$T2_T3, pe[risk == "het" & gender == "m"])
vparameters$T2_T3 = T2_T3

##T2_T1: 12 to 18, m=f for pwid, mwid=pwiD
T2_T1 = numeric(length(names18))
T2_T1[gp18.gn$all.pwid] = with(vlist$T2_T1, pe[risk == "pwid"])
T2_T1[gp18.gn$msm]      = with(vlist$T2_T1, pe[risk == "msm"])
T2_T1[gp18.gn$het.f]    = with(vlist$T2_T1, pe[risk == "het" & gender == "f"])
T2_T1[gp18.gn$het.m]    = with(vlist$T2_T1, pe[risk == "het" & gender == "m"])
vparameters$T2_T1 = T2_T1

##T3_T1: 12 to 18, m=f for pwid, mwid=pwiD
T3_T1 = numeric(length(names18))
T3_T1[gp18.gn$all.pwid] = with(vlist$T3_T1, pe[risk == "pwid"])
T3_T1[gp18.gn$msm]      = with(vlist$T3_T1, pe[risk == "msm"])
T3_T1[gp18.gn$het.f]    = with(vlist$T3_T1, pe[risk == "het" & gender == "f"])
T3_T1[gp18.gn$het.m]    = with(vlist$T3_T1, pe[risk == "het" & gender == "m"])
vparameters$T3_T1 = T3_T1

##T3_T2: 12 to 18, m=f for pwid, mwid=pwiD
T3_T2 = numeric(length(names18))
T3_T2[gp18.gn$all.pwid] = with(vlist$T3_T2, pe[risk == "pwid"])
T3_T2[gp18.gn$msm]      = with(vlist$T3_T2, pe[risk == "msm"])
T3_T2[gp18.gn$het.f]    = with(vlist$T3_T2, pe[risk == "het" & gender == "f"])
T3_T2[gp18.gn$het.m]    = with(vlist$T3_T2, pe[risk == "het" & gender == "m"])
vparameters$T3_T2 = T3_T2

##O_T: 12 to 18, m=f for pwid, mwid=pwiD
O_T = numeric(length(names18))
O_T[gp18.gn$all.pwid] = with(vlist$O_T, pe[risk == "pwid"])
O_T[gp18.gn$msm]      = with(vlist$O_T, pe[risk == "msm"])
O_T[gp18.gn$het.f]    = with(vlist$O_T, pe[risk == "het" & gender == "f"])
O_T[gp18.gn$het.m]    = with(vlist$O_T, pe[risk == "het" & gender == "m"])
vparameters$O_T = O_T

##rho.m
rho.m = numeric(length(names18))
rho.m[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$rho.m, pe[gender == "m" & risk == "pwid"])
rho.m[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$rho.m, pe[gender == "f" & risk == "pwid"])
rho.m[gp18.gn$msm]  = with(vlist$rho.m, pe[risk == "msm"])
rho.m[gp18.gn$mwid] = with(vlist$rho.m, pe[risk == "mwid"])
rho.m[gp18.gn$het.m] = with(vlist$rho.m, pe[gender == "m" & risk == "het"])
rho.m[gp18.gn$het.f] = with(vlist$rho.m, pe[gender == "f" & risk == "het"])
vparameters$rho.m = rho.m

##trans.red: 3 to 18, only by ethnicity
trans.red = numeric(length(names18))
trans.red[gp18.gn$white] = with(vlist$trans.red, pe[ethnicity == "w"])
trans.red[gp18.gn$black] = with(vlist$trans.red, pe[ethnicity == "b"])
trans.red[gp18.gn$hisp]  = with(vlist$trans.red, pe[ethnicity == "h"])
vparameters$trans.red = trans.red

##rho: 6 to 18, only by ethnicity and gender
rho = numeric(length(names18))
rho[gp18.gn$male]   = with(vlist$rho, pe[gender == "m"])
rho[gp18.gn$female] = with(vlist$rho, pe[gender == "f"])
vparameters$rho = rho

##mat: 6 to 18, only by ethnicity and gender
mat = numeric(length(names18))
mat[gp18.gn$male]   = with(vlist$mat, pe[gender == "m"])
mat[gp18.gn$female] = with(vlist$mat, pe[gender == "f"])
vparameters$mat = mat

##phi1, phi2, phi3, alpha1, alpha2, alpha3: derived from phi1_, phi2_, phi3_, p1, alpha1_, alpha2_, alpha3_
##phi1_, phi2_, phi3_: 7 to 18, msm*ethnicity, pwid, mwid, het*gender
##alpha1_, alpha2_, alpha3_: 6 to 18, msm*ethnicity, pwid, het*gender, mwid=msm
##p1 = prop.ever.art
##phi1 =phi1_*p1; phi2 =phi2_*p1; phi3 =phi3_*p1; alpha1 =alpha1_*(p1*(1-phi1_))/(1-p1*phi1_); alpha2 =alpha2_*(p1*(1-phi1_))/(1-p1*phi2_); alpha3 =alpha3_*(p1*(1-phi3_))/(1-p1*phi3_)
phi1_ = numeric(length(names18)); phi2_ = numeric(length(names18)); phi3_ = numeric(length(names18))
p1 = numeric(length(names18))
alpha1_ = numeric(length(names18)); alpha2_ = numeric(length(names18)); alpha3_ = numeric(length(names18))

phi1_[gp18.gn$msm]     = with(vlist$phi1_, pe[risk == "msm"])
phi1_[gp18.gn$pwid]    = with(vlist$phi1_, pe[risk == "pwid"])
phi1_[gp18.gn$mwid]    = with(vlist$phi1_, pe[risk == "mwid"])
phi1_[gp18.gn$het.m]   = with(vlist$phi1_, pe[risk == "het" & gender == "m"])
phi1_[gp18.gn$het.f]   = with(vlist$phi1_, pe[risk == "het" & gender == "f"])

phi2_[gp18.gn$msm]     = with(vlist$phi2_, pe[risk == "msm"])
phi2_[gp18.gn$pwid]    = with(vlist$phi2_, pe[risk == "pwid"])
phi2_[gp18.gn$mwid]    = with(vlist$phi2_, pe[risk == "mwid"])
phi2_[gp18.gn$het.m]   = with(vlist$phi2_, pe[risk == "het" & gender == "m"])
phi2_[gp18.gn$het.f]   = with(vlist$phi2_, pe[risk == "het" & gender == "f"])

phi3_[gp18.gn$msm]     = with(vlist$phi3_, pe[risk == "msm"])
phi3_[gp18.gn$pwid]    = with(vlist$phi3_, pe[risk == "pwid"])
phi3_[gp18.gn$mwid]    = with(vlist$phi3_, pe[risk == "mwid"])
phi3_[gp18.gn$het.m]   = with(vlist$phi3_, pe[risk == "het" & gender == "m"])
phi3_[gp18.gn$het.f]   = with(vlist$phi3_, pe[risk == "het" & gender == "f"])

p1[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$p1, pe[risk == "pwid" & gender =="m"])
p1[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$p1, pe[risk == "pwid" & gender =="f"])
p1[gp18.gn$het.m] = with(vlist$p1, pe[risk == "het" & gender == "m"])
p1[gp18.gn$het.f] = with(vlist$p1, pe[risk == "het" & gender == "f"])
p1[gp18.gn$msm]   = with(vlist$p1, pe[risk == "msm"])
p1[gp18.gn$mwid]  = with(vlist$p1, pe[risk == "mwid"])

alpha1_[gp18.gn$msm]   = with(vlist$alpha1_, pe[risk == "msm"])
alpha1_[gp18.gn$mwid]  = alpha1_[gp18.gn$msm]
alpha1_[gp18.gn$pwid]  = with(vlist$alpha1_, pe[risk == "pwid"])
alpha1_[gp18.gn$het.m] = with(vlist$alpha1_, pe[risk == "het" & gender == "m"])
alpha1_[gp18.gn$het.f] = with(vlist$alpha1_, pe[risk == "het" & gender == "f"])

alpha2_[gp18.gn$msm]   = with(vlist$alpha2_, pe[risk == "msm"])
alpha2_[gp18.gn$mwid]  = alpha2_[gp18.gn$msm]
alpha2_[gp18.gn$pwid]  = with(vlist$alpha2_, pe[risk == "pwid"])
alpha2_[gp18.gn$het.m] = with(vlist$alpha2_, pe[risk == "het" & gender == "m"])
alpha2_[gp18.gn$het.f] = with(vlist$alpha2_, pe[risk == "het" & gender == "f"])

alpha3_[gp18.gn$msm]   = with(vlist$alpha3_, pe[risk == "msm"])
alpha3_[gp18.gn$mwid]  = alpha3_[gp18.gn$msm]
alpha3_[gp18.gn$pwid]  = with(vlist$alpha3_, pe[risk == "pwid"])
alpha3_[gp18.gn$het.m] = with(vlist$alpha3_, pe[risk == "het" & gender == "m"])
alpha3_[gp18.gn$het.f] = with(vlist$alpha3_, pe[risk == "het" & gender == "f"])

vparameters$phi1 = phi1_ * p1
vparameters$phi2 = phi2_ * p1
vparameters$phi3 = phi3_ * p1
vparameters$alpha1 = alpha1_ * (p1 * (1 - phi1_)) / (1 - p1 * phi1_)
vparameters$alpha2 = alpha2_ * (p1 * (1 - phi2_)) / (1 - p1 * phi2_)
vparameters$alpha3 = alpha3_ * (p1 * (1 - phi3_)) / (1 - p1 * phi3_)

##prop.HIV.aware: 1 to 18
vparameters$prop.HIV.aware = rep(vlist$prop.HIV.aware$pe, 18)

##prop.ever.art: 9 to 18, 1-D data to 3-D data, use male white het as the reference level..= p1
vparameters$prop.ever.art = p1

##prop.curr.art: 3 to  18, only by ethnicity
prop.curr.art = numeric(length(names18))
prop.curr.art[gp18.gn$white] = with(vlist$prop.curr.art, pe[ethnicity == "w"])
prop.curr.art[gp18.gn$black] = with(vlist$prop.curr.art, pe[ethnicity == "b"])
prop.curr.art[gp18.gn$hisp]  = with(vlist$prop.curr.art, pe[ethnicity == "h"])
vparameters$prop.curr.art = prop.curr.art

##prop.high.sus: 7 to  18, 0.25 certain for msm, mpwid, NA for pwid
prop.high.sus = rep(NA, length(names18))
prop.high.sus[gp18.gn$all.msm] = with(vlist$prop.high.sus, pe[risk == "msm"])
prop.high.sus[gp18.gn$het.m]   = with(vlist$prop.high.sus, pe[risk == "het" & gender == "m"])
prop.high.sus[gp18.gn$het.f]   = with(vlist$prop.high.sus, pe[risk == "het" & gender == "f"])
vparameters$prop.high.sus = prop.high.sus

##prop.high.inf: 8 to  18, 0.27 certain for msm, 0.3 certain for mpwid, NA for pwid
prop.high.inf = rep(NA, length(names18))
prop.high.inf[gp18.gn$msm] = with(vlist$prop.high.inf, pe[risk == "msm"])
prop.high.inf[gp18.gn$mwid] = with(vlist$prop.high.inf, pe[risk == "mwid"])
prop.high.inf[gp18.gn$het.m]   = with(vlist$prop.high.inf, pe[risk == "het" & gender == "m"])
prop.high.inf[gp18.gn$het.f]   = with(vlist$prop.high.inf, pe[risk == "het" & gender == "f"])
vparameters$prop.high.inf = prop.high.inf


#### Parameter in 42 full gp domain ####
gp.gn = gp.gn.fun (names.gp)

##noG: 18 to 42, msm=mwid, pwid=high.het, non-OAT=OAT
noG = numeric(length(names.gp))
noG[gp.gn$L.allmsm] = with(vlist$noG, pe[risk == "msm" & sexual.intensity == "low"])
noG[gp.gn$H.allmsm] = with(vlist$noG, pe[risk == "msm" & sexual.intensity == "high"])
noG[gp.gn$L.het.m]  = with(vlist$noG, pe[risk == "het" & sexual.intensity == "low" & gender =="m"])
noG[gp.gn$L.het.f]  = with(vlist$noG, pe[risk == "het" & sexual.intensity == "low" & gender =="f"])
noG[gp.gn$H.het.m]  = with(vlist$noG, pe[risk == "het" & sexual.intensity == "high" & gender =="m"])
noG[gp.gn$H.het.f]  = with(vlist$noG, pe[risk == "het" & sexual.intensity == "high" & gender =="f"])
noG[gp.gn$pwid.m]   = noG[gp.gn$H.het.m]
noG[gp.gn$pwid.f]   = noG[gp.gn$H.het.f]
vparameters$noG = noG

##uio: 12 to 42, no risk diff in low,  pwid=high het, msm=mwid, H.het.f=L.het.f
uio = numeric(length(names.gp))
uio[intersect(gp.gn$L.m, gp.gn$white)] = with(vlist$uio, pe[ethnicity == "w" & gender == "m" & sexual.intensity == "low"])
uio[intersect(gp.gn$L.m, gp.gn$black)] = with(vlist$uio, pe[ethnicity == "b" & gender == "m" & sexual.intensity == "low"])
uio[intersect(gp.gn$L.m, gp.gn$hisp)]  = with(vlist$uio, pe[ethnicity == "h" & gender == "m" & sexual.intensity == "low"])

uio[intersect(gp.gn$female, gp.gn$white)] = with(vlist$uio, pe[ethnicity == "w" & gender == "f"])
uio[intersect(gp.gn$female, gp.gn$black)] = with(vlist$uio, pe[ethnicity == "b" & gender == "f"])
uio[intersect(gp.gn$female, gp.gn$hisp)]  = with(vlist$uio, pe[ethnicity == "h" & gender == "f"])

uio[gp.gn$H.allmsm] = with(vlist$uio, pe[risk == "msm" & sexual.intensity == "high"])
uio[gp.gn$H.het.m]  = with(vlist$uio, pe[risk == "het" & gender =="m" & sexual.intensity == "high"])
uio[intersect(gp.gn$pwid, gp.gn$male)] = uio[gp.gn$H.het.m]
vparameters$uio = uio

##psi: directly pasted from the model calibration, no need to manipulate
vparameters$psi = vlist$psi$pe

##mor_S: 13 to 42, all pwid equal, low=high
mor_S = numeric(length(names.gp))
mor_S[gp.gn$pwid] = with(vlist$mor_S, pe[risk == "pwid"])
mor_S[gp.gn$msm]  = with(vlist$mor_S, pe[risk == "msm"])
mor_S[gp.gn$mwid] = with(vlist$mor_S, pe[risk == "mwid"])
mor_S[gp.gn$het.m]= with(vlist$mor_S, pe[risk == "het" & gender == "m"])
mor_S[gp.gn$het.f]= with(vlist$mor_S, pe[risk == "het" & gender == "f"])
mor_S[gp.gn$OAT] = mor_S[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_S = mor_S

##mor_Ia: = mor_S
vparameters$mor_Ia = vparameters$mor_S

##mor_I1: 4 to 42, only diff by risk
mor_I1 = numeric(length(names.gp))
mor_I1[gp.gn$pwid] = with(vlist$mor_I1, pe[risk == "pwid"])
mor_I1[gp.gn$msm]  = with(vlist$mor_I1, pe[risk == "msm"])
mor_I1[gp.gn$mwid] = with(vlist$mor_I1, pe[risk == "mwid"])
mor_I1[gp.gn$het]  = with(vlist$mor_I1, pe[risk == "het"])
mor_I1[gp.gn$OAT]  = mor_I1[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_I1 = vparameters$mor_I1

##mor_I2: 4 to 42, only diff by risk
mor_I2 = numeric(length(names.gp))
mor_I2[gp.gn$pwid] = with(vlist$mor_I2, pe[risk == "pwid"])
mor_I2[gp.gn$msm]  = with(vlist$mor_I2, pe[risk == "msm"])
mor_I2[gp.gn$mwid] = with(vlist$mor_I2, pe[risk == "mwid"])
mor_I2[gp.gn$het]  = with(vlist$mor_I2, pe[risk == "het"])
mor_I2[gp.gn$OAT]  = mor_I2[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_I2 = vparameters$mor_I2

##mor_I3: 4 to 42, only diff by risk
mor_I3 = numeric(length(names.gp))
mor_I3[gp.gn$pwid] = with(vlist$mor_I3, pe[risk == "pwid"])
mor_I3[gp.gn$msm]  = with(vlist$mor_I3, pe[risk == "msm"])
mor_I3[gp.gn$mwid] = with(vlist$mor_I3, pe[risk == "mwid"])
mor_I3[gp.gn$het]  = with(vlist$mor_I3, pe[risk == "het"])
mor_I3[gp.gn$OAT]  = mor_I3[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_I3 = vparameters$mor_I3

##mor_T1: 4 to 42, pwid and msm all equal, mwid=pwid
mor_T1 = numeric(length(names.gp))
mor_T1[gp.gn$pwid]  = with(vlist$mor_T1, pe[risk == "pwid"])
mor_T1[gp.gn$msm]   = with(vlist$mor_T1, pe[risk == "msm"])
mor_T1[gp.gn$mwid]  = with(vlist$mor_T1, pe[risk == "pwid"])
mor_T1[gp.gn$het.m] = with(vlist$mor_T1, pe[risk == "het" & gender == "m"])
mor_T1[gp.gn$het.f] = with(vlist$mor_T1, pe[risk == "het" & gender == "f"])
mor_T1[gp.gn$OAT]  = mor_T1[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_T1 = vparameters$mor_T1

##mor_T2: 4 to 42, pwid and msm all equal, mwid=pwid
mor_T2 = numeric(length(names.gp))
mor_T2[gp.gn$pwid]  = with(vlist$mor_T2, pe[risk == "pwid"])
mor_T2[gp.gn$msm]   = with(vlist$mor_T2, pe[risk == "msm"])
mor_T2[gp.gn$mwid]  = with(vlist$mor_T2, pe[risk == "pwid"])
mor_T2[gp.gn$het.m] = with(vlist$mor_T2, pe[risk == "het" & gender == "m"])
mor_T2[gp.gn$het.f] = with(vlist$mor_T2, pe[risk == "het" & gender == "f"])
mor_T2[gp.gn$OAT]  = mor_T2[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_T2 = vparameters$mor_T2

##mor_T3: 4 to 42, pwid and msm all equal, mwid=pwid
mor_T3 = numeric(length(names.gp))
mor_T3[gp.gn$pwid]  = with(vlist$mor_T3, pe[risk == "pwid"])
mor_T3[gp.gn$msm]   = with(vlist$mor_T3, pe[risk == "msm"])
mor_T3[gp.gn$mwid]  = with(vlist$mor_T3, pe[risk == "pwid"])
mor_T3[gp.gn$het.m] = with(vlist$mor_T3, pe[risk == "het" & gender == "m"])
mor_T3[gp.gn$het.f] = with(vlist$mor_T3, pe[risk == "het" & gender == "f"])
mor_T3[gp.gn$OAT]  = mor_T3[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_T3 = vparameters$mor_T3

##prop.S2: 21 to 42, LOW: pwid=mwid, msm=het, HIGH: msm=mwid
prop.S2 =  numeric(length(names.gp))
prop.S2[gp.gn$pwid.m]  = prop.S2[intersect(gp.gn$mwid, gp.gn$low)] = with(vlist$prop.S2, pe[risk == "pwid"  & gender == "m"])
prop.S2[gp.gn$pwid.f]  = with(vlist$prop.S2, pe[risk == "pwid"  & gender == "f"])
prop.S2[gp.gn$L.het.m] = prop.S2[intersect(gp.gn$msm, gp.gn$low)] = with(vlist$prop.S2, pe[risk == "het" & gender == "m" & sexual.intensity == "low"])
prop.S2[gp.gn$L.het.f] = with(vlist$prop.S2, pe[risk == "het" & gender == "f" & sexual.intensity == "low"])
prop.S2[intersect(gp.gn$msm, gp.gn$high)] = with(vlist$prop.S2, pe[risk == "msm" & sexual.intensity == "high"])
prop.S2[intersect(gp.gn$mwid, gp.gn$high)] = prop.S2[intersect(gp.gn$msm, gp.gn$high)]
prop.S2[gp.gn$H.het.m] = with(vlist$prop.S2, pe[risk == "het" & gender == "m" & sexual.intensity == "high"])
prop.S2[gp.gn$H.het.f] = with(vlist$prop.S2, pe[risk == "het" & gender == "f" & sexual.intensity == "high"])
vparameters$prop.S2 = prop.S2

##prop.a: 1 to 42
prop.a  = rep(vlist$prop.a$pe, 42)
vparameters$prop.Ia = vparameters$prop.Da = prop.a

##prop.I: 1*3 to 42*3
prop.I = matrix(0, nrow=length(names.gp), ncol=3)
prop.I[ ,1] = as.numeric(with(vlist$prop.I, pe[CD4 ==1]))
prop.I[ ,2] = as.numeric(with(vlist$prop.I, pe[CD4 ==2]))
prop.I[ ,3] = as.numeric(with(vlist$prop.I, pe[CD4 ==3]))
vparameters$prop.I1 = prop.I[ ,1]
vparameters$prop.I2 = prop.I[ ,2]
vparameters$prop.I3 = prop.I[ ,3]

##prop.D: 13*3 to 42*3
prop.D = matrix(0, nrow=length(names.gp), ncol=3)
prop.D[ ,1][gp.gn$pwid]  = with(vlist$prop.D, pe[CD4 ==1 & risk == "pwid"])
prop.D[ ,2][gp.gn$pwid]  = with(vlist$prop.D, pe[CD4 ==2 & risk == "pwid"])
prop.D[ ,3][gp.gn$pwid]  = with(vlist$prop.D, pe[CD4 ==3 & risk == "pwid"])
prop.D[ ,1][gp.gn$msm]   = with(vlist$prop.D, pe[CD4 ==1 & risk == "msm"])
prop.D[ ,2][gp.gn$msm]   = with(vlist$prop.D, pe[CD4 ==2 & risk == "msm"])
prop.D[ ,3][gp.gn$msm]   = with(vlist$prop.D, pe[CD4 ==3 & risk == "msm"])
prop.D[ ,1][gp.gn$mwid]  = with(vlist$prop.D, pe[CD4 ==1 & risk == "mwid"])
prop.D[ ,2][gp.gn$mwid]  = with(vlist$prop.D, pe[CD4 ==2 & risk == "mwid"])
prop.D[ ,3][gp.gn$mwid]  = with(vlist$prop.D, pe[CD4 ==3 & risk == "mwid"])
prop.D[ ,1][gp.gn$het.m] = with(vlist$prop.D, pe[CD4 ==1 & risk == "het" & gender == "m"])
prop.D[ ,2][gp.gn$het.m] = with(vlist$prop.D, pe[CD4 ==2 & risk == "het" & gender == "m"])
prop.D[ ,3][gp.gn$het.m] = with(vlist$prop.D, pe[CD4 ==3 & risk == "het" & gender == "m"])
prop.D[ ,1][gp.gn$het.f] = with(vlist$prop.D, pe[CD4 ==1 & risk == "het" & gender == "f"])
prop.D[ ,2][gp.gn$het.f] = with(vlist$prop.D, pe[CD4 ==2 & risk == "het" & gender == "f"])
prop.D[ ,3][gp.gn$het.f] = with(vlist$prop.D, pe[CD4 ==3 & risk == "het" & gender == "f"])
vparameters$prop.D1 = prop.D[ ,1]
vparameters$prop.D2 = prop.D[ ,2]
vparameters$prop.D3 = prop.D[ ,3]

##prop.T: 12*3 to 42*3
prop.T = matrix(0, nrow=length(names.gp), ncol=3)
prop.T[ ,1][gp.gn$pwid]  = with(vlist$prop.T, pe[CD4 ==1 & risk == "pwid"])
prop.T[ ,2][gp.gn$pwid]  = with(vlist$prop.T, pe[CD4 ==2 & risk == "pwid"])
prop.T[ ,3][gp.gn$pwid]  = with(vlist$prop.T, pe[CD4 ==3 & risk == "pwid"])
prop.T[ ,1][gp.gn$msm]   = with(vlist$prop.T, pe[CD4 ==1 & risk == "msm"])
prop.T[ ,2][gp.gn$msm]   = with(vlist$prop.T, pe[CD4 ==2 & risk == "msm"])
prop.T[ ,3][gp.gn$msm]   = with(vlist$prop.T, pe[CD4 ==3 & risk == "msm"])
prop.T[ ,1][gp.gn$mwid]  = with(vlist$prop.T, pe[CD4 ==1 & risk == "pwid"])
prop.T[ ,2][gp.gn$mwid]  = with(vlist$prop.T, pe[CD4 ==2 & risk == "pwid"])
prop.T[ ,3][gp.gn$mwid]  = with(vlist$prop.T, pe[CD4 ==3 & risk == "pwid"])
prop.T[ ,1][gp.gn$het.m] = with(vlist$prop.T, pe[CD4 ==1 & risk == "het" & gender == "m"])
prop.T[ ,2][gp.gn$het.m] = with(vlist$prop.T, pe[CD4 ==2 & risk == "het" & gender == "m"])
prop.T[ ,3][gp.gn$het.m] = with(vlist$prop.T, pe[CD4 ==3 & risk == "het" & gender == "m"])
prop.T[ ,1][gp.gn$het.f] = with(vlist$prop.T, pe[CD4 ==1 & risk == "het" & gender == "f"])
prop.T[ ,2][gp.gn$het.f] = with(vlist$prop.T, pe[CD4 ==2 & risk == "het" & gender == "f"])
prop.T[ ,3][gp.gn$het.f] = with(vlist$prop.T, pe[CD4 ==3 & risk == "het" & gender == "f"])
vparameters$prop.T1 = prop.T[ ,1]
vparameters$prop.T2 = prop.T[ ,2]
vparameters$prop.T3 = prop.T[ ,3]

##prop.O: 12*3 to 42*3
prop.O = matrix(0, nrow=length(names.gp), ncol=3)
prop.O[ ,1][gp.gn$pwid]  = with(vlist$prop.O, pe[CD4 ==1 & risk == "pwid"])
prop.O[ ,2][gp.gn$pwid]  = with(vlist$prop.O, pe[CD4 ==2 & risk == "pwid"])
prop.O[ ,3][gp.gn$pwid]  = with(vlist$prop.O, pe[CD4 ==3 & risk == "pwid"])
prop.O[ ,1][gp.gn$msm]   = with(vlist$prop.O, pe[CD4 ==1 & risk == "msm"])
prop.O[ ,2][gp.gn$msm]   = with(vlist$prop.O, pe[CD4 ==2 & risk == "msm"])
prop.O[ ,3][gp.gn$msm]   = with(vlist$prop.O, pe[CD4 ==3 & risk == "msm"])
prop.O[ ,1][gp.gn$mwid]  = with(vlist$prop.O, pe[CD4 ==1 & risk == "pwid"])
prop.O[ ,2][gp.gn$mwid]  = with(vlist$prop.O, pe[CD4 ==2 & risk == "pwid"])
prop.O[ ,3][gp.gn$mwid]  = with(vlist$prop.O, pe[CD4 ==3 & risk == "pwid"])
prop.O[ ,1][gp.gn$het.m] = with(vlist$prop.O, pe[CD4 ==1 & risk == "het" & gender == "m"])
prop.O[ ,2][gp.gn$het.m] = with(vlist$prop.O, pe[CD4 ==2 & risk == "het" & gender == "m"])
prop.O[ ,3][gp.gn$het.m] = with(vlist$prop.O, pe[CD4 ==3 & risk == "het" & gender == "m"])
prop.O[ ,1][gp.gn$het.f] = with(vlist$prop.O, pe[CD4 ==1 & risk == "het" & gender == "f"])
prop.O[ ,2][gp.gn$het.f] = with(vlist$prop.O, pe[CD4 ==2 & risk == "het" & gender == "f"])
prop.O[ ,3][gp.gn$het.f] = with(vlist$prop.O, pe[CD4 ==3 & risk == "het" & gender == "f"])
vparameters$prop.O1 = prop.O[ ,1]
vparameters$prop.O2 = prop.O[ ,2]
vparameters$prop.O3 = prop.O[ ,3]


#### Parameter in transmissibility domain: add acute stage ####
sigmaO.FM = numeric(4)
sigmaO.FM[2:4] = vlist$sigmaO.FM$pe
sigmaO.FM[1]   = vparameters$trans.acute * sigmaO.FM[2]
vparameters$sigmaO.FM = sigmaO.FM

sigmaO.MF = numeric(4)
sigmaO.MF[2:4] = vlist$sigmaO.MF$pe
sigmaO.MF[1]   = vparameters$trans.acute * sigmaO.MF[2]
vparameters$sigmaO.MF = sigmaO.MF

sigmaS = numeric(4)
sigmaS[2:4] = vlist$sigmaS$pe
sigmaS[1]   = vparameters$trans.acute * sigmaS[2]
vparameters$sigmaS = sigmaS

tau = numeric(4)
tau[2:4] = vlist$tau$pe
tau[1]   = vparameters$trans.acute * tau[2]
vparameters$tau = tau


#### Parameter in assortativeness domain ####
ass.eO = numeric(length(names.e))
ass.eO[grep("low", names.e)]  = with(vlist$ass.eO, pe[sexual.intensity == "low"])
ass.eO[grep("high", names.e)] = with(vlist$ass.eO, pe[sexual.intensity == "high"])
vparameters$ass.eO = ass.eO

ass.eS = numeric(length(names.e))
ass.eS[grep("low", names.e)]  = with(vlist$ass.eS, pe[sexual.intensity == "low"])
ass.eS[grep("high", names.e)] = with(vlist$ass.eS, pe[sexual.intensity == "high"])
vparameters$ass.eS = ass.eS


#### Parameter in population demongraphics domain ####

##pop.male
pop.male = numeric(3)
pop.male[1] = with(vlist$pop.male, pe[ethnicity == "w"])
pop.male[2] = with(vlist$pop.male, pe[ethnicity == "b"])
pop.male[3] = with(vlist$pop.male, pe[ethnicity == "h"])
vparameters$pop.male = pop.male

##pop.female
pop.female = numeric(3)
pop.female[1] = with(vlist$pop.female, pe[ethnicity == "w"])
pop.female[2] = with(vlist$pop.female, pe[ethnicity == "b"])
pop.female[3] = with(vlist$pop.female, pe[ethnicity == "h"])
vparameters$pop.female = pop.female

##prop.msm.among.male: 1to 3
prop.msm.among.male = numeric(3)
if (city == "NYC"){
  region.prop_m.msm <- numeric(5)
  region.prop_m.msm[1:4] = vlist$prop.msm.among.male$pe[1:4]
  region.prop_m.msm[5]   = with(vlist$prop.msm.among.male, pe[region == "Brooklyn"])
  prop.msm.among.male    = sum(region.prop_m.msm * vlist$prop.msm.among.male$male.pop) / sum(vparameters$pop.male)
  vparameters$prop.msm.among.male = rep(prop.msm.among.male, 3)
} else {
  vparameters$prop.msm.among.male = rep(vlist$prop.msm.among.male$pe, 3)
}

##prop.male.among.pwid: 1 to 3
prop.male.among.pwid <- rep(vlist$prop.male.among.pwid$pe, 3)
vparameters$prop.male.among.pwid = prop.male.among.pwid

##prop.pwid.among.total
prop.pwid.among.total = numeric(3)
prop.pwid.among.total[1] = with(vlist$prop.pwid.among.total, pe[ethnicity == "w"])
prop.pwid.among.total[2] = with(vlist$prop.pwid.among.total, pe[ethnicity == "b"])
prop.pwid.among.total[3] = with(vlist$prop.pwid.among.total, pe[ethnicity == "h"])
vparameters$prop.pwid.among.total = prop.pwid.among.total

##prop.mpwid.among.pwid
prop.mpwid.among.pwid = numeric(3)
prop.mpwid.among.pwid[1] = with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "w"])
prop.mpwid.among.pwid[2] = with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "b"])
prop.mpwid.among.pwid[3] = with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "h"])
vparameters$prop.mpwid.among.pwid = prop.mpwid.among.pwid


#### PrEP parameter ####
vparameters$prep.total = vlist$prep.total$pe

prep.proportion = numeric(3)
prep.proportion[1] = with(vlist$prep.proportion, pe[ethnicity == "w"])
prep.proportion[2] = with(vlist$prep.proportion, pe[ethnicity == "b"])
prep.proportion[3] = with(vlist$prep.proportion, pe[ethnicity == "h"])
vparameters$prep.proportion = prep.proportion