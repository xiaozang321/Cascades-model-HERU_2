#### Functions to derive group number within different domains of parameter ####

pwid.gn.fun = function (names.pwid){
  female = grep("female",  names.pwid)
  male   = grep("female",  names.pwid, invert =T)
  white  = grep("white",   names.pwid)
  black  = grep("black",   names.pwid)
  hisp   = grep("hispanic",names.pwid)
  F.w    = intersect(female, white)
  F.b    = intersect(female, black)
  F.h    = intersect(female, hisp)
  M.w    = intersect(male, white)
  M.b    = intersect(male, black)
  M.h    = intersect(male, hisp)
  return(list(female=female, male=male, white=white, black=black, hisp=hisp, F.w=F.w, F.b=F.b, F.h=F.h, M.w=M.w, M.b=M.b, M.h=M.h))
}

msm.gn.fun = function (names.msm){
  white  = grep("white",   names.msm)
  black  = grep("black",   names.msm)
  hisp   = grep("hispanic",names.msm)
  high   = grep("high",    names.msm)
  low    = grep("low",     names.msm)
  H.w    = intersect(high, white)
  H.b    = intersect(high, black)
  H.h    = intersect(high, hisp)
  L.w    = intersect(low, white)
  L.b    = intersect(low, black)
  L.h    = intersect(low, hisp)
  return(list(white=white, black=black, hisp=hisp, high=high, low=low, H.w=H.w, H.b=H.b, H.h=H.h, L.w=L.w, L.b=L.b, L.h=L.h))
}

gp18.gn.fun = function (names.18){
  white    = grep("white",   names.18)
  black    = grep("black",   names.18)
  hisp     = grep("hispanic",names.18)
  female   = grep("female",  names.18)
  male     = grep("female",  names.18, invert =T)
  all.msm  = grep("MSM",     names.18)
  all.pwid = grep("PWID",    names.18)
  mwid     = grep("MSM/PWID",names.18)
  het      = grep("HET",     names.18)
  het.m    = intersect(het, male)
  het.f    = intersect(het, female)
  msm      = setdiff(all.msm, all.pwid)
  pwid     = setdiff(all.pwid, all.msm)
  return(list(white=white, black=black, hisp=hisp, female=female, male=male, all.msm=all.msm, all.pwid=all.pwid, msm=msm, pwid=pwid, mwid=mwid, het=het, het.m=het.m, het.f=het.f))
}

gp.gn.fun = function (names.gp){
  white    = grep("white",   names.gp)
  black    = grep("black",   names.gp)
  hisp     = grep("hispanic",names.gp)
  female   = grep("female",  names.gp)
  male     = grep("female",  names.gp, invert =T)
  low      = grep("low",     names.gp)
  high     = grep("high",    names.gp)
  all.msm  = grep("MSM",     names.gp)
  all.pwid = grep("PWID",    names.gp)
  msm      = setdiff(all.msm, all.pwid)
  pwid     = setdiff(all.pwid, all.msm)
  mwid     = grep("MSM/PWID",names.gp)
  het      = grep("HET",     names.gp)
  H.allmsm = intersect(high, all.msm)
  L.allmsm = intersect(low,  all.msm)
  het.m    = intersect(het, male)
  het.f    = intersect(het, female)
  pwid.m   = intersect(pwid, male)
  pwid.f   = intersect(pwid, female)
  H.het.m  = intersect(high, het.m)
  H.het.f  = intersect(high, het.f)
  L.het.m  = intersect(low, het.m)
  L.het.f  = intersect(low, het.f)
  H.m      = intersect(high, male)
  H.f      = intersect(high, female)
  L.m      = intersect(low, male)
  L.f      = intersect(low, female)
  return(list(white=white, black=black, hisp=hisp, female=female, male=male, 
              low=low, high=high, all.msm=all.msm, all.pwid=all.pwid, 
              msm=msm, pwid=pwid, mwid=mwid, het=het, het.m=het.m, het.f=het.f,
              pwid.m=pwid.m, pwid.f=pwid.f, H.allmsm=H.allmsm, L.allmsm=L.allmsm, 
              H.het.m=H.het.m, H.het.f=H.het.f, L.het.m=L.het.m, L.het.f=L.het.f,
              H.m=H.m, H.f=H.f, L.m=L.m,  L.f=L.f))
}

