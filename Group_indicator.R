####Generate grouping indicators for the 42 population groups####
##in all, originally from obj_func##
m        = c(grep("MSM", names.gp), grep("/male", names.gp))
f        = grep("female", names.gp)
white    = grep("white", names.gp)
black    = grep("black", names.gp)
hisp     = grep("hisp", names.gp)
all.msm  = grep("MSM", names.gp)
all.idu  = grep("PWID", names.gp)
midu     = intersect(all.idu, all.msm)
oat      = grep("OAT", names.gp)
het      = grep("HET", names.gp)
low      = grep("low", names.gp)
high     = grep("high", names.gp)

##in FoI_JM_v9_ass_low##
msm.l    = intersect(all.msm, low)  #including midu
msm.h    = setdiff(all.msm, msm.l)
het.l    = intersect(het, low)      
het.m.l  = intersect(het.l, m)
het.f.l  = intersect(het.l, f)
m.high   = setdiff(m, het.m.l)   #including idu and msm, actually represents m.others
f.high   = setdiff(f, het.f.l)   #including idu, actually represents f.others

##in obj_func##
msm      = setdiff(all.msm, all.idu)
idu      = setdiff(all.idu, all.msm)
het.m    = intersect(het, m)
het.f    = intersect(het, f)
off.oat  = setdiff(all.idu, oat)

##in psi_func##
idu.m    = intersect(idu, m)
idu.f    = intersect(idu, f)

msmL     = intersect(msm, low)   #excluding midu
msmH     = intersect(msm, high)  #excluding midu
miduL    = intersect(midu, low)  #excluding midu
miduH    = intersect(midu, high) #excluding midu

het.mL   = intersect(het.m, low)  #=het.m.l
het.mH   = intersect(het.m, high) #excluding idu and msm
het.fL   = intersect(het.f, low)  #=het.f.l
het.fH   = intersect(het.f, high) #excluding idu