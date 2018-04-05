####This module will check whether in-migration is an important factor to account for in each city

###########
#Read in taregt data and save them in diag18.obs, ndiag18.obs, death18.obs

library(openxlsx)

#install.packages("reshape")
library(reshape)
mig.File.name <- "par_all_v23"
WB.target <- loadWorkbook(paste0(mig.File.name,".xlsx"))

###########
# get in-migration rates

# maturation rate
par18 = read.xlsx(WB.target, sheet = "par18")
mat   = par18$mat*12 # read yearly maturation rate data

# calcualted diagnosed PLHIV in 2012-2105 from previous diagnosed PLHIV,
#           Total diagnosed  + new diagnoses     - deaths            - maturation-out    (total diagnosed*maturation rate)
diag.der  = diag18.obs[-5, ] + ndiag18.obs[-1, ] - death18.obs[-1, ] - diag18.obs[-5, ]*mat

# difference between the calcuated diagnosed PLHIV and observed diagnosed #
diff.diag = diag18.obs[-1, ] - diag.der # considered as in-migration among PLHIV

# prop of in-migration for 2012-2014
mig.p = colMeans((diff.diag /diag18.obs[-1, ])[-4, ]) # average over 2012-2014

mig.r = -log(1 -mig.p)/12 # rate

# copy the result to excel
write.excel <- function(tab, ...) write.table( tab, "clipboard", sep="\t", row.names=F)
write.excel(mig.r)
#the variable "rho.m" in the parameter file