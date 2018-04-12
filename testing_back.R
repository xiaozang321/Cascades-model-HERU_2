## 1. Back calcaute HIV testing rates ##
##rate = [-ln(new diag/infected)/12]

source("psi_func_v2.R")
psi.out  = psi(y =init, names42 =names.gp, v2 =vparameters$v2, v3 =vparameters$v3, ndiag.obs =ndiag18.obs[2, ], high.p=(vparameters$high.psi-1), high.p.het=(vparameters$high.psi-1))
psi.out$num.test # 487,437 total tests
psi42    = psi.out$psi # monthly testing rates for 36 groups
vparameters$psi = psi42
## Copy the result to excel
write.excel <- function(tab, ...) write.table(tab, "clipboard", sep="\t", row.names =F)
write.excel(psi42)
