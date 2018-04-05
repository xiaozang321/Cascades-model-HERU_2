prepentry <- function(x, vt, vpar) {
    out_euler <- euler(x, vt, ode_model, vpar)[ ,-1]
    outa = array(out_euler[-1, ], dim = c(n, length(vpar$state.name), length(vpar$names.gp))) # initial value deleted

    msm.h.scep<-outa[ ,1:3, msm.h]

    msm.h.scep.rowsum<-matrix(0, 48, 9)

    for (z in 1:9){
      msm.h.scep.rowsum[ ,z]<-rowSums(msm.h.scep[ , ,z])
    }

    msm.h.scep.mean <- apply(matrix(rowSums(msm.h.scep.rowsum), 4, 12, byrow=T), 1, mean)

    eta.prop<-matrix(0, 3, 4)
    eta.m<-matrix(0, 42, 5)

    eta.prop[ , 1] <- vpar$prep.total[1] * vpar$prep.proportion/ msm.h.scep.mean[1] #M6-M12
    eta.prop[ , 2] <- vpar$prep.total[2] * vpar$prep.proportion/ msm.h.scep.mean[2] #M13-M24
    eta.prop[ , 3] <- vpar$prep.total[3] * vpar$prep.proportion/ msm.h.scep.mean[3] #M25-M36
    eta.prop[ , 4] <- vpar$prep.total[4] * vpar$prep.proportion/ msm.h.scep.mean[4] #M37-M48

    eta.m[msm.h, 2] <- rep(-log(1 - eta.prop[ , 1])/6  ,3)
    eta.m[msm.h, 3] <- rep(-log(1 - eta.prop[ , 2])/12 ,3)
    eta.m[msm.h, 4] <- rep(-log(1 - eta.prop[ , 3])/12 ,3)
    eta.m[msm.h, 5] <- rep(-log(1 - eta.prop[ , 4])/12 ,3)

  return (eta.m)
}