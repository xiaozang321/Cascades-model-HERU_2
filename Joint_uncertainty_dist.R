####To build correlation between variables for joint uncertainty distribution####
####Developed by Jeremy D. Goldhaber-Fiebert####

####Function definition####
CorrelateUtils <- function(U, Q, epsilon, delta){
  n <- nrow(U) #number of PSA samples
  s <- ncol(U) #number of states
  R <- matrix(rnorm(n*s,0,1), n, s) #the reference matrix.
  C <- matrix(0,s,s) #a place holder for the correlation matrix
  Viol <- matrix(0,s,s) #violations matrix.
  for (j in 2:s){ # {j,k} is the selected pair of state comparisons
    for (k in 1:(j-1)){
      rho <- 1 # #bivariate correlations
      X = U[,c(j,k)] #selected columns of U
      Y = R[,c(j,k)] #selected columns of R
      viol <- 0
      while(viol<epsilon & rho>=0){ #if these conditions are met, continue
        rho <- rho - delta #reduce correltion
        Xstar = induceRankCorrelation(X, Y, rho) #correlated utilities.
        viol = mean((Q[j,k] * Xstar[,1]) < (Q[j,k] * Xstar[,2])) #compute %violations between the col. vectors.
      }
      #Viol[j,k] <- viol
      C[j,k] <- rho + delta #record the desired correlation.
    }
    print(j) #just to show the column indices.
    print(k)
  }
  #Fill in the other elements of C.
  C = C + t(C)
  for (j in 1:s){
    C[j,j] <- 1 # % the diagonal of ones.
  }
  ## Eigenvectors and Eigenvalues correction of C
  eigenResults <- eigen(C)
  B <- eigenResults$values
  V <- eigenResults$vectors
  B[B<=0] <- 0.0001 #to make sure C is positive definite, set eigenvalues<=0 to a very small positive number
  Cstar <- V %*% diag(B) %*% solve(V) #reconstruct C
  Ustar <- induceRankCorrelation(U, R, Cstar) #similar to above, induce the correlation.
  return(Ustar)
}

## To induce Rank correlation: inputs X: QoL vectors, Y is the reference vectors, and Sigma is the correlation matrix.
induceRankCorrelation <- function(X, Y, Sigma){
  if (length(Sigma)==1){ #if Sigma is a single value, convert it to a 2x2 matrix.
    Sigma <- matrix(c(1, Sigma,
                      Sigma, 1), 2, 2)
  }
  n <- nrow(X)
  s <- ncol(X)
  #Initialize matrices.
  Xsorted <- matrix(0, n, s)
  Yrank <- matrix(0, n, s)
  Xstar <- matrix(0, n, s)
  P <- chol(Sigma) #compute the upper triangular matrix
  Ystar <- Y %*% P #Sort the values in the reference vectors by multiplying by P
  cor(Ystar)
  for (j in 1:s){
    Xsorted[,j] <- sort(X[,j]) #Sort each variable
    Yrank[order(Ystar[,j]),j] <- seq(1:n) #Reverse sort
    Xstar[,j]=Xsorted[Yrank[,j],j] #sort Xsorted to have the same ranks as Ystar.
  }
  return(Xstar) #return the sorted vectors.
}


####Parameter setup and derive results####
n = 30 #number of PSA samples
s = 3  #number of states

U      = matrix(0, n, s)  #utility, transmission risl, cost
U[ ,1] = rbeta (n, 9, 1)  #independent distribution for 1st state
U[ ,2] = rbeta (n, 8, 2)  #independent distribution for 2nd state
U[ ,3] = rbeta (n, 7, 3)  #independent distribution for 3rd state

#print(U)
#cor(U)

Q = matrix( c(  0,  0, 0,   #matrix for preference order, -1 if column is preferred over row
               -1,  0, 0,   #1 if row is preferred over column
               -1, -1, 0), s, s, byrow=T)

epsilon = 0.05   #tolerance threshold
delta   = 0.01   #decrement correlation (rho)
Ustar  <- CorrelateUtils(U, Q, epsilon, delta)  #induce correlation & return Ustar

print(Ustar)
