## Explicit solutions to the linear equations of the Markov Chain for the
## Bernoulli CUSUM based on Marion Reynolds' Fortan code

# Initialize variables

Bernoulli.linear.ARL <- function(m, h, p, fast=TRUE, verbose=FALSE){

   # Check
   if ((m < 1) | m%%1) stop("\nm must be an integer >=1\n")

#   if ((m*h)%%1) cat("\nNote in linear.arl(): m*h = ",m*h,"which is not an exact integer.\n",
#                     "Number of states (t) will be rounded to",round(m*h,0),".\n")

   # Many times m*h is not an integer due to machine error--but it should be close:
   if (round(m*h,5)%%1) cat("\nWarning in linear.arl(): round(m*h,5) = ",round(m*h,5),"which",
                            "is not an exact integer.  \nNumber of states (t) will be",
                            "rounded to",round(m*h,0),".\n")
   
   # Define the constants
   k.t <- round(m*h,0)       # t
   k.c <- k.t - m + 1        # c
   iprime <- k.c %/% m       # i'
   k <- k.c %% m             # k
   q <- 1 - p                # q
   
   # check
   if (k.c != (iprime*m+k)) stop("\niprime and k not choosen correctly\n")
   
   if (k.t < m) N <- rep(1/p, k.t)

   else {
     
     if (fast)
       N <- .C("BernoulliLinearARL",
               as.integer(m),
               as.integer(k.t),
               as.integer(k.c),
               as.integer(iprime),
               as.integer(k),
               as.double(p),
               as.integer(verbose),
               N=double(k.t))$N

     else {

        a <- p*q^(m-1)
        b <- 1-q^(m-1)

        N <- rep(NA,k.t)

        # if iprime=0 (or equivalently, c = k and c < m)
        if (iprime == 0) {
           Tc.c <- b - (k.c - 1)*a
           N[k.c] <- (1 + Tc.c) / (p*Tc.c)
        }
   
        # If c >= m
        else { 
   
           ac <- function(x) as.character(x)
          
           # Create the matrix of T[i',i]
           Tc <- matrix(NA,nrow=iprime+2,ncol=m+1)
           rownames(Tc) <- ac(0:(iprime+1))
           colnames(Tc) <- ac(-1:(m-1))
   
           for (i in 0:(m-1)) {
               Tc[ac(0),ac(i)] <- 1
               Tc[ac(1),ac(i)] <- b - i*a
           }
   
           Tc[ac(0),ac(-1)] <- Tc[ac(0),ac(0)] <- 0
           Tc[ac(1),ac(-1)] <- b
   
           for (ip in 2:(iprime+1)) {
               Tc[ac(ip),ac(-1)] <- Tc[ac(ip-1),ac(m-1)]
               sum1 <- 0
               for (i in 0:(m-1)) {
                   sum1 <- sum1 + Tc[ac(ip-1),ac(i)]
                   Tc[ac(ip),ac(i)] <- Tc[ac(ip-1),ac(m-1)] - a*sum1
               }
           }
   
           # c1
           if (k==0) c1 <- 0
           else {
             sum2 <- 0
             for (i in 0:(k-1)) sum2 <- sum2 + (q^i)*Tc[ac(iprime),ac(k-1-i)]
             c1 <- 2*sum2
           }
           
           # c2
           if (iprime <= 1) c2 <- 0
           else {
             sum3 <- 0
             for (i in 0:(m-1)) {
                 sum4 <- 0
                 for (j in 0:(iprime-2)) sum4 <- sum4 + (q^(j*m))*Tc[ac(iprime-1-j),ac(m-1-i)]
                 sum3 <- sum3 + (q^i)*sum4
             }
             c2 <- (q^k)*sum3
           }
           
           # c3
           if ((k == (m-1)) | (iprime == 0)) c3 <- 0
           else {
             sum5 <- 0
             for (i in 0:(m-1-k-1)) sum5 <- sum5 + (q^i)*Tc[ac(iprime-1),ac(m-1-i)]
             c3 <- (q^k)*sum5
           }
           
           # c4
           c4 <- ( q^((iprime-1)*m + k) - Tc[ac(iprime),ac(k)] + Tc[ac(iprime+1),ac(k-1)] ) / p
   
           # Calculate Nc
           N[k.c] <- (c1 + c2 + c3 + c4) / Tc[ac(iprime+1),ac(k-1)]
   
           if (verbose)
              cat("Nc =",round(N[k.c],5),
                  "  c1 =",round(c1,5),
                  "  c2 =",round(c2,5),
                  "  c3 =",round(c3,5),
                  "  c4 =",round(c4,5),"\n")
   
        }
   
        # Calculate remaining values of N
        for (i in (k.c+1):k.t) N[i] <- (1 - q^(i-k.c))/p + (q^(i-k.c))*N[k.c]
      
        for (i in (k.c-1):2) N[i] <- (1/q)*(N[i+1] - p*N[i+m] - 1)
      
        N[1] <- (1 + p*N[m])/p
   
     } #else if !FAST

   } #else 

   # Check that N is decreasing
   n.check <- sort(N,decreasing=TRUE)
#   if (any(n.check != N)) cat("Warning in linear.arl():  N vector is not decreasing.\n")
   if (any(n.check != N)) stop("\nN vector is not decreasing.\n")
   
   return(N)
   
} ## end linear.arl()
   

# This recursive function also works
#Tcf <- function(iprime,i,m,p) {

#  if ((iprime%%1) | (iprime < 0)) stop("iprime must be an integer >= 0\n")
#  if ((m%%1) | (m < 2)) stop("m must be an integer >= 2\n")
#  if (!(i %in% -1:(m-1))) stop("i index not in -1:(m-1)\n")
#  if ((p <= 0) | (p >= 1)) stop("p must be in (0,1)\n")
  
#  q <- 1-p
#  a <- p*q^(m-1)
#  b <- 1-q^(m-1)
  
#  if (iprime == 0) {
#     if (i %in% (-1:0)) return(0)
#     else if (i %in% (1:(m-1))) return(1)
#     else stop("E1 This should not happen.\n")
#  }

#  else if (iprime == 1) {
#     if (i == -1) return(b)
#     else if (i %in% 0:(m-1)) return(b - i*a)
#     else stop("E1 This should not happen.\n")
#  }

#  else if (iprime >= 2) {
#     if (i == -1) return(Tcf(iprime-1,m-1,m,p))
#     else if (i == 0) return(Tcf(iprime-1,m-1,m,p) - a*Tcf(iprime-1,0,m,p))
#     else if (i %in% 0:(m-1)) {
#        summ <- 0
#        for (j in 0:i) summ <- summ + Tcf(iprime-1,j,m,p)
#        return(Tcf(iprime-1,m-1,m,p) - a*summ)
#     }

#     else stop("E1 This should not happen.\n")
#  }

#  else stop("E2 This error should not happen.\n")

#} ## end Tcf

