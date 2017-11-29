## Bernoulli CUSUM
## Uses Markov chain and normalized left eigen vector to find the ARL
## ss = TRUE requests the Steady State ARL
Bernoulli.mc.arl <- function(p0, gamma, m, h, ss=TRUE, head.start.state=NULL, stationary=NULL, ...){

  if ((m < 1) | m%%1) stop("\nm must be an integer >=1\n")

  # Many times m*h is not an integer due to machine error--but it should be close:
  if (round(m*h,5)%%1) cat("\nNote in Bernoulli.mc.arl(): round(m*h,5) = ",round(m*h,5),"which",
                           "is not an exact integer.  \nNumber of states (t) will be",
                           "rounded to",round(m*h,0),".\n")

  if (gamma < 1) stop("  gamma = ", gamma, " which is < 1.\n")

  p1 <- p0 * gamma
  
  if (p1 > 1) stop("  p1 = ",p1," which is > 1.\n")
  
  # t = num.states
  num.states <- round(m*h,0)

  # c = highest.state
  highest.state <- num.states - m + 1

  makeQ <- function(p) {

     Q <- matrix(0,ncol=num.states,nrow=num.states)
   
     Q[1,1] <- 1 - p
     Q[1,m] <- p
   
     for (i in 2:num.states)  Q[i,i-1] <- 1 - p
   
     if (highest.state >= 2)
        for (i in 2:highest.state)  Q[i,i+m-1] <- p

     return(Q)
  }


  if (gamma > 1) {
    M <- solve(diag(num.states) - makeQ(p1))
    N <- M %*% rep(1,num.states)
  }
  else N <- Bernoulli.linear.ARL(m, h, p0)

  if (!is.null(head.start.state))
   firstState <- max(1, head.start.state)
  else
   firstState <- 1

  if (ss) {
#     stationary <- normalized.left.ev(makeQ(p0),check=T)
     if (is.null(stationary)) {

        if (num.states > 500) check <- FALSE
        else check <- TRUE

        if (class(try(
            stationary <- dominant.left.ev(makeQ(p0), check=check, ...)
            )) != "try-error") {
            ssarl.b <- as.numeric(t(stationary) %*% N)
#            calc.something <- sum(stationary[ceiling(length(stationary)/2):length(stationary)])
#            cat("\nSum of lower half of stationary vector =", calc.something, "\n\n")
        }
        else {
            ssarl.b <- NA
            stationary <- NULL
            cat("Error:  dominant.left.ev() failed in Bernoulli.mc.arl.\n")
        }
     }
     else ssarl.b <- as.numeric(t(stationary) %*% N)

     return(list(stationary = stationary,
                 arl.b = N[firstState],
                 arl.g = N[firstState] * p1,
                 ss.arl.b = ssarl.b,
                 ss.arl.g = ssarl.b * p1))
  }
  else return(list(arl.b = N, arl.g = N[firstState] * p1))
}
