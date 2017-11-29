#BETA GOODNESS OF FIT FUNCTION

betaGof <- function(X,
		    theta,
		    nbins = NULL,
	            alpha = 0.05)
{
 #Author: Stephen J. Walsh
 #Date: 10/08/2010

 #H0: Data come from fitted beta distribution.
 # large p.value provides no evidence against this hypothesis.

  X <- X[!is.na(X)]
  N <- length(X)
 #num params
  k <- length(theta)

 #get bins of equal probability: don't run test with 3 or fewer bins -- need at least 1 DF
  if(is.null(nbins))
  {
    nbins <- floor(min(N/5,20))
  }
   
  if(nbins < 4) stop("Not enough data to perform the test!!") 

  eq.prob <- 1/nbins
  eq.prob.seq <- seq(0,1,by = eq.prob)
  bins <- qbeta(eq.prob.seq,shape1 = theta[1],shape2 = theta[2])
 
 #get obs.freqs
 
 obs.f <- NULL
 for(i in 1:nbins)
 {
  if(i == 1)
  {
    obs.f.i <- sum(X >= bins[i] & X <= bins[i+1]) 
  }else{
   obs.f.i <- sum(X >bins[i] & X <= bins[i+1])
  }  
  obs.f <- c(obs.f,obs.f.i)
 }

 #compute expected number in bins under estimated model
  exp.f <- round(N*eq.prob)
 #chisq test stat
  df <- nbins - k - 1
  chi.t <- sum(((obs.f - exp.f)^2)/exp.f)
  p.value <- 1 - pchisq(chi.t,df)
  if(p.value > alpha)
  {
   conclusion <- "Fitted Beta is Reasonable"
  }else{
   conclusion <- "Fitted Beta is Not Reasonable"
  }
  list(p.value = p.value,
       conclusion = conclusion,
       chi.star = chi.t,
       nbins = nbins)
}