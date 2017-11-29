## Test function for comparing the results of my lambda nu solver against the theoretical 
## bound for the original CJR model

#' Compute minimum viable lambda
#' 
#' Computes the minimum lambda such that the posterior predictive distribution
#' is a strictly non-decreasing function of the supplied parameters.
#' 
#' 
#' @param lambda the upper tolerance limit, i.e, "I desire that no more than (1
#' - lambda)*N of the low-risk, unsampled items in the population are
#' unacceptable."
#' @param n the number of low-risk, random samples
#' @param nh the number of high-risk, judgmental samples
#' @param xh the estimated number of unacceptable, high-risk items present in
#' the population
#' @param rho the expectation relation that governs the relationship between
#' sampled, judgmental items and low-risk, random items in the population
#' @param Bh the hyperparameter for theta_h
#' @param pl the estimated probability that the measurement device or method
#' incorrectly determines that an unacceptable low-risk item is acceptable
#' @param N the total number of items in the population (or sample space).
#' @return The minimum viable lambda such that the confidence function is
#' strictly non-decreasing. Note that if the supplied lambda is already
#' sufficient, then lmin = lambda. In any case, lmin >= lambda.
#' @author Alex Venzin

min.lamb.cjr <- function(lambda, n, nh, xh, rho, Bh, pl, N){ 
  
  # If the lambda is not sufficient, we know the true minimum is between (lambda, 1]
  
  # Bl in CJR model (not true of FNR model)
  Bl <- rho*(Bh+nh+1)
  
  lmax <- 1
  lmin <- lambda
  
  fnr.conf <- function(lambda, m, nh, rho, Bh, N){
    
    Bl <- rho*(Bh+nh+1)
    
    A1 <- N - nh - m + 1
    A2 <- lambda*N - nh + Bl - 1
    A3 <- lambda*N - nh - m
    A4 <- N - nh + Bl
    
    conf <- 1 - exp(lgamma(A1) + lgamma(A2) - lgamma(A3) - lgamma(A4))
    
    return(conf)
  }
  
  # Create a sequence of increasing N = {N_0, N_1, ..., N_m}. These will be used to 
  # compute forward differences to estimate the first derivative of the confidence 
  # function.
  
  N <- 2*(n + nh + 5)
  
  M <- seq(n+nh+5, N + 1e5, length.out = 2000)
  
  flag <- 0
  lmid <- (lmin + lmax)/2
  
  # .Machine$double.eps = 2^-52 (machine precision of R)
  
  while(flag <= 0){
    state <- any(diff(fnr.conf(lmid, n, nh, Bl, M)) > .Machine$double.eps)
    
    # state == TRUE implies that at least one estimated f'(N) > 0   
    # state == FALSE implies that all estimated f'(N) < 0 and the choice of lambda may 
    # be prohibitively large
    
    if(state == TRUE){
      lmin <- lmid  
      lmid <- (lmax + lmid)/2
    } else {
      lmax <- lmid
      lmid <- (lmid + lmin)/2
    }
    
    # When the absolute difference between the estimated upper bound for lambda 
    # and the current value of the optimal lambda is less than 1e-04
    # then we can terminate the search.
    
    if(abs(lmax - lmid) < 1e-06){
      flag = 1
    }
  }    
  return(signif(lmid,3))
}
