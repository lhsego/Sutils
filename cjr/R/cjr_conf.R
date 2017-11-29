##' Compute the confidence for the original CJR model
##' @export
##' @param lambda the upper tolerance limit for the percentage of acceptable,
##' low risk items (technically 1 - lambda)
##' @param m the number of low risk, random samples
##' @param nh the number of high-risk, judgmental samples
##' @param rho the expectation relation between the low-risk samples and 
##' high risk samples such that one is rho times more likely to observe
##' an unacceptable \code{nh} than an unaccpetable \code{m}
##' @param Bh the hyperparameter for the beta prior distribution
##' @param N the total number of items in the population
##' @author Alex Venzin
##' @examples
##' # I want no more than 1% of all items in the population to be unacceptable
##' # (same as saying 99% of items acceptable)
##' lambda <- 0.99
##' 
##' # Experts have decided that there are 30 high-risk items, all of which must
##' # be sampled and verified clean
##' nh <- 30
##' 
##' # There are 5000 total grid cells in the population
##' N <- 5000
##' 
##' # Experts determine one is 2x as likely to observe an unacceptable 
##' # item in the high-risk sample relative to the low-risk sample
##' rho <- 2
##' 
##' # Experts believe, a priori, that there is a 1/100 probability of 
##' # observing an unacceptable item in the high risk population. To estimate
##' # the hyperparameter: (1 - 1/100)/(1/100) = 99
##' Bh <- 99
##' 
##' # Inverting the problem shows that n = 38 random samples are required to 
##' # reach a target confidence of 0.95
##' n <- 38
##' 
##' Conf <- cjr.conf(0.99, 38, 30, 2, 99, 5000)

cjr.conf <- function(lambda, m, nh, rho, Bh, N){
  
  Bl <- rho*(Bh+nh+1)
  
  A1 <- N - nh - m + 1
  A2 <- lambda*N - nh + Bl - 1
  A3 <- lambda*N - nh - m
  A4 <- N - nh + Bl
  
  conf <- 1 - exp(lgamma(A1) + lgamma(A2) - lgamma(A3) - lgamma(A4))
  
  return(conf)
}
