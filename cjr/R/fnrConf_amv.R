##' Calculate cjr confidence with fixed false negative probability for judgmental and random samples
##' @export
##' @param nh The number of judgmental samples
##' @param nl The number of random samples
##' @param N The number of total items in the population. Note that \code{nh + nl <= N}
##' @param pl The false negative rate associated with items in the random subpopulation
##' @param ph The false negative rate associated with items in the judgmental subpopulation
##' @param rho The likelihood parameter that determines how many times more likely,
##' on average, a user will sample an unacceptable item in the judgmental population 
##' compared to the random population.
##' @param prior.prob The estimate of the prior probability of finding an acceptable item
##' in the judgmental subpopulation
##' @param unacceptables The number of unacceptable items you are willing to permit in the sampling area
##' @examples
##' # Suppose there are 500 items in the population
##' N <- 500
##' 
##' # Domain experts determine that 10 items make up the judmental partition
##' nh <- 10
##' 
##' # Suppose we'll take 50 additional random samples
##' nl <- 50
##' 
##' # Our particular classification algorithm has a probability of .05 of incorrectly
##' # calling an unacceptable item acceptable. We assume the same probability for both
##' # populations
##' pl <- .05
##' ph <- .05 
##' 
##' # Experts decide that items in the judgmental population are twice as likely
##' # as items in the random population to harbor an unacceptable item
##' rho <- 2
##' 
##' # Experts believe, prior to sampling, that 10% of the nh items are unacceptable
##' prior.prob <- 0.9
##' 
##' # It is determined that 99% of all items must be acceptable
##' pct.clean <- 0.99

fnrConf <- function(nh, nl, N, pl, ph, rho, prior.prob, unacceptables){
  
  nArray <- expand.grid(0:nl, 0:nh)
  colnames(nArray) <- c('nl', 'nh')
  
  Beta <- prior.prob/(1 - prior.prob)
  
  .C("optimCCFNR",
     N = as.integer(N),
     nh = as.integer(nh),
     nl = as.integer(nl),
     nlArray = as.integer(nArray$nl),
     nhArray = as.integer(nArray$nh),
     nLength = as.integer(nrow(nArray)),
     beta = as.double(Beta),
     rho = as.double(rho),
     phi_lo = as.double(pl),
     phi_hi = as.double(ph),
     thresh = as.integer(unacceptables),
     conf = as.double(1))$conf
  
}