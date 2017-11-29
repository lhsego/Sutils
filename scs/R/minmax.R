##' Find the minimum sample size that maximizes alpha 
##' @export
##' @param results the data.frame produced by \code{\link{findSS}}
##' @param epsilon the tolerance parameter for accepting a larger sample
##' size in exchange for a larger alpha. Choose larger sample size if 
##' \eqn{(min \sum n - candidate \sum n)/(min \sum n)
##' < \epsilon}
##' @return The minimum sample allocation with the maximum alpha (more proportional sampling)
##' @examples
##' 
##' # generate a candidate data frame of sample sizes 
##' 
##' sampleAllocations <- findSS(conf = 0.95, rho = c(1, 0.5), N = c(1000,2000),
##'                             prior.prob = 0.99, pct.clean = 0.99, 
##'                             alphaSeq = c(0.5, 0.8), minmax = FALSE, nJobs = 2)
##' 
##' # Return the allocation that minimizes sum n and maximizes alpha.
##' optimum <- minmax(sampleAllocations)
##'                                                         

minmax <- function(results, epsilon = 0.01){
  
  index.of.min <- which(results$n.sum == min(results$n.sum))
  
  # Save all those instances where sum n is minimized
  minima <- results[index.of.min,]

  # Set of values where sum n > minima
  remainder <- results[which(results$n.sum != min(results$n.sum)),]
  
  # keep the minimum with the maximum alpha
  maxMinima <- minima[which(minima$alpha == max(minima$alpha)),]
  
  # pull out the alpha for use below
  mmAlpha <- minima$alpha[which(minima$alpha == max(minima$alpha))]
  
  # compare maxMinima to elements in remainder. If 
  # % difference in total sample size is less than 
  # epsilon, then min <- remainder[i,] since we prefer
  # sampling proportionally
  
  relative.diff <- 0
  
  if(nrow(remainder) == 0){
    # Every row of results is same sample size
    maxMinima <- maxMinima
    
  } else {
  
      for(i in 1:nrow(remainder)){
  
        if(remainder$n.sum[i] == 0){
          # If the remainders all take 0 samples => maxMinima also 
          # has zero samples. Increase relative.diff so it fails 
          # line 43.
          relative.diff <- epsilon + .01
        } else {
          relative.diff <- abs(maxMinima$n.sum - remainder$n.sum[i])/maxMinima$n.sum 
        }
        # if relative.diff is smaller than epsilon and the set of n's 
        # has a larger alpha, replace the current min with remainder[i,]
        if(relative.diff < epsilon & remainder$alpha[i] > mmAlpha){
          maxMinima <- remainder[i,]
        }
  
      } # for
  
  }
  
  return(maxMinima)
  
}
