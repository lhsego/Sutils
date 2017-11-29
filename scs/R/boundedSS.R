##' Compute the upper bound or lower bound for stratified sample size 
##' @export
##' @param N The sum of all grid cells within the population
##' @param B The value of the hyperparameter associated with the beta prior
##' @param ub The number of unacceptable items one is willing to accept
##' @param targetConf The confidence that you want pct.clean of the grid cells
##' in the population to be acceptable
##' @param rho Determines whether you are computing the upper bound (rho = 1) or 
##' the lower bound (min(rho)) for sample size. By default, the upper bound is 
##' computed (rho = 1)
##' @return 
##' \itemize{
##' \item{samples}{ The required sample size}
##' \item{achievedConf}{ The confidence achieved when taking samples}
##' \item{targetConf}{ The confidence you were trying to achieve}
##' }

boundedSS <- function(N, B, ub, targetConf, rho = 1){
  
  if(rho < 1){
    # Lower bound calculation
    
    objFun <- function(xi) {
      
      n <- get_n(rho, N, alpha = 0, xi = xi)
      
      obj <- boundaryConf(N, n, B, ub, rho = rho) - targetConf
      
      return(obj)
      
    } 
    
    zeroCalc <- boundaryConf(N, rep(0, length(N)), B, ub, rho = rho)
    
    if(zeroCalc > targetConf){
      
      message('No Samples required')
      
      samples <- rep(0, length(N))
      
      conf <- zeroCalc
      
    } else {
      
      maxXi <- get_n(rho, N, alpha = 0)
    
      # Solve for the xi that gives the desired sample size
      xiRoot <- uniroot(objFun, c(1, maxXi), tol = 1e-10)$root
    
      samples <- ceiling(get_n(rho, N, alpha = 0, xi = xiRoot))
    
      # We need to match the sample size here that is used to calculate
      # the confidence when our strata sizes are small
    
      if(any(samples == N)){
      
        index <- which(samples == N)
      
        samples[index] <- N[index] - 1
      
      }
      
      conf <- boundaryConf(N, samples, B, ub, rho = rho)
      
    }
    
  } else {
    
    objFun <- function(x){
      obj <- boundaryConf(N, x, B, ub, rho = rho) - targetConf
      return(obj)
    }
    
    if(objFun(0) > 0){
      
      message('No samples required')
      
      conf <- boundaryConf(N, 0, B, ub, rho = rho)
      
      samples <- 0
      
    } else {
      
      samples <- round(uniroot(objFun, c(0, N))$root, 0)
      
      conf <- boundaryConf(N, samples, B, ub, rho = rho)
      
    }
    
  }
  
  out <- list('samples' = samples, 'achievedConf' = conf, 'targetConf' = targetConf)
  
  return(out)
  
}