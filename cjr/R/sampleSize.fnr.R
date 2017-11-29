##' Compute the minimum sample size to reach a specified target confidence using either the 
##' exact method or the asymptotic approximation.
##' 
##' @author Alex Venzin
##' @export
##' @param N The total number of items in the population
##' @param nh The number of judgmental samples
##' @param fnr The false negative probability
##' @param rho The expectation relation multiplier
##' @param thresh The number of items that one is willing to accept following sampling that
##' are unacceptable
##' @param prior.prob The prior probability of observing an unacceptable item in the judgmental
##' subpopulation
##' @param targetConf The target confidence you are trying to achieve
##' @param method Can be one of 'exact' or 'approximation'. If \code{(1-pct.clean)*N}
##' is large, then the approximation method is significantly faster, but less accurate
##' @param nProcesses The number of parallel processes required to calculate the exact confidence 

sampleSize.fnr <- function(N, nh, fnr, rho, thresh, prior.prob, targetConf, 
                           method = c('discrete', 'continuous'), nProcesses = 1){
  
  method <- match.arg(method)
  
  if(method == 'discrete'){
  
    objFun <- function(nl){
      
      calcConf.fnr(n1 = nh, n2 = nl, fnr = fnr, 
                   r = rho, N = N, prior.prob = prior.prob,
                   thresh = thresh, nProcesses = nProcesses, method = 'lbeta') - targetConf
      
    }
    
  
    if(objFun(N-nh) < 0){
    
      warning('Number of samples required exceeds number of items in population.')
    
      out <- list('samples' = Inf, 'confidence' = NaN) 
    
    } else if(objFun(0) > 0){
      
      message('No samples required to achieve target confidence')
    
      achieved.conf <- calcConf.fnr(n1 = nh, n2 = 0, fnr = fnr, 
                                    r = rho, N = N, prior.prob = prior.prob, 
                                    thresh = thresh, nProcesses = nProcesses, method = 'lbeta')
    
      out <- list('samples' = 0, 'achieved.conf' = achieved.conf, 'true.conf' = targetConf)
    
    } else {
    
      samps <- uniroot(objFun, c(0, N-nh), tol = 1e-08)$root
    
      achieved.conf <- calcConf.fnr(n1 = nh, n2 = ceiling(samps), fnr = fnr, 
                                    r = rho, N = N, prior.prob = prior.prob, 
                                    thresh = thresh, , nProcesses = nProcesses,
                                    method = 'lbeta')
    
      out <- list('samples' = ceiling(samps), 'achieved.conf' = achieved.conf, 'true.conf' = targetConf)
    
    }
    
    # fnrConf.hgeo
  
  } else {
    
    objFun <- function(nl){
      fnrConf.hgeo(n1 = nh, n2 = nl, N = N, fnr = fnr, r = rho, 
                   prior.prob = prior.prob, thresh = thresh) - targetConf
    }
    
    test1 <- try(objFun(N-nh))
    
    test2 <- try(objFun(0))
    
    if(inherits(test1, 'try-error')){
      
      message('Algorithm failure. Returning parameters.')
      
      out <- list(nh = nh, nl = N-nh, N = N, fnr = fnr, rho = rho,
                  prior.prob = prior.prob, thresh = thresh, 
                  targetConf = targetConf)
      
    } else if(test1 < 0){
      
      message('Number of samples required exceeds number of items in population.')
      
      out <- list('samples' = Inf, 'confidence' = NaN) 
      
    } else if(inherits(test2, 'try-error')){
      
      message('Algorithm failure. Returning parameters.')
      
      out <- list(nh = nh, nl = 0, N = N, fnr = fnr, rho = rho,
                  prior.prob = prior.prob, thresh = thresh, 
                  targetConf = targetConf)
      
    } else if(test2 > 0){
      
      message('No samples required to achieve target confidence')
        
      achieved.conf <- fnrConf.hgeo(n1 = nh, n2 = 0, N = N, fnr = fnr, r = rho, 
                                      prior.prob = prior.prob, thresh = thresh)
      
      out <- list('samples' = 0, 'achieved.conf' = achieved.conf, 'true.conf' = targetConf)
       
    } else {
    
      samps <- try(uniroot(objFun, c(0, N-nh), tol = 1e-08)$root)
      
      if(inherits(samps, 'try-error')){
        
        message('Algorithm failure. Returning parameters.')
        
        out <- list(nh = nh, nl = NA, N = N, fnr = fnr, rho = rho,
                    prior.prob = prior.prob, thresh = thresh, 
                    targetConf = targetConf)
        
      } else {
      
        achieved.conf <- fnrConf.hgeo(n1 = nh, n2 = ceiling(samps), N = N, fnr = fnr, r = rho, 
                                    prior.prob = prior.prob, thresh = thresh)
      
        out <- list('samples' = ceiling(samps), 'achieved.conf' = achieved.conf, 'true.conf' = targetConf)
      
      }
    
    }
  
  }
  
  return(out)
  
}