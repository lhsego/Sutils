##' Calculate the confidence that at most t samples are unacceptable in 
##' the unsampled population
##' @export
##' @param n1 The number of judgmental samples
##' @param n2 The number of random samples
##' @param fnr The false negative rate associated with the measurements
##' @param prior.prob The prior probability of observing an unacceptable
##' item in the judgmental population
##' @param N The total number of items in the population
##' @param r The likelihood multiplier than an item sampled from the 
##' judgmental subpopulation is going to be unacceptable relative 
##' to the random subpopulation
##' @param subdivisions The number of points to sample in the simpson's rule
##' approximation
##' @param method One of 'C' or 'R'. The default option is to export the 
##' computation to C (much faster). 
##' 
##' @examples
##' 
##' # There are 10 judgmental items
##' nh <- 10
##' 
##' # The prior probability of observing an unacceptable item 
##' # in nh items is 1%
##' prior.prob <- .01
##' 
##' # I will take 50 random samples
##' nl <- 50
##' 
##' # The probability my measurement device/algorithm produces
##' # a false positive is, on average, 0.1
##' fnr <- 0.1
##' 
##' # I believe that the nh items are twice as likely as the nl
##' # items to be unacceptable
##' r <- 2
##' 
##' # There are 500 total items in the population
##' N <- 500
##' 
##' # I will accept at most 5 unacceptable items remaining 
##' # in the population after sampling nh + nl items out of N
##' thresh <- 5
##' 
##' # The confidence is...
##' conf <- fnrConf.hgeo(n1 = nh, n2 = nl, fnr = fnr, N = N,
##'                      r = r, thresh = thresh, prior.prob = prior.prob)
##' 
fnrConf.hgeo <- function(n2, ..., arg.check = TRUE, subdivisions = 1e05+1, method = c('C', 'R')){
  
  # Check that n2 is present
  if (missing(n2))
    stop("'n2', the first (and unnamed) argument, must be specified\n")
  
  # n2 must be >= 0
  if (n2 < 0) {
    warning("'n2 = ", n2, "' was set to 'n2 = 0'\n")
    n2 <- 0
  }
  
  cjrParms(..., arg.check = arg.check)
  
  nh <- n1
  nl <- n2
  Bh <- beta
  rho <- r
  
  # lchoose and lbeta are more robust than lgamma
  sNum <- 0
  sDen <- 0
  for(xh in 0:nh){
    sNum <- sNum + exp(xh*log(fnr) + lchoose(nh, xh) + lbeta(xh+2, nh-xh+Bh))
    sDen <- sDen + exp(xh*log(fnr) + lchoose(nh, xh) + lbeta(xh+1, nh-xh+Bh))
  }
  
  Bl <- rho/(sNum/sDen) - 1
  
  method <- match.arg(method)
  
  if(method == 'R'){
    
    tlIntegral <- function(index){
      
      integrand <- function(x) pbinom(thresh - index, N-nh, x)*(1-x)^(Bl-1)*(1-(1-fnr)*x)^(nl)
      
      theta1 <- seq(0, 0.08, length = 16001)
      theta2 <- seq(0.08, 1, length = 4001)
      
      out <- integ(sapply(theta1, integrand), a = 0, b = 0.08) + integ(sapply(theta2, integrand), a = 0.08, b = 1)
      
      return(out)
      
    }
    
    sum_xl <- sum(sapply(0:nl, function(xl){
      exp(sum(lchoose(nl, xl), xl*log(fnr), lbeta(xl+1, nl-xl+Bl)))
    }))

    confDenom <- prod(sDen, sum_xl)

    cnFun <- function(i,j){
      sum(j*log(fnr), lchoose(nh, i), lchoose(nh, j), log(tlIntegral(i)), lbeta(i+j+1, 2*nh+Bh-i-j))
    }

    confNum <- 0
    for(i in 0:min(nh, thresh)){
      for(j in 0:nh){
        confNum <- confNum + exp(cnFun(i,j))
      }
    }

    conf <- confNum/confDenom

  } else {
    
    if(subdivisions %% 2 == 0){
      subdivisions <- subdivisions + 1
    }
    
    theta <- seq(0, 1, length = subdivisions)
    vLength <- min(nh, thresh) + 1
    
    integralVals <- .C('simpsonsRule',
                       N = as.double(N),
                       nl = as.double(nl),
                       nh = as.double(nh),
                       thresh = as.double(thresh),
                       fnr = as.double(fnr),
                       thetaSeq = as.double(theta),
                       Bl = as.double(Bl),
                       a = as.double(0),
                       b = as.double(1),
                       lfy = as.integer(vLength),
                       lth = as.integer(length(theta)),
                       out = as.double(vector(length = vLength)))$out
    
    # Pass the values of the integral into the probability calculation
    conf <- .C('contFNR',
               nh = as.integer(nh),
               nl = as.integer(nl),
               thresh = as.integer(thresh),
               N = as.double(N),
               integral = as.double(integralVals),
               Bh = as.double(Bh),
               Bl = as.double(Bl),
               rho = as.double(rho),
               fnr = as.double(fnr),
               conf = as.double(1))$conf
    
  }
  
  return(conf)
  
}








