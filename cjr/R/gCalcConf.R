# Simulate the confidence of the Generalized CJR method


##'Simulate the confidence of the generalized CJR
##'
##'Simulate the true confidence using a generalization of the CJR method.
##'Useful for sensitivity studies regarding misspecification of the prior
##'parameters, the number and location of judgmental samples.
##'
##'
##' @export
##' 
##'@param n1 Number of judgmental samples taken (perceived number of high risk
##'cells)
##'@param nh True number of high-risk cells
##'@param n2 Number of random samples taken from unsampled areas, which are
##'presumed by the investigator to be low-risk cells.
##'@param N Population size
##'@param beta The true value of \deqn{\frac{1-E(\theta_h)}{E(\theta_h)}}{(1 -
##'E(\theta_h)) / E(\theta_h)}
##'@param rho The true value of
##'\deqn{\frac{E(\theta_h)}{E(\theta_l)}}{E(\theta_h) / E(\theta_l)}
##'@param p The probability that a judgmental sample is placed in a high-risk
##'cell.
##'@param lambda The nominal fraction of the population that we desire to show
##'is acceptable
##'@param c.prime The desired confidence level
##'@param nreps The number of Monte Carlo replicates
##'@return A list with the following components: \item{conf}{The simulated
##'confidence} \item{se}{The standard error of the confidence estimate}
##'\item{nreps}{The number of Monte Carlo replicates} \item{z}{The z-score
##'comparing the simulated \code{conf} and the desired \code{c.prime}}
##'@author Landon Sego
##'@seealso \code{\link{calcConf}}
##'@keywords misc
##'@examples
##'
##'gCalcConf(25, 18, 75, 10^4, 90, 2, 0.80, 0.99)
##'
gCalcConf <- function(n1, nh, n2, N, beta, rho, p, lambda, c.prime=0.95, nreps=10^4) {

  # n1 = number of judgmental samples taken (percieved # of high risk)
  # nh = true number of high-risk cells
  # n2 = number of random samples taken (from unsampled areas--presumed, but not necessarily, low risk)
  # N = population size
  # beta = The true value of (1 - E(theta_h)) / E(theta_h)
  # rho = The true value of E(theta_h) / E(theta_l)
  # p = accuracy in chossing high-risk cells for judgmental samples
  # lambda = nominal pct.clean -- will be translated into a discrete number of defective items
  # c.prime = nominal confidence level
  # nreps = number of monte carlo reps

  num.unacceptable <- floor(N - lambda*N)
  
  c.hat <- .C("gconf",
              as.integer(nreps),
              as.integer(n1),
              as.integer(nh),
              as.integer(n2),
              as.integer(N),
              as.double(beta),
              as.double(rho),
              as.double(p),
              as.integer(num.unacceptable),
              conf=double(1),
              PACKAGE="cjr")$conf

  se <- sqrt(c.hat*(1-c.hat)/nreps)

  return(list(conf=c.hat, se=se, nreps=nreps, z=(c.hat-c.prime)/se))

} # conf



# Testing the use of rbinom and rhyper in C--making sure the ints and doubles are correct

## testH <- function(nh, nb, n) {

##  return(.C("testH",
##            as.integer(nh),
##            as.integer(nb),
##            as.integer(n),
##            out=integer(1),
##            PACKAGE="cjr")$out)  

## } # testH


## testB <- function(n, p) {

##  return(.C("testB",
##            as.integer(n),
##            as.double(p),
##            out=integer(1),
##            PACKAGE="cjr")$out)  

## } # testB
