# Calculates the number of judgement samples needed to give the required level of
# confidence.

### Uses new model with 
#  prob of judgmental   sample contaminated is distributed Beta(1, beta)
#  prob of random       sample contaminated is distributed Beta(1, r*(beta + 1) - 1)
#  posterior for random sample contaminated is             Beta(1, n2 +  r*n1 + r*(beta + 1) - 1)

# Sample size formulas

##'Finds the number of random samples needed to satisfy CJR input parameters
##'
##'Finds the number of random samples needed to satisfy CJR input parameters
##'
##'
##' @export
##' 
##'@param conf A number in (0,1) giving the desired Bayesian confidence
##'@param \dots Arguments to \code{\link{cjrParms}}
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas.
##'@param round.up \code{=TRUE} rounds the sample size up to the nearest whole
##'integer.
##'@param search.viable.lambda \code{=TRUE} Searches for a large enough value
##'of the percentage clean so that the confidence function is always a
##'non-increasing function of N.
##'@param decimal.precision The level of precision for the viable lambda,
##'passed to \code{\link{find.viable.lambda}}
##'@param arg.check \code{=TRUE} checks the arguments supplied in \code{...}
##'using \code{\link{cjrParms}}.  I would not recommend setting this to
##'\code{FALSE}.
##'@param show.viable.plot \code{=TRUE} makes a plot which illustrates the
##'identification of the viable percentage clean value.
##'@param discrete \code{=TRUE} uses \code{\link{calcConf.discrete}} to
##'calculate the confidence when determining the sample size.
##'@return A list with the following elements: \item{n2}{The required number of
##'random samples} \item{conf.acheived}{The confidence acheived by the
##'\code{n2}} \item{prior.pct.clean}{The expected value of the percentage clean
##'prior to collecting data, which depends the choices for \code{N}, \code{n1},
##'\code{prior.prob} (or \code{beta}), and \code{r}.} \item{use.pct.clean}{The
##'percentage clean used to calculate \code{n2}}
##'\item{recommended.pct.clean}{The percentage clean that will ensure a
##'non-increasing confidence function for the given parameter inputs.}
##'\item{is.viable.request}{A logical indicating whether the requested
##'\code{pct.clean} is viable (i.e. results in a confidence function that is
##'non-decreasing in \code{N}).}
##'@author Landon Sego
##'@seealso \code{\link{find.n2.viable}}
##'@keywords misc
##'@examples
##'
##'find.n2(0.95, n1=10, N=10^4, pct.clean=0.99, prior.prob=0.05, r=1.5, show.viable.plot = TRUE)
##'find.n2(0.95, n1=10, N=10^4, pct.clean=0.99, prior.prob=0.01, r=3, show.viable.plot = TRUE)
##'
find.n2 <- function(conf,
                    ...,
                    maxN = 10^6,
                    round.up = FALSE,
                    search.viable.lambda = TRUE,
                    decimal.precision = 5,
                    arg.check = TRUE,
                    show.viable.plot = FALSE,
                    discrete = FALSE) {

  #  ...  arguments for cjrParms
  
  if (arg.check) {
    
    # Check that conf is present
    if (missing(conf))
      stop("'conf', the first (and unnamed) argument, must be specified\n")
    
    # Check input for conf (the other values are checked in probRemain,
    if ((conf <= 0) | (conf > 1))
      stop("'conf' must be in (0,1].\n")
  
    # If conf >= 1, then uniroot will fail.  By setting conf to a specific
    # number very close to 1, we control the precision (instead of the machine)
    # that determines when the sample size is large enough to get confidence
    # 'close' to 1.
    if (conf >= 1) {
       cat("'conf >= 1' was requested, but 'conf' will be set to 0.9999999999999.\n")
       conf <- 0.9999999999999
    }
  }
  
  # Check the cjr arguments and assign them to this environment
  cjrParms(..., arg.check = arg.check)

  # The following objects are now available for use within the 'find.n2' function:
  # n1
  # N
  # pct.clean
  # prior.prob
  # beta
  # r
  # thresh

  # Only accept singleton (non-vector) arguments to this function:
  if ((length(N) > 1) |
      (length(n1) > 1) |
      (length(r) > 1) |
      (length(beta) > 1) |
      (length(pct.clean) > 1) |
      (length(maxN) > 1))
    stop("'N', 'n1', 'r', 'beta', 'lambda', and 'maxN' must all be singletons\n")
  

  # Catch the silly cases
  if (n1 == N) {
    cat("Warning:  n1 = N = ", n1, "\n")
    return(list(n2 = 0, conf.achieved = NA, use.pct.clean = NA, recommended.pct.clean = NA, is.viable.request = NA))
  }

  # Exact solution for when lambda = 1, note this matches Wright/Grieve method
  # No need to go hunting for a viable pct.clean, since the confidence function is always decreasing
  # for lambda = 1
  # Incidentally, this also works well for very large N
  # Note how the max(0, formula) protects against negative sample sizes
  if (pct.clean >= 1) 
    n2 <- max(0, N - n1 + (conf - 1) * (N - n1 + r * (n1 + beta + 1) - 1))

  # Key Idea:  the viable lambda is the one that coincides with n2=0, this results in the largest viable lambda
  #            for 'all' n2 over 'all' N.
  else { 

    # For large N, calculate the sample size for infinite population, using requested lambda (pct.clean)
    if (N > maxN)
      n2 <- max(0, (log(1  - conf) / log(pct.clean)) - r * (n1 + beta + 1) + 1)
    
    # Otherwise, calculate the naive sample size for finite population, using the requested lambda
    else {
  
      # Make a one-argument objective wrapper function for calcConf
      calcConf.objective <- function(n2) {
        calcConf(n2, maxN, ..., arg.check = FALSE, discrete=discrete) - conf
      }
      
      # Verify that confidence is not already achieved only with the judgmental samples
      if (calcConf.objective(0) >= 0)
        n2 <- 0

      # Otherwise, search for n2
      else {

        # Search for n2 in the interval 0 to (N - n1 - 0.0001)
#        n2 <- try(uniroot(calcConf.objective, c(0, N - n1 - 0.0001), tol=1e-10)$root, silent=TRUE)
        n2 <- try(uniroot(calcConf.objective, c(0, pct.clean*N - n1), tol=1e-14)$root, silent=TRUE)
        
        if (class(n2) == "try-error") 
          stop("uniroot() failed in find.n2(", pvar(conf, n1, r, N, pct.clean, beta, verbose=FALSE), "):\n", n2)
        
      } # else (calcConf.objective(0) < 0)

    } #  else ( <= maxN)

  } # else (pct.clean != 1)

  
  # Round up the random sample size
  if (round.up)    
    n2 <- as.integer(ceiling(round(n2, 14)))

  # Calculate accompanying confidence 
  conf.achieved <- calcConf(n2, maxN, ..., arg.check = FALSE, discrete=discrete)

  # Check to verify whether the current n2 and the requested pct.clean are viable
  is.viable.request <- is.viable.lambda(pct.clean, n1, 0, r, beta, maxN)$viable
  
  # If we need to find a viable lambda because 1) the requested choice was not viable, and 2) the user
  # requested it via 'search.viable.lambda'
  if (!is.viable.request & search.viable.lambda) {

    # Note how the n2 argument is 0.   This is critical.
    recommended.pct.clean <- find.viable.lambda(pct.clean, conf, n1, 0, r, beta, maxN,
                                                decimal.precision = decimal.precision,
                                                show.plot = show.viable.plot)

    #####  Message to display in VSP ###########
##     if (recommended.pct.clean > pct.clean)
##       cat("The choice of '", 100 * pct.clean, "% of the selected area' may be too small given the evidence\n", 
##           "that the decision area is likely free of detectable contamination. This may result in undesirable behavior\n",
##           "in the CJR model.  For more information, please see [link to a help page description].\n",
##           "Increasing the value to at least ", round(100 * recommended.pct.clean, 6),
##           "% will eliminate this phenomena.\n", sep="")
    
  }
  
  # Otherwise, if it is viable, then set the recommended = to the pct.clean
  else if (is.viable.request)
    recommended.pct.clean <- pct.clean
  
  # If it's not viable & no requested search
  else
    recommended.pct.clean <- NA

  # Make a plot of conf versus N
  if (show.viable.plot) {

    # if a device wasn't opened earlier by 'find.viable.lambda', then open one
    if (is.viable.request | !search.viable.lambda) {
      X11(height=7, width=17)
      par(mfrow=c(1,3))
    }

    Nmin <- (n1 + n2) / pct.clean + 0.1
    Nseq <- c(exp(seq(log(Nmin), log(maxN), length=10^4)), maxN + 1)
    
    if (!discrete)
      conf.seq <- calcConf.raw(Nseq, n1, n2, r, beta, pct.clean, maxN, output="confidence")$confidence
    else
      conf.seq <- calcConf.discrete(Nseq, n1, n2, r, beta, pct.clean, maxN)  # This doesn't actually work well due to the osscilations
    
    ylims <- range(c(conf.seq, conf))
    
    plot(log10(Nseq[-length(Nseq)]), conf.seq[-length(conf.seq)], ylim=ylims,
         xlab="log10(N)", ylab="Confidence", type="l",
         main=paste("Profile for requested value of lambda and resulting n2: ",
                    pvar(n2, pct.clean, verbose=FALSE, digits=5)),
         font.main=1)
    
    abline(h = conf.seq[length(conf.seq)], col="Blue")
    abline(h = conf, col="Green")
    abline(v = log10(N), col="Gray", lty=2)
    legend(log10(maxN / 50), 1, c("Aymptotic Conf", "Desired Conf"), col=c("Blue", "Green"), lty=1, lwd=3)
    
  } # if (show.viable.plot)  

  # return results for find.n2()    
  return(list(n2 = n2,
              conf.achieved = conf.achieved,
              prior.pct.clean = 1 - (N + n1 * (r - 1)) / (N * r * (beta + 1)),
              use.pct.clean = pct.clean,
              recommended.pct.clean = recommended.pct.clean,
              is.viable.request = is.viable.request))
    
} # find.n2()

