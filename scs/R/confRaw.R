##' Compute the Bayesian confidence for the stratified sampling model
##'
##' @rdname confRaw
##'
##' @export
##' @param n the vector of samples to take from each stratum
##'
##' @param rho a vector of relative weights of finding an unacceptable item relative to
##' stratum 1. Note that rho[1] = 1 always.
##'
##' @param N the vector of total grid cells for each stratum
##'
##' @param B the beta hyperparameter of theta ~ beta(1, B)
##' larger values of B imply lower probability of finding an unacceptable item
##'
##' @param ub the total number of unacceptable items one is willing to accept
##' that remain in the unsampled grid cells
##'
##' @param subInts the number of sub-intervals to use for the application of
##' Simpson's rule. The number must be odd. Default = 10001.
##'
##' @param method The integration method:  Simpson's rule or quadrature
##'
##' @param clusterName The name of a cluster created by
##' \code{\link[parallel:makeCluster]{parallel::makeCluster}}, which,
##' if supplied will initiate parallel computation of the integrand before passing it into
##' Simpson's method.  Ignored for \code{method = "quadrature"}.  If \code{NULL}, no
##' parallelization is performed.
##'
##' @return the Bayesian confidence as estimated by an application of Simpson's Rule.
##'
##' @author Alex Venzin
##'
##' @examples
##'
##' # N must be an integer
##' N <- c(1000, 1000, 1000)
##'
##' # n must be an integer
##' n <- c(100, 75, 50)
##'
##' rho <- c(1, 0.85, 0.75)
##'
##' B <- 70
##'
##' # 1% of total grid cells
##' ub <- floor(.01*sum(N))
##'
##' confRaw(n, rho, N, B, ub)

confRaw <- function(n, rho, N, B, ub, subInts = 10001, method = c("simpson", "quadrature"), clusterName = NULL){

  method <- match.arg(method)

  # Argument checks
  stopifnot(length(n) == length(rho),
            length(rho) == length(N),
            all(rho <= 1),
            all(rho > 0),
            all(n <= N),
            max(rho) == 1)


  # If subInts is not odd, make it so
  if (subInts %% 2 == 0)
    subInts <- subInts + 1

  integrandText <- paste("function(x) { (1 - x)^(B - 1) * ",
                         paste(paste("(1 - rho[", 1:length(rho), "] *  x)^n[",
                                     1:length(n), "]", sep = ""),
                               collapse = " * "),
                         "}", sep = "")

  integrand <- eval(parse(text = integrandText))

  derivIntegrand <- function(x) {

     integrand(x) * (((1 - B) / (1 - x)) + sum((n * rho) / (rho * x - 1))) + 1e-04

  }

  # Create cutpoint
  # If B = 1, the derivative is 0 (derivIntegrand is a constant)
  # If B = 2, derivIntegrand is strictly negative and < -0.01 over c(0,1)

  if (derivIntegrand(1 - 1e-10) > -0.01 && B > 2) {

    # Find the cutpoint by equating the derivative to -0.01
    cutPoint <- uniroot(derivIntegrand, c(0, 1 - 2.2e-16))$root

  # This is the case where the slope never gets to -0.01
  } else {

    cutPoint <- NULL

  }

  # Define the kernel of the posterior
  posterior <- function(w) integrand(w)

  # Prepare for parallel processing
  if ((method == "simpson") & (!is.null(clusterName))) {

    if (!inherits(clusterName, "cluster")) {
      stop("'clusterName' must inhererit the 'cluster' class")
    }

    para <- TRUE

  }
  else {
    para <- FALSE
  }

  # Define the integrand of the posterior predictive distribution
  ppInt <- function(theta){

    # Must place the constraint here as uniroot will try to sample
    # everything
    ## Comment from Landon:  wouldn't it be better to control this
    ## issue in the bounds of uniroot, and not here?  Here, this condition
    ## will be checked many thousands of times...

    if(any(n == N)){

      index <- which(n == N)

      n[index] <- N[index] - 1

    }

    # Function for calculating the pkbinom
    pkb <- function(x) {

      pkbinom(ub, ceiling(N - n), rho * x)

    } # pkb

    # Non parallelized processing
    if (!para) {

      pkbEval <- sapply(theta, pkb)

    }
    else {

      # Set up the environment for parallel processing
      clusterExport(clusterName, c("N", "n", "rho", "ub"), envir = environment())

      # Compute the pkbinom portion of the integrand
      pkbEval <- unlist(parLapply(clusterName, theta, pkb))

    }

    return(pkbEval * posterior(theta))

  } # ppInt

  # Integrate function
  if (method == "simpson") {

    Integ <- function(fun, lower = 0, upper = 1) {

       integ(fun(seq(lower, upper, length = subInts)), a = lower, b = upper)

    }
  }
  else {

    Integ <- function(fun, lower = 0, upper = 1) {

       integrate(fun, lower = lower, upper = upper, rel.tol = 1e-10)[[1]]

    }
  }

  # Calculate the confidence
  if (!is.null(cutPoint)) {

      denom <- sum(Integ(integrand, 0, cutPoint),
                   Integ(integrand, cutPoint, 1))

      postPredict <- sum(Integ(ppInt, 0, cutPoint),
                         Integ(ppInt, cutPoint, 1)) / denom

  }

  else {

    postPredict <- Integ(ppInt) / Integ(integrand)

  }

  return(postPredict)

} ## confRaw



