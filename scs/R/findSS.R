##' Find the minimum sample size for a given set of parameters
##'
##' Find the minimum sample size for a given set of parameters
##'
##' Either \code{prior.prob} or \code{beta} must be specified.  But not both.  Same for
##' \code{pct.clean} and \code{ub}.  If parallelization is desired, only one of \code{nJobs} or
##' \code{pJobs} may be greater than 1.
##'
##' @rdname findSS
##'
##' @export
##' @param conf the target confidence that one wishes to achieve
##'
##' @param rho a vector of relative weights of finding an unacceptable item relative to
##' stratum 1. Note that rho[1] = 1 always.
##'
##' @param N the vector of total grid cells for each stratum
##'
##' @param prior.prob A priori expected proportion of grid cells that are acceptable in the highest-risk stratum.
##'
##' @param beta The value of the beta parameter of the Beta prior distribution for the highest-risk stratum, equal to
##' \code{prior.prob} / (1 - \code{prior.prob}).
##'
##' @param pct.clean the percentage of the total sampling area that is desired to be free
##' of contamination
##'
##' @param ub The maximum number of 'allowable' unacceptable items
##'
##' @param alphaSeq a numeric vector of arbitrary length with elements in c(0,1). This function
##' chooses a locally optimal allocation according to the following function:
##' alpha * (N/sum(N)) + (1 - alpha) * rho. That is to say, an alpha close to zero favors
##' allocating samples by risk (rho) and an alpha close to one favors allocating samples by
##' relative proportion.
##'
##' @param nJobs The number of parallel jobs to spawn at the alpha level using
##' \code{\link[parallel:parLapply]{parallel::parLapply}}.
##'
##' @param pJobs The number parallel jobs to spawn at the confidence calculation level. The calls to
##' \code{\link[Smisc:pkbinom]{Smisc::pkbinom}} are paralellized using
##' \code{\link[parallel:parLapply]{parallel::parLapply}}
##'
##' @param minmax If \code{TRUE}, identify the minimal sample size that maxmizes alpha
##'
##' @param epsilon the tolerance parameter to pass to \code{\link{minmax}}. Default value is
##' epsilon = 0.01. A numeric object. Ignored if \code{minimax = FALSE}.
##'
##' @param \dots Optional arguments to \code{\link{confRaw}}
##'
##' @return A data frame with the following columns:
##' \item{alpha}{The value of alpha}
##' \item{n1}{The sample size of the first strata}
##' \item{n2}{The sample size of the second strata, and so on}
##' \item{n.sum}{The total sample size}
##' \item{achieved.conf}{The actual confidence achieved with those samples}
##' If \code{minmax = TRUE}, only a single row of the data frame is returned--the one that
##' minimizes the sample size with the largest alpha.  For \code{mimmax = FALSE}, the results
##' from all the alpha values in  \code{alphaVec} are returned.
##'
##' @author Alex Venzin
##'
##' @examples
##'
##' # N must be an integer
##' N <- c(1000, 1000, 1000)
##'
##' rho <- c(1, 0.85, 0.75)
##'
##' prior.prob <- 0.99
##'
##' pct.clean <- 0.99
##'
##' # I want to be 95% confident that no more than ub items were left undiscovered
##' # in the unsampled area
##' conf <- 0.95
##'
##' # I want to calculate a sequence of sample sizes with respect to alpha. The
##' # smallest total sample size of this set is the returned value.
##'
##' alpha <- seq(0, 1, length = 10)
##'
##' minSamps <- findSS(conf, rho, N, prior.prob = prior.prob, pct.clean = pct.clean, alphaSeq = alpha)
##' minSamps
##'


findSS <- function(conf, rho, N, prior.prob = NULL, beta = NULL, pct.clean = NULL, ub = NULL,
                   alphaSeq = seq(0, 1, by = 0.1), nJobs = 1, pJobs = 1,
                   minmax = TRUE, epsilon = 0.01, ...) {

  # If both are specified
  if ((nJobs > 1) & (pJobs > 1))
    stop("Only one of 'nJobs' or 'pJobs' may be greater than 1")

  # If neither specified
  if (is.null(prior.prob) & is.null(beta))
    stop("Either 'prior.prob' or 'beta' must be specified")

  if (is.null(pct.clean) & is.null(ub))
    stop("Either 'pct.clean' or 'ub' must be specified")

  # If both specified
  if (!is.null(prior.prob) & !is.null(beta))
    stop("Only one of 'prior.prob' and 'beta' may be specified")

  if (!is.null(pct.clean) & !is.null(ub))
    stop("Only one of 'pct.clean' and 'ub' may be specified")

  # Calculate the terms
  if (is.null(beta)) {
    beta <- prior.prob / (1 - prior.prob)
  }
  else {
    prior.prob <- beta / (1 + beta)
  }

  if (is.null(ub)) {
    ub <- as.integer(round((1 - pct.clean) * sum(N), 14))
  }
  else {
    pct.clean <- 1 - ub / sum(N)
  }

  # Check the arguments
  stopifnot(length(rho) == length(N),
            length(conf) == 1,
            min(alphaSeq) >= 0,
            max(alphaSeq) <= 1,
            prior.prob > 0 & prior.prob < 1,
            beta >= 1,
            pct.clean > 0 & pct.clean <= 1,
            ub >= 0 & ub < sum(N),
            conf > 0 & conf < 1,
            nJobs >= 1,
            pJobs >= 1)


  # Start the cluster for processing the pkbinom() calls in confRaw() if requested
  if (pJobs > 1) {

    cName <- makeCluster(min(detectCores(), pJobs))

    clusterEvalQ(cName, library(Smisc))

  }

  ssFun <- function(alpha) {

    inner <- function() {

      # alpha must be scalar
      stopifnot(length(alpha) == 1)

      nvec <- findSSRaw(conf, rho, N, beta, ub, alpha,
                        clusterName = if (pJobs > 1) cName else NULL,
                        ...)

      nsum <- sum(nvec)

      # Get the achieved confidence
      conf <- confRaw(nvec, rho, N, beta, ub,
                      clusterName = if (pJobs > 1) cName else NULL,
                      ...)

      out <- c(alpha, nvec, nsum, conf)

      names(out) <- c("alpha", paste("n", 1:length(N), sep = ""), "n.sum", "achieved.conf")

      return(out)

    } # inner()

    out1 <- try(inner())

    if (inherits(out1, "try-error")) {

      cat(out1, "\n")

      out1 <- rep(NA, 3 + length(N))

    }

    names(out1) <- c("alpha", paste("n", 1:length(N), sep = ""), "n.sum", "achieved.conf")

    return(out1)

  } # ssFun()

  # Run in serial if nJobs = 1 or nJobs = NULL
  if (nJobs == 1) {

    out <- lapply(alphaSeq, ssFun)

  }

  # Run in parallel over the alpha sequence
  else {

    # Make sure the number of jobs isn't too large relative to the machine capability or the length
    # of the alpha sequence
    nJobs <- min(c(length(alphaSeq), detectCores(), nJobs))

    # Make the cluster
    cName <- makeCluster(nJobs)

    clusterEvalQ(cName, library(scs))

    objectsToExport <- c('conf', 'rho', 'N', 'prior.prob', 'pct.clean','alphaSeq', 'beta', 'ub',
                         'confRaw', 'findSS', 'findSSRaw', 'get_n')

    clusterExport(cName, objectsToExport, envir = environment())

    # Calculate the smple sizes over the alpha sequence
    out <- parLapply(cName, alphaSeq, ssFun)

  }

  # Convert to dataframe
  out <- list2df(out)

  # Shut down the cluster if we opened one
  if ((pJobs > 1) | (nJobs > 1)) {
    stopCluster(cName)
  }

  # Find the optimal sample size
  if(minmax){

    return(minmax(out, epsilon))

  } else {

    return(out)

  }


} # findSS
