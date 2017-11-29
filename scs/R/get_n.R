##' Solve for the strata loading parameter
##'
##' @export
##' @param rho a vector of relative weights of finding an unacceptable item relative to
##' stratum 1. Note that rho[1] = 1 always.
##'
##' @param N the vector of total grid cells for each stratum
##'
##' @param alpha a numeric vector of arbitrary length with elements in c(0,1). This parameter
##' is used to obtain a locally optimal allocation according to the following function:
##' alpha*(N/sum(N)) + (1 - alpha)*rho.
##' Hence, an alpha close to zero favors allocating samples by risk (rho) and
##' an alpha close to one favors allocating samples by size of stratum.
##'
##' @param xi the strata loading parameter. If specified, \code{get_n} will return
##' \code{pmin(xi * alpha*(N/sum(N)) + (1 - alpha)*rho, N)}. Otherwise, \code{get_n}
##' solves for the maximum possible loading parameter. Default = NULL.

get_n <- function(rho, N, alpha, xi = NULL) {

  stopifnot(length(rho) == length(N),
            all(rho <= 1),
            all(rho > 0))

  if(length(rho) > 1 && max(rho) != 1){
    stop('max rho must  be 1')
  }

  nFun <- function(xi.prime) {

    stopifnot(length(xi.prime) == 1)
    pmin(xi.prime * (alpha * N / sum(N) + (1 - alpha) * rho), N)

  }

  if (!is.null(xi))
    return(nFun(xi))

  # Find the largest xi that is allowed
  else {

   # Define an upper bound for xi.primo
   xi.primo.upper <- max(N / ((alpha * N / sum(N) + (1 - alpha) * rho)))

   return(xi.primo.upper)

  }


} # get_n
