##' Find the minimum sample size for a given set of parameters
##'
##' @rdname findSSRaw
##'
##' @export
##' @param conf the target confidence that one wishes to achieve
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
##' @param alpha a numeric vector of arbitrary length with elements in c(0,1). This function
##' chooses a locally optimal allocation according to the following function:
##' alpha*(N/sum(N)) + (1 - alpha)*rho. That is to say, an alpha close to zero favors
##' allocating samples by risk (rho) and an alpha close to one favors allocating samples by
##' relative proportion.
##'
##' @return the minimum sample size with respect to a value of alpha.
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
##' B <- 70
##'
##' # 1% of total grid cells
##' ub <- ceiling(.01*sum(N))
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
##' minSamps <- findSSRaw(conf, rho, N, B, ub, alpha)


findSSRaw <- function(conf, rho, N, B, ub, alpha, ...) {

  stopifnot(conf < 1,
            conf > 0,
            length(rho) == length(N),
            all(rho <= 1),
            all(rho > 0),
            all(alpha >= 0))

  if(length(rho) > 1 && max(rho) != 1){
    stop('max rho must  be 1')
  }

  # Find the maximum xi we'll scan over
  maxXi <- get_n(rho, N, alpha)

  # Now an objective function to solve for the confidence
  confObj <- function(xi) {

    n <- get_n(rho, N, alpha, xi = xi)

    obj <- confRaw(n, rho, N, B, ub, ...) - conf

#    print(obj)

    return(obj)

  } # confObj

  if(confRaw(rep(0, length(N)), rho, N, B, ub) > conf){

    warning('Actual confidence greater than requested confidence with zero samples.')

    return(rep(0, length(N)))

  } else {

    # Solve for the xi that gives the desired sample size
    xiRoot <- uniroot(confObj, c(0, maxXi), tol = 1e-10)$root

    sample.size <- ceiling(get_n(rho, N, alpha, xi = xiRoot))

    # We need to match the sample size here that is used to calculate
    # the confidence when our strata sizes are small

    if(any(sample.size == N)){

      index <- which(sample.size == N)

      sample.size[index] <- N[index] - 1

    }

    # Get the sample size
    return(sample.size)

  }


} # findSSRaw
