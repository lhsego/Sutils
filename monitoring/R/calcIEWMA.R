# A Wrapper for the c method 'calcIEWMA'

# Calculates a IEWMA as discussed by Shu, 2007, "A One-sided EWMA Control Chart for monitoring process means"
# Does not calculate the limit

# X[i] = (1-lambda) * X[i-1] + lambda * max(Y[i], mu0)
# It initializes with mu0

# Signal occurs when X[i] > h


calcIEWMA <- function(Y, mu0, lambda, h, reset=FALSE) {

  # Verify inputs
  if (!is.numeric(Y))
    stop(deparse(substitute(Y)), " must be a numeric vector.")
  if (!is.numeric(mu0))
    stop("'mu0' must be a real number")
  if (!is.numeric(lambda))
    stop("'lambda' must be a real number in (0, 1]")
  if ((lambda <= 0) | (lambda > 1))
    stop("'lambda must be in (0, 1]")
  if (!is.numeric(h))
    stop("'h' must be a real number > mu0")
  if (h < mu0)
    stop("The control limit 'h' must be larger than 'mu0'")  

  iewma <- .C("calcIEWMA",
              as.double(Y),
              as.integer(length(Y)),
              as.double(mu0),
              as.double(lambda),
              as.double(h),
              as.integer(reset),
              X = double(length(Y)),
              PACKAGE = "monitoring")$X

  if (!is.null(names(Y)))
    names(iewma) <- names(Y)

  return(list(iewma = iewma, mu0 = mu0, h = h))

} # calcIEWMA
