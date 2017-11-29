# A Simple function for calculating the ARL using the cutoff from the data. This could
# be improved perhaps by creating bootstrap estimate of the probability of a signal:  P(Y > h)

# This will work best if Y has a lot of data

nonparShewhartARL <- function(Y, h, method = c("upper", "lower", "both")) {

  method <- match.arg(method)

  if (method != "upper")
    stop("'lower' and 'both' methods not yet supported")
  
  # The nonparametric estimate of P(Y > h) is simply...
  p.signal <- sum(Y > h) / length(Y)

  if (p.signal <= 0)
    warning("No signals were produced.  Consider tightening the control limits")

  # And the ARL is 1 / p.signal
  return(1 / p.signal)
  
} # noparShewharARL
