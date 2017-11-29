# Gives the probability of detecting a hotspot using a rectangular grid (square
# grid a special case).  Incorporates the false negative error rate.  This 
# algorithm matches the approach described in the technical report:
# "Accounting_for_false_negatives_in_hotspot_detection_report_v03.doc"

# Landon Sego, 27 Aug 2007

pdetectRect <- function(R,s,rho,fn) {

  # R = radius of hotspot
  # s = length of the short side of rectangle  (height)
  # rho * s = length of the long side of the rectangle (width)
  # fn = false negative rate, which must satisfy 0 <= fn <= 1

  r <- R/s

  # Check r
  if (r < 0) 
    stop("'R' and 's' must both be positive.\n")
  else if (r > 1) 
    stop("'R must be <= s.  For R > s, use mesh algorithm.\n")

  # Check rho
  if (rho < 1) 
    stop("'rho' should be >= 1.\n")

  # Check fn
  if ((fn < 0) | (fn > 1))
    stop("'fn' must be in [0,1].\n")

  # This function calculates the area of the intersection of two circles
  # that are d units apart, both with radius r.  If they don't intersect,
  # it returns 0.
  h <- function(d) {
    if (r >= d/2)
      return(2 * r^2 * acos(d / (2 * r)) - d * sqrt(r^2 - (d^2 / 4)))
    else
      return(0)
  }

  # If quadruple overlap exists
  if (r > sqrt(1+rho^2)/2)
      A4 <- 2 * r^2 * (asin(sqrt(r^2 - 0.25) / r) - asin(rho / (2 * r))) +
            rho * (1 - sqrt(r^2 - rho^2/4)) - sqrt(r^2 - 0.25)
  # Otherwise
  else
      A4 <- 0

  # Triple overlap
  A3 <- 2 * (h(sqrt(1+rho^2)) - A4)

  # Double overlap
  A2 <- h(rho) + h(1) - 4 * h(sqrt(1+rho^2))
  
  # Single overlap
  A1 <- pi * r^2 - 2 * (h(1) + h(rho) - h(sqrt(1+rho^2)) - A4)

  return((A1 * (1 - fn) + A2 * (1 - fn^2) + A3 * (1 - fn^3) + A4 * (1 - fn^4)) / rho)

} # end pdetectRect()
