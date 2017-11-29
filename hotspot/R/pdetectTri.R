# Gives the probability of detection for triangular grids.  The
# 'pdetectTri.old' function uses the approach that involves cases, as described in 
# "Documention_detecting_hotpots_with_msmt_error_v04.doc".

# Landon Sego, 27 Aug 2007

pdetectTri.old <- function(R,s,fn) {

  # R = radius of hotspot
  # s = length of side of triangle
  # fn = false negative rate, which must satisfy 0 <= fn <= 1
  
  r <- R/s

  # reciprocal of the area of the triangle
  rat <- 4 / sqrt(3)
  
  if (r < 0) stop("'R' and 's' must both be positive.\n")

  # Case 1
  else if (r <= 1/2)   
    return(2 * pi * r^2 * (1 - fn) / sqrt(3))

  # Case 2, 3, & 4
  else if (r <= 1) { 

    h <- function(d) 2 * r^2 * acos(d / (2 * r)) - d * sqrt(r^2 - (d^2 / 4))

    # Case 2
    if (r <= 1 / (2 * cos(pi / 6))) {

      B1 <- pi * r^2 / 2 - 3 * h(1)
      B2 <- 1.5 * h(1)

      return(rat * (B1 * (1 - fn) + B2 * (1 - fn^2)))

    }

    # Cases 3 & 4
    else {

      theta <- 2 * acos(1/(2*r)) - pi/3
      C3 <- 0.5 * r^2 * (sqrt(3) * (1 - cos(theta)) + 3 * (theta - sin(theta)))

      # Case 3
      if (r <= sqrt(3)/2) {

        C2 <- 3 * (0.5 * h(1) - C3)
        C1 <- 1/rat - C3 - C2
       
        return(rat * (C1 * (1 - fn) + C2 * (1 - fn^2) + C3 * (1 - fn^3)))
      }

      # Case 4
      else {

        D4 <- 1.5 * h(sqrt(3))
        D3 <- C3 - 3 * h(sqrt(3))
        D2 <- 3 * (0.5 * (h(1) + h(sqrt(3))) - C3)
        D1 <- 1/rat - D4 - D3 - D2
     
        return(rat * (D1 * (1 - fn  ) + D2 * (1 - fn^2) +
                      D3 * (1 - fn^3) + D4 * (1 - fn^4)))
      }

    }

  }

  else stop("R must be <= s.  If greater probability of detection is desired\n",
            "square the value of the value false negative rate, which corresponds to\n",
            "double sampling.\n")
  
} # pdetectTri.old()


# A more straightforward way that matches the paper and the technical report, 
# as described in "Accounting_for_false_negatives_in_hotspot_detection_report_v03.doc"

pdetectTri <- function(R,s,fn) {

  # R = radius of hotspot
  # s = length of side of triangle
  # fn = false negative rate, which must satisfy 0 <= fn <= 1
  
  r <- R/s

  if (r < 0)
    stop("'R' and 's' must both be positive.\n")

  if (r > 1) 
    stop("'r' should be <= 1.\n")
    
  # This function calculates the area of the intersection of two circles
  # that are d units apart, both with radius r.  If they don't intersect,
  # it returns 0.
  h <- function(d) {
    if (r >= d/2)
      return(2 * r^2 * acos(d / (2 * r)) - d * sqrt(r^2 - (d^2 / 4)))
    else
      return(0)
  }

  # Quadruple overlap
  A4 <- 1.5 * h(sqrt(3))

  # Triple overlap
  if (r > 1/sqrt(3))
    theta <- 2 * acos(1 / (2 * r)) - pi/3
  else
    theta <- 0
  A3 <- (r^2 / 2) * (sqrt(3) * (1 - cos(theta)) + 3 * (theta - sin(theta))) - 2 * A4

  # Double overlap
  A2 <- 3 * (0.5 * h(1) - (5/3) * A4 - A3)

  # Single overlap
  A1 <- 3 * (pi * r^2 / 6 - (4/3) * A4 - A3 - (2/3) * A2)
  
  return(4 * (A1 * (1 - fn) + A2 * (1 - fn^2) + A3 * (1 - fn^3) + A4 * (1 - fn^4)) / sqrt(3))
  
} # pdetectTri()
