# Siegmund's approximation to the standardized normal CUSUM

## Simulates the initial state ARL of the standard normal CUSUM
normalCusumARLSiegmund <- function(k, h, twoSided=TRUE) {

  # Preliminary checks
  if (!is.numeric(k))
    stop("'k' must be a real number\n")
  if (!is.numeric(h))
    stop("'h' must be a real number\n")

  # From page 416-417 of Montgomery
  # Note that Hawkins & Olwell, p 157 has a typo
  arl <- (exp(-2 * -k * (h + 1.166)) + 2 * -k * (h + 1.166) - 1) / (2 * k^2)

  if (twoSided)
    arl <- arl / 2

  return(arl)

} # normalCusumARLSiegmund
