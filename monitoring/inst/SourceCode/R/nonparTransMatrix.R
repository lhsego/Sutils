# Calculate the transition matrix for an upper CUSUM
# for a continuous RV using a nonparametric estimator
# of the density (a logspline fit)
nonparTransMatrix <- function(fit, h, k, numPartitions=500) {

  cdf <- function(x) plogspline(x, fit)
#  cdf <- function(x) pgamma(x, shape=fit$shape, scale=fit$scale)

  # Set up a vector of partitions
  ep <- seq(0, h, length=numPartitions+1)

  # A vector for the transition probabilities
  Q <- double((numPartitions+1)^2)

  # Loop across the columns
  for (j in 1:(numPartitions+1)) {

    # Loop down the rows
    for (i in 1:(numPartitions+1)) {

      # index for the Q vector
      position <- i + (numPartitions + 1)* (j-1)

      # Transitions from (c, d] to (a, b]
      if ((i > 1) & (j > 1)) {

        a1 <- ep[j-1]; b1 <- ep[j]; c1 <- ep[i-1]; d1 <- ep[i]
        cprime <- seq(c1, d1, length=5)
        Q[position] <- integ(cdf(b1 - cprime + k) -
                             cdf(a1 - cprime + k), c1, d1) / (d1 - c1)
        
      }

      # Transitions from (c, d] to 0
      else if ((i > 1) & (j == 1)) {
        
        c1 <- ep[i-1]; d1 <- ep[i]
        cprime <- seq(c1, d1, length=5)
        Q[position] <- integ(cdf(k - cprime), c1, d1) / (d1 - c1)

      }
      
      # Transitions from 0 to (a, b]
      else if ((i == 1) & (j > 1))
        Q[position] <- cdf(ep[j] + k) - cdf(ep[j-1] + k)

      # From 0 state to 0 state
      else
        Q[position] <- cdf(k)

      # Making sure we don't have negative probabilities
      if (Q[position] < -1e-12)
        warning(sprintf("Matrix coordinate (%i,%i) has a value of %.10f",
                        i+1, j+1, Q[position]))

      if (Q[position] < 0)
        Q[position] <- 0
      
    } # for i

  } # for j

  matrix(Q, nrow = numPartitions + 1)
  
} # nonparTransMatrix()
