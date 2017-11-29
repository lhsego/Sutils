## Finds the normalized dominant left eigen vector of a matrix
## This one appears to converge in fewer iterations...
dominant.left.ev <- function(Q, max.iter = 20000, tol = 1e-15,
                             verbose=FALSE,
                             check = FALSE, check.accuracy = 7) {

  if (check) {
     # Find the Eigen values
     ev <- eigen(Q)$values
   
     # Verify that there is a unique largest eigen value
     magnitude.max.ev <- max(abs(ev))
     find.dom <- magnitude.max.ev == abs(ev)
     if (sum(find.dom) > 1)
       stop("There does not exist a strictly dominant eigen value.\n")
   
     # Find maximum eigen value (and make sure it's Real)
     dom.ev <- ev[find.dom]
     if (Im(dom.ev)) stop("Dominant eigen value is Complex.\n")
     dom.ev <- Re(dom.ev)
  }
   
  ## Use a version of the power method to find the left dominant eigen vector
  v.old <- t(rep(1,NROW(Q))) %*% Q
  # Scale so that largest entry is a 1
  max.v.old <- v.old[max(abs(v.old))==v.old][1]
  v.old <- v.old / max.v.old
  iter <- 0
  a.tol <- 1
  
  
  while ((iter <= max.iter) & (a.tol > tol)) {

       v.new <- v.old %*% Q
       max.v.new <- v.new[max(abs(v.new))==v.new][1]
       v.new <- v.new %*% Q * (1/max.v.new)
       # Convergence criteria is based on the angle between the two vectors going to 0
       denom <- as.numeric(sqrt( v.old %*% t(v.old) )) * as.numeric(sqrt( v.new %*% t(v.new) ))
       a.tol <- abs( (v.new %*% t(v.old)) / denom - 1 )
       iter <- iter + 1
       if (verbose) cat("iter =", iter,"  a.tol =", a.tol, "\n")
       v.old <- v.new

       if (iter > 10000) tol <- max(tol*10, 1e-5)
  }

  if (iter > max.iter)
    stop("Convergence for left evector not reached in max.iter = ",max.iter," iterations.\n")

  if (check) {
     # Check the result:
     left <- v.new %*% Q
     right <- dom.ev * v.new
#     cat("Convergence for left evector occurred in", iter, "iterations.\n")
#     cat("Accuracy for dominant left eigen vector achieved to within",max(abs(left-right)),"\n")
#     cat("Accuracy for dominant eigen value achieved to within",abs(max.v.new - dom.ev),"\n")
#     print(t(v.new/sum(v.new)))
     if (any(round(left-right, check.accuracy) != 0))
        cat("Accuracy for dominant left eigen vector achieved to within",max(abs(left-right)),"\n")
#        cat("Warning in dominant.left.ev():  Left eigen vector not found to within 7 decimal places.\n")
  }

  # All elements of the dominant evector should be positive and non-zero...
  if (any(v.new <= 0)) stop("dominant left eigen vector has at least one element that is <= 0.\n")
  
  # Return the conditional stationary vector (as a column vector)
  return(t(v.new / sum(v.new)))

}

