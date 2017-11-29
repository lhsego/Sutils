# idParetoFrontier <- function(X, orientation, tol = 1e-10) {
  
#   # Check arguments
#   stopifnot(is.data.frame(X),
#             "signatureID" %in% colnames(X),
#             is.vector(orientation),
#             all(names(orientation) %in% colnames(X)),
#             !("signatureID" %in% names(orientation)),
#             all(abs(as.integer(orientation)) == 1),
#             !("frontier" %in% colnames(X)),
#             !("frontier" %in% names(orientation)))
#   
#   # The trivial case
#   if (NROW(X) == 1) {
#     X$frontier <- "yes"
#     return(X)
#   }
#   
#   # Verify that each signatureID has only one row
#   if (!all(table(X$signatureID) == 1))
#     stop("Each 'signatureID' must have only 1 row")
  
  # Create the matrix on which we'll operate
  Y <- as.matrix(X[, names(orientation)])
  
  if (!is.numeric(Y))
    stop("All the columns in 'X' selected by the names of 'orientation' must be numeric")
  
  # Number of rows and columns
  nR <- NROW(Y)
  nC <- NCOL(Y)
  
  # A matrix to make all positive comparisons
  orien.mat <- matrix(orientation, nrow = nR, ncol = nC, byrow = TRUE)
  
  # Multiply Y by orien.vec to orient them all to make positive comparisons
  # i.e. the ordering of the attributes of negative comparisons is reversed using -1
  Yorien <- Y * orien.mat
  rownames(Yorien) <- paste("r", 1:nrow(Yorien), sep="")
  
  tot.dominance <- function(r1, r2){
    # By altering the sign, we make sure everything is maximized. Therefore, r1 is superior to r2
    # if d >= 0 for all entries.
    d <- r1 - r2;
    if(all(d >= 0)){
      return("r2")
    } else {
      return("r1")
    }
  }
  
  # Need to check dominance of every pair of rows
  
  
  
  