##' Generate a set indexes which define a single bootstrap sample for each signature
##'
##' Generate a set of indexes which define a single bootstrap sample in a stratified manner
##' where each signature ID is a strata,
##' and a bootstrap sample is generated separately for each signature ID. This function is
##' a helper to other functions in the SQM package, like \code{\link{calcScore}}.
##'
##' @param signatureID A vector of signature labels
##' @return An integer vector of indexes corresponding to signatureID that indicate
##' the elements of the bootstrap sample
##'
##' @examples
##' # A single sample
##' sigLabels <- rep(letters[1:3], each = 4)
##' print(sigLabels)
##' SQM:::genBootstrapIndexes(sigLabels)

genBootstrapIndexes <- function(signatureID) {

  # Generate the indexes
  indexes <- unlist(lapply(as.list(unique(signatureID)),
                           function(x) sample(which(signatureID == x), replace = TRUE)))

  # Sanity check
  t1 <- table(signatureID)
  t2 <- table(signatureID[indexes])
  names(attributes(t2)$dimnames) <- "signatureID"

  # These should be equivalent
  if (!identical(t1, t2))
    stop("Indexes not generated correctly")
  
  return(indexes)

} # genBootstrapIndexes
