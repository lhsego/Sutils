#' Example data for calculating proper scoring rules
#'
#' Example data format for calculating proper scoring rules  
#' 
#' This data frame contains 5 example signatures to illustrate how
#' \code{\link{calcScore}} calcaultes scores for a signature that produces probability
#' estimates of 10 classes for each observational unit.  The \code{uniform}
#' signature consists of equal probability assignments (1/10) for each
#' class.  The \code{worst} signature captures the situation where
#' proability of 1 is assigned to the incorrect class, and all other
#' classes are 0.  \code{perfect} is when probability 1 is assigned to the
#' correct class, and \code{realistic1} and \code{realistic2} are simulated
#' data that illustrate how a "real" signatures might behave.
#' 
#' @examples
#' data(exampleScore)
#' 
#' @docType data
#' @export
#' @format data frame with 500 observations on the following 12 variables.
#'   \describe{
#'     \item{\code{signatureID}}{a factor with levels \code{uniform}
#'       \code{worst} \code{perfect} \code{realistic1} \code{realistic2}}
#'     \item{\code{truthClass}}{a numeric vector}
#'     \item{\code{predictionClass1}}{a numeric vector}
#'     \item{\code{predictionClass2}}{a numeric vector}
#'     \item{\code{predictionClass3}}{a numeric vector}
#'     \item{\code{predictionClass4}}{a numeric vector}
#'     \item{\code{predictionClass5}}{a numeric vector}
#'     \item{\code{predictionClass6}}{a numeric vector}
#'     \item{\code{predictionClass7}}{a numeric vector}
#'     \item{\code{predictionClass8}}{a numeric vector}
#'     \item{\code{predictionClass9}}{a numeric vector}
#'     \item{\code{predictionClass10}}{a numeric vector}
#' 
#' @source Source code that created these data can be found in the \code{data}
#' folder of the SQM package source.
#' @keyword datasets
wrapper <- function() {

  ## Uniform prediction test set
  unifSignatures <- data.frame(signatureID=rep("uniform", nrow=100), 
      truthClass=rep(1:10, times=10), 
      matrix(0.1, nrow=100, ncol=10))
  colnames(unifSignatures) <- c("signatureID", "truthClass", 
      paste("predictionClass", 1:10, sep=""))
  
  ## Perfect prediction test set
  perfectSignatures <- data.frame(signatureID=rep("perfect", nrow=100), 
      truthClass=rep(1:10, times=10), 
      matrix(0, nrow=100, ncol=10))
  colnames(perfectSignatures) <- c("signatureID", "truthClass", 
      paste("predictionClass", 1:10, sep=""))
  for (i in 1:10) {
      perfectSignatures[perfectSignatures$truthClass == i, 
          paste("predictionClass", i, sep="")] <- 1
  }
  
  ## Worst prediction test set (probability 1 on wrong answer)
  worstSignatures <- data.frame(signatureID=rep("worst", nrow=100), 
      truthClass=rep(1:10, times=10), 
      matrix(0, nrow=100, ncol=10))
  colnames(worstSignatures) <- c("signatureID", "truthClass", 
      paste("predictionClass", 1:10, sep=""))
  for (i in 1:9) {
      worstSignatures[worstSignatures$truthClass == i, 
          paste("predictionClass", i+1, sep="")] <- 1
  }
  worstSignatures[worstSignatures$truthClass == 10, 
      paste("predictionClass", 1, sep="")] <- 1
  
  
  # Create a random (more realistic) one as well

  set.seed(12)
  
  single.rep <- function(b1, b2) {
    
    scores.on.correct.class <- diag(runif(10, b1, 10))
    scores.on.incorrect.classes <- matrix(runif(100, 2, b2), nrow = 10) *
                                   (matrix(rep(1, 100), nrow = 10) - diag(10))
    final.scores <- scores.on.correct.class + scores.on.incorrect.classes
  
    # Normalize
    return(final.scores / rowSums(final.scores))
    
  } # single.rep

  build.rep1 <- build.rep2 <- NULL
  for (i in 1:10) {
    build.rep1 <- rbind(build.rep1, single.rep(8, 8.5))
    build.rep2 <- rbind(build.rep2, single.rep(9.5, 3))
  }    

  
  realSigs1 <- data.frame(signatureID=rep("realistic1", nrow=100),
                                    truthClass=rep(1:10, times=10), build.rep1)
  colnames(realSigs1) <- c("signatureID", "truthClass", 
                                     paste("predictionClass", 1:10, sep=""))

  
  realSigs2 <- data.frame(signatureID=rep("realistic2", nrow=100),
                                    truthClass=rep(1:10, times=10), build.rep2)
  colnames(realSigs2) <- c("signatureID", "truthClass", 
                                     paste("predictionClass", 1:10, sep=""))
  
  # Combine them into a single data frame
  return(rbind(unifSignatures, worstSignatures, perfectSignatures, realSigs1, realSigs2))

} # wrapper
 

exampleScoreData <- wrapper()

rm(wrapper)

 
