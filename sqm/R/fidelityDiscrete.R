##' Calculate the a discrete loss (or utility) for a categorical classifier
##'
##' Calculate the a discrete loss (or utility) for a categorical classifier
##'
##' In the \code{signatures} data frame, each row corresponds to an observation, or instance,
##' for which the loss will be calculated.  It must contain at least two columns, one for the
##' truth labels and one for the predicted labels.  The names of these columns are specifed by
##' \code{truthClassLabel} and \code{predictedClassLabel}.
##' 
##' @export
##'
##' @param signatures A data frame containing the predicted and true classes for each observation.
##' See details.
##'
##' @param lossMatrix A matrix containing values of the loss (or utility) for each
##' combination of truth class and predicted class.  The predicted class labels should be the column names
##' and the truth class labels should be the rownames.  The matrix should not contain any missing values.
##'
##' @param truthClassLabel A character string indicating the name of the column in \code{signatures} that
##' contains the truth labels.
##'
##' @param predClassLabel A character string indicating the name of the column in \code{signatures} that contains the
##' predicted labels.
##' 
##' @param lossLabel A character string indicating the name of the column that will be added
##' to the \code{signatures} data frame containing the loss.
##'
##' @return The \code{signatures} data frame, with an additional column named \code{lossLabel} which has
##' the loss for each observation.
##'
##' @author Landon Sego
##' 
##' @examples
##' 
##' # A an example of 3 truth classes being mapped to one of the same 3 classes
##' # Some truth data
##' truthData <- sample(rep(letters[1:3], each = 4))
##' 
##' # Some predicted data
##' predData <- sample(rep(letters[1:3], each = 4))
##' 
##' # The loss matrix
##' lossM <- matrix(c(0,2,1,1,0,3,1,5,0), nrow = 3, ncol = 3,
##'                dimnames = list(letters[1:3], letters[1:3]))
##' 
##' lossM
##' 
##' # Calculate the loss
##' fidelityDiscrete(data.frame(truthClass = truthData, predictedClass = predData),
##'                  lossM)
##' 
##' # An assymetric example where 3 classes are mapped to 2
##' # An assymetric loss matrix, where predicted data are binary                   
##' lossM <- matrix(c(0,2,4,5,0,0), nrow = 3, ncol = 2,
##'                 dimnames = list(letters[1:3], letters[4:5]))
##' 
##' lossM
##' 
##' # Binary predicted data
##' newData <- data.frame(truthClass = truthData, predictedClass = sample(rep(letters[4:5], each = 6)))
##' 
##' # Calculate the loss
##' fidelityDiscrete(newData, lossM)
##' 
##' # An example with missing values
##' newData1 <- newData
##' newData1[2, 1] <- NA
##' newData1[4, 2] <- NA
##' newData1[7, ] <- NA
##' newData1
##' 
##' fidelityDiscrete(newData1, lossM)

fidelityDiscrete <- function(signatures, lossMatrix,
                             truthClassLabel = "truthClass",
                             predClassLabel = "predictedClass",
                             lossLabel = "loss") {

  # Sanity checks
  stopifnot(is.data.frame(signatures),
            all(c(truthClassLabel, predClassLabel) %in% colnames(signatures)),
            !(lossLabel %in% colnames(signatures)),
            is.numeric(lossMatrix),
            all(!is.na(lossMatrix)))

  # Extract key stuff from 'signatures' data frame, convert to character
  truthClass <- as.character(signatures[,truthClassLabel])
  predClass <- as.character(signatures[,predClassLabel])
  
  # Get class names from loss matrix
  truthClass.loss <- rownames(lossMatrix)
  predClass.loss <- colnames(lossMatrix)
                
  # Verify all elements of truthClass are in the rownames of the lossMatrix
  if (!all(unique(truthClass) %in% c(truthClass.loss, as.character(NA))))
    stop("There are elements in 'truthClass' that are not in the row names of 'lossMatrix'")

  # Verify all elements of predClass are in the colnames of the lossMatrix
  if (!all(unique(predClass) %in% c(predClass.loss, as.character(NA))))
    stop("There are elements in 'predClass' that are not in the column names of 'lossMatrix'")

  # Convert truth and pred classes in the loss matrix to integers for fast processing in C
  truthClass.loss <- as.factor(truthClass.loss)
  truthClass.loss.i <- as.integer(truthClass.loss)

  predClass.loss <- as.factor(predClass.loss)
  predClass.loss.i <- as.integer(predClass.loss)

  # Convert truth and pred classes from data to integers using the same mapping
  # that was peformed above for the truth and pred classess in the loss matrix
  truthClass.i <- as.integer(factor(truthClass, levels(truthClass.loss)))
  predClass.i <- as.integer(factor(predClass, levels(predClass.loss)))

  # Transform the loss matrix into a set of vectors that identify the loss mapping
  trueClassLoss <- rep(truthClass.loss.i, NCOL(lossMatrix))
  predClassLoss <- rep(predClass.loss.i, each = NROW(lossMatrix))
  lossValue <- as.vector(lossMatrix)

  # Number of data elements
  nData <- length(truthClass)

  # Quickly calculate the loss
  signatures[, lossLabel] <- .C("discreteLoss",
                                as.integer(length(lossValue)),
                                as.integer(trueClassLoss),
                                as.integer(predClassLoss),
                                as.double(lossValue),
                                as.integer(nData),
                                as.integer(truthClass.i),
                                as.integer(predClass.i),
                                loss = double(nData),
                                NAOK = TRUE)$loss

  # Return the data frame
  return(signatures)

} # fidelityDiscrete
