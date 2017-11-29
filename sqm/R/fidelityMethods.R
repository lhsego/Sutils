#' Lists the SQM classification fidelity methods.
#'
#' This function enumerates the fidelity methods available in the SQM package.
#'
#' By default, this function returns a vector with all of the available fidelity
#' methods that are used within the SQM package. This function also acts as a
#' lookup to determine if a particular fidelity method is available. If 'method'
#' is specified, then a lookup is performed, and either TRUE or FALSE is returned
#' to indicate the presence or absence of the given 'method, respectively.
#'
#' @export
#' @rdname fidelityMethods
#' 
#' @param method a character string for a fidelity method. If no value is
#' specified, then all available fidelity methods are returned.
#' 
#' @return By default, a vector with all available fidelityMethods. Otherwise,
#' returns a logical value indicating whether the specified 'method' is available in
#' the SQM package.

## TODO:  Add Positive Predicted Value (PPV) which is equal to precision TP / (TP + FP)
## TODO:  Add Negative Predicted Value (NPV) which is equal to TN / (TN + FN)
## TODO:  Add the False Discovery Rate (FDR) which is equal to FP / (FP + TP) = 1 - PPV

fidelityMethods <- function(method = NULL) {
  
  methods <- c("accuracy", "precision", "recall", "sensitivity", "TPR", "specificity",
               "TNR", "FPR", "FNR", "Fscore")
  
  if (is.null(method)) {
      
    out <- methods
    
  } else {
      
    out <- method %in% methods
    
    if (!all(out))
      warning("'", paste(method[!out], collapse = "', "), "' is/are not valid. ",
              "See fidelityMethods() for a list of valid methods")
  }

  return(out)

} # fidelityMethods


# A non-exported worker function for fidelity methods that share a common structure

fidelityCalc <- function(confusionSummary, numExpr, denomExpr, aggregate = c("micro", "macro")) {

  # Check arguments
  stopifnot(inherits(confusionSummary, "confusion"))
  aggregate <- match.arg(aggregate)

  # Calculate the fidelity metric for each class
  byClass <- sapply(confusionSummary$classSummary,
                    function(clSummary) {
                      with(clSummary, eval(numExpr) / eval(denomExpr))
                    })
  
  names(byClass) <- names(confusionSummary$classSummary)

  # Calculate aggregate measures of the metric
  if (aggregate == "micro") {
      
    numerator <- sum(sapply(confusionSummary$classSummary,
                            function(clSummary) {
                              with(clSummary, eval(numExpr))
                            }))
    
    denom <- sum(sapply(confusionSummary$classSummary,
                        function(clSummary) {
                        	with(clSummary, eval(denomExpr))
                        }))
    
    aggregate <- numerator / denom
    
  } else {
    aggregate <- mean(byClass)
  }

  # Return results
  return(list(byClass = byClass, aggregate = aggregate))
    
} # fidelityCalc


#' Computes classification accuracy from the confusion matrix summary based on a
#' set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the classification accuracy in order to summarize
#' its performance for the signature. We compute one of two aggregate scores,
#' to summarize the overall performance of the signature.
#' 
#' We define the accuracy as the proportion of correct classifications.
#'
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#'
#' Note that the macro- and micro-aggregate scores are the same for classification
#' accuracy.
#'
#' The accuracy measure ranges from 0 to 1 with 1 being the optimal value.
#'
#' @rdname accuracy
#@aliases fidelity_accuracy
#@family fidelity methods
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#'
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#'
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).
#'
accuracy <- function(confusionSummary, aggregate) {
    
  fidelityCalc(confusionSummary,
               expression(truePos + trueNeg),
               expression(truePos + trueNeg + falsePos + falseNeg),
               aggregate = aggregate)

} # accuracy


#' Computes classification precision from the confusion matrix summary based on a
#' set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the classification precision in order to
#' summarize its performance for the signature. We compute one of two aggregate
#' scores, to summarize the overall performance of the signature.
#'
#' We suppose that an observation can be classified into one
#' (and only one) of K classes. For the jth class (j = 1, ..., K), we define
#' the precision as the conditional probability
#'
#' Precision_j = Pr(y = j | y_hat = j),
#'
#' where y_hat and y are the empirical and true classifications,
#' respectively.
#'
#' To estimate Precision_j for the jth class, we compute
#'
#' (TP_j) / (TP_j + FP_j),
#'
#' where TP_j and FP_j are the true positives and false positives,
#' respectively. More specifically, TP_j is the number of observations
#' that we correctly classified into the jth class, and FP_j is
#' is the number of observations that we incorrectly classified as class j.
#' 
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#'
#' In statistical terms, notice that in the binary case (K = 2), the precision
#' is the positive predictive value.
#'
#' The precision measure ranges from 0 to 1 with 1 being the optimal value.
#'
#' @rdname precision
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

precision <- function(confusionSummary, aggregate) {
    
  fidelityCalc(confusionSummary,
               expression(truePos),
               expression(truePos + falsePos),
               aggregate = aggregate)
  
} # precision


#' Computes classification recall from the confusion matrix summary based on a
#' set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the classification recall in order to
#' summarize its performance for the signature. We compute one of two aggregate
#' scores, to summarize the overall performance of the signature.
#'
#' We suppose that an observation can be classified into one
#' (and only one) of K classes. For the jth class (j = 1, ..., K), we define
#' the recall as the conditional probability
#'
#' Recall_j = Pr(y_hat = j | y = j),
#'
#' where y_hat and y are the empirical and true classifications,
#' respectively.
#'
#' To estimate recall_j for the jth class, we compute
#'
#' (TP_j) / (TP_j + FN_j),
#'
#' where TP_j and FN_j are the true positives and false negatives,
#' respectively. More specifically, TP_j is the number of observations
#' that we correctly classified into the jth class, and FN_j is
#' is the number of observations that we should have classified into class j
#' but failed to do so.
#' 
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#'
#' In statistical terms, notice that in the binary case (K = 2), the recall
#' is the sensitivity.
#'
#' The recall measure ranges from 0 to 1 with 1 being the optimal value.
#'
#' @rdname recall
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

recall <- function(confusionSummary, aggregate) {
    
  fidelityCalc(confusionSummary,
               expression(truePos),
               expression(truePos + falseNeg),
               aggregate = aggregate)

} # recall

#' Computes classification sensitivity from the confusion matrix summary based on
#' a set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the classification sensitivity in order to
#' summarize its performance for the signature. We compute one of two aggregate
#' scores, to summarize the overall performance of the signature.
#'
#' We suppose that an observation can be classified into one
#' (and only one) of K classes. For the jth class (j = 1, ..., K), we define
#' the sensitivity as the conditional probability
#'
#' Sensitivity_j = Pr(y_hat = j | y = j),
#'
#' where y_hat and y are the empirical and true classifications,
#' respectively.
#'
#' To estimate sensitivity_j for the jth class, we compute
#'
#' (TP_j) / (TP_j + FN_j),
#'
#' where TP_j and FN_j are the true positives and false negatives,
#' respectively. More specifically, TP_j is the number of observations
#' that we correctly classified into the jth class, and FN_j is
#' is the number of observations that we should have classified into class j
#' but failed to do so.
#'
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#' 
#' In statistical terms, notice that in the binary case (K = 2), the sensitivity
#' is the recall.
#'
#' Also, note that the sensitivity is equal to the TPR.
#'
#' The sensitivity measure ranges from 0 to 1 with 1 being the optimal value.
#'
#' @rdname sensitivity
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

sensitivity <- function(confusionSummary, aggregate) {

  fidelityCalc(confusionSummary,
               expression(truePos),
               expression(truePos + falseNeg),
               aggregate = aggregate)

} # sensitivity


#' Computes the true positive rate (TPR) from the confusion matrix summary based
#' on a set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the TPR in order to summarize its performance for
#' the signature. We compute one of two aggregate scores, to summarize the
#' overall performance of the signature.
#'
#' We suppose that an observation can be classified into one
#' (and only one) of K classes. For the jth class (j = 1, ..., K), we define
#' the TPR as the conditional probability
#'
#' TPR_j = Pr(y_hat = j | y = j),
#'
#' where y_hat and y are the empirical and true classifications,
#' respectively.
#'
#' To estimate TPR_j for the jth class, we compute
#'
#' (TP_j) / (TP_j + FN_j),
#'
#' where TP_j and FN_j are the true positives and false negatives,
#' respectively. More specifically, TP_j is the number of observations
#' that we correctly classified into the jth class, and FN_j is
#' is the number of observations that we should have classified into class j
#' but failed to do so.
#'
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#' 
#' In statistical terms, notice that in the binary case (K = 2), the TPR
#' is the recall.
#'
#' Also, note that the TPR is equal to sensitivity.
#'
#' The TPR measure ranges from 0 to 1 with 1 being the optimal value.
#'
#' @rdname TPR
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

TPR <- function(confusionSummary, aggregate) {
    
  fidelityCalc(confusionSummary,
               expression(truePos),
               expression(truePos + falseNeg),
               aggregate = aggregate)
} # TPR

#' Computes classification specificity from the confusion matrix summary based on
#' a set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the classification specificity in order to
#' summarize its performance for the signature. We compute one of two aggregate
#' scores, to summarize the overall performance of the signature.
#'
#' To estimate specificity_j for the jth class, we compute
#'
#' (TN_j) / (TN_j + FP_j),
#'
#' where TN_j and FP_j are the true negatives and false positives,
#' respectively. More specifically, TN_j is the number of observations
#' that we correctly classified into other classes than the jth class, and FP_j
#' is the number of observations that we have incorrectly classified into class
#' j.
#'
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#'
#' Notice that the specificity is equal to the TNR.
#'
#' The specificity measure ranges from 0 to 1 with 1 being the optimal value.
#'
#' @rdname specificity
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

specificity <- function(confusionSummary, aggregate) {
    
  fidelityCalc(confusionSummary,
               expression(trueNeg),
               expression(trueNeg + falsePos),
               aggregate = aggregate)
  
} # specificity

#' Computes true negative rate (TNR) from the confusion matrix summary based on
#' a set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the classification TNR in order to
#' summarize its performance for the signature. We compute one of two aggregate
#' scores, to summarize the overall performance of the signature.
#'
#' To estimate TNR_j for the jth class, we compute
#'
#' (TN_j) / (TN_j + FP_j),
#'
#' where TN_j and FP_j are the true negatives and false positives,
#' respectively. More specifically, TN_j is the number of observations
#' that we correctly classified into other classes than the jth class, and FP_j
#' is the number of observations that we have incorrectly classified into class
#' j.
#'
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#'
#' Notice that the TNR is equal to the specificity.
#'
#' The TNR measure ranges from 0 to 1 with 1 being the optimal value.
#'
#' @rdname TNR
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

TNR <- function(confusionSummary, aggregate) {

  fidelityCalc(confusionSummary,
               expression(trueNeg),
               expression(trueNeg + falsePos),
               aggregate = aggregate)

} # TNR

#' Computes the false positive rate (FPR) from the confusion matrix summary based
#' on a set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the FPR in order to summarize its performance for
#' the signature. We compute one of two aggregate scores, to summarize the
#' overall performance of the signature.
#'
#' To estimate FPR_j for the jth class, we compute
#'
#' (FP_j) / (FP_j + TN_j),
#'
#' where FP_j and TN_j are the false positives and true negatives,
#' respectively. More specifically, FP_j is the number of observations
#' that we incorrectly classified into the jth class, and TN_j is
#' is the number of observations that we correctly classified into a different
#' class than class j.
#'
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#'
#' The FPR measure ranges from 0 to 1 with 0 being the optimal value.
#'
#' @rdname FPR
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

FPR <- function(confusionSummary, aggregate) {
    
  fidelityCalc(confusionSummary,
               expression(falsePos),
               expression(trueNeg + falsePos),
               aggregate = aggregate)
} # FPR

#' Computes the false negative rate (FNR) from the confusion matrix summary based
#' on a set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the FNR in order to summarize its performance for
#' the signature. We compute one of two aggregate scores, to summarize the
#' overall performance of the signature.
#'
#' We suppose that an observation can be classified into one
#' (and only one) of K classes. For the jth class (j = 1, ..., K), we define
#' the FNR as the conditional probability
#'
#' FNR_j = 1 - Pr(y_hat = j | y = j),
#'
#' where y_hat and y are the empirical and true classifications,
#' respectively.
#'
#' To estimate FNR_j for the jth class, we compute
#'
#' (FN_j) / (TP_j + FN_j),
#'
#' where TP_j and FN_j are the true positives and false negatives,
#' respectively. More specifically, TP_j is the number of observations
#' that we correctly classified into the jth class, and FN_j is
#' is the number of observations that we should have classified into class j
#' but failed to do so.
#'
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#'
#' The FNR measure ranges from 0 to 1 with 0 being the optimal value.
#'
#' @rdname FNR
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

FNR <- function(confusionSummary, aggregate) {
    
  fidelityCalc(confusionSummary,
               expression(falseNeg),
               expression(truePos + falseNeg),
               aggregate = aggregate)
  
} # FNR


#' Computes classification F-score from the confusion matrix summary based on a
#' set of predicted and truth classes for a signature.
#'
#' For each class, we calculate the classification F-score in order to
#' summarize its performance for the signature. We compute one of two aggregate
#' scores, to summarize the overall performance of the signature.
#'
#' The F-score is a weighted average of the precision and recall. The weight of
#' this average is controlled by a coefficient beta, which is defaulted to 1.
#' For the default, the F-score is the harmonic mean of the precision and recall.
#'
#' The two aggregate score options are the macro- and micro-aggregate (average)
#' scores. The macro-aggregate score is the arithmetic mean of the binary scores
#' for each class. The micro-aggregate score is a weighted average of each class'
#' binary score, where the weights are determined by the sample sizes for each
#' class. By default, we use the micro-aggregate score because it is more robust,
#' but the macro-aggregate score might be more intuitive to some users.
#'
#' The F-score measure ranges from 0 to 1 with 1 being the optimal value.
#'
#' @rdname Fscore
#' 
#' @param confusionSummary list containing the confusion summary for a set of
#' classifications
#' 
#' @param aggregate string that indicates the type of aggregation; by default,
#' micro. See Details.
#' 
#' @param beta numeric coefficient that controls the F-score's weight of the
#' precision and recall
#' 
#' @return list with the accuracy measure for each class as well as the macro-
#' and micro-averages (aggregate measures across all classes).

Fscore <- function(confusionSummary, aggregate = c('micro', 'macro'), beta = 1) {
    
  stopifnot(inherits(confusionSummary, "confusion"),
            is.numeric(beta))

  aggregate <- match.arg(aggregate)
  
  byClass <- sapply(confusionSummary$classSummary, function(clSummary) {
    with(clSummary,
         (beta^2 + 1) * truePos /
         ((beta^2 + 1) * truePos + beta^2 * falseNeg + falsePos)
    )
  })
  names(byClass) <- names(confusionSummary$classSummary)

  objPre <- precision(confusionSummary, aggregate = aggregate)
  objRec <- recall(confusionSummary, aggregate = aggregate)

  numerator <- (beta^2 + 1) * objPre$aggregate * objRec$aggregate
  denominator <- beta^2 * objPre$aggregate + objRec$aggregate
  aggregate <- numerator / denominator

  list(byClass = byClass, aggregate = aggregate)
  
} # Fscore
