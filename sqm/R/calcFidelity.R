# Stuff to test / do
# 1.5 test running on single, double, no grouping variable
# 2. test differnt types of class (pred & truth) variables (character, int, factor, numeric)
# 2.5 test factors where not all the levels are present
# 3. Verify F-score method.  It seems weird.
# 4. Add additional methods
# 5. Consolidate the documentation of the methods
# 5. How does it deal with NA's (in the group, pred, or truth fields?)
# 6. Review the class checking condition in confusion()--how does it behave when levels are present that are not observed in the data?
# 7. What happens when no correct predictions are made (classes for pred entirely different than truth?)

##' Calculate fidelity metric(s) for a classifier
##'
##' With a vector of predicted classes and corresponding
##' vector of truth classes, calculate the fidelity of a classifier using one or
##' more metrics derived from the confusion matrix (e.g. accuracy, sensitivity, false
##' positive rate, etc.).
##' 
##' The data.frame \code{X} must have has at least two columns: a column containing the
##' true class, and another column containing the predicted class.  Optionally, it may
##' have other columns that indicate groups for which the fidelity metrics will be
##' calculated separately.  If \code{X[,truthVar]} and \code{X[,predVar]} are both factors,
##' they must have the same levels.  If they are not both factors, they must at least
##' be of the same type (character, numeric, integer, etc.) and they are converted
##' to factors with the levels being defined as the union of the unique values in
##' \code{X[,truthVar]} and \code{X[,predVar]}. The rows of \code{X} correspond to each
##' observation, or example, for which a classification is made.
##'
##' The two aggregate score options are the macro- and micro-aggregate (average)
##' scores. The macro-aggregate score is the arithmetic mean of the binary scores
##' for each class. The micro-aggregate score is a weighted average of each class'
##' binary score, where the weights are determined by the sample sizes for each
##' class. By default, we use the micro-aggregate score because it is more robust,
##' but the macro-aggregate score might be more intuitive to some users.
##' If the classification is binary, aggregation is still performed, but the user
##' may be interested in only one of the classes which is available in \code{byClass}.
##'
##' The available fidelity methods can be viewed with
##' \code{\link{fidelityMethods}}.
##'
##' @export
##'
##' @param X data.frame containing the predicted and true classes for
##' each group. See Details
##'
##' @param truthVar Character string indicating the column name in \code{X} that
##' contains the true class of the observations
##'
##' @param predVar Character string indicating the column name in \code{X} that
##' contains the class predicted by the classifier
##'
##' @param groupVars Character vector indicating one or more column names in \code{X}
##' that define the groups for which the fidelity metrics will be separately calculated
##'
##' @param methods vector of fidelity methods that will be used. For a list of
##' available fidelity methods, see fidelityMethods()
##'
##' @param aggregate string that uniquely indicates the type of aggregation; by default,
##' \code{micro}. See Details
##'
##' @param positiveLabel For binary classification only, the value of the class label that corresponds to
##' a positive response. Should be the same type (character, numeric, or integer) as the true and predicted
##' labels.
##'
##' @return a list that contains two components: \code{aggregate} and \code{byClass}. The former
##' contains the aggregate fidelity results, and the latter contains the fidelity
##' results for each class within each signature.
##'
##' @examples
##' # Load data
##' data(exampleSignatures)
##'
##' # Calculate the TPR and FPR
##' calcFidelity(exampleSignatures, truthVar = "truthClass", predVar = "predictedClass",
##'              groupVars = "signatureID", methods = c("TPR","FPR"))
##'
##'
##'# An example where there is more than one grouping variables
##'# Create a simpler dataset
##'d <- exampleSignatures[exampleSignatures$signatureID %in% 1:4,]
##'d[d$signatureID %in% c(1,2), "gender"] <- "Male"
##'d[d$signatureID %in% c(3,4), "gender"] <- "Female"
##'d[d$signatureID %in% c(1,3), "nationality"] <- "American"
##'d[d$signatureID %in% c(2,4), "nationality"] <- "French"
##'d <- d[,-which(colnames(d) == "signatureID")]
##'
##'# Now calculate using the grouping variables
##'y <- calcFidelity(d, groupVars = c("gender", "nationality"))
##'y

calcFidelity <- function(X,
                         truthVar,
                         predVar,
                         groupVars = NULL,
                         methods = fidelityMethods(),
                         aggregate = c("micro", "macro"),
                         positiveLabel = NULL) {

  # Match (and check) the methods
  fMethods <- methods[fidelityMethods(methods)]

  # Match the aggregate approach
  aggregate <- match.arg(aggregate)

  # Check the types of inputs for calcFidelity
  stopifnot(is.data.frame(X),
            is.character(truthVar),
            is.character(predVar),
            length(truthVar) == 1,
            length(predVar) == 1,
            predVar != truthVar,
            all(c(truthVar, predVar) %in% colnames(X)),
            all(class(X[,truthVar]) == class(X[,predVar])))

  # Check groupVars
  if (!is.null(groupVars)) {
    stopifnot(is.character(groupVars),
              !(truthVar %in% groupVars),
              !(predVar %in% groupVars),
              all(groupVars %in% colnames(X)))
  }

  # If the pred and truth vars are factors, their levels need to be the same
  if (is.factor(X[,truthVar])) {
    stopifnot(seteqal(levels(X[,truthVar]), levels(X[,predVar])))
  }

  # Now check the truthVar and predVar's
  tVar <- X[,truthVar]
  pVar <- X[,predVar]

  # If they're both factors, they need to have the same levels
  if (is.factor(tVar) & is.factor(pVar)) {
      
    if (!setequal(levels(tVar), levels(pVar))) {
      stop("When 'truthvar' and 'predVar' are factors, they must both have the same levels")
    }
    
    classes <- levels(tVar)
    nClass <- nlevel(tVar)
    
  }
  # Otherwise, they need to be of the same type and they will be converted to factors
  else {
      
    if (typeof(tVar) != typeof(pVar)) {
      stop("If 'truthVar' and 'predVar' are not both factors, they must be the same type")
    }

    # Convert them to factors and insert them into X
    classes <- sort(unique(c(tVar, pVar)))

    X[,truthVar] <- factor(tVar, levels = classes)
    X[,predVar] <- factor(pVar, levels = classes)

    nClass <- length(classes)
    
  }

  # Check
  if (nClass < 2) {
    stop("The 'truthVar' and 'predVar' must have at least 2 classes to calculate fidelity")
  }

  # Function to calculate the fidelity metrics for a single group
  cFidel <- function(x) {

    # Calculate confusion summary once
    confusionSummary <- confusion(x[,truthVar], x[,predVar])

    # Calculate each of the fidelity metrics
    fidelitySummary <- lapply(fMethods,
                              function(fMethod) {
                                do.call(fMethod, list(confusionSummary = confusionSummary, aggregate = aggregate))
                              })

    # Add names of the fidelity metrics to the list
    names(fidelitySummary) <- fMethods

    # Rearrange the list into two elements, each with a data frame: 'aggregate' and 'byClass'
    aggregate <- as.data.frame(rbind(unlist(lapply(fidelitySummary, function(x) x$aggregate))))
    byClass <- as.data.frame(t(Smisc::list2df(lapply(fidelitySummary, function(x) x$byClass))))

    # Add in the rownames as a variable
    byClass$class <- rownames(byClass)

    # Rearrange the rows
    byClass <- byClass[, c("class", fMethods)]

    # Reset the rownames
    rownames(byClass) <- 1:nrow(byClass)

    return(list(aggregate = aggregate , byClass = byClass))
    
  } # cFidel

  # If there are no groups
  if (is.null(groupVars)) {
    out <- cFidel(X)
  }

  # But if there are groups...  
  else {

    out <- plyr::dlply(X, groupVars, cFidel)
  
    # Get the group indicator data.frame
    groupInd <- attributes(out)$split_labels
    
    # Collapse results into a list of 2 data frames
    # Get the aggregate first
    aggOut <- cbind(groupInd,
                    Smisc::list2df(lapply(out, function(x) x$aggregate)))
  
    # A function to add in the grouping variables to each list element
    i <- 0
  
    addGroup <- function(x) {
        
      i <<- i + 1
      gi <- groupInd[i,]
      rownames(gi) <- NULL
      
      return(cbind(gi, x$byClass))
  
    } # addGroup
  
    byClassOut <- Smisc::list2df(lapply(out, addGroup))
  
    # Fix up the column names
    colnames(byClassOut)[1:ncol(groupInd)] <- colnames(groupInd)
  
    # Restore factors to "class" label
    byClassOut$class <- factor(byClassOut$class, levels = classes)
  
    # If groupVars were originally character, then change them back to character.  All other types
    # (int, factor, numeric) should remain as they originally were...
    for (g in groupVars) {

      # Change to character
      if (is.character(X[,g])) {
        byClassOut[,g] <- as.character(byClassOut[,g])
      }

      # Verify the type is preserved
      if (typeof(byClassOut[,g]) != typeof(X[,g])) {
        warning("For 'groupVar = ", g, "', the output type, ", typeof(byClassOut[,g]),
                ", is not the same as the input type, ", typeof(X[,g]))
      }
      
    } # for (g in groupVars)

    # Near finished outcome
    out <- list(aggregate = aggOut, byClass = byClassOut)

  } # else there are groups
  
  ## Identify whether only two classes are used (binary classification)
  ## The positiveLabel for simplification of the binary classification case
  if (nClass == 2) {
    
    # See whether the positiveLabel argument is present
    if (!is.null(positiveLabel)) {

      positiveLabel <- as.character(positiveLabel)
        
      # Make sure the positive response they picked was actually a viable class
      # Grab only the rows that had positive responses      
      if (positiveLabel %in% classes) {
        out$byClass <- out$byClass[out$byClass$class == positiveLabel, ]
      }
      else { 
        warning("'positiveLabel = ", positiveLabel, "' was not one of the classes")
      }
    }
  }
  else {
    if (!is.null(positiveLabel))
      warning("'positiveLabel' argument was ignored because there are more than two classes")
  }

  # Return final results
  return(out)
  
} # calcFidelity


  ## # Identify whether only two classes are used (binary classification)
  ## # The positiveLabel for simplification of the binary classification case
  
  ## else if (nClass == 2) {
    
  ##   # Set the aggregate calculation to NULL
  ##   # Removing this because they may be interested in the aggregate result 
  ##   # when two classes are present.
  ##   # aggregate <- NULL
    
  ##   # See whether the positiveLabel argument is present
  ##   if (!is.null(positiveLabel)) {
      
  ##     # Make sure the positive response they picked was actually a viable class
  ##     # Grab only the rows that had positive responses      
  ##     if (positiveLabel %in% classes)
  ##       byClass <- byClass[byClass$class == positiveLabel, ]
  ##     else 
  ##       warning("'positiveLabel = ", positiveLabel, "' was not one of the classes")
  ##   }
  ## }
  ## else {
  ##   if (!is.null(positiveLabel))
  ##     warning("'positiveLabel' argument was ignored because there are more than two classes")
  ## }

  ## # Return the final list
  ## list(aggregate = aggregate, byClass = byClass)



### Script to test
## library(SQM)
## data(exampleSignatures)
## a <- exampleSignatures
## a <- a[a$signatureID == 1,]

## #calcFidelity(a, truthVar = "truthClass", predVar = "predictedClass")# methods = c("accuracy","nothing","something"))

## out <- calcFidelity(exampleSignatures, truthVar = "truthClass", predVar = "predictedClass", groupVars = "signatureID")

## b <- exampleSignatures
## b$sVar <- c("b", rep(c("a","b"), each = 210))

## out <- calcFidelity(b, truthVar = "truthClass", predVar = "predictedClass", groupVars = c("signatureID","sVar"))

## set.seed(7)

## # Make a dataset that will work
## a <- expand.grid(g1 = rep(1:3, each = 4), g2 = rep(letters[1:2], each = 5))

## # Test a groupVar with only one level
## a$pred <- sample(rep(1:3, 40))
## a$truth <- sample(rep(1:3, 40))

## out <- calcFidelity(a, truthVar = "truth", predVar = "pred", groupVars = c("g1", "g2"))

## out1 <- calcFidelity(a, truthVar = "truth", predVar = "pred")
