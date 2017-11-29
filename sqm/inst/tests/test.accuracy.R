### make sure to load(exampleSignatures)

test.accuracy <- function(confusionSummary, aggregate=c("micro","macro")){

    aggregate <- match.arg(aggregate)

    nC <- confusionSummary$numClasses
    byClass <- rep(0,nC)
    # Compute the accuracy for each class:
    for (i in 1:nC) {
	byClass[i] <- (confusionSummary$classSummary[[i]]$truePos + confusionSummary$classSummary[[i]]$trueNeg)/(confusionSummary$classSummary[[i]]$truePos + confusionSummary$classSummary[[i]]$trueNeg + confusionSummary$classSummary[[i]]$falsePos + confusionSummary$classSummary[[i]]$falseNeg)
    }
    names(byClass) <- names(confusionSummary$classSummary)

    if (aggregate == "micro") {
	numerator <- 0
	denominator <- 0
	for (i in 1:nC) {
	  numerator <- numerator + confusionSummary$classSummary[[i]]$truePos + confusionSummary$classSummary[[i]]$trueNeg
	  denominator <- denominator + confusionSummary$classSummary[[i]]$truePos + confusionSummary$classSummary[[i]]$trueNeg + confusionSummary$classSummary[[i]]$falsePos + confusionSummary$classSummary[[i]]$falseNeg
	}
	aggregate <- numerator/denominator
    } else {
	aggregate <- mean(byClass)
    }
    
    # new list with elements byClass and aggregate
    accuracy <- list("byClass"=byClass,"aggregate"=aggregate)
    
    #return accuracy list invisibly
    invisible(accuracy) 
}


# Tests an example using the exampleSignatures data:
exec.test.accuracy <- function() {

	# Example of a check between test and accuracy() is below.
	# Note that for this example, we ignore the "signatureID" distinctions.
	# Now to test if have same results between test.accuracy and accuracy functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	accuracymicroresult <- accuracy(confresult,aggregate="micro")
	test.accuracymicroresult <- test.accuracy(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(accuracymicroresult))!=length(unlist(test.accuracymicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(accuracymicroresult)-unlist(test.accuracymicroresult))>(10^(-15)))>0) {
		  stop("Results between test and accuracy for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	accuracymacroresult <- accuracy(confresult,aggregate="macro")
	test.accuracymacroresult <- test.accuracy(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(accuracymacroresult))!=length(unlist(test.accuracymacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(accuracymacroresult)-unlist(test.accuracymacroresult))>(10^(-15)))>0) {
		  stop("Results between test and accuracy for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}
