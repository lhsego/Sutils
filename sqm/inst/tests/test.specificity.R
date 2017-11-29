test.specificity <- function(conf.summary, aggregate = c("micro","macro")){
  ag <- match.arg(aggregate)
  return(test.TNR(conf.summary,ag))
}

# Tests an example using the exampleSignatures data:
exec.test.specificity <- function() {

	# Example of a check between test and specificity() is below.
	# Now to test if have same results between test.specificity and specificity functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	specificitymicroresult <- specificity(confresult,aggregate="micro")
	test.specificitymicroresult <- test.specificity(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(specificitymicroresult))!=length(unlist(test.specificitymicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(specificitymicroresult)-unlist(test.specificitymicroresult))>(10^(-15)))>0) {
		  stop("Results between test and specificity for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	specificitymacroresult <- specificity(confresult,aggregate="macro")
	test.specificitymacroresult <- test.specificity(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(specificitymacroresult))!=length(unlist(test.specificitymacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(specificitymacroresult)-unlist(test.specificitymacroresult))>(10^(-15)))>0) {
		  stop("Results between test and specificity for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}

