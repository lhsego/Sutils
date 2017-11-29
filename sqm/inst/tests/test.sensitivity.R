test.sensitivity <- function(conf.summary, aggregate = c("micro","macro")){
  ag <- match.arg(aggregate)
  return(test.TPR(conf.summary,ag))
}

# Tests an example using the exampleSignatures data:
exec.test.sensitivity <- function() {

	# Example of a check between test and sensitivity() is below.
	# Now to test if have same results between test.sensitivity and sensitivity functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	sensitivitymicroresult <- sensitivity(confresult,aggregate="micro")
	test.sensitivitymicroresult <- test.sensitivity(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(sensitivitymicroresult))!=length(unlist(test.sensitivitymicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(sensitivitymicroresult)-unlist(test.sensitivitymicroresult))>(10^(-15)))>0) {
		  stop("Results between test and sensitivity for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	sensitivitymacroresult <- sensitivity(confresult,aggregate="macro")
	test.sensitivitymacroresult <- test.sensitivity(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(sensitivitymacroresult))!=length(unlist(test.sensitivitymacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(sensitivitymacroresult)-unlist(test.sensitivitymacroresult))>(10^(-15)))>0) {
		  stop("Results between test and sensitivity for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}