###
test.TPR <- function(conf.summary,aggregate = c("micro","macro")){
  ag = match.arg(aggregate)
  return(list("byClass" = 1-test.FNR(conf.summary,aggregate=aggregate)[[1]], 
              "aggregate" = 1-test.FNR(conf.summary,aggregate=aggregate)[[2]]))
}

# Tests an example using the exampleSignatures data:
exec.test.TPR <- function() {

	# Example of a check between test and TPR() is below.
	# Now to test if have same results between test.TPR and TPR functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	TPRmicroresult <- TPR(confresult,aggregate="micro")
	test.TPRmicroresult <- test.TPR(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(TPRmicroresult))!=length(unlist(test.TPRmicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(TPRmicroresult)-unlist(test.TPRmicroresult))>(10^(-15)))>0) {
		  stop("Results between test and TPR for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	TPRmacroresult <- TPR(confresult,aggregate="macro")
	test.TPRmacroresult <- test.TPR(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(TPRmacroresult))!=length(unlist(test.TPRmacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(TPRmacroresult)-unlist(test.TPRmacroresult))>(10^(-15)))>0) {
		  stop("Results between test and TPR for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}

