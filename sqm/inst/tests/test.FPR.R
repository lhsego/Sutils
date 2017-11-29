###
test.FPR <- function(conf.summary,aggregate = c("micro","macro")){
ag = match.arg(aggregate)
return(list("byClass" = 1-test.TNR(conf.summary,aggregate=aggregate)[[1]], 
            "aggregate" = 1-test.TNR(conf.summary,aggregate=aggregate)[[2]]))
}

# Tests an example using the exampleSignatures data:
exec.test.FPR <- function() {

	# Example of a check between test and FPR() is below.
	# Now to test if have same results between test.FPR and FPR functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	FPRmicroresult <- FPR(confresult,aggregate="micro")
	test.FPRmicroresult <- test.FPR(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(FPRmicroresult))!=length(unlist(test.FPRmicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(FPRmicroresult)-unlist(test.FPRmicroresult))>(10^(-15)))>0) {
		  stop("Results between test and FPR for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	FPRmacroresult <- FPR(confresult,aggregate="macro")
	test.FPRmacroresult <- test.FPR(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(FPRmacroresult))!=length(unlist(test.FPRmacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(FPRmacroresult)-unlist(test.FPRmacroresult))>(10^(-15)))>0) {
		  stop("Results between test and FPR for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}


