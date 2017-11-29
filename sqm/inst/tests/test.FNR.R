### False Negative Rate###

test.FNR <- function(conf.summary,aggregate=c("micro","macro")){
  aggregate <- match.arg(aggregate)
  
  if (aggregate == "micro"){
    
    byClass <- lapply(1:conf.summary$numClasses, function(x){
      
      # Returns names of different classes according to confusionSummary
      nval <- names(conf.summary$classSummary);
      
      # Just so I don't have to keep typing it.
      q <- conf.summary$classSummary;
      
      # Recall by class
      q[nval][x][[1]]$falseNeg/(q[nval][x][[1]]$falseNeg + q[nval][x][[1]]$truePos)
    })
    
    nval <- names(conf.summary$classSummary);
    q <- conf.summary$classSummary;
    
    fneg <- 0;
    tpos <- 0;
    for(i in 1:conf.summary$numClasses){
      fneg <- fneg + q[nval][i][[1]]$falseNeg
      tpos <- tpos + q[nval][i][[1]]$truePos
    }
    
    byClass <- unlist(as.data.frame(t(as.matrix(byClass))));
    names(byClass) <- names(conf.summary$classSummary);
    
    return(list("byClass" = byClass,"aggregate" = fneg/(fneg+tpos)))
    
  } else {
    
    byClass <- lapply(1:conf.summary$numClasses, function(x){
      
      # Returns names of different classes according to confusionSummary
      nval <- names(conf.summary$classSummary);
      
      # Just so I don't have to keep typing it.
      q <- conf.summary$classSummary;
      
      # Recall by class
      q[nval][x][[1]]$falseNeg/(q[nval][x][[1]]$falseNeg+q[nval][x][[1]]$truePos)
    })
    
    byClass <- unlist(as.data.frame(t(as.matrix(byClass))));
    names(byClass) <- names(conf.summary$classSummary);
    
    return(list("byClass" = byClass, "aggregate" = mean(byClass)))
    
  } # else
  
} # function

# Tests an example using the exampleSignatures data:
exec.test.FNR <- function() {

	# Example of a check between test and FNR() is below.
	# Now to test if have same results between test.FNR and FNR functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	FNRmicroresult <- FNR(confresult,aggregate="micro")
	test.FNRmicroresult <- test.FNR(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(FNRmicroresult))!=length(unlist(test.FNRmicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(FNRmicroresult)-unlist(test.FNRmicroresult))>(10^(-15)))>0) {
		  stop("Results between test and FNR for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	FNRmacroresult <- FNR(confresult,aggregate="macro")
	test.FNRmacroresult <- test.FNR(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(FNRmacroresult))!=length(unlist(test.FNRmacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(FNRmacroresult)-unlist(test.FNRmacroresult))>(10^(-15)))>0) {
		  stop("Results between test and FNR for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}


