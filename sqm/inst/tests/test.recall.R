test.recall <- function(conf.summary, aggregate = c("micro","macro")){
  # Choose aggregation method specified by aggregate
  aggregate <- match.arg(aggregate)
  
  if (aggregate == "micro"){
  
  byClass <- lapply(1:conf.summary$numClasses, function(x){
      
    # Returns names of different classes according to confusionSummary
    nval <- names(conf.summary$classSummary);
    
    # Just so I don't have to keep typing it.
    q <- conf.summary$classSummary;
     
    # Recall by class
    q[nval][x][[1]]$truePos/(q[nval][x][[1]]$truePos+q[nval][x][[1]]$falseNeg)
  })
  
  nval <- names(conf.summary$classSummary);
  q <- conf.summary$classSummary;
  
  tpos <- 0;
  fneg <- 0;
  for(i in 1:conf.summary$numClasses){
    tpos <- tpos + q[nval][i][[1]]$truePos
    fneg <- fneg + q[nval][i][[1]]$falseNeg
  }
  
  byClass <- unlist(as.data.frame(t(as.matrix(byClass))));
  names(byClass) <- names(conf.summary$classSummary);
  
  return(list("byClass" = byClass,"aggregate" = tpos/(tpos+fneg)))
  
  } else {
   
    byClass <- lapply(1:conf.summary$numClasses, function(x){
      
      # Returns names of different classes according to confusionSummary
      nval <- names(conf.summary$classSummary);
      
      # Just so I don't have to keep typing it.
      q <- conf.summary$classSummary;
      
      # Recall by class
      q[nval][x][[1]]$truePos/(q[nval][x][[1]]$truePos+q[nval][x][[1]]$falseNeg)
    })
    byClass <- unlist(as.data.frame(t(as.matrix(byClass))));
    names(byClass) <- names(conf.summary$classSummary);
    
    return(list("byClass" = byClass, "aggregate" = mean(byClass)))
 
  } # else
  
} # function
  
# Tests an example using the exampleSignatures data:
exec.test.recall <- function() {

	# Example of a check between test and recall() is below.
	# Now to test if have same results between test.recall and recall functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	recallmicroresult <- recall(confresult,aggregate="micro")
	test.recallmicroresult <- test.recall(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(recallmicroresult))!=length(unlist(test.recallmicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(recallmicroresult)-unlist(test.recallmicroresult))>(10^(-15)))>0) {
		  stop("Results between test and recall for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	recallmacroresult <- recall(confresult,aggregate="macro")
	test.recallmacroresult <- test.recall(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(recallmacroresult))!=length(unlist(test.recallmacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(recallmacroresult)-unlist(test.recallmacroresult))>(10^(-15)))>0) {
		  stop("Results between test and recall for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}


  
  
 