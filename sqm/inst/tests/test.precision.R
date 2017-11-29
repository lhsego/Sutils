test.precision <- function(conf.summary, aggregate = c("micro","macro")){
  # Choose aggregation method specified by aggregate
  aggregate <- match.arg(aggregate)
  
  if (aggregate == "micro"){
    
    byClass <- lapply(1:conf.summary$numClasses, function(x){
      
      # Returns names of different classes according to confusionSummary
      nval <- names(conf.summary$classSummary);
      
      # Just so I don't have to keep typing it.
      q <- conf.summary$classSummary;
      
      # Recall by class
      q[nval][x][[1]]$truePos/(q[nval][x][[1]]$truePos+q[nval][x][[1]]$falsePos)
    })
    
    nval <- names(conf.summary$classSummary);
    q <- conf.summary$classSummary;
    
    tpos <- 0;
    fpos <- 0;
    for(i in 1:conf.summary$numClasses){
      tpos <- tpos + q[nval][i][[1]]$truePos
      fpos <- fpos + q[nval][i][[1]]$falsePos
    }
    
    byClass <- unlist(as.data.frame(t(as.matrix(byClass))));
    names(byClass) <- names(conf.summary$classSummary);
    
    return(list("byClass" = byClass,"aggregate" = tpos/(tpos+fpos)))
    
  } else {
    
    byClass <- lapply(1:conf.summary$numClasses, function(x){
      
      # Returns names of different classes according to confusionSummary
      nval <- names(conf.summary$classSummary);
      
      # Just so I don't have to keep typing it.
      q <- conf.summary$classSummary;
      
      # Recall by class
      q[nval][x][[1]]$truePos/(q[nval][x][[1]]$truePos+q[nval][x][[1]]$falsePos)
    })
    byClass <- unlist(as.data.frame(t(as.matrix(byClass))));
    names(byClass) <- names(conf.summary$classSummary);
    
    return(list("byClass" = byClass, "aggregate" = mean(byClass)))
    
  } # else
  
} # function

# Tests an example using the exampleSignatures data:
exec.test.precision <- function() {

	# Example of a check between test and precision() is below.
	# Now to test if have same results between test.precision and precision functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	precisionmicroresult <- precision(confresult,aggregate="micro")
	test.precisionmicroresult <- test.precision(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(precisionmicroresult))!=length(unlist(test.precisionmicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(precisionmicroresult)-unlist(test.precisionmicroresult))>(10^(-15)))>0) {
		  stop("Results between test and precision for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	precisionmacroresult <- precision(confresult,aggregate="macro")
	test.precisionmacroresult <- test.precision(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(precisionmacroresult))!=length(unlist(test.precisionmacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(precisionmacroresult)-unlist(test.precisionmacroresult))>(10^(-15)))>0) {
		  stop("Results between test and precision for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}


