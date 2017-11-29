### False Negative Rate###

test.TNR <- function(conf.summary,aggregate=c("micro","macro")){
  aggregate <- match.arg(aggregate)
  
  if (aggregate == "micro"){
    
    byClass <- lapply(1:conf.summary$numClasses, function(x){
      
      # Returns names of different classes according to confusionSummary
      nval <- names(conf.summary$classSummary);
      
      # Just so I don't have to keep typing it.
      q <- conf.summary$classSummary;
      
      # Recall by class
      q[nval][x][[1]]$trueNeg/(q[nval][x][[1]]$trueNeg + q[nval][x][[1]]$falsePos)
    })
    
    nval <- names(conf.summary$classSummary);
    q <- conf.summary$classSummary;
    
    tneg <- 0;
    fpos <- 0;
    for(i in 1:conf.summary$numClasses){
      tneg <- tneg + q[nval][i][[1]]$trueNeg
      fpos <- fpos + q[nval][i][[1]]$falsePos
    }
    
    byClass <- unlist(as.data.frame(t(as.matrix(byClass))));
    names(byClass) <- names(conf.summary$classSummary);
    
    return(list("byClass" = byClass,"aggregate" = tneg/(tneg+fpos)))
    
  } else {
    
    byClass <- lapply(1:conf.summary$numClasses, function(x){
      
      # Returns names of different classes according to confusionSummary
      nval <- names(conf.summary$classSummary);
      
      # Just so I don't have to keep typing it.
      q <- conf.summary$classSummary;
      
      # Recall by class
      q[nval][x][[1]]$trueNeg/(q[nval][x][[1]]$trueNeg+q[nval][x][[1]]$falsePos)
    })
    
    byClass <- unlist(as.data.frame(t(as.matrix(byClass))));
    names(byClass) <- names(conf.summary$classSummary);
    
    return(list("byClass" = byClass, "aggregate" = mean(byClass)))
    
  } # else
  
} # function

# Tests an example using the exampleSignatures data:
exec.test.TNR <- function() {

	# Example of a check between test and TNR() is below.
	# Now to test if have same results between test.TNR and TNR functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	TNRmicroresult <- TNR(confresult,aggregate="micro")
	test.TNRmicroresult <- test.TNR(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(TNRmicroresult))!=length(unlist(test.TNRmicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(TNRmicroresult)-unlist(test.TNRmicroresult))>(10^(-15)))>0) {
		  stop("Results between test and TNR for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	TNRmacroresult <- TNR(confresult,aggregate="macro")
	test.TNRmacroresult <- test.TNR(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(TNRmacroresult))!=length(unlist(test.TNRmacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(TNRmacroresult)-unlist(test.TNRmacroresult))>(10^(-15)))>0) {
		  stop("Results between test and TNR for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}


