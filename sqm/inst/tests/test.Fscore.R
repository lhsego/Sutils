# conf.summary is the output of the function test.confusion.R, the confusion summary

test.Fscore <- function(conf.summary, beta=1, aggregate = c("micro","macro")){
  
  aggregate <- match.arg(aggregate)
  
  # Calculate F1 score in terms of classification rates by class.
  F1s <- lapply(1:conf.summary$numClasses, function(x){
    
    # Returns names of different classes according to confusionSummary
    nval <- names(conf.summary$classSummary);
    
    # Just so I don't have to keep typing it.
    q <- conf.summary$classSummary;
    
    # w = weight
    w <- (1+beta^2);
    
    # The F-score
    w*q[nval][x][[1]]$truePos/(w*q[nval][x][[1]]$truePos+(w-1)*q[nval][x][[1]]$falseNeg + q[nval][x][[1]]$falsePos)
    
  })
  
  objPre <- test.precision(conf.summary,aggregate=aggregate)
  
  objRec <- test.recall(conf.summary,aggregate=aggregate)
  
  byClass <- unlist(as.data.frame(t(as.matrix(F1s))))
  
  names(byClass) <- names(conf.summary$classSummary)
  
  return(list("byClass" = byClass, 
  "aggregate" = (1+beta^2)*objPre$aggregate*objRec$aggregate/(beta^2*objPre$aggregate + objRec$aggregate)))
 
}

# Tests an example using the exampleSignatures data:
exec.test.Fscore <- function() {

	# Example of a check between test and Fscore() is below.
	# Now to test if have same results between test.Fscore and Fscore functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])
	# Check results if aggregate type "micro" is called:
	Fscoremicroresult <- Fscore(confresult,aggregate="micro")
	test.Fscoremicroresult <- test.Fscore(confresult,aggregate="micro")
	# Check all results match wihin a tolerance:
	if (length(unlist(Fscoremicroresult))!=length(unlist(test.Fscoremicroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(Fscoremicroresult)-unlist(test.Fscoremicroresult))>(10^(-15)))>0) {
		  stop("Results between test and Fscore for micro aggregate type do not agree within tolerance of 1-e15.")
		}
	}
	# Check results if aggregate type "micro" is called:
	Fscoremacroresult <- Fscore(confresult,aggregate="macro")
	test.Fscoremacroresult <- test.Fscore(confresult,aggregate="macro")
	# Check all results match wihin a tolerance:
	if (length(unlist(Fscoremacroresult))!=length(unlist(test.Fscoremacroresult))) {
	 stop("Different number of arguments returned.")
	} else {
		if (sum(abs(unlist(Fscoremacroresult)-unlist(test.Fscoremacroresult))>(10^(-15)))>0) {
		  stop("Results between test and Fscore for macro aggregate type do not agree within tolerance of 1-e15.")
		}
	}

	# If no stops, return NULL:
	return(NULL)
}



