# need to take in a data_frame with
# v2 being truthClass and 
# v3 being the predictedClass

test.confusion <- function(v2,v3){
 
  #creates the confusion matrix and stores it in w
  w <- table(v3,v2)
  # If a predicted class is missing from the table, need to add it:
  missingpredictedclasses <- setdiff(colnames(w),rownames(w))
  if (length(missingpredictedclasses)>0) {
    # Make the new matrix, fill it with zeros:
    neww <- matrix(rep(0,length(colnames(w))^2),nrow=length(colnames(w)))
    colnames(neww) <- rownames(neww) <- colnames(w)
    # Add in the rest of the data:
    for (j in rownames(w)) {
	neww[(sum((1:length(colnames(w))*(colnames(w)==j)))),] <- w[(sum((1:length(rownames(w))*(rownames(w)==j)))),]
    }
    # Replace w with the neww matrix:
    w <- neww
  }	

  
  #needed values of matrix w
  # numClasses should be based on the number of classes represented in truthClass
  numClasses <- ncol(w)
  s <- sum(w)
  cs <- colSums(w)
  t <- diag(w)
  
  #list classSummary which is used in for-loop
  classSummary <- list()
  classSummary1 <- list()
  for(i in seq(numClasses)){
    
     #calculates the number of truePositves from w
     v <- t
     truePos <- v
     classSummary1$truePos[i] <- v[i]
     #calculates the number of falsePositives from w
     y <- (rowSums(w)-t[i])
     falsePos <- y
     classSummary1$falsePos[i] <- y[i]
     #calculates the number of falseNegatives from w
     z <- (colSums(w)-t[i])
     falseNeg <- z
     classSummary1$falseNeg[i] <- z[i]
     #calculates the number of trueNegatives from w
     x <- (s-t[i]-(rowSums(w)-t[i])-(colSums(w)-t[i]))
     trueNeg <- x
     classSummary1$trueNeg[i] <- x[i]
     #calculates the class Sample Size of w
     p <- cs
     classSampleSize <- p
     classSummary1$classSampleSize[i] <- p[i]
  } 
  # Take classSummary1 and put in first argument of classSummary:
  pos <- 0
  for (i in colnames(w)) {
	pos <- pos + 1
	classSummary$classSummary[[i]]$truePos <- classSummary1$truePos[pos]
	classSummary$classSummary[[i]]$trueNeg <- classSummary1$trueNeg[pos]
	classSummary$classSummary[[i]]$falsePos <- classSummary1$falsePos[pos]
	classSummary$classSummary[[i]]$falseNeg <- classSummary1$falseNeg[pos]
	classSummary$classSummary[[i]]$classSampleSize <- classSummary1$classSampleSize[pos]
  }

  #calculates the sample size of w
  classSummary$sampleSize <- s
  #calculates the number of classes in w
  classSummary$numClasses <- numClasses
  #returns the classSummary in a list invisibly
  invisible(classSummary)
}

# Tests an example using the exampleSignatures data:
exec.test.confusion <- function() {

	# Example of a check between test and confusion() is below.
	# Now to test if have same results between test.confusion and confusion functions:
	#need to load(data) using:
   	suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	test.confresult <- test.confusion(exampleSignatures[,2],exampleSignatures[,3])
	confresult <- confusion(exampleSignatures[,2],exampleSignatures[,3])

	# Faster Method:
	if (all.equal(unlist(test.confresult),unlist(confresult))) {
	  return(NULL)
	} else {
	  print("Results of test function and confusion() are not equal.")
	}

	# Code below gives greater detail on where differences exist:
	# Check highest-level agreement:
	if (length(test.confresult)!=length(confresult)) {
		stop("Dimensions of results for test and confusion() do not agree.")
	} else {
		if (length(test.confresult$classSummary)!=length(confresult$classSummary)) {
	  	  stop("Different number of classes represented between test and confusion().")
		} else {
		  for (i in seq(length(confresult$classSummary))) {
			if (test.confresult$classSummary[[i]]$truePos != confresult$classSummary[[i]]$truePos) {
			  stop("Number of true positives does not agree between test and confusion() for at least one of the classes.")
			} else {
			  if (test.confresult$classSummary[[i]]$trueNeg != confresult$classSummary[[i]]$trueNeg) {
			  	stop("Number of true negatives does not agree between test and confusion() for at least one of the classes.")
			  } else {
				if (test.confresult$classSummary[[i]]$falsePos != confresult$classSummary[[i]]$falsePos) {
			  	  stop("Number of false positives does not agree between test and confusion() for at least one of the classes.")
			  	} else {
				  if (test.confresult$classSummary[[i]]$falseNeg != confresult$classSummary[[i]]$falseNeg) {
			  	  	stop("Number of false negatives does not agree between test and confusion() for at least one of the classes.")
			  	  } else {
					if (test.confresult$classSummary[[i]]$classSampleSize != confresult$classSummary[[i]]$classSampleSize) {
					  stop("Class sample size does not agree between test and confusion() for at least one of the classes.") 
					}
			  	  }
			  	}
			  }
			}
		  }
		}
		if (test.confresult$sampleSize != confresult$sampleSize) {
		  stop("Sample size does not agree between test and confusion().")
		}
		if (test.confresult$numClasses != confresult$numClasses) {
		  stop("Number of classes does not agree between test and confusion().")
		}
	}
	# If everything worked correctly, return NULL.
	return(NULL)
}