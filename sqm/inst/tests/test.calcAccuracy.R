test.calcAccuracy <- function(signatures, methods = accuracyMethods(),
					aggregate = c("micro", "macro"), positiveLabel = NULL){
  
#  To make this test function return exactly the same results as calcAccuracy() if methods==NULL, getting rid of this code: 
#  if (methods == NULL) {
#	warning("No methods were selected.")
#	return(NULL)
#  }
  
  accuracyMethods <- match.arg(methods, accuracyMethods(), several.ok = TRUE)
  aggregate <- match.arg(aggregate)

  byClass <- NULL
  ag <- NULL
  sigClass <- factor(sort(unique(signatures$signatureID)),levels=sort(levels(factor(unique(signatures$signatureID)))))
  for (i in sigClass) {
	sigClassSignature <- subset(signatures,signatureID==i)
	conf.summary <- confusion(sigClassSignature$truthClass,sigClassSignature$predictedClass)
	if (is.null(byClass)) {
	  byClass <- sapply(1:length(accuracyMethods), function(x){
        	tline <- paste(accuracyMethods[x],"(conf.summary,aggregate='",aggregate,"')$byClass",sep="")
        	eval(parse(text=tline))
	  })
	  byClass <- cbind(as.data.frame(rep(i,dim(byClass)[1])),as.data.frame(rownames(byClass)),as.data.frame(byClass))
	  colnames(byClass) <- c("signatureID","class",accuracyMethods)
	  ag <- sapply(1:length(accuracyMethods), function(x){
        	tline <- paste(accuracyMethods[x],"(conf.summary,aggregate='",aggregate,"')$aggregate",sep="")
        	eval(parse(text=tline))
	  })
	  ag <- cbind(as.data.frame(i),as.data.frame(t(ag)))
	  colnames(ag) <- c("signatureID",accuracyMethods)
	} else {
	  addbyClass <- sapply(1:length(accuracyMethods), function(x){
        	tline <- paste(accuracyMethods[x],"(conf.summary,aggregate='",aggregate,"')$byClass",sep="")
        	eval(parse(text=tline))
	  })
	  addbyClass <- cbind(as.data.frame(rep(i,dim(addbyClass)[1])),as.data.frame(rownames(addbyClass)),as.data.frame(addbyClass))
	  colnames(addbyClass) <- c("signatureID","class",accuracyMethods)
	  addag <- sapply(1:length(accuracyMethods), function(x){
        	tline <- paste(accuracyMethods[x],"(conf.summary,aggregate='",aggregate,"')$aggregate",sep="")
        	eval(parse(text=tline))
	  })
	  addag <- cbind(as.data.frame(i),as.data.frame(t(addag)))
	  colnames(addag) <- c("signatureID",accuracyMethods)
	  byClass <- rbind(byClass,addbyClass)
	  ag <- rbind(ag,addag)
	}
	
  }
  ag[,1] <- sigClass
  byClass[,1] <- factor(byClass[,1],levels=levels(sigClass))

  rownames(byClass) <- NULL
  rownames(ag) <- NULL
  
  return(list("aggregate" = ag, "byClass" = byClass))
  
}

# Tests an example using the exampleSignatures data:
exec.test.calcAccuracy <- function() {

	# Example of a check between test and calcAccuracy() is below.
	# Now to test if have same results between test.accuracy and accuracy functions:
	# need to load(data) using:
   	# suppressPackageStartupMessages(require(SQM))
   	data(exampleSignatures)

	# Check results if aggregate type "micro" is called:
	calcAccuracymicroresult <- calcAccuracy(exampleSignatures,aggregate="micro")
	test.calcAccuracymicroresult <- test.calcAccuracy(exampleSignatures,aggregate="micro")
	# Check to see if the returned data.frames are the same size:
	if (!identical(dim(calcAccuracymicroresult$aggregate),dim(test.calcAccuracymicroresult$aggregate))) {
	  stop("Different number of columns and/or rows returned for $aggregate")
	}
	if (!identical(dim(calcAccuracymicroresult$byClass),dim(test.calcAccuracymicroresult$byClass))) {
	  stop("Different number of columns and/or rows returned for $byClass")
	}
	# Check if the class and signatureID columns match:
	if (!identical(calcAccuracymicroresult$aggregate[,1],test.calcAccuracymicroresult$aggregate[,1])) {
	  stop("signatureID columns don't match between test and calcAccuracy() for $aggregate")
	}
	if (!identical(calcAccuracymicroresult$byClass[,1:2],test.calcAccuracymicroresult$byClass[,1:2])) {
	  stop("signatureID and/or class columns don't match between test and calcAccuracy() for $byClass")
	}
	# Check all numeric (non-factor) results match exactly:
	if (sum(abs(calcAccuracymicroresult$aggregate[,2:dim(calcAccuracymicroresult$aggregate)[2]]-test.calcAccuracymicroresult$aggregate[,2:dim(test.calcAccuracymicroresult$aggregate)[2]])>0)>0) {
		stop("$aggregate results between test and calcAccuracy for micro aggregate type do not agree within tolerance of 1-e15.")
	}
	if (sum(abs(calcAccuracymicroresult$byClass[,3:dim(calcAccuracymicroresult$byClass)[2]]-test.calcAccuracymicroresult$byClass[,3:dim(test.calcAccuracymicroresult$byClass)[2]])>0)>0) {
		stop("$byClass results between test and calcAccuracy for micro aggregate type do not agree within tolerance of 1-e15.")
	}

	# Check results if aggregate type "macro" is called:
	calcAccuracymacroresult <- calcAccuracy(exampleSignatures,aggregate="macro")
	test.calcAccuracymacroresult <- test.calcAccuracy(exampleSignatures,aggregate="macro")
	# Check to see if the returned data.frames are the same size:
	if (!identical(dim(calcAccuracymacroresult$aggregate),dim(test.calcAccuracymacroresult$aggregate))) {
	  stop("Different number of columns and/or rows returned for $aggregate")
	}
	if (!identical(dim(calcAccuracymacroresult$byClass),dim(test.calcAccuracymacroresult$byClass))) {
	  stop("Different number of columns and/or rows returned for $byClass")
	}
	# Check if the class and signatureID columns match:
	if (!identical(calcAccuracymacroresult$aggregate[,1],test.calcAccuracymacroresult$aggregate[,1])) {
	  stop("signatureID columns don't match between test and calcAccuracy() for $aggregate")
	}
	if (!identical(calcAccuracymacroresult$byClass[,1:2],test.calcAccuracymacroresult$byClass[,1:2])) {
	  stop("signatureID and/or class columns don't match between test and calcAccuracy() for $byClass")
	}
	# Check all numeric (non-factor) results match exactly:
	if (sum(abs(calcAccuracymacroresult$aggregate[,2:dim(calcAccuracymacroresult$aggregate)[2]]-test.calcAccuracymacroresult$aggregate[,2:dim(test.calcAccuracymacroresult$aggregate)[2]])>0)>0) {
		stop("$aggregate results between test and calcAccuracy for macro aggregate type do not agree within tolerance of 1-e15.")
	}
	if (sum(abs(calcAccuracymacroresult$byClass[,3:dim(calcAccuracymacroresult$byClass)[2]]-test.calcAccuracymacroresult$byClass[,3:dim(test.calcAccuracymacroresult$byClass)[2]])>0)>0) {
		stop("$byClass results between test and calcAccuracy for macro aggregate type do not agree within tolerance of 1-e15.")
	}

	# If no stops, return NULL:
	return(NULL)
}
  