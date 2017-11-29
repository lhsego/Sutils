test.brierScore <- function(data, scale = TRUE){
  
  # data = the data frame of sigID, truthclass, and prediction class

  numclass <- length(unique(data$truthClass))
  
  class.column.order <- gsub("predictionClass", "", 
                        colnames(data)[grep("predictionClass", colnames(data))])
  
  sig.names <- levels(data$signatureID)
  
  sep_by_sig <- lapply(1:length(sig.names),function(x){
    ind <- which(data$signatureID == sig.names[x])
    data[ind,]
  })
  
  score.list <- lapply(1:length(sep_by_sig), function(x){
    
  truth <- delta_ij(sep_by_sig[[x]]$truthClass);
  
  pred <- sep_by_sig[[x]][,grep("pred",colnames(data))]
  
  bs <- sum((truth - pred)^2)/nrow(sep_by_sig[[x]])
  
  sharp <- -sum(pred*(1-pred))/nrow(sep_by_sig[[x]])
  
  list("brierScore" = bs, "sharpness" = sharp)
  
  })
  
  scores <- unlist(lapply(score.list, function(x = score.list){x$brierScore}))
  sharpness <- unlist(lapply(score.list, function(x = score.list){x$sharpness}))
  
  if (scale) {
    nclasses <- numclass;
    scores <- 1 + nclasses/(1-nclasses)*scores
    sharpness <- 1 + nclasses/(nclasses-1)*sharpness
  }
  
  return(data.frame("signatureID" = sig.names, "brierScore" = scores, "sharpness" = sharpness))
  
}

# Test to see the function works as intended

check.brierScore <- function(){
  
  suppressPackageStartupMessages(require(SQM))
  data(exampleScoreData)
  esd <- exampleScoreData
  
  if(!identical(test.logScore(esd),logScore(esd)))
    stop("The output of test.brierScore and brierScore is not exactly equal for the given input")
  else
    return(NULL)
}

check.brierScore()
   