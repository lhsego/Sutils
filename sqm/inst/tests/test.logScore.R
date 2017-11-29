test.logScore <- function(data, scale = TRUE){
  
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
    
    pred <- sep_by_sig[[x]][,grep("pred",colnames(data))]
    
    lscore <- sum(by(sep_by_sig[[x]], sep_by_sig[[x]]$truthClass, 
                    function(sum_over_pred_class) {
                    sum(log(sum_over_pred_class[,
                        paste("predictionClass", sum_over_pred_class[1, "truthClass"], sep="")]))
                    }))/nrow(sep_by_sig[[x]])
    
    i2 <- unlist(pred)
    
    info <- sum(ifelse(i2 <= 0, 0, i2*log(i2)))/nrow(pred)
    
    list("logScore" = lscore, "information" = info)
    
  })
  
  scores <- unlist(lapply(score.list, function(x = score.list){x$logScore}))
  information <- unlist(lapply(score.list, function(x = score.list){x$information}))
  
  if (scale) {
    scores <- 1+1/log(numclass)*scores
    information <- 1+1/log(numclass)*information
  }
  
  return(data.frame("signatureID" = sig.names, "logScore" = scores, "information" = information))
  
}

check.logScore <- function(){
  
suppressPackageStartupMessages(require(SQM))
data(exampleScoreData)
esd <- exampleScoreData

if(!identical(test.logScore(esd),logScore(esd)))
  stop("The output of test.logScore and logScore is not exactly equal for the given input")
else
  return(NULL)
}

check.logScore()