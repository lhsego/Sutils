# recreating the bootstrap 

# At the heart of the bootstrap routine is the following
# 1) Resampling with replacement of the data input
# 2) Computing the desired "score" of the data frame with the bootstrap replicate
# 3) Sort the bootstrap replicates by signatureID
# 4) Make all the ladies swoon
# 5) summaritheFun = list of functions to apply to the bootstrap reppies. For example, 
# if the max is requested, then return the max scores from the set of replicates

# test.bootStrap <- function(data, 
#                            score_funs,
#                            seed = NULL,
#                            n = 1,
#                            nJobs = 1,
#                            summarizeFun = NULL){
  
  require(SQM)
  
  data(exampleScoreData); esd <- exampleScoreData; rm(exampleScoreData);
  
  score_funs <- c("logScore", "brierScore")
  summary_funs <- c("mean", "sd")
  
  # the number of truth classes 
  numclass <- length(unique(esd$truthClass))
  
  # the number of predicted classes
  class.column.order <- gsub("predictionClass", "", 
                             colnames(esd)[grep("predictionClass", colnames(esd))])
  
  # the names of the different signatures
  sig.names <- levels(esd$signatureID)
  
  # The function for identifying the indexes of the bootstrap sample
  
  selInd <- function() {
    SQM:::genBootstrapIndexes(esd$signatureID)
  }
  
  
  # if the score_funs isn't already in list format
  score_funs <- lapply(score_funs, function(x = score_funs) get(x))
  summary_funs <- lapply(summary_funs, function(x = summary_funs) get(x)) 
  
  # At this point, run a bootstrap on the data set, apply the log score function, and reuturn in
  # the ordered format
  
  tsc_fun <- get(unlist(score_funs))
  
  test <- lapply(1:length(tsc_fun), function(x) tsc_fun[x](esd[(selInd()),]))
  
  
  
  
# }