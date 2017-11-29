test.fidelityDiscrete <- function(data, 
                                  lossMatrix,
                                  truthClassLabel = "truthClass",
                                  predClassLabel = "predictedClass",
                                  lossLabel = "loss"){
  # Needs to do the following...
  # rows = truth class
  # columns = predicted class
  # loss = (row,column)-th entry of lossMatrix
  
  # The original files outputs redundant results. There are only as many outcomes as there are entries
  # in the loss matrix, however this produces 2*nrow(loss)*ncol(loss) results. 
  
  # Sanity checks
  stopifnot(is.data.frame(data),
            all(c(truthClassLabel, predClassLabel) %in% colnames(data)),
            !(lossLabel %in% colnames(data)),
            is.numeric(lossMatrix),
            all(!is.na(lossMatrix)))
  
  # Extract key stuff from 'data' data frame, convert to character
  truthClass <- as.character(data[,truthClassLabel])
  predClass <- as.character(data[,predClassLabel])
  
  # Get class names from loss matrix
  truthClass.loss <- rownames(lossMatrix)
  predClass.loss <- colnames(lossMatrix)
  
  # Verify all elements of truthClass are in the rownames of the lossMatrix
  if (!all(unique(truthClass) %in% c(truthClass.loss, as.character(NA))))
    stop("There are elements in 'truthClass' that are not in the row names of 'lossMatrix'")
  
  # Verify all elements of predClass are in the colnames of the lossMatrix
  if (!all(unique(predClass) %in% c(predClass.loss, as.character(NA))))
    stop("There are elements in 'predClass' that are not in the column names of 'lossMatrix'")
  
  lvec <- c(NA)
  for(i in 1:nrow(l)){
    for(j in 1:length(predClass)){
      if(truthClass[i] == predClass[j]){
        lvec <- c(lvec, 0)
      } else {
        lvec <- c(lvec, lossMatrix[which(rownames(lossMatrix)==truthClass[i]), which(colnames(lossMatrix)==predClass[j])])
      }
    }
  }
  
  
  
}