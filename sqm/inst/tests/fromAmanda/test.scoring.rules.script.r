## Test scoring methods
## Amanda M. White (9/6/2012)

library(SQM)
options(error=dump.frames)

load("/home/d3l348/SQM/Data/testData_all_15_combos.Rdata")

signatures <- out
colnames(signatures) <- c("signatureID", "dataID", "truthClass", 
    paste("predictionClass", 1:11, sep=""))
head(signatures)

#################################################################
## Test data sets

## Uniform prediction test set
unifSignatures <- data.frame(signatureID=rep(1, nrow=100), 
    truthClass=rep(1:10, times=10), 
    matrix(0.1, nrow=100, ncol=10))
colnames(unifSignatures) <- c("signatureID", "truthClass", 
    paste("predictionClass", 1:10, sep=""))

## Perfect prediction test set
perfectSignatures <- data.frame(signatureID=rep(1, nrow=100), 
    truthClass=rep(1:10, times=10), 
    matrix(0, nrow=100, ncol=10))
colnames(perfectSignatures) <- c("signatureID", "truthClass", 
    paste("predictionClass", 1:10, sep=""))
for (i in 1:10) {
    perfectSignatures[perfectSignatures$truthClass == i, 
        paste("predictionClass", i, sep="")] <- 1
}

## Worst prediction test set (probability 1 on wrong answer)
worstSignatures <- data.frame(signatureID=rep(1, nrow=100), 
    truthClass=rep(1:10, times=10), 
    matrix(0, nrow=100, ncol=10))
colnames(worstSignatures) <- c("signatureID", "truthClass", 
    paste("predictionClass", 1:10, sep=""))
for (i in 1:9) {
    worstSignatures[worstSignatures$truthClass == i, 
        paste("predictionClass", i+1, sep="")] <- 1
}
worstSignatures[worstSignatures$truthClass == 10, 
    paste("predictionClass", 1, sep="")] <- 1
    
## Classes with names rather than numbers
nameSignatures <- signatures
colnames(nameSignatures)[4:14] <- paste("predictionClass", LETTERS[1:11], sep="")
nameSignatures$truthClass <- LETTERS[nameSignatures$truthClass]

## Signatures data.frame missing a prediction column
missingSignatures <- signatures[,1:(ncol(signatures)-1)]

#################################################################
## LOG SCORE

logScore(signatures)
logScore(signatures, scale=TRUE)

##Test to see if entirely uniform predictions gives rescaled score of 0:
logScore(unifSignatures)
logScore(unifSignatures, scale=TRUE)

##Test to see if perfect predictions gives rescaled score of 1:
logScore(perfectSignatures)
logScore(perfectSignatures, scale=TRUE)

##Test to see if worst predictions gives rescaled score of < 0:
logScore(worstSignatures)
logScore(worstSignatures, scale=TRUE)

##Make sure logScore works with named classes
logScore(nameSignatures, scale=TRUE)

##Signatures with missing column: this should fail
logScore(missingSignatures, scale=FALSE)

#################################################################
## BRIER SCORE

brierScore(signatures)
brierScore(signatures, scale=TRUE)

## rescaled Brier score for uniform predictions should be 0
brierScore(unifSignatures)
brierScore(unifSignatures, scale=TRUE)

## rescaled Brier score for perfect predictions should be 1
brierScore(perfectSignatures)
brierScore(perfectSignatures, scale=TRUE)

## rescaled Brier score for worst predictions should be < 0
brierScore(worstSignatures)
brierScore(worstSignatures, scale=TRUE)

##Make sure brier score works with named classes
brierScore(nameSignatures, scale=TRUE)

##Signatures with missing column: this should fail
brierScore(missingSignatures, scale=FALSE)

#################################################################
## TEST WRAPPER FUNCTIONS

calcScore(signatures, methods=c("brier", "log"))

calcScore(signatures, methods="brier", scale=TRUE)

calcScore(signatures, methods="logr", scale=FALSE) ## should fail

calcScore(signatures, methods=c("log", "brier"), scale=TRUE)


#################################################################
## TEST ANALYTICAL FRAMEWORK FUNCTIONS

output.dir <- "/home/d3l348/temp/"
write.table(signatures, file=file.path(output.dir, "sqm_sig_example.csv"), 
    sep=",", col.names=TRUE, quote=FALSE, row.names=FALSE)

tmp <- readScore(file.path(output.dir, "sqm_sig_example.csv"))
head(tmp)

afScore(file.path(output.dir, "sqm_sig_example.csv"), 
  outputFilePrefix=paste(output.dir, "sqm_score_example1", sep=""))

afScore(file.path(output.dir, "sqm_sig_example.csv"), outputFilePrefix=NULL) #returns data.frame

afScore(file.path(output.dir, "sqm_sig_example.csv"), digits=2, 
  outputFilePrefix=paste(output.dir, "sqm_score_example2", sep=""))

