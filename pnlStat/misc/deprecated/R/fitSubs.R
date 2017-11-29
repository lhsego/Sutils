# Fit the linear model to a subset of all the possible models.  The subset is determined
# by the process id.

# Landon Sego  2011-11-18

fitSubs <- function(data.file, predList.file, resp, pred.vars, outDir, num.processes) {

  # data.file = text string indicating the .Rdata file containing the dataframe with the data.
  #        The column names of the data should include the resp and pred.vars
  # predList.file - text string indicating the .Rdata file produced by comboList()
  # resp  - character string indicating the name of the response
  # pred.vars - character string indicating the names of the predictors 
  # outDir = path indicating the location where final output should be stored
  # num.processes - the total number of processes

  # This function is called in the rscript file that is written by fitAll(), and thus is
  # executed in parallel.  It returns nothing, but writes the output of the fitted models
  # to the a csv file. 

  # Identify the process id passed in from the system call
  process.id <- as.numeric(commandArgs(TRUE))

  # Print the proces id number and the number of processes to the .Rout file
  pvar(process.id, num.processes)

  # Load the list of predictors
  pList <- load.object(predList.file)

  # Bring in the data that will be used to fit the models  
  models.to.fit <- parseJob(pList[["len"]], num.processes, random.seed = 726)[[process.id + 1]]
 
  # Bring in the data
  data <- load.object(data.file)

  # Function to fit the linear models and extract the fit statistics
  fitLM <- function(elem.i) {
    
    # Create model fitting text
    formula.text <- paste(resp, "~", paste(pred.vars[elem.i], collapse = " + "))

    # Fit the model
    m.fit <- eval(parse(text = paste("lm(", formula.text, ", data = data)", sep="")))

    # Extract the fit statistics, add in the formula text
    out <- summary(m.fit)[c("sigma", "r.squared", "adj.r.squared")]
    out$BIC <- BIC(m.fit)
    out$RMSE <- sqrt(mean(resid(m.fit)^2))
    out$model <- formula.text

    # Return the list
    return(out)
    
  } # fitLM

  # Show the list to be fit
#  pvar(models.to.fit)
#  print(pList[["pList"]][models.to.fit])
  
  # Use lapply to fit the linear models over the subsets selected for this process
  fits <- list2df(lapply(pList[["pList"]][models.to.fit], fitLM))

  # Reorder the columns
  fits <- fits[,c("model", "sigma", "RMSE", "adj.r.squared", "r.squared", "BIC")]

  # Now save the file--these will be concatentated into a single file
  fout <- paste(outDir, "/", stripExtension(stripPath(data.file)), "_completed_",
                padZero(process.id, nchar(num.processes)), ".csv", sep="")
  
  write.table(fits, file = fout, sep = ",", row.names = FALSE, col.names = FALSE)
  
  cat("File saved to '", fout, "'\n\n", sep="")

} # fitSubs

## Example call
## timeIt(fitSubs("/pic/scratch/d3p423/BMA/data_with_15_predictors_5000_obs.Rdata",
##                "/pic/scratch/d3p423/BMA/combinations_for_15_predictors.Rdata",
##                "y",
##                paste("x", 1:15, sep=""),
##                "/pic/scratch/d3p423/BMA/out"))
