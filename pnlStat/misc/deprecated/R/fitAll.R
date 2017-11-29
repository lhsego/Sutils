##################################################################################
# fitAll() uses comboList and fits all of the models in parallel (or in serial, if njobs=1)
# It returns a data frame with the BIC, R^2 and adj-R^2 for each model
##################################################################################

# Landon Sego, 2011-11-21

# Just need to finish checking the predList and arguments that follow it.  Also compare the output of fitAll and fitAll.hpc

fitAll <- function(data, resp, predVars,
                   predList = NULL,
                   outFile = NULL,
                   njobs = 5,
                   sort.results.by = "+ sigma + BIC", #"+ BIC - adj.r.squared - r.squared",
                   include.logical.pred = FALSE,
                   check.num.models = FALSE,
                   verbose = FALSE) {

  # resp  - character string indicating the name of the response
  # predVars - character string indicating the names of the predictors -- for now, these
  #             predictors are assumed to be continuous
  # data - data frame.  The column names of the data should include the resp and predVars
  # njobs - the number of jobs to run in parallel while fitting the data
  # combos - provide the combos if they're already present.
  # include.log.pred -- Logical requesting that logical indicator columns of the predictors be included
  #                     in the output data
  # check.num.models = TRUE counts the number of models that will be fit, gives a message, and
  #                    then returns NULL

  # Get the start time
  if (verbose)
    s.time <- Sys.time()

  # Check the number of models only
  if (check.num.models) {
    num.models <- sum(choose(length(predVars), 1:length(predVars)))
    cat(num.models, "model subsets will be fit.\n")
    return(NULL)
  }

  # Get the data
  data <- dataIn(data)

  # Check that resp and predVars are in the colnames of data
  if (!all(c(resp, predVars) %in% colnames(data)))
    stop("'resp' and 'predVars' must be in the column names of 'data'")

  # If predList is NULL, then create it
  if (is.null(predList)) {

    if (verbose)
      cat("\nCreating predictor combination list...")
    
    time.elapsed <- timeIt(comboList(length(predVars),
                                     njobs = ifelse(length(predVars) < 20, 1, 5),
                                     verbose = verbose),
                           verbose = FALSE, return.time = TRUE)

    pv <- time.elapsed$out

    if (verbose)
      cat("completed in", time.elapsed$elapsed, time.elapsed$units, "\n")
    
  }
       
  # Othwerwise, read it in and save it to the working directory     
  else {

    # If it's a .Rdata file name, load the data
    if (is.character(predList)) {

      if (tolower(getExtension(predList)) != "rdata")
        stop("Filename for 'predList' must be an Rdata file")

      pv <- load.object(predList)

    }
    # Otherwise, it should be the list itself
    else if (is.list(predList))
      pv <- predList
    else
      stop("'predList' did not follow the specified format")

    # Check the output is of class 'combolist'
    if (class(pv) != "combolist")
      stop("'predList' does not refer to an object of class 'combolist'")

  }


  # Function for fitting the linear models, to be called by plapply
  fitLM <- function(elem.i) {
    
    # Create model fitting text
    formula.text <- paste(resp, "~", paste(predVars[elem.i], collapse=" + "))

    # Fit the model
    m.fit <- eval(parse(text = paste("lm(", formula.text, ", data = data)", sep="")))

    # Extract the fit statistics, add in the formula text
    out <- summary(m.fit)[c("sigma", "r.squared", "adj.r.squared")]
    out$BIC <- BIC(m.fit)
    out$RMSE <- sqrt(mean(resid(m.fit)^2))
    out$model <- formula.text
    
    # Rearrange the list
    return(out[c("model", "sigma", "RMSE", "adj.r.squared", "r.squared", "BIC")])
    
  } # fitLM

  if (verbose)
    cat("\nFitting all subsets...")

  # Now call this function in parallel over the data
  res <- timeIt(list2df(plapply(pv$pList, fitLM, needed.objects = c("data", "resp", "predVars"),
                                jobName = "fitAll", njobs = njobs, check.interval.sec = 2, random.seed = 7)),
                verbose = FALSE, return.time = TRUE)

  out <- res$out

  if (verbose)
    cat("completed in", res$elapsed, res$units, "\n")

  # Sort the output if requested
  if (!is.null(sort.results.by)) {
    
    out <- eval(parse(text = paste("sort.data.frame(out, ~", sort.results.by, ")", sep = "")))
    rownames(out) <- 1:NROW(out)

  }

  # Add in the logical indicators of the predictors
  if (include.logical.pred) {
    
    get.logical <- function(formula.text) {
      predVars %in% unlist(strsplit(grabLast(formula.text, " ~ "), " + ", fixed = TRUE))
    }
    
    log.preds <- list2df(lapply(as.list(out[,"model"]), get.logical), convert.numeric = FALSE,
                         col.names = predVars)

    out <- cbind(out[,"model"], log.preds, out[, -which(colnames(out) == "model")])

  }
  
  # Save out file if requested
  if (!is.null(outFile)) {
    save(out, file = outFile)
    if (verbose)
      cat("\nResults saved to '", outFile, "'\n", sep = "")
  }

  # Print elapsed time
  if (verbose) {
    cat("\nTotal elapsed time:",
        round(as.numeric(difftime(Sys.time(), s.time, unit = "m")), 2),
        "minutes\n\n")
  }
  
  # invisibly return the results
  invisible(out)

} # fitAll
