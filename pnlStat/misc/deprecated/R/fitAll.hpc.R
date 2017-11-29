# Overall wrapper function for launching parallel hpc jobs to fit all subsets of a set of predictors
# of a multiple linear regression model.

# Landon Sego 18 Nov 2011

fitAll.hpc <- function(data, resp, predVars, account,
                       predList = NULL,
                       outFile = NULL,
                       numNodes = 10,
                       time.limit.mins = 30,
                       sort.results.by = "+ sigma + BIC", #"+ BIC - adj.r.squared - r.squared",
                       include.logical.pred = FALSE,
                       tarball.file = NULL,
                       mpi.compiler = NULL,
                       email.notification = NULL,
                       remove.working.dir = TRUE,
                       verbose = TRUE) {
  
  # data -- a data frame object or a text string of the filename of an .Rdata or
  #         .csv file that contains the data. Will be read in using dataIn()
  #
  # resp -- a character string indicating the column name of 'data' containing
  #         the response
  #
  # predVars -- a character vector giving the column names of 'data' that will be used
  #             as predictors
  #
  # account -- a character string indicating the pic account name (like "CSI", "SDI", "USERS")
  #
  # predList -- The object returned by comboList() or the .Rdata file containing this object which
  #             contains all the predictor variables combinations.
  #             If NULL, then comboList() is called within fitAll.hpc()
  #             to produce the list. If multiple calls to fitAll.hpc() will be made using the same
  #             number of predictors, then creating the combination list with comboList() initially and
  #             passing it via this argument is more efficient than using NULL.
  #
  # outFile -- A character string indicating the .Rdata filename where the final output of model fits
  #            will be saved.  If NULL, no file is saved.
  #
  # numNodes -- Integer indicating the number of nodes requested for parallel processing.
  #             Each node has 32 cores.
  #
  # time.limit.mins -- Integer indicating the the time limit (in minutes) of the SLURM batch job.
  #
  # sort.results.by -- Text string giving the ordering of the sorting of the output data set.
  #                    Variables names in the 'formula' should match those in the output data set,
  #                    and '-' indicates descending sorting and '+' ascending sorting.  This is passed
  #                    as right hand side of the 'formula' in sort.data.frame().
  #
  # include.log.pred -- Logical requesting that logical indicator columns of the predictors be included
  #                     in the output data
  #
  # tarball.file -- A text string indicating the file (ending in .tar.gz) where all the files used to
  #                 create and support the SLURM job will be stored.  If NULL, a file of the form
  #                 'fitAll.hpc_output_####.tar.gz' will be stored in the working directory.
  #                 If "none", no tarball will be created.
  #
  # remove.working.dir -- logical indicating if the working directory of the SLURM job files should
  #                       be delted on successful completion.  If the SLURM job fails, this directory
  #                       will not be removed.
  #
  # mpi.compiler -- text string indicating the mpi compiler
  #
  # email.notification -- test string containing an email address to which an email will be sent upon
  #                       completion of the SLURM job.  If NULL, no email will be sent.
  #
  # verbose -- logical indicating whether details (and timing) of the job should be printed.

  ################################################################################
  # Check input arguments
  ################################################################################

  # Get the start time
  if (verbose)
    s.time <- Sys.time()
  
  # Verify we're on olympus
  if (Sys.getenv("HOSTNAME") != "olympus.local")
    stop("This function was written for use on olympus only")

  # Verify these arguments are character
  for (v in c("resp", "predVars", "account", "sort.results.by")) {
    if (!is.character(get(v)))
      stop("'", v, "' should be a character string (or vector)")
  }

  # Verify these arguments are character (if they're not NULL)
  for (v in c("predList", "outFile", "mpi.compiler", "tarball.file", "email.notification")) {
    cv <- get(v)
    if (!is.null(cv)) {
      if (!is.character(cv))
        stop("'", v, "' should be a character string (or vector) or NULL")
    }
  }

  # Verify these are postive integers
  for (v in c("numNodes", "time.limit.mins")) {
    v.bad <- TRUE
    cv <- get(v)
    if (is.numeric(cv)) {
      if ((cv > 0) & (cv %% 1 == 0))
        v.bad <- FALSE
    }
    if (v.bad)
      stop("'", v, "' must be a postive integer")
  }

  # Verify these are logicals
  for (v in c("remove.working.dir", "include.logical.pred", "verbose")) {
    if (!is.logical(get(v)))
      stop("'", v, "' should be TRUE or FALSE\n")
  }  

  # Print warnings as they occur
  op <- options(warn = 1)

  ################################################################################
  # Implement checks and create working directories & files
  ################################################################################
  
  # Create the working directory
  userScratch <- paste("/pic/scratch/", system("whoami", intern = TRUE), sep = "")
  if (!file.exists(userScratch)) 
    system(paste("mkdir", userScratch))

  # Create temporary working directories
  tmpNum <- system("echo $$", intern = TRUE)
  wkdir <- paste(userScratch, "/fitAll_tmp_", tmpNum, sep = "")
  wkdir.logs <- paste(wkdir, "logs", sep = "/")
  wkdir.out <- paste(wkdir, "out", sep = "/")
  system(paste("mkdir ", wkdir, "; ",
               "mkdir ", wkdir.logs, "; ",
               "mkdir ", wkdir.out, sep=""))

  # Create temporary working directories and files
  data.file <- paste(wkdir, "data_to_fit.Rdata", sep = "/")
  predList.file <- paste(wkdir, "predictor_combinations.Rdata", sep = "/")
  rscript.file <- paste(wkdir, "doFits.R", sep = "/")
  sb <- paste(wkdir, "launch.sh", sep = "/")
  sl.out <- paste(wkdir, "slurmLaunch.out", sep = "/")
  sl.err <- paste(wkdir, "slurmLaunch.err", sep = "/")
  sb.err <- paste(wkdir, "sbatch.err", sep = "/")

  # If a tarball.file was not specified
  if (is.null(tarball.file)) 
    tarball.file <- paste(getwd(), "/fitAll_output_", tmpNum, ".tar.gz", sep = "")
 
  # If a tarball.file did not have a path (in which case, it should have the path
  # of the working directory
  else if ((tarball.file != "none") & (stripPath(tarball.file) == tarball.file)) 
    tarball.file <- paste(getwd(), tarball.file, sep = "/")
  
  if (verbose) {
    cat("\nCreated working directories and filenames:\n\n")
  
    # Display the various paths and filenames
    pvar(wkdir)
    pvar(wkdir.logs)
    pvar(wkdir.out)
    pvar(data.file)
    pvar(predList.file)
    pvar(rscript.file)
    pvar(sl.out)
    pvar(sl.err)
    pvar(sb.err)
    pvar(tarball.file)
        
  }

  # Identify how many models need to be fit
  nModels <- sum(choose(length(predVars), 1:length(predVars)))

  # If number of processes exceeds the number of models, return an error
  if (nModels < 32 * numNodes)
    stop("Number of models to be fit, ", nModels, " is less than the number of requested cores: ",
         32 * numNodes, "\n")


  ################################################################################
  # Get the data in place in the working directory
  ################################################################################

  # Read in and save the data
  df <- dataIn(data)
  save(df, file = data.file)

  # Check that resp and pred.vars are in the colnames of data
  if (!all(c(resp, predVars) %in% colnames(df)))
    stop("'resp' and 'predVars' must be in the column names of 'data'")
 
  # If predList is NULL, then create it
  if (is.null(predList)) {

    if (verbose)
      cat("\nCreating predictor combination list\n")
    
    time.elapsed <- timeIt(comboList(length(predVars), outFile = predList.file,
                                     njobs = ifelse(length(predVars) < 20, 1, 5),
                                     verbose = verbose),
                           verbose = FALSE, return.time = TRUE)

    if (verbose)
      cat("Predictor combinations completed in", time.elapsed$elapsed, time.elapsed$units, "\n")
    
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

    # Save results to the working diretory
    save(pv, file = predList.file)
  }

  if (verbose)
    cat("\nData and predictor combinations saved to '", wkdir, "'\n", sep = "")

  ################################################################################
  # Write the R script that will be launched  
  ################################################################################

  cat('require(pnlStat)\n',
      'pnlStat:::fitSubs("', data.file, '",\n',
      '                  "', predList.file, '",\n',
      '                  "', resp, '",\n',
      '                  ', paste('c("', paste(predVars, collapse = '", "'), '")', sep=""), ',\n',
      '                  "', wkdir.out, '",\n',
      '                  ', 32 * numNodes, ')\n', 
      'warnings()\n',
      sep = "",
      file = rscript.file)

  if (verbose)
    cat("\nR script file '", rscript.file, "' written\n", sep = "")
  
  
  ################################################################################  
  # Write and compiler the launch.c file with MPI
  ################################################################################

  compileMPI(wkdir, rscript.file, mpi.compiler = mpi.compiler, verbose = verbose)
  
  ################################################################################
  # Write and launch the sbatch script
  ################################################################################

  email.notice <- ifelse(is.null(email.notification), "\n",
                         paste('\n',
                               '#SBATCH --mail-type=END\n',
                               '#SBATCH --mail-user=', email.notification, '\n\n', sep=""))

  # Write the launch.sh script
  cat('#!/bin/csh\n',
      '\n',
      'date "+DateTime %Y-%m-%d %T"\n',
      '#SBATCH -A ', account, '\n',
      '#SBATCH -t ', time.limit.mins, '\n',
      '#SBATCH -N ', numNodes, '\n',
      '#SBATCH -n ', 32 * numNodes, '\n',  
      '#SBATCH -o ', sl.out, '\n',
      '#SBATCH -e ', sl.err, '\n',
      email.notice,
      'source /etc/profile.d/modules.csh\n',
      'module purge\n',
      'module load pnnl_env\n',
      'unlimit\n',
      '\n',
      'echo\n',
      'echo "loaded modules"\n',
      'echo\n',
      'module list >& _modules.lis_\n',
      'cat _modules.lis_\n',
      '/bin/rm -f _modules.lis_\n',
      'echo\n',
      'echo "Limits"\n',
      'echo\n',
      'limit\n',
      'echo\n',
      'echo "Environmental Variables"\n',
      'echo\n',
      'printenv\n',
      'echo\n',
      'echo "ldd output"\n',
      'echo\n',
      'ldd ', wkdir, '/launch.o\n',
      '\n',
      'echo "Launch the R jobs"\n',
      'srun --mpi=none ', wkdir, '/launch.o\n',
      'echo "srun is complete"\n',
      '\n',
      'echo "Scan the Rout files for errors and warnings"\n',
      'echo "Errors"\n',
      'echo\n',
      'grep "Error" ', wkdir.logs, '/*.Rout\n',
      'echo\n',
      'echo "Warnings"\n',
      'echo\n',
      'grep "Warning" ', wkdir.logs, '/*.Rout\n',
      'echo\n',
      'echo "Total number of completed .Rout files:"\n',
      'grep "proc.time()" ', wkdir.logs, '/*.Rout | wc -l\n',
      '\n',
      'echo\n',
      'echo "Concatentate the results"\n',
      'echo "model,sigma,RMSE,adj.r.squared,r.squared,BIC" > ', wkdir, '/header.txt\n',
      'cat ', wkdir, '/header.txt ', wkdir.out, '/*.csv > ', wkdir, '/results.csv\n',
      '/bin/rm -f ', wkdir, '/header.txt\n',
      'date "+DateTime %Y-%m-%d %T"\n',      
      'echo "Job completion"\n',
      sep = "",
      file = sb)

  # Launch the parallel jobs, redirect stderr to a file
  sb.stdout <- system(paste("sbatch ", sb, " 2>", sb.err, sep = ""), intern = TRUE)

  # Count the number of lines in the stderr of the sbatch command
  n.sb.err <- as.numeric(system(paste("wc -l", sb.err, "| awk '{print $1}'"), intern = TRUE))

  # Check for errors in the sbatch launch
  if (!length(sb.stdout) & n.sb.err) {
    cat("SLURM job failed to launch.  Printing '", sb.err, "':\n", sep = "")
    system(paste("cat", sb.err))
    stop()
  }

  ################################################################################
  # Wait for the sbatch script to complete
  ################################################################################
  
  if (verbose)
    cat("\n", sb.stdout, ". Launching ", 32 * numNodes, " jobs.\n",
        "You can check the status of the SLURM job by typing 'squeue' at the system prompt.\n",
        "Waiting for SLURM job to complete...\n",        
        sep = "")
 
  # Initialize variables for the waiting loop
  continueWaiting <- TRUE
  elapsed.min <- 0
  start.time <- NULL
  timeOut <- FALSE

  # Stop if an error happens, if the job completes successfully, or if the elapsed time expires
  while (continueWaiting & !timeOut) {

    # Wait for 10 seconds in each iteration of this loop
    Sys.sleep(10)
    
    # If any errors occur in the R out files
    if (length(dir(wkdir.logs, pattern = ".Rout"))) {

      n.Rerr <- as.numeric(system(paste("grep -e 'Error' -e 'Execution halted' ",
                                        wkdir.logs, "/*.Rout | wc -l", sep = ""), intern = TRUE))

      # If there are R errors, cancel the slurm batch job
      if (n.Rerr) {

        # Get the slurm job id 
        s.jobid <- system(paste("grep SLURM_JOBID", sl.out), intern = TRUE)
        s.jobid <- as.numeric(strsplit(s.jobid, "=")[[1]][[2]])

        # Kill the SLURM job
        killSlurm(s.jobid)

        # Stop the function
        stop("R errors detected in '", wkdir.logs, "/*.Rout'")
        
      }
    }

    # If the slurmLaunch.err file exists, check to see if it has lines.
    if (file.exists(sl.err)) {
      # Get the number of lines in the file
      nlines <- as.numeric(system(paste("wc -l", sl.err, "| awk '{print $1}'"), intern = TRUE))
      if (nlines)
        stop("SLURM job failed, see '", sl.err, "'")
    }

    # Check the slurm output file, look for the 'Job completion' string
    if (file.exists(sl.out)) {

      # If the last line says 'Job completion', then stop waiting
      last.line <- system(paste("tail -1", sl.out), intern = TRUE)
      continueWaiting <- !(last.line == "Job completion")

      if (is.null(start.time))
        start.time <- Sys.time()
        
      elapsed.min <- as.numeric(difftime(Sys.time(), start.time, units="m"))
#      pvar(start.time, Sys.time(), elapsed.min)

    }

    if (elapsed.min > time.limit.mins + 90) {
      timeOut <- TRUE
      warning("fitAll.hpc() timed out, waiting ", round(elapsed.min, 2),
              " minutes for the SLURM job to launch (or complete).\n")
    }

  } # while waiting


  ################################################################################
  # Check the R log files for errors or incompletion, get completion times
  ################################################################################

  # Calculate the elapsed time for the slurm job
  if (verbose) {
    sts <- as.POSIXct(system(paste("grep DateTime", sl.out, "| awk '{print $2 \" \" $3}'"), intern = TRUE))
    slurm.et <- round(as.numeric(difftime(sts[2], sts[1], units = "m")), 2)
    cat("\nSLURM job completed in", slurm.et, "minutes\n\nChecking the log files of the R jobs\n")
  }

  # Look for errors and warnings in the log files
  n.err <- as.numeric(system(paste("grep Error ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                             intern = TRUE))
  n.halt <- as.numeric(system(paste("grep 'Execultion halted' ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                              intern = TRUE))
  n.war <- as.numeric(system(paste("grep Warning ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                             intern = TRUE))
  n.comp <- as.numeric(system(paste("grep 'proc.time()' ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                              intern = TRUE))

  if (n.err) 
    warning("Errors were identified in ", n.err, " of the R log files in '", wkdir.logs, "'\n")

  if (n.halt)
    warning("Execution was halted for ", n.halt, " of the R log files in '", wkdir.logs, "'\n")

  if (n.war) 
    warning("Warnings were identified in ", n.war, " of the R log files in '", wkdir.logs, "'\n")

  # The number of proc.time()'s in the .Rout files should match the number of processes
  if (n.comp != 32 * numNodes)
    warning("The number of completed .Rout files, ", n.comp,
            " does not match the number of processes, ", 32 * numNodes, ".\n")

  # Get sense of the average completion time in minutes for the individual R jobs
  if (verbose) {
    Rout.completion.time <- round(mean(as.numeric(system(paste("grep 'proc.time()' -A3 ", wkdir.logs,
                                                               "/*.Rout | sed -n '3~4p' | awk '{print $4}'",
                                                               sep = ""), intern = TRUE))) / 60, 3)
    cat("\nThe individual R jobs completed on average in", Rout.completion.time, "minutes\n")
  }

  
  ################################################################################
  # Read in the output data, add logical indicators of predictors, and sort
  # by 'sort.results.by'
  ################################################################################

  out <- read.csv(paste(wkdir, "/results.csv", sep = ""), stringsAsFactors = FALSE)

  # Sort data if necessary
  if (!is.null(sort.results.by)) {
    
    if (verbose)
      cat("\nSorting the fitted models...")
    
    tmp <- timeIt(eval(parse(text = paste("sort.data.frame(out, ~",
                                          sort.results.by, ")", sep = ""))),
                  verbose = FALSE, return.time = TRUE)

    out <- tmp$out
    rownames(out) <- 1:NROW(out)
    
    if (verbose)
      cat("completed in ", tmp$elapsed, " ", tmp$units, ".\n", sep = "")
  }

  # Add in the logical indicators of the predictors
  if (include.logical.pred) {
    
    get.logical <- function(formula.text) {
      predVars %in% unlist(strsplit(grabLast(formula.text, " ~ "), " + ", fixed = TRUE))
    }
    
    log.preds <- list2df(lapply(as.list(out[,"model"]), get.logical), convert.numeric = FALSE,
                         col.names = predVars)

    out <- cbind(out[,"model"], log.preds, out[,-which(colnames(out) == "model")])

  }
  
  ################################################################################
  # Create tarball of the SLURM job files and remove the directory
  ################################################################################

  if (tarball.file != "none") {
    system(paste("cd ", userScratch, "; tar -Ppczf ",
                 tarball.file, " ", stripPath(wkdir), sep = ""))
    if (verbose)
      cat("\n'", tarball.file, "' created\n", sep = "")
  }
  
  if (remove.working.dir) {
    system(paste("rm -r", wkdir))
    if (verbose)
      cat("\n'", wkdir, "' removed\n", sep = "")
  }


  ################################################################################
  # Return/save results
  ################################################################################  

  if (!is.null(outFile)) {
    save(out, file = outFile)
    if (verbose)
      cat("\nResults saved to '", outFile, "'\n", sep = "")
  }
  
  # Restore default options
  options(op)

  if (verbose) {
    cat("\nTotal elapsed time:",
        round(as.numeric(difftime(Sys.time(), s.time, unit = "m")), 2),
        "minutes\n")
  }
  
  # invisibly return the results
  invisible(out)

} # fitAll.hpc
