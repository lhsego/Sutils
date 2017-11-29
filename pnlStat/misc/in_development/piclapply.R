
# TODO:  If a job is waiting in the queue, but then a user cancels it with scancel,
# The waiting script below doesn't detect that situation because no slurmLaunch.err file is produced
# and no slurmLaunch.out is produced either--so it would have to just time out.    Perhaps add
# a condition such that if the job vanishes from the 'squeue | grep user' result with not .out or .err file,
# then cancel the job?

# TODO:  If the sbatch call produces an error, don't stop piclappy--just report the error and see what happens?

# HPC lapply, created for PNNL institutional computing cluster olympus.pnl.gov

# Landon Sego

piclapply <- function(X, FUN, account, ...,
                      packages = NULL,
                      header.file = NULL,
                      needed.objects = NULL,
                      needed.objects.env = parent.frame(),
                      jobName = "piclapply",
                      numNodes = 2,
                      time.limit.mins = 30,
                      collate = FALSE,
                      random.seed = NULL,
                      check.interval.sec = 30,
                      lenX = length(X),
                      tarball.file = NULL,
                      remove.working.dir = TRUE,                      
#                      mpi.compiler = NULL,
                      email.notification = NULL,                      
                      verbose = FALSE) {


  ################################################################################
  # Check input arguments
  ################################################################################

  # Verify these are logicals
  for (v in c("collate", "remove.working.dir", "verbose")) {
    if (!is.logical(get(v)))
      stop("'", v, "' should be TRUE or FALSE\n")
  }  

  # Get the start time
  if (verbose)
    s.time <- Sys.time()
  
  # Verify we're on olympus or one of its nodes
  if (!(gsub("[0-9]", "", Sys.getenv("HOSTNAME")) %in%
        c("olympus.local", "olympus-e.local", "node.local", "gpu.local")))
    stop("'piclapply' was written for use on olympus or its computing nodes only")

  # Get the start time
  if (verbose)
    s.time <- Sys.time()
  
  # Verify these arguments are character
  for (v in c("account", "jobName")) {
    if (!is.character(get(v)))
      stop("'", v, "' should be a character string (or vector)")
  }

  # Verify these arguments are character (if they're not NULL)
#  for (v in c("packages", "header.file", "needed.objects", "mpi.compiler", "tarball.file", "email.notification")) {
  for (v in c("packages", "header.file", "needed.objects", "tarball.file", "email.notification")) {
    cv <- get(v)
    if (!is.null(cv)) {
      if (!is.character(cv))
        stop("'", v, "' should be a character string (or vector) or NULL")
    }
  }

  # Verify these are postive integers
  numeric.arguments <- c("numNodes", "time.limit.mins", "check.interval.sec")
  if (!is.null(random.seed))
    numeric.arguments <- c(numeric.arguments, "random.seed")
                        
  for (v in numeric.arguments) {
    v.bad <- TRUE
    cv <- get(v)
    if (is.numeric(cv)) {
      if ((cv > 0) & (cv %% 1 == 0))
        v.bad <- FALSE
    }
    if (v.bad)
      stop("'", v, "' must be a positive integer")
  }

  # Print warnings as they occur
  op <- options(warn = 1)  

  # Assign the number of processes
  numProcesses <- 32 * numNodes

  # If number of processes exceeds the number elements in the list, then reduce it
  # appropriately
  if (lenX < numProcesses) {
    numNodes <- max(1, lenX %/% 32)
    numProcesses <- ifelse(lenX < 32, lenX, numNodes * 32)
    warning("Job size has been reduced to accomodate the size of the ",
            "list and utilize each\nnode at 100%, if possible:  ",
            pvar(length(X), numNodes, numProcesses, verbose = FALSE))
  }
  
  ################################################################################
  # Implement checks and create working directories & files
  ################################################################################
  
  # Create the working directory
  userScratch <- paste("/pic/scratch/", system("whoami", intern = TRUE), sep = "")
  if (!file.exists(userScratch)) 
    system(paste("mkdir", userScratch))

  # Create temporary working directories
  tmpNum <- system("echo $$", intern = TRUE)
  wkdir <- paste(userScratch, "/", jobName, "_tmp_", tmpNum, sep = "")
  wkdir.logs <- paste(wkdir, "logs", sep = "/")
  wkdir.out <- paste(wkdir, "out", sep = "/")
  system(paste("mkdir ", wkdir, "; ",
               "mkdir ", wkdir.logs, "; ",
               "mkdir ", wkdir.out, sep=""))

  # Create temporary working directories and files
  envir.file <- paste(wkdir, "Renvironment.Rdata", sep = "/")
  rscript.file <- paste(wkdir, "pic_lapply.R", sep = "/")
  sb <- paste(wkdir, "launch.sh", sep = "/")
  sl.out <- paste(wkdir, "slurmLaunch.out", sep = "/")
  sl.err <- paste(wkdir, "slurmLaunch.err", sep = "/")
  sb.err <- paste(wkdir, "sbatch.err", sep = "/")

  # If a tarball.file was not specified
  if (is.null(tarball.file)) 
    tarball.file <- paste(getwd(), "/", jobName, "_output_", tmpNum, ".tar.gz", sep = "")
 
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
    pvar(envir.file)
    pvar(rscript.file)
    pvar(sl.out)
    pvar(sl.err)
    pvar(sb.err)
    pvar(tarball.file)
        
  }

  ################################################################################
  # Create environmental data file that will be loaded in each process
  ################################################################################
  
  # Match functions
  FUN <- match.fun(FUN)

  # Capture optional arguments to the function
  optional.args <- list(...)

  # Identify how to parse the job
  subsets <- parseJob(lenX, numProcesses, collate = collate, random.seed = random.seed)

  # Now add needed.objects into this environment so they can be saved from here, along with other objects
  # in this environment.  Add suffix to their names so they will be unique from any other objects
  # in this environment
  if (!is.null(needed.objects)) {

    # Make sure 'needed.objects' does not include the following
    reserved.global.objects <- c("process.id", "tmpNum", "X", "FUN.p",
                                 "FUN.argnames", "optional.args", "subsets",
                                 "wkdir.out", "numProcesses")

    conflict.objects <- reserved.global.objects[reserved.global.objects %in% needed.objects]
    
    if (length(conflict.objects))
      stop("The following are reserved objects for 'piclapply', and should not be included ",
           "in the 'needed.objects' argument:\n '", paste(conflict.objects, collapse = "', '"), "'\n")
    
    for (v in needed.objects)
      assign(paste(v, "neededObjects", tmpNum, sep = "."), get(v, envir = needed.objects.env))

    needed.objects <- paste(needed.objects, "neededObjects", tmpNum, sep = ".")    
    
  }

  # Match the arguments that actually exist in FUN
  FUN.argnames <- names(formals(FUN))

  if (length(optional.args)) {

    # If any optional arguments are provided that are not in FUN
    if (!all(names(optional.args) %in% FUN.argnames[-1])) 
      stop("Optional arguments do not match the optional arguments of FUN")
          
    FUN.p <- function(x) {
      xList <- eval(parse(text = paste("list(", FUN.argnames[1], "= x)", sep = "")))
      do.call("FUN", c(xList, optional.args))
    }
    
  }
  else
    FUN.p <- FUN

  # Rename the list so that it can be reordered as it was originally
  orig.X.names <- names(X)
  names(X) <- as.character(1:lenX)
 
  # Save the environment that will be needed for each instance of lapply
  save(list = needed.objects, tmpNum, X, FUN.p, FUN.argnames,
       optional.args, subsets, wkdir.out, numProcesses, file = envir.file)

  if (verbose)
    cat("\nR environment for each R instance saved to '", envir.file, "'\n", sep = "")
    
  ################################################################################
  # Write the rscript file that will be called from the MPI code
  ################################################################################
  
  # Load packages as requested
  if (is.null(packages))
    pk.text <- "# No packages needed\n"
  else {
    pk.text <- NULL
    for (pk in packages)
      pk.text <- c(pk.text, paste("require(", pk, ")\n", sep = ""))
  }
    
  # Prepare the header file
  hf.text <- ifelse(is.null(header.file), "# No header file to source\n",
                    paste("source(\"", header.file, "\")\n", sep=""))

  # Rename needed objects if they are present
  if (!is.null(needed.objects))
    mv.text <- c("neededObjects.vec <- ls(pattern = paste(\"neededObjects\", tmpNum, sep=\".\"))\n",
                 "for (v in neededObjects.vec)\n",
                 "  assign(stripExtension(stripExtension(v)), get(v))\n",
                 "rm(list = neededObjects.vec, neededObjects.vec)\n")
  else
    mv.text <- "# No objects needed\n"

  # Write the .R file that will be launched
  cat("require(pnlStat)\n",

      # Load additional packages if needed
      pk.text,
      
      # Identify the process id passed in from the system call
      "process.id <- as.numeric(commandArgs(TRUE))\n",
      
      # Print the proces id number and the number of processes to the .Rout file
      "print(process.id)\n",
      
      # Source the header file if needed
      hf.text,
      
      # Load the environment file that contains the R objects needed to call lapply
      "load(\"", envir.file, "\")\n",
      
      # Rename the 'needed.objects' objects if needed
      mv.text,

      # Evaluate pre-processing expression if it's present
      "if (exists(\"pre.process.expression\"))\n",
      "  eval(pre.process.expression)\n",      
      
      # Identify the subset to be processed by this instance of R
      "subset <- subsets[[process.id + 1]]\n",

      # Call lapply
      "X.sub.out <- lapply(X[subset], FUN.p)\n",

      # Save the results
      "fout <- paste(\"", paste(wkdir.out, "/", jobName, "_completed_\"", sep = ""), 
               ", process.id, \".Rdata\", sep=\"\")\n",      
      "save(X.sub.out, file = fout)\n",

      # Evaluate post-processing expression if it's present
      "if (exists(\"post.process.expression\"))\n",
      "  eval(post.process.expression)\n",
      
      # Print warnings
      "warnings()\n",
      
      sep = "",
      file = rscript.file)

                
  if (verbose)
    cat("\nR script file '", rscript.file, "' written\n", sep = "")

  ################################################################################  
  # Write the launch.c file that will be compiled
  ################################################################################

#  compileMPI(wkdir, rscript.file, mpi.compiler = mpi.compiler, verbose = verbose)
  compileMPI(wkdir, rscript.file, verbose = verbose)

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
      '#SBATCH -A ', account, '\n',
      '#SBATCH -t ', time.limit.mins, '\n',
      '#SBATCH -N ', numNodes, '\n',
      '#SBATCH -n ', numProcesses, '\n',  
      '#SBATCH -o ', sl.out, '\n',
      '#SBATCH -e ', sl.err, '\n',
      email.notice,
      'date "+DateTime %Y-%m-%d %T"\n',
      'source /etc/profile.d/modules.csh\n',
      'module purge\n',
#      'module load pnnl_env\n',
      'module load gcc/4.6.0\n',
      'module load mvapich2/1.7\n',
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
    cat("\n", sb.stdout, ". Launching ", numProcesses, " jobs.\n",
        "You can check the status of the SLURM job by typing 'squeue' at the system prompt.\n",
        "Waiting for SLURM job to complete...\n",
        sep = "")

  # Initialize variables for the waiting loop
  continueWaiting <- TRUE
  elapsed.min <- 0
  start.time <- NULL
  timeOut <- FALSE
  notFirst <- FALSE
  wait.counter <- 0
  workReported <- FALSE

  # Stop if an error happens, if the job completes successfully, or if the elapsed time expires
  while (continueWaiting & !timeOut) {

    # Wait during each iteration of this loop
    if (notFirst) {
      Sys.sleep(check.interval.sec)
      wait.counter <- wait.counter + 1
    }
    else
      notFirst <- TRUE
 
    # If any errors occur in the R out files
    if (length(dir(wkdir.logs, pattern = ".Rout"))) {

      # Timing of how long it took for the SLURM job to actually begin
      if (verbose) {
        if (!workReported) {
          if (wait.counter == 0)
            cat("\nWork on SLURM job began immediately after launching\n")
          else {
            lo <- (wait.counter - 1) * check.interval.sec
            hi <- wait.counter * check.interval.sec
            cat("\nWork on SLURM job began between", lo, "and", hi, "seconds after launching\n")
          }
          workReported <- TRUE
        }
      }

      n.Rerr <- as.numeric(system(paste("grep -e 'Error' -e 'Execution halted' ",
                                        wkdir.logs, "/*.Rout | wc -l", sep = ""), intern = TRUE))

      # If there are R errors, cancel the SLURM batch job
      if (n.Rerr) {

        # Get the SLURM job id 
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

    # Check the SLURM output file, look for the 'Job completion' string
    if (file.exists(sl.out)) {

      # If the last line says 'Job completion', then stop waiting
      last.line <- system(paste("tail -1", sl.out), intern = TRUE)
      if (length(last.line))
        continueWaiting <- !(last.line == "Job completion")

      if (is.null(start.time))
        start.time <- Sys.time()
        
      elapsed.min <- as.numeric(difftime(Sys.time(), start.time, units="m"))

    }

    if (elapsed.min > time.limit.mins + 90) {
      timeOut <- TRUE
      warning("piclapply() timed out, waiting ", round(elapsed.min, 2),
              " minutes for the SLURM job to launch (or complete).\n")
    }

  } # while waiting


  ################################################################################
  # Check the R log files for errors or incompletion, get completion times
  ################################################################################

  # Calculate the elapsed time for the SLURM job
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
  if (n.comp != numProcesses)
    warning("The number of completed .Rout files, ", n.comp,
            " does not match the number of processes, ", numProcesses, ".\n")

  # Get sense of the average completion time in minutes for the individual R jobs
  if (verbose) {

    sys.text <- paste("grep 'proc.time()' -A3 ", wkdir.logs,
                      "/*.Rout | sed -n '3~4p' | awk 'BEGIN {FS = \"[ -]+\"};{print $4}'", sep = "")

    Rout.completion.time <- round(mean(as.numeric(system(sys.text, intern = TRUE)) / 60), 3)
    
    cat("\nThe individual R jobs completed on average in", Rout.completion.time, "minutes\n")

  }

  ################################################################################
  # Concatentate results
  ################################################################################
  
  recombine <- function() {
    
    # Concatenate the files into a list
    files.to.aggregate <- dir(wkdir.out, full.names = TRUE, pattern = ".Rdata")
    n.files.to.aggregate <- length(files.to.aggregate)

    # Assign each list file to an object
    for (i in 1:n.files.to.aggregate) 
      assign(paste("f", i, sep = ""), load.object(files.to.aggregate[i]))
  
    # Combine the all using a single call to c()
    combine.text <- paste("c(", paste(paste("f", 1:n.files.to.aggregate, sep = ""),
                                      collapse = ", "), ")",
                          sep = "")
      
    out <- eval(parse(text = combine.text))

    # Remove individual files
    rm(list = paste("f", i, sep = ""))

    # Restore the original ordering and names of the list
    out <- out[as.character(1:lenX)]
    names(out) <- orig.X.names

    return(out)
                 
  } # recombine

  if (verbose) 
    cat("\nRecombining completed R jobs (and reordering to match original list order).\n",
        "Completed in ")

  Xout <- timeIt(recombine(), verbose = verbose)
  
  
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
  # Return results
  ################################################################################  

  # Restore default options
  options(op)

  if (verbose) {
    cat("\nTotal elapsed time for piclapply():",
        round(as.numeric(difftime(Sys.time(), s.time, unit = "m")), 2),
        "minutes\n")
  }
  
  # Return the results from piclapply
  return(Xout)

} # piclapply
