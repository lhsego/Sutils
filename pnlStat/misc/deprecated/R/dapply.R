##  this could be a template for dapply which uses plapply
## bind.p <- function() {

##   d.tpox.list <- lapply(parseJob(NROW(d.tpox), 4), function(x) d.tpox[x,])

##   dtlp <- plapply(d.tpox.list, process, packages = "pnlStat",
##                   needed.objects = paste("c.0", 0:19, sep="."),
##                   njobs = 4, check.interval.sec = 2)

##   return(rbind(dtlp[[1]], dtlp[[2]], dtlp[[3]], dtlp[[4]]))

## } # bind.p

# NOT currently exported...

dapply <- function(dframe.infile,                
                   dframe.outfile,
                   fun.file,
                   fun.name,
                   njobs = 7,
                   max.hours = 24,
                   check.interval.sec = 120,
                   launch.delay = 5,
                   random.seed = NULL,
                   clean.up = TRUE,
                   verbose = TRUE) {

  if (.Platform$OS.type == "unix")
    dapply.unix(dframe.infile,
                dframe.outfile,
                fun.file,
                fun.name,
                njobs = njobs,
                max.hours = max.hours,
                check.interval.sec = check.interval.sec,
                launch.delay = launch.delay,
                random.seed = random.seed,
                clean.up = clean.up,
                verbose = verbose)
  
  else if (.Platform$OS.type == "windows")
    dapply.windows(dframe.infile,
                   dframe.outfile,
                   fun.file,
                   fun.name,
                   njobs = njobs,
                   max.hours = max.hours,
                   check.interval.sec = check.interval.sec,
                   launch.delay = launch.delay,
                   random.seed = random.seed,
                   clean.up = clean.up,
                   verbose = verbose)
  else
    stop("dapply() currently only functions on Unix and Windows platforms\n")

} # dapply

# For UNIX
dapply.unix <- function(dframe.infile,                
                        dframe.outfile,
                        fun.file,
                        fun.name,
                        njobs = 7,
                        max.hours = 24,
                        check.interval.sec = 120,
                        launch.delay = 5,
                        random.seed = NULL,
                        clean.up = TRUE,
                        verbose = TRUE) {
  
  # Files that will be deleted
  tmp.files <- NULL

  # Load the infile
  d.in <- load.object(dframe.infile)
  nrows <- NROW(d.in)

  # Root name of the job files
  tstamp <- stripExtension(timeStamp("",""))
  rsf <- paste(paste(stripExtension(dframe.outfile), "code", sep="_"), tstamp, sep="")
  
  # .R source file
  f.R <- NULL
  # parsed dataframe outfiles
  dframe.outfiles <- NULL

  # Parse the job
  # Create .R scripts and launch them with 5 seconds waiting between each one
  for (i in 1:njobs) {

    fname <- paste(rsf, "_", i, ".R", sep="")
    f.R <- c(f.R, fname)
    
    if (is.null(random.seed)) {
      
      rws <- parseJob(nrows, njobs)[[i]]
      dframe.outfile.i <- paste(stripExtension(dframe.outfile), tstamp, "_rows_",
                                padZero(min(rws), nchar(as.character(nrows))), "-",
                                padZero(max(rws), nchar(as.character(nrows))), ".Rdata", sep="")
      
    }
    else 
      dframe.outfile.i <- paste(stripExtension(dframe.outfile), tstamp, "_", i, ".Rdata", sep="")

    dframe.outfiles <- c(dframe.outfiles, dframe.outfile.i)
    tmp.files <- c(tmp.files, dframe.outfile.i, fname)

    # Write the .R file that will be launched
    cat("library(pnlStat)\n",
        "source('", fun.file, "')\n",
        "rws <- parseJob(", nrows, ", ", njobs, ", random.seed = ",
                         ifelse(is.null(random.seed), "NULL", random.seed), ")[[", i, "]]\n",
        "d <- load.object('", dframe.infile, "')\n",
        "d <- d[rws,]\n",
        "d <- ", fun.name, "(d)\n",
        "save(d, file='", dframe.outfile.i, "')\n",
        "warnings()\n",
        sep="",
        file = fname)

    # Now launch the i_th job, that annoying 'nohup.out' routed to /dev/null
    system(paste("nohup R CMD BATCH --vanilla", fname, "> /dev/null 2>&1 &"))
#    system(paste("nohup R CMD BATCH --vanilla", fname, "&"))
    
    if (verbose)
      cat("Launching job", i, "\n")

    # Pause a bit between jobs, since they read from the same dframe.infile
    if (launch.delay > 0)
      Sys.sleep(launch.delay)
  
  } # for

  # Wait for the jobs to finish
  njobs.finished <- 0
  elapsed.hours <- 0
  start.time <- Sys.time()

  if (verbose)
    cat("Waiting for jobs to complete...\n")

  while ((njobs.finished < njobs) & (elapsed.hours < max.hours)) {

    Sys.sleep(check.interval.sec)
    njf.1 <- as.numeric(system(paste("cat ", rsf, "_*.Rout | grep 'proc.time()' | wc -l", sep=""), intern=TRUE))
    njf.2 <- as.numeric(system(paste("cat ", rsf, "_*.Rout | grep 'Execution halted' | wc -l", sep=""), intern=TRUE))
    njobs.finished <- njf.1 + njf.2
    elapsed.hours <- as.numeric(difftime(Sys.time(), start.time, units="h"))

    # Show waiting progress
    if (verbose)
      pvar(njf.1, njf.2, njobs.finished, elapsed.hours, digits=4)

  }

  # If one of the jobs failed  
  if (njf.2)
    stop("It appears that ", njf.2, " of the jobs failed.\n",
         "Look at the '", rsf, "_*.Rout' files of the separate jobs.\n")
  
  # If they all finished and we didn't time out
  if (njobs.finished == njobs) {

    d <- NULL

    # Concatenate the files
    for (f in dframe.outfiles) {
      d <- rbind(d, load.object(f))
      if (verbose)
        cat("Adding data from '", f, "'\n", sep="")
    }

    # Some checks
    rownames.d <- rownames(d)
    rownames.d.in <- rownames(d.in)

    if (length(rownames.d) != length(rownames.d.in))
      stop("Length of rownames of completed data frame do not match length of rownames of original data frame:\n",
           pvar(length(rownames.d), length(rownames.d.in), verbose=FALSE))

    if (!setequal(rownames.d, rownames.d.in))
      stop("Rownames of completed data frame do not match rownames of original data frame")

    # If no randomization took place, they should be identical
    if (is.null(random.seed)) {
      if (!all(rownames.d == rownames.d.in))
        warning("Rownames of completed data frame are not identical to rownames of original data frame")
    }
    # Order the rows as they originally were
    else 
      d <- d[rownames.d.in,]

    # Save the data
    save(d, file = dframe.outfile)
    if (verbose)
      cat("Saved output dataframe to '", dframe.outfile, "'\n", sep="")

    # cat all .Rout files into a single file
    system(paste("more ", rsf, "_*.Rout > ", stripExtension(fun.file), ".Rout", sep=""))

    
    # Deleting extra files
    if (clean.up) {

      dfiles <- c(tmp.files, paste(stripExtension(f.R), "Rout", sep="."))
      unlink(dfiles)
      if (verbose) {
        cat("The following temporary files were deleted:\n")
        print(sort(dfiles))
      }
    }
  
    
  } # if (njobs.finished == njobs) {

  else
    warning("Jobs did not complete in the maximum waiting time of ",
            round(max.hours,5), " hours.  Output files were not concatenated.\n")

  
} # dapply.unix



# For Windows
dapply.windows <- function(dframe.infile,                
                           dframe.outfile,
                           fun.file,
                           fun.name,
                           njobs = 7,
                           max.hours = 24,
                           check.interval.sec = 120,
                           launch.delay = 5,
                           random.seed = NULL,
                           clean.up = TRUE,
                           verbose = TRUE) {
  
  # Load the infile
  d.in <- load.object(dframe.infile)
  nrows <- NROW(d.in)

  # Root name of the job files
  tstamp <- stripExtension(timeStamp("",""))
  rsf <- paste(paste(stripExtension(dframe.outfile), "code", sep="_"), tstamp, sep="")  

  # File used to keep track of completed jobs
  jobTrack <- paste(rsf, "jobTracker.txt", sep="_")
  cat("0\n", file=jobTrack)

  # Files that will be deleted
  tmp.files <- jobTrack
  
  # .R source file
  f.R <- NULL
  # parsed dataframe outfiles
  dframe.outfiles <- NULL

  # Parse the job
  # Create .R scripts and launch them with 5 seconds waiting beteween each one
  for (i in 1:njobs) {

    fname <- paste(rsf, "_", i, ".R", sep="")
    f.R <- c(f.R, fname)

    if (is.null(random.seed)) {
      
      rws <- parseJob(nrows, njobs)[[i]]
      dframe.outfile.i <- paste(stripExtension(dframe.outfile), tstamp, "_rows_",      
                                padZero(min(rws), nchar(as.character(nrows))), "-",
                                padZero(max(rws), nchar(as.character(nrows))), ".Rdata", sep="")
      
    }
    else
      dframe.outfile.i <- paste(stripExtension(dframe.outfile), tstamp, "_", i, ".Rdata", sep="")      

    dframe.outfiles <- c(dframe.outfiles, dframe.outfile.i)
    tmp.files <- c(tmp.files, dframe.outfile.i, fname)

    # Write the .R file that will be launched
    cat("library(pnlStat)\n",
        "source('", fun.file, "')\n",
        "rws <- parseJob(", nrows, ", ", njobs, ", random.seed = ",
                         ifelse(is.null(random.seed), "NULL", random.seed), ")[[", i, "]]\n",
        "d <- load.object('", dframe.infile, "')\n",
        "d <- d[rws,]\n",
        "d <- ", fun.name, "(d)\n",
        "save(d, file='", dframe.outfile.i, "')\n",
        "warnings()\n",
        # Write to the jobTrack file indicating the job has been completed
        "cat('1\\n', file='", jobTrack, "', append=TRUE)\n",
        sep="",
        file = fname)

    # Now launch the i_th job
    shell(paste("R CMD BATCH --vanilla", fname), mustWork=TRUE, wait=FALSE, translate=TRUE)
    
    if (verbose)
      cat("Launching job", i, "\n")

    # Pause a bit between jobs, since they read from the same dframe.infile
    if (launch.delay > 0)
      Sys.sleep(launch.delay)
  
  } # for

  # Wait for the jobs to finish
  njobs.finished <- 0
  elapsed.hours <- 0
  start.time <- Sys.time()

  if (verbose)
    cat("Waiting for jobs to complete...\n")

  while ((njobs.finished < njobs) & (elapsed.hours < max.hours)) {

    Sys.sleep(check.interval.sec) 
    njobs.finished <- sum(read.table(jobTrack))
    elapsed.hours <- as.numeric(difftime(Sys.time(), start.time, units="h"))

    # Show waiting progress
    if (verbose)
      pvar(njobs.finished, elapsed.hours, digits=4)

  }

  # If they all finished and we didn't time out
  if (njobs.finished == njobs) {

    d <- NULL

    # Concatenate the files
    for (f in dframe.outfiles) {
      d <- rbind(d, load.object(f))
      if (verbose)
        cat("Adding data from '", f, "'\n", sep="")
    }

    # Some checks
    rownames.d <- rownames(d)
    rownames.d.in <- rownames(d.in)

    if (length(rownames.d) != length(rownames.d.in))
      stop("Length of rownames of completed data frame do not match length of rownames of original data frame:\n",
           pvar(length(rownames.d), length(rownames.d.in), verbose=FALSE))

    if (!setequal(rownames.d, rownames.d.in))
      stop("Rownames of completed data frame do not match rownames of original data frame")

    # If no randomization took place, they should be identical
    if (is.null(random.seed)) {
      if (!all(rownames.d == rownames.d.in))
        warning("Rownames of completed data frame are not identical to rownames of original data frame")
    }
    # Order the rows as they originally were
    else 
      d <- d[rownames.d.in,]

    # Save the data
    save(d, file = dframe.outfile)
    if (verbose)
      cat("Saved output dataframe to '", dframe.outfile, "'\n", sep="")

    # Combine all .Rout files into a single file
    shell(paste("copy ", rsf, "_*.Rout ", stripExtension(fun.file), ".Rout", sep=""), translate=TRUE)
    
    # Deleting extra files
    if (clean.up) {

      dfiles <- c(tmp.files, paste(stripExtension(f.R), "Rout", sep="."))
      unlink(dfiles)
      if (verbose) {
        cat("The following temporary files were deleted:\n")
        print(sort(dfiles))
      }
    }
  
    
  } # if (njobs.finished == njobs) {

  else
    warning("Jobs did not complete in the maximum waiting time of ",
            round(max.hours,5), " hours.  Output files were not concatenated.\n")

  
} # dapply.windows
