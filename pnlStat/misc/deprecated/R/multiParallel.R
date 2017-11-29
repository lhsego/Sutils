# A function that can be launched many times in order to process a dataframe

multiParallel <- function(source.df.file, index.file, out.dir, process.function,
                          rows.to.process=100, max.hours=48, verbose=TRUE) {

  # index file:  a file containing a single (characater) vector of rownames of the source data frame,
  #              presuming these rownames are just numbers
  # source.df.file:  filename of the source data frame
  # out.dir:  Output directory for the procssed jobs that will be concatenated together
  # process.function:  the function that will operate on the data frame by calculating the desired quantities
  #                    It should have a single argument:  the data frame, and return a modification of the data frame


  # Verify we're working in Unix
  if (.Platform$OS.type != "unix")
    stop("multiParallel only works on Unix platforms\n")  
    
  # Load the source.df and get the # of rows
  d <- load.object(source.df.file)
  nrd <- NROW(d)

  # Initialize timers
  elapsed.hours <- 0
  start.time <- Sys.time()

  # Loop over gathering multiple sets of cases from source.df
  while (TRUE) {

    # Get the index file by locking it and loading it
    lockCode <- lockFile(index.file)
    indexes <- load.object(index.file)

    if (verbose)
      cat("\nLocked the index file '", index.file, "'\n", sep="")

    # If there are no more indexes in the index file, then unlock it and break the loop
    if (!(i.length <- length(indexes))) {
      
      if (verbose)
        cat("No indexes left in '", index.file, "'.  Processing completed.\n")
      
      if (!unlockFile(lockCode))
        stop("In multiParallel:",  lockCode, "was not successfully unlocked")        
        
      break
      
    }

    # Select a set of indexes to work with, subtract them from the original indexes
    index.set <- indexes[1:min(rows.to.process, i.length)]
    indexes <- setdiff(indexes, index.set)

    if (verbose) {
      cat("Selected the following rows indexes:\n")
      print(index.set)
    }

    # Write back the indexes and unlock the index file
    save(indexes, file=index.file)
    
    if (!unlockFile(lockCode))
      stop("In multiParallel:",  lockCode, "was not successfully unlocked")

    if (verbose)
      cat("Unlocked the index file '", index.file, "'\n")

    # Now select the rows from the source data frame and process them
    dsub <- d[index.set,]

    # Now process it with process.function
    dout <- process.function(dsub)

    # Save the results
    vals <- padZero(c(range(as.numeric(index.set)), nrd))
    
    fname.out <- paste(out.dir, "/", stripPath(stripExtension(source.df.file)), "_", vals[1], "_to_", vals[2], ".Rdata", sep="")
    
    save(dout, file=fname.out)

    if (verbose)
      cat("Results saved to '", fname.out, "'\n", sep="")

    # Update the elapsed hours and check
    elapsed.hours <- as.numeric(difftime(Sys.time(), start.time, units = "h"))

    if (elapsed.hours > max.hours) {
      
      warning("Processing stopped:  Processing time of ", elapsed.hours, " hours has exceeded ",
              pvar(max.hours, verbose=FALSE))

      break
    }

  } # while (TRUE)


} # multiParallel
