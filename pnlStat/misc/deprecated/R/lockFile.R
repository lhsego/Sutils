lockFile <- function(file, maxWait=5, maxAttempts=10^5, startingDelay=TRUE, verbose=FALSE) {

  # Verify we're working in Unix
  if (.Platform$OS.type != "unix")
    stop("lockFile only works on Unix platforms\n")

  wait <- 0
  ret <- FALSE

  # Introduce a small random delay before attempting lock the file
  if (startingDelay)
    Sys.sleep(round(runif(1) * 10, 3))

  newLockDir <- paste(file, ".LockID-",  format(Sys.time(), "%Y-%m-%d-%H%M%S"), "-", round(1000 * proc.time()[[3]]), sep="")

  # While loop to get the case to process
  while (wait < maxAttempts) {

    # Check to see if the lock directory is there... if not, then create the directory
    if (!length(oldLockDir <- dir(path=getPath(file), pattern=paste(stripPath(file), "LockID-", sep=".")))) {
  
      if (system(paste("mkdir", newLockDir)))
        stop("'", newLockDir, "' could not be created\n")
      
      # Break this loop since we succeeded in locking the file
      wait <- maxAttempts

      ret <- TRUE

      if (verbose)
        cat("'", file, "' locked by creating the directory '", newLockDir, "'\n", sep="")
    
    }
    else {
      
      # Wait a random amount of time (between 0 and maxWait seconds)
      waitTime <- round(runif(1) * maxWait, 3)

      if (verbose)
        cat("File '", file, "' is currently locked by '", oldLockDir, "'\n  Waiting ", waitTime,
            " seconds before attempting again to create the directory '", newLockDir, "'\n", sep="")
      
      Sys.sleep(waitTime)
      
      wait <- wait + 1
    }
    
  } # while loop for waiting for file to become unlocked

  
  # If the file locked correctly
  if (ret) {
    
    class(newLockDir) <- "fileLock"
    return(newLockDir)
    
  }
  else {
    warning("Could not lock the file '", file, "' by creating the folder '", newLockDir, "' after ", maxAttempts, " attempts.\n")
    return(NULL)
  }

} # lockFile


unlockFile <- function(lockDir, verbose=FALSE) {

  # Verify we're working in Unix
  if (.Platform$OS.type != "unix")
    stop("unlockFile only works on Unix platforms\n")

  if (class(lockDir) != "fileLock")
    stop("'lockDir' must be of class 'fileLock', which is the object returned by lockFile()\n")

  if (!system(paste("rmdir", lockDir))) {

    if (verbose)
      cat("'", stripExtension(lockDir), "' unlocked by removing the directory '", lockDir, "'\n", sep="")

    invisible(TRUE)
    
  }

  else {

    warning("Could not unlock the file '", stripExtension(lockDir), "' by removing the folder '", lockDir, "'.")
    invisible(FALSE)

  }

} # unlockFile


