##' Kill a SLURM job from R
##'
##' Kill a SLURM job from R using the system command \code{scancel jobID}
##'
##' If the job is not present in the queue, then \code{scancel} command is not
##' issued.  This function is used for error control in \code{\link{piclapply}}--but it can
##' also be called from the R terminal.
##'
##' @export
##'
##' @param jobID SLURM job number.  May be passed as a numeric value or as a
##' string.
##'
##' @param verbose Logical indicating whether message should be printed regarding the job cancelation.
##'
##' @return Nothing.  Only a message indicating whether or not the job was
##' canceled.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{piclapply}}.
##'
##' @keywords misc

killSlurm <- function(jobID, verbose = TRUE) {

  # Convert the jobID to numeric if possible
  jobID <- Smisc::as.numericSilent(jobID)

  # Verify it's a valid job that can be an integer
  jobID.bad <- TRUE
  if (is.numeric(jobID)) {
    if ((jobID > 0) & (jobID %% 1 == 0)) {
      jobID.bad <- FALSE
    }
  }
  if (jobID.bad) {
    stop("'jobID' must be a numeric value or string that refers to a SLURM job number")
  }

  Smisc::stopifnotMsg(if (length(verbose) == 1) is.logical(verbose) else FALSE,
                      "'verbose' must be TRUE or FALSE")

  # Verify we're using an acceptable system
  verifySystem(names(getOption("piclapplyParms")))

  # Get a list of the current SLURM jobs
  jobs <- system("squeue | awk '{print $1}'", intern = TRUE)

  if (as.character(jobID) %in% jobs) {

    if (verbose) {
      star("Killing SLURM job ", jobID)
    }
    system(paste("scancel", jobID), ignore.stdout = !verbose, ignore.stderr = !verbose)
  }
  else {
    if (verbose) {
      star("SLURM job ", jobID, " not in the queue")
    }
  }

} # killSlurm

