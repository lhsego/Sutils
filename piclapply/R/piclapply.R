##' High performance computing (HPC) parallelization of lapply() on PIC
##'
##' Parses a list into subsets and submits a separate R job using lapply() for each subset.
##' Jobs are executed using parallelized high
##' performance computing on the PNNL institutional computing cluster (PIC).
##' The parallel jobs are instantiated on multiple nodes/cores
##' using a SLURM batch job.
##'
##' \code{piclapply} applies \code{FUN} to each element of the list \code{X} by
##' parsing the list into sublists of equal (or almost equal) size (using
##' \code{\link{parseJob}}) and then applying \code{FUN} to each sublist using
##' \code{\link{lapply}}. The list, \code{X}, will be parsed using \code{\link{parseJob}} into
##' \code{n * numNodes} sublists where \code{n} is the number of CPU cores on the requested nodes.
##' Each of the sublists will be processed by
##' \code{\link{lapply}} on a separate core running its own instance of R.
##'
##' If the length of the list is shorter than the number of requested cores (which
##' is \code{n * numNodes}), then the number of nodes is reassigned to a value
##' that will utilize full nodes to the extent possible.
##'
##' After calling \code{piclapply}, it is also good practice to log into one (or
##' more) of the nodes that are assigned by SLURM and verify (using \code{top})
##' that all of the cores are computing as expected.
##'
##' After the jobs complete, the output lists are reassembled into a single output list.
##' The order of names of the output list will match those of the input list, provided the two lists
##' have the same lenth and \code{keep.names = TRUE}.
##'
##' A number of objects are made available in the global environment of each
##' instance of R.  These objects can be used by \code{FUN} if needed.  They
##' are:
##'
##' \itemize{
##'
##' \item \code{process.id}: An integer uniquely identifying the process of R
##' that is running. This will range from
##' 0 to \code{n * numRprocesses - 1}.
##' \item \code{numRprocesses}: The total
##' number of R instances launched by SLURM.  In general, this will be \code{n * numNodes}.
##' \item \code{wkdir.out}: A character string indicating the directory where output from
##' each R instance is saved.
##' }
##'
##' Each instance of R runs a script that performs the following steps:
##'
##' \enumerate{
##'
##' \item The \code{piclapply} package is loaded.
##'
##' \item Any other packages indicated in the \code{packages} argument are
##' loaded.
##'
##' \item The \code{process.id} global variable is assigned (having been passed
##' in via a command line argument).  This variable identifies the particular
##' instance of R.
##'
##' \item The header file (if there is one) is sourced.
##'
##' \item The R environment file is loaded, which contains the list (\code{X}),
##' the function (\code{FUN}), any \code{needed.objects}, as well as all other
##' objects (internally created by \code{piclapply}) that will be needed.
##'
##' \item The expression \code{pre.process.expression} is evaluated if an object
##' of that name is present in the global environment. The object
##' \code{pre.process.expression} may be passed in via the header file or via
##' \code{needed.objects}.
##'
##' \item The subset of the indexes of the list \code{X} (that will create the
##' sublist) is identified for the particular instance of R, using the
##' \code{process.id} variable.
##'
##' \item The sublist, \code{X.sub}, of the list \code{X} is created and
##' \code{X} is removed to save memory.
##'
##' \item \code{\link{lapply}} is called as follows:
##' \code{X.sub.out <- lapply(X.sub, FUN.p)}, where \code{FUN.p} is a wrapper to
##' \code{FUN} containing named arguments.
##'
##' \item The expression \code{post.process.expression} is evaluated if an
##' object of that name is present in the global environment.  The object
##' \code{post.process.expression} may be passed in via the header file or via
##' \code{needed.objects}.  This provides a mechanism to reduce the output in
##' \code{X.sub.out} if needed (see Example 3).
##'
##' \item The list \code{X.sub.out} is saved to a file in \code{wkdir.out} where
##' it will be collected after all jobs have completed.
##'
##' \item Warnings are printed.
##' }
##'
##' @export
##'
##' @param X The list, each element of which will be the input to \code{FUN}
##'
##' @param FUN A function whose first argument is an element of list \code{X}
##'
##' @param account A character string indicating the PIC account (e.g. "CSI",
##' "SDI", "USERS") that will charged for the computing time.
##'
##' @param \dots Additional named arguments to \code{FUN}
##'
##' @param packages Character vector giving the names of packages that will be
##' loaded in each new instance of R. If \code{NULL}, no packages are loaded for
##' each new instance of R.
##'
##' @param header.file Text string indicating a file that will be initially
##' sourced prior calling \code{\link{lapply}} in order to create an
##' 'environment' that will satisfy all potential dependencies for \code{FUN}.
##' If \code{NULL}, no file is sourced.
##'
##' @param needed.objects Character vector giving the names of objects which
##' reside in the evironment specified by \code{needed.objects.env} that may be
##' needed by \code{FUN}.  These objects are loaded into the GLOBAL ENVIRONMENT
##' of each new instance of R that is launched.  If \code{NULL}, no additional
##' objects are passed the the global environment of the various R instances.
##'
##' @param needed.objects.env Environment where \code{needed.objects} reside.
##' This defaults to the environment in which \code{piclapply} is called.
##'
##' @param keep.names Logical, that when TRUE, causes the original names of the list to
##' be retained on the output list if possible.  This also ensures the output list
##' is returned in the same order as the input list.
##'
##' @param jobName Text string indicating the prefix for temporary files and folders that
##' will be created while launching the separate instances of R.
##'
##' @param numNodes Integer indicating the number of nodes requested for
##' parallel processing. For example, each slurm node on constance has 24 cores. So, for example,
##' requesting 3 nodes is equivalent to requesting 72 parallel jobs.
##'
##' @param partition Character string indicating the PIC partition to which the
##' job should be submitted. Defaults to \code{slurm},
##' which is the partition for 'usual' jobs.  The \code{short} partition is for
##' testing and code development for short jobs of 1 hour or less.
##' The \code{gpu}
##' partition contains nodes with GPU capability, and \code{long} is for long jobs,
##' Use the system command
##' \code{sview} to see a graphical depiction of the entire cluster and to see
##' more detailed information about the partitions, and which nodes are available on
##' a given partition.
##'
##' @param time.limit.mins Integer indicating the time limit (in minutes) of the
##' SLURM batch job. If the SLURM job exceeds this limit it will be canceled.
##' The tradeoff is that jobs with shorter limits are likely to be scheduled for launch
##' sooner.
##'
##' @param check.interval.sec The number of seconds to wait between checking to
##' see whether the SLURM batch job has completed.
##'
##' @param tarball A text string indicating the file (ending in .tar.gz)
##' where all the files used to create and support the SLURM job will be stored.
##' If \code{NULL}, a file of the form 'jobName_output_####.tar.gz' will be
##' stored in the working directory.  If \code{tarball = "none"}, no
##' tarball will be created.
##'
##' @param parseJob.args A named list of optional (named) arguments to
##' \code{\link{parseJob}} in the \code{Smisc} package. These arguments govern how
##' the list \code{X} will be subdivided for parallel processing. If \code{parseJob.args = NULL},
##' the default arguments of \code{\link{parseJob}} are used.
##'
##' @param remove.working.dir \code{= TRUE} requests the temporary working directory of
##' the SLURM job files be deleted on successful completion.  If the SLURM job
##' fails, this directory will not be removed.
##'
##' @param tmp.file.list A character string indicating the name of a list containing the path of the
##' temporary folder and the output tarball file.  This list is
##' assigned to the global environment.  If NULL, the list is not written to
##' the global environment.
##'
##' @param email.notification Text string containing an email address to which
##' an email will be sent upon completion of the SLURM job.  If \code{NULL}, no
##' email will be sent.
##'
##' @param verbose \code{= TRUE} prints details (and timing) of the job
##'
##' @return A list equivalent to that returned by \code{lapply(X, FUN, ...)}.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{lapply}}, \code{\link{plapply}}, \code{\link{dfplapply}},
##' \code{\link{showAvailableSystems}}, \code{\link{killSlurm}}
##'
##'
##' @keywords misc
##'
##' @examples
##'
##' ########################################
##' # Example 1
##' ########################################
##'
##' ## You must set the PIC account name before launching this example:
##' ## i.e., account <- "PIC_account_name"
##'\dontshow{
##' if (exists("account", envir = .GlobalEnv)) {
##'   account <- get("account", envir = .GlobalEnv)
##' } else {
##'    stop("To run this example, you need to assign the PIC account name to 'account' in the Global Environment, as follows:\n",
##'        "account <- \"PIC_account_name\"")
##' }
##' }
##' # Create a simple list
##' a <- list(a = rnorm(10), b = rnorm(20), c = rnorm(15), d = rnorm(13), e = rnorm(15), f = rnorm(22))
##'
##' # Some objects that will be needed by f1:
##' b1 <- rexp(20)
##' b2 <- rpois(10, 20)
##'
##' # The function
##' f1 <- function(x) mean(x) + max(b1) - min(b2)
##'
##' # Call piclapply
##' res.1 <- piclapply(a, f1, account,
##'                    needed.objects = c("b1", "b2"),
##'                    jobName = "example.1",
##'                    numNodes = 1,
##'                    partition = "short",
##'                    check.interval.sec = 1,
##'                    time.limit.mins = 1,
##'                    tarball = "none",
##'                    verbose = TRUE)
##' # Call lapply to check the results
##' res.2 <- lapply(a, f1)
##' print(res.2)
##'
##' # Compare results
##' all.equal(res.1, res.2)
##'
##'
##' ########################################
##' # Example 2
##' ########################################
##'
##' # Create a function that calculates the mean of a normal variate
##' mean.tmp <- function(list.element, sigma = 1) {
##'   set.seed(list.element)
##'   mean(rnorm(500, mean = list.element, sd = sigma))
##' }
##'
##' # Create a list of means (and seeds) to operate over
##' aList <- as.list(0:10000)
##'
##' # Calculate it without piclapply
##' res.3 <- lapply(aList, mean.tmp, sigma = 0.5)
##'
##' res.4 <- piclapply(aList, mean.tmp, account, sigma = 0.5,
##'                    jobName = "example.2",
##'                    time.limit.mins = 1,
##'                    numNodes = 3,
##'                    partition = "slurm",
##'                    parseJob.args = list(collate = TRUE, text.to.eval = TRUE),
##'                    check.interval.sec = 1,
##'                    tarball = "none",
##'                    verbose = TRUE)
##'
##' head(res.3)
##' tail(res.3)
##'
##' # These should be the same...
##' all.equal(res.3, res.4)
##'
##'
##' ########################################
##' # Example 3:
##' # A more complicated example with packages,
##' # header files, and pre- and post-expressions
##' ########################################
##'
##' # Create the header file
##'
##' hf <- cat(# Print the process id and the number of R processes to the .Rout file
##'           "Smisc::pvar(process.id)\n",
##'
##'           # Create the pre-process expression by writing it into the header file
##'           "pre.process.expression <- expression({\n",
##'
##'           # Summarize the muscle data set to illustrate using another package (MASS)
##'           "   Smisc::pvar(numRprocesses)\n",
##'           "   data(muscle)\n",
##'           "   summary(muscle)\n",
##'
##'           "})\n",
##'
##'           # List all the objects in the environment
##'           "print(ls())\n",
##'
##'           # Write to the header file
##'           file = "tmpHeader.R", sep = "")
##'
##' # Display the header file
##' Smisc::more("tmpHeader.R")
##'
##' # This post expression will be passed in via 'needed.arguments'
##' post.process.expression <- expression({
##'
##'   # Show original length of X.sub.out
##'   Smisc::pvar(length(X.sub.out))
##'   cat("Reducing X.sub.out now...\n")
##'
##'   # Calculate the mean for all the observations in the R process to reduce the sublist
##'   # into a list with 1 element
##'   X.sub.out <- as.list(mean(unlist(X.sub.out)))
##'
##'   # Now show its reduced length
##'   Smisc::pvar(length(X.sub.out))
##'
##' })
##'
##' # Create a function that calculates the mean of a normal variate
##' mean.tmp <- function(list.element, sigma = 1) {
##'
##'   # Set the seed using the process id, which exists in the global environment of the R instance
##'   set.seed(process.id)
##'
##'   # Return the mean
##'   return(mean(rnorm(500, mean = list.element, sd = sigma)))
##'
##' }
##'
##' # Create a list of means to operate over
##' aList <- as.list(0:10000)
##'
##' # Run with lots of options
##' res.4 <- piclapply(aList, mean.tmp, account,
##'                    sigma = 0.5,
##'                    header.file = "tmpHeader.R",
##'                    packages = "MASS",
##'                    needed.objects = "post.process.expression",
##'                    jobName = "example.3",
##'                    time.limit.mins = 1,
##'                    numNodes = 2,
##'                    partition = "short",
##'                    parseJob.args = list(text.to.eval = TRUE),
##'                    check.interval.sec = 1,
##'                    tmp.file.list = "tmpFiles",
##'                    remove.working.dir = FALSE,
##'                    tarball = "none",
##'                    verbose = TRUE)
##'
##' # Due to the post.process expression, this list has relatively few elements: one for each R
##' # process, whereas the original list had length 10000
##' length(res.4)
##' head(res.4)
##'
##' # Remove the header file
##' unlink("tmpHeader.R")
##'
##' # Now print the 'tmpFiles' list object that was written to the global environment
##' print(tmpFiles)
##'
##' # Look at the .Rout file from one of the instances
##' anRoutFile <- dir(file.path(tmpFiles$wkdir, "logs"), full.name = TRUE)[2]
##' anRoutFile
##' Smisc::more(anRoutFile)
##'
##' # Now remove the temporary folders
##' system(paste("rm -r", tmpFiles$wkdir))

piclapply <- function(X, FUN, account, ...,
                      packages = NULL,
                      header.file = NULL,
                      needed.objects = NULL,
                      needed.objects.env = parent.frame(),
                      keep.names = TRUE,
                      jobName = "piclapply",
                      numNodes = 2,
                      partition = "slurm",
                      time.limit.mins = 30,
                      check.interval.sec = 30,
                      tarball = NULL,
                      parseJob.args = NULL,
                      remove.working.dir = TRUE,
                      tmp.file.list = NULL,
                      email.notification = NULL,
                      verbose = FALSE) {

  # Get the start time
  if (verbose) {
    s.time <- Sys.time()
    cat("\n")
    Star("Launching piclapply()")
  }

  # Print warnings as they occur
  op <- options(warn = 1)

  ################################################################################
  # Check input arguments
  ################################################################################

  # Verify these are logicals
  for (v in c("keep.names", "remove.working.dir", "verbose")) {
    good <- FALSE
    if (length(get(v)) == 1) {
      if (is.logical(get(v))) {
        good <- TRUE
      }
    }
    if (!good)
      stop("'", v, "' should be TRUE or FALSE\n")
  }

  # Verify these arguments are strings
  for (v in c("account", "jobName", "partition")) {
    if (!(is.character(get(v)) & length(get(v)) == 1)) {
      stop("'", v, "' should be a character string")
    }
  }

  # Verify these arguments are character (if they're not NULL)
  for (v in c("packages", "header.file", "needed.objects", "tarball",
              "email.notification", "tmp.file.list")) {
    cv <- get(v)
    if (!is.null(cv)) {

      if (!is.character(cv)) {
        stop("'", v, "' should be a character string, character vector, or NULL")
      }

      # Verify all are length 1 except packages and needed.objects
      if (!(v %in% c("packages", "needed.objects"))) {
        if (length(cv) != 1) {
          stop("'", v, "' should be a character string or NULL")
        }
      }
    }
  }

  # Verify these are positive integers
  numeric.arguments <- c("numNodes", "time.limit.mins", "check.interval.sec")

  for (v in numeric.arguments) {
    v.bad <- TRUE
    cv <- get(v)
    if (is.numeric(cv)) {
      if ((cv > 0) & (cv %% 1 == 0))
        v.bad <- FALSE
    }
    if (v.bad) {
      stop("'", v, "' must be a positive integer")
    }
  }


  # Get the piclapplyParms
  parms <- getOption("piclapplyParms")

  # Verify the system name
  systemName <- verifySystem(names(parms))

  # Get the MPIpath
  MPIpath <- parms[[systemName]]$MPIpath

  # Verify it's character and that the file exists
  Smisc::stopifnotMsg(is.character(MPIpath),
                      paste("The 'MPIpath' element for '", systemName, "' is not character.",
                            "'\nUse 'showAvailableSystems(details = TRUE)' to view the system-specific settings", sep = ""))
  Smisc::stopifnotMsg(file.exists(MPIpath),
                      paste("The MPIpath of '", MPIpath, "' is not a valid filename on '", systemName,
                            "'.\nUse 'showAvailableSystems(details = TRUE)' to view the system-specific settings", sep = ""))

  # Verify the partition
  acceptablePartitions <- names(parms[[systemName]]$partitions)
  Smisc::stopifnotMsg(partition %in% acceptablePartitions,
                      paste("When using '", systemName, "', 'partition' must be one of '",
                            paste(acceptablePartitions, collapse = "', '"), "'", sep = ""))


  # Verify that needed objects exist
  if (!is.null(needed.objects)) {
    for (v in needed.objects) {
      if (!exists(v, envir = needed.objects.env)) {
        stop("needed.object '", v, "' does not exist in environment '",
             deparse(substitute(needed.objects.env)), "'")
      }
    }
  }

  # Check the optional args to parseJob
  if (!is.null(parseJob.args)) {

    if (!is.list(parseJob.args)) {
      stop("'parseJob.args' must be a named list of optional arguments to Smisc::parseJob()")
    }

    # possible optional args
    possible.args <- setdiff(names(formals(Smisc::parseJob)), c("n", "njobs"))

    # Make sure the arguments actually match the optional arguments of parseJob
    if (!all(names(parseJob.args) %in% possible.args)) {
      stop("The following are not acceptable arguments to 'parseJob':\n",
           "'", paste(setdiff(names(parseJob.args), possible.args), collapse = "', '"), "'")
    }
  }


  # Check installed packages
  if (!is.null(packages)) {

      # Vector indicating which packages are not installed
      notInst <- !(packages %in% installed.packages()[,"Package"])

      if (any(notInst)) {
        stop("The following requested package(s) is/are not installed: '",
             paste(packages[notInst], collapse = "', '"), "'")
      }

  }

  # Create the temporary number used to name the temporary directories
  tmpNum <- system("echo $$", intern = TRUE)

  ################################################################################
  # Now add needed.objects into this environment so they can be saved
  # along with other objects in this environment.  Add suffix to their names so they
  # will be unique from any other objects in this environment
  ################################################################################
  if (!is.null(needed.objects)) {

    # Make sure 'needed.objects' does not include the following
    reserved.global.objects <- c("process.id", "tmpNum", "X", "FUN.p",
                                 "FUN.argnames",
                                 "optional.args", "subsets",
                                 "wkdir.out", "numRprocesses")

    conflict.objects <- reserved.global.objects[reserved.global.objects %in% needed.objects]

    if (length(conflict.objects))
      stop("The following are reserved objects for 'piclapply', and should not be included\n",
           "in the 'needed.objects' argument: '", paste(conflict.objects, collapse = "', '"), "'\n")

    for (v in needed.objects) {
      assign(paste(v, "neededObjects", tmpNum, sep = "."), get(v, envir = needed.objects.env))
    }

    needed.objects <- paste(needed.objects, "neededObjects", tmpNum, sep = ".")

  }

  # Capture optional arguments to the function
  optional.args <- list(...)

  # Match the arguments that actually exist in FUN
  FUN.argnames <- names(formals(if (is.primitive(FUN)) args(FUN) else FUN))

  if (length(optional.args)) {

    # Bad optional argumetns
    badOptArgs <- !(names(optional.args) %in% FUN.argnames[-1])

    # If any optional arguments are provided that are not in FUN
    if (any(badOptArgs)) {
      stop("The following optional argument(s) does/do not match the optional arguments of FUN:\n'",
           paste(names(optional.args)[badOptArgs], collapse = "', '"), "'")
    }

    FUN.p <- function(x) {
      xList <- eval(parse(text = paste("list(", FUN.argnames[1], "= x)", sep = "")))
      do.call("FUN", c(xList, optional.args))
    }

  }
  else {
    FUN.p <- FUN
  }

  # Match functions
  FUN <- match.fun(FUN)

  ################################################################################
  # If the the partition is short, then limitations apply
  ################################################################################
  if (partition == "short") {

    if (numNodes > 2) {

      numNodes <- 2

      warning("'numNodes' cannot exceed 2 when using the short partition. ",
              "'numNodes' set to 2.")
    }

    if (time.limit.mins > 60) {

      time.limit.mins <- 60

      warning("'time.limit.mins' cannot exceed 60 when using the short partition. ",
              "'time.limit.mins' set to 60.")

    }

  }

  ################################################################################
  # Set the number of slurm processes and rescale the job if the list size
  # isn't large enough
  ################################################################################

  # Get the length of the list
  lenX <- length(X)

  # Get the number of cores per node for the given partition
  nCoresPerNode <- parms[[systemName]][["partitions"]][[partition]][["numCores"]]

  # Assign the number of processes
  numRprocesses <- nCoresPerNode * numNodes

  # If number of processes exceeds the number elements in the list, then reduce it
  # appropriately
  if (lenX < numRprocesses) {
    numNodes <- max(1, lenX %/% nCoresPerNode)
    numRprocesses <- ifelse(lenX < nCoresPerNode, lenX, numNodes * nCoresPerNode)

    if (verbose) {
      star("Job size has been reduced to accomodate the size of the ",
           "list and utilize each node at 100%, if possible:\n",
           Smisc::pvar(partition, nCoresPerNode, length(X), numNodes, numRprocesses,
                       verbose = FALSE))
    }
  }

  ################################################################################
  # Create working directories & files
  ################################################################################

  # Create the working directory
  usrName <- system("whoami", intern = TRUE)
  userScratch <- file.path("/pic/scratch", usrName)
  if (!file.exists(userScratch)) {
    system(paste("mkdir", userScratch))
  }

  # Create temporary working directories
  wkdir <- paste(userScratch, "/", jobName, "_tmp_", tmpNum, sep = "")
  wkdir.logs <- file.path(wkdir, "logs")
  wkdir.out <- file.path(wkdir, "out")
  system(paste("mkdir ", wkdir, "; ",
               "mkdir ", wkdir.logs, "; ",
               "mkdir ", wkdir.out, sep = ""))

  # Create temporary working files
  envir.file <- file.path(wkdir, "Renvironment.Rdata")
  rscript.file <- file.path(wkdir, "piclapply.R")
  slurm.batch.script <- file.path(wkdir, "slurmLaunch.sh")
  sl.out <- file.path(wkdir, "slurmLaunch.out")
  sl.err <- file.path(wkdir, "slurmLaunch.err")
  sb.err <- file.path(wkdir, "sbatch.err")

  # If a tarball was not specified
  if (is.null(tarball)) {
    tarball <- paste(getwd(), "/", jobName, "_output_", tmpNum, ".tar.gz", sep = "")
  }

  # If a tarball did not have a path (in which case, it should have the path
  # of the working directory
  else if ((tarball != "none") & (Smisc::stripPath(tarball) == tarball)) {
    tarball <- paste(getwd(), tarball, sep = "/")
  }

  # Create an object that will contain the temporary file names--to be used in programming if needed
  if (!is.null(tmp.file.list)) {
    assign(tmp.file.list, list(wkdir = wkdir, tarball = tarball), envir = .GlobalEnv)
  }

  # List the temporary files
  if (verbose) {

    Star("Created working directories and filenames:")

    # Display the various paths and filenames
    star("Working directory: ", wkdir)
    star("README file describing working directory contents: ", file.path(wkdir, "README.txt"))
    star("Output tarball file: ", tarball)

  }

  ################################################################################
  # Write the README.txt file containing a description of the files
  ################################################################################
  cat("This folder contains working files created by piclapply() on ",  as.character(Sys.time()), ":\n",
      "\n",
      "launch.c\n",
      "   The MPI C code that launches one R instance per core in a SLURM batch allocation\n",
      "\n",
      "launch.o\n",
      "   The compilation (executable) of the MPI code\n",
      "\n",
      Smisc::stripPath(slurm.batch.script), "\n",
      "   The SLURM batch script launched via 'sbatch'\n",
      "\n",
      Smisc::stripPath(sl.out), "\n",
      "   Standard output file for the SLURM batch job\n",
      "\n",
      Smisc::stripPath(sl.err), "\n",
      "   Error file for the SLURM batch job\n",
      "\n",
      Smisc::stripPath(sb.err), "\n",
      "   Error file from the 'sbatch' system command\n",
      "\n",
      Smisc::stripPath(rscript.file), "\n",
      "   The R script that is executed by each R instance\n",
      "\n",
      Smisc::stripPath(envir.file), "\n",
      "   The R environment that is loaded into each R instance that containins all needed objects\n",
      "\n",
      Smisc::stripPath(wkdir.logs), "\n",
      "   Directory containing the *.Rout files for the individual R jobs\n",
      "\n",
      Smisc::stripPath(wkdir.out), "\n",
      "   Directory containing the output lists for the individual R jobs that will be recombined\n",

      file = file.path(wkdir, "README.txt"),
      sep = "")


  ################################################################################
  # Create the subsets that identify how the list X will be parsed.  Each element of 'subsets'
  # corresponds to a the elements that will be processed by a core
  ################################################################################
  subsets <- do.call(Smisc::parseJob, c(list(n = lenX, njobs = numRprocesses), parseJob.args))

  # Rename the list so that it can be reordered as it was originally
  orig.X.names <- names(X)
  names(X) <- as.character(1:lenX)

  ################################################################################
  # Save environmental data file that will be loaded in each process
  ################################################################################
  save(list = needed.objects, tmpNum, X, FUN.p, FUN.argnames,
       optional.args, subsets, wkdir.out, numRprocesses, file = envir.file)

  ################################################################################
  # Write the rscript file that will be called from the MPI code
  ################################################################################

  # Gather the names of all packages that will need to be loaded on each parallel
  # instance of R.  Start with Smisc.
  pkgs <- "piclapply"

  # Add in user requested packages
  if (!is.null(packages))
    pkgs <- c(pkgs, packages)

  # Create text for package loading
  pkg.text <- paste("library(", pkgs, ")\n", sep = "")

  # Rename needed objects if they are present
  if (!is.null(needed.objects)) {
    mv.text <- c("\n# Restoring original names to needed objects and remove their copies\n",
                 "neededObjects.vec <- ls(pattern = paste(\"neededObjects\", tmpNum, sep=\".\"))\n",
                 "for (v in neededObjects.vec)\n",
                 "  assign(Smisc::stripExtension(Smisc::stripExtension(v)), get(v))\n",
                 "rm(list = neededObjects.vec, neededObjects.vec)\n")
  }
  else {
    mv.text <- NULL
  }

  # Evaluation of subsets from text...
  get.subset.indexes.text <- "subset.i <- subsets[[process.id + 1]]\n"

  if (!is.null(parseJob.args)) {
    if ("text.to.eval" %in% names(parseJob.args)) {
      if (parseJob.args$text.to.eval)
        get.subset.indexes.text <- "subset.i <- eval(parse(text = subsets[[process.id + 1]]))\n"
    }
  }

  # Write the .R file that will be launched by each R instance
  cat("# Load packages\n",
      pkg.text,

      "\n# Identify the process id passed in from the system call that launched this R process\n",
      "process.id <- as.numeric(commandArgs(TRUE))\n",

      if (!is.null(header.file))
        paste("\n# Source the header file\n",
              "source(\"", header.file, "\")\n", sep="")
      else NULL,

      "\n# Load the environment file that contains the R objects needed to call lapply()\n",
      "load(\"", envir.file, "\")\n",

      # Restoring original names to needed objects
      mv.text,

      "\n# Evaluate pre-processing expression if it's present\n",
      "if (exists(\"pre.process.expression\"))\n",
      "  eval(pre.process.expression)\n",

      "\n# Identify the subset to be processed by this instance of R\n",
      get.subset.indexes.text,

      "\n# Create the subsetted list and remove the old one (to free up memory in case it's large)\n",
      "X.sub <- X[subset.i]\n",
      "rm(X, subsets, subset.i)\n",

      "\n# Call lapply\n",
      "X.sub.out <- lapply(X.sub, FUN.p)\n",

      "\n# Evaluate post-processing expression if it's present\n",
      "if (exists(\"post.process.expression\"))\n",
      "  eval(post.process.expression)\n",

      "\n# Save the results\n",
      "fout <- paste(\"", paste(wkdir.out, "/", jobName, "_completed_\"", sep = ""),
               ", process.id, \".Rdata\", sep=\"\")\n",
      "save(X.sub.out, file = fout)\n",

      "\n# Print warnings\n",
      "warnings()\n",

      sep = "",
      file = rscript.file)


  if (verbose) {
    Star("Launching SLURM job")
  }

  ################################################################################
  # Write the launch.c file that will be compiled
  ################################################################################
  compileMPI(wkdir, rscript.file, MPIpath, verbose = verbose)

  ################################################################################
  # Write and launch the sbatch script
  ################################################################################

  email.notice <- ifelse(is.null(email.notification), "\n",
                         paste('\n',
                               '#SBATCH --mail-type=END\n',
                               '#SBATCH --mail-user=', email.notification, '\n\n', sep=""))

  # Set the number of processes per node
  num.processes.text <- paste("#SBATCH -n ", numRprocesses, "\n", sep = "")

  # Write the SLURM batch script
  cat('#!/bin/csh\n',
      '\n',
      '#SBATCH -A ', account, '\n',
      '#SBATCH -t ', time.limit.mins, '\n',
      '#SBATCH -N ', numNodes, '\n',
      num.processes.text,
      '#SBATCH -p ', partition, '\n',
      '#SBATCH -o ', sl.out, '\n',
      '#SBATCH -e ', sl.err, '\n',
      email.notice,
      'date "+DateTime %Y-%m-%d %T"\n',
      'source /etc/profile.d/modules.csh\n',
      'module purge\n',
      parms[[systemName]]$moduleCommands, '\n',
      '\n',
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
      file = slurm.batch.script)

  # The function for launching the SLURM batch job
  launchSLURM <- function() {

    # Launch the parallel jobs, redirect stderr to a file
    sb.stdout <- system(paste("sbatch ", slurm.batch.script, " 2>", sb.err, sep = ""), intern = TRUE)

    # Count the number of lines in the stderr of the sbatch command
    n.sb.err <- as.numeric(system(paste("wc -l", sb.err, "| awk '{print $1}'"), intern = TRUE))

    # Check for errors in the sbatch launch
    if (!length(sb.stdout) & n.sb.err) {
      star("SLURM job failed to launch.  These are the contents of '", sb.err, "':")
      Smisc::more(sb.err)
      cat("\n")
      stop("SLURM job failed to launch")
    }

    # Get the SLURM job id
    slurm.jobid <- sub("[^0-9]*", "", sb.stdout)


    if (verbose) {

      star(sb.stdout, ". Launching ", numRprocesses,
           ifelse(numRprocesses > 1, " instances ", " instance "), "of R")

      star("You can check the status of the SLURM job by typing 'squeue -u ", usrName,
            "' at the system prompt")

    }

    return(list(sb.stdout = sb.stdout,
                n.sb.err = n.sb.err,
                slurm.jobid = slurm.jobid))

  } # launchSLURM()

  # Launch the SLURM job and write 'sb.stdout', 'n.sb.err' and 'slurm.jobid' to this
  # environment with Smisc::sepList()
  Smisc::sepList(launchSLURM())

  ################################################################################
  # Wait for the sbatch script to complete
  ################################################################################

  # Initialize variables for the waiting loop
  continueWaiting <- TRUE
  elapsed.min <- 0
  start.time <- NULL
  notFirst <- FALSE
  wait.counter <- 0
  workReported <- FALSE
  retrySLURM <- TRUE

  # Stop if an error happens, if the job completes successfully, or if the elapsed time expires
  while (continueWaiting) {

    # Wait during each iteration of this loop
    if (notFirst) {
      Sys.sleep(check.interval.sec)
      wait.counter <- wait.counter + 1
    }
    else {
      notFirst <- TRUE
    }

    # If any errors occur in the R out files
    if (length(dir(wkdir.logs, pattern = ".Rout"))) {

      # Timing of how long it took for the SLURM job to actually begin
      if (verbose) {
        if (!workReported) {
          if (wait.counter == 0) {
            star("Work on SLURM job began immediately after launching")
          }
          else {
            lo <- (wait.counter - 1) * check.interval.sec
            hi <- wait.counter * check.interval.sec
            star("Work on SLURM job began between ", lo, " and ", hi, " seconds after launching")
          }
          star("Waiting for SLURM job to complete...")
          workReported <- TRUE
        }
      }

#  Grepping on 'Error' was catching try-catch errors that were acceptable
#      n.Rerr <- as.numeric(system(paste("grep -e 'Error' -e 'Execution halted' ",
      n.Rerr <- as.numeric(system(paste("grep -e 'Execution halted' ",
                                        wkdir.logs, "/*.Rout | wc -l", sep = ""), intern = TRUE))

      # If there are R errors, cancel the SLURM batch job
      if (n.Rerr) {

        # Kill the SLURM job
        killSlurm(slurm.jobid, verbose = FALSE)

        # Stop the function
        stop("R error(s) detected in R log files:  '", wkdir.logs, "/*.Rout'")

      }
    }

    # If the slurmLaunch.err file exists, check to see if it has lines.
    if (file.exists(sl.err)) {

      # Get the number of lines in the file
      nlines <- as.numeric(system(paste("wc -l", sl.err, "| awk '{print $1}'"), intern = TRUE))

      if (nlines) {

        # Kill the SLURM job just in case it's not dead :)
        killSlurm(slurm.jobid, verbose = FALSE)

        # Write the outcome of the failure
        star("SLURM job failed.  These are the contents of '", sl.err, "':")
        Smisc::more(sl.err)
        cat("\n")

        if (retrySLURM) {

          # Copy SLURM error and out files to preserve a record of the first attempt
          if (file.exists(sl.err)) {
            file.copy(sl.err, paste(sl.err, "firstLaunch", sep = "_"))
          }
          if (file.exists(sl.out)) {
            file.copy(sl.out, paste(sl.out, "firstLaunch", sep = "_"))
          }

          star("Will wait ten seconds and then attempt to launch SLURM job again")

          # Wait 10 seconds
          Sys.sleep(10)

          # Reinitialize variables for the waiting loop
          continueWaiting <- TRUE
          elapsed.min <- 0
          start.time <- NULL
          notFirst <- FALSE
          wait.counter <- 0
          workReported <- FALSE
          retrySLURM <- FALSE

          # Relaunch
          Smisc::sepList(launchSLURM())

          # Restart the while loop
          next

        }

        else {
          stop("SLURM job failed twice")
        }

      }
    }

    # Check the SLURM output file, look for the 'Job completion' string
    if (file.exists(sl.out)) {

      # If the last line says 'Job completion', then stop waiting
      last.line <- system(paste("tail -1", sl.out), intern = TRUE)

      if (length(last.line)) {
        continueWaiting <- !(last.line == "Job completion")
      }

      if (is.null(start.time)) {
        start.time <- Sys.time()
      }

      elapsed.min <- as.numeric(difftime(Sys.time(), start.time, units = "m"))

    }

    # Verify the job is still on the queue
    smj <- system(paste("squeue -u", usrName), intern = TRUE)
    smj <- smj[grepl(slurm.jobid, smj)]

    if (length(smj) > 1) {
      stop("Unexpected format returned from 'squeue'")
    }

    # Check conditions for which the job should be canceled
    if (length(smj)) {

      if (grepl("PartitionTimeLimit", smj)) {

        killSlurm(slurm.jobid)

        stop("'time.limit.mins = ", time.limit.mins,
             "' exceeds the acceptable time limit of the ", partition, " partition")
      }

      if (grepl("PartitionNodeLimit", smj)) {

        killSlurm(slurm.jobid)

        stop("'numNodes = ", numNodes,
             "' exceeds the acceptable number of nodes for the ", partition, " partition")
      }

    }
    # If the job is no longer in the queue, then wait no more
    else {

      if (verbose) {
        star("SLURM job ", slurm.jobid, " is no longer in the queue")
      }

      continueWaiting <- FALSE
    }

    # Verify we haven't exceeded the time limit.  Build in a 12 hour lag for SLURM job launch...
    if (elapsed.min > time.limit.mins + 60 * 12) {

      killSlurm(slurm.jobid, verbose = FALSE)

      stop("piclapply() timed out after waiting ", round(elapsed.min, 2),
           " minutes for the SLURM job to launch (or complete).\n")

    }

  } # while waiting


  ################################################################################
  # Check the R log files for errors or incompletion, get completion times
  ################################################################################

  # Calculate the elapsed time for the SLURM job
  if (verbose) {

    Star("Job completion summary")
    sts <- as.POSIXct(system(paste("grep DateTime", sl.out, "| awk '{print $2 \" \" $3}'"), intern = TRUE))
    slurm.et <- round(as.numeric(difftime(sts[2], sts[1], units = "m")), 2)
    star("SLURM job completed in ", slurm.et, " minutes")

  }

  # Look for errors and warnings in the log files
  n.err <- as.numeric(system(paste("grep Error ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                             intern = TRUE))
  n.halt <- as.numeric(system(paste("grep 'Execultion halted' ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                              intern = TRUE))
  n.war <- as.numeric(system(paste("grep Warning ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                             intern = TRUE))

  # Count the number of .Rout files with 'proc.time()' at the end
  n.comp <- as.numeric(system(paste("tail -n 3 -q ", wkdir.logs, "/*.Rout | grep 'proc.time()' | wc -l", sep = ""),
                              intern = TRUE))

  if (n.err) {
    warning("Errors were identified in ", n.err, " of the R log files in '", wkdir.logs, "'\n")
  }

  if (n.halt) {
    warning("Execution was halted for ", n.halt, " of the R log files in '", wkdir.logs, "'\n")
  }

  if (n.war) {
    warning("Warnings were identified in ", n.war, " of the R log files in '", wkdir.logs, "'\n")
  }

  # The number of proc.time()'s in the .Rout files should match the number of processes
  if (n.comp != numRprocesses) {
    warning("The number of completed .Rout files, ", n.comp,
            " does not match the number of processes, ", numRprocesses, ".\n")
  }

  # Get sense of the average completion time in minutes for the individual R jobs
  if (verbose) {

    # Gather the vector of elapsed times that come from the call the proc.time() at the end of each .Rout file
    sys.text <- paste("tail -n 3 -q ", wkdir.logs, "/*.Rout | grep 'proc.time()' -A3 | egrep -v 'proc.time|user' | ",
                      "awk 'BEGIN {FS = \"[ -]+\"};{print $4}'", sep = "")

    # The average completion time for the R instances, in minutes
    Rout.completion.time <- round(mean(as.numeric(system(sys.text, intern = TRUE)) / 60), 3)

    star("The individual R jobs each completed on average in ", Rout.completion.time, " minutes")

  }

  ################################################################################
  # Concatentate results
  ################################################################################

  recombine <- function() {

    # Concatenate the files into a list
    files.to.aggregate <- dir(wkdir.out, full.names = TRUE,
                              pattern = paste(jobName, "_completed_[0-9]*.Rdata", sep = ""))

    n.files.to.aggregate <- length(files.to.aggregate)

    # Make sure the files are present
    if (!length(n.files.to.aggregate)) {
      stop("No completed .Rdata files in '", wkdir.out, "' were available to aggregate")
    }

    # Assign each list file to an object
    for (i in 1:n.files.to.aggregate) {
      assign(paste("f", i, sep = ""), Smisc::loadObject(files.to.aggregate[i]))
    }

    # Combine the all using a single call to c()
    combine.text <- paste("c(", paste(paste("f", 1:n.files.to.aggregate, sep = ""),
                                      collapse = ", "), ")",
                          sep = "")

    out <- eval(parse(text = combine.text))

    # Remove individual files
    rm(list = paste("f", i, sep = ""))

    # Restore the original ordering and names of the list if requested and if possible
    changeNames <- FALSE

    if (keep.names) {

      if (length(out) != lenX) {
        star("Length of the output list does not match length of the input list. ",
             "Original list names and ordering will not be preserved")
      }
      else {
        changeNames <- TRUE
      }

    }
    if (changeNames) {
      out <- out[as.character(1:lenX)]
      names(out) <- orig.X.names
    }
    else {
      names(out) <- as.character(1:length(out))
    }

    # Return the recombined list
    return(out)

  } # recombine

  if (verbose) {
    star("Recombining lists from completed R jobs...")
  }

  # Execute the recombination
  timedXout <- Smisc::timeIt(recombine(), verbose = FALSE, return.time = TRUE)

  if (verbose) {
    star("Recombination completed in ", round(timedXout$elapsed, 3), " ", timedXout$units)
  }

  # Get the output list
  Xout <- timedXout$out
  rm(timedXout)

  ################################################################################
  # Create tarball of the SLURM job files and remove the directory
  ################################################################################

  if (tarball != "none") {
    system(paste("cd ", userScratch, "; tar -Ppczf ",
                 tarball, " ", Smisc::stripPath(wkdir), sep = ""))
    if (verbose) {
      star("'", tarball, "' created")
    }
  }

  if (remove.working.dir) {
    system(paste("rm -r", wkdir))
    if (verbose) {
      star("Working directory '", wkdir, "' removed")
    }
  }


  ################################################################################
  # Return results
  ################################################################################

  # Restore default option for warning
  options(warn = 0)

  if (verbose) {
    Star("Total elapsed time for piclapply():  ",
         round(as.numeric(difftime(Sys.time(), s.time, units = "m")), 2),
         " minutes")
  }

  # Return the results from piclapply
  return(Xout)

} # piclapply
