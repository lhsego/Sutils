##' Display PIC systems for which the 'piclapply' package currently works
##'
##' @details Extracts information contained in the source code file \code{systemSpecifics.R}.
##' This information is also contained in \code{getOption("piclapplyParms")}.
##'
##' @param details A logical indicating whether details should be displayed
##'
##' @export
##'
##' @return System names with details if requested

showAvailableSystems <- function(details = FALSE) {

  Smisc::stopifnotMsg(is.logical(details) & length(details) == 1,
                      "'details' must be TRUE or FALSE")

  parms <- getOption("piclapplyParms")

  if (!details) {
    return(names(parms))
  }

  else {

    cat("These system-specific settings are set in 'systemSpecifics.R':\n\n")

    for (i in 1:length(parms)) {

        cat("System: ", names(parms)[i], "\n\n")

        cat("  MPI compiler:", parms[[i]]$MPIpath, "\n\n")

        d <- as.data.frame(matrix(unlist(parms[[i]]$partitions), byrow = TRUE, ncol = 2,
                                  dimnames = list(NULL, c("computeNodeNames", "numCores"))))
        d$numCores <- Smisc::factor2numeric(d$numCores)
        d$partitions <- names(parms[[i]]$partitions)

        print(d[,c("partitions", "computeNodeNames", "numCores")])

        cat("\n\n")

    }

    return(NULL)
  }

} # showAvailableSystems
