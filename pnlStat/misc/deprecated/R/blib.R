# A function for building and installing packages interactively in R while developing them.
blib <- function(lib) {

  # Check the platform and machine
  if (!(.Platform$OS.type == "unix"))
    stop("blib is only supported on martingale.pnl.gov")
#  if (system("echo $HOSTNAME", intern = TRUE) != "martingale")
#    stop("blib is only supported on martingale.pnl.gov")

  # Convert to a character string if necessary
  lib <- as.character(substitute(lib))

  if (lib == "pnlStat")
    stop("Can build any other package besides pnlStat")

  # Unload the namespace (which should also detach the package
  if (lib %in% loadedNamespaces())
    unloadNamespace(lib)

  # If not in the namespace, it may be in the search path in earlier R versions
  pkg <- paste("package", lib, sep=":")
  if (pkg %in% search()) {
    detach(pkg, character.only=TRUE)
    cat("\n")
  }
  
  # Build the package
  system(paste("blibcp", lib))

  cat("\n")

  # Load the new package
  require(lib, character.only=TRUE)
  
} # blib
