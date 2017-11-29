# This function is called when the library is loaded.  It loads the dll's and gives an introductory message.
.First.lib <- function(libname, pkgname) {
  
  library.dynam("aoz", package=pkgname)

  cat("\nWelcome to the aoz package, version ", packageDescription("aoz", fields="Version"), ".\n\n",
      "Commented source code can be found in\n", .path.package(package="aoz"), "/SourceCode.\n\n", sep="")
  
}

# This function is called when the library is detached. It unloads the dll's.
.Last.lib <- function(libpath) {
  confirm <- try(library.dynam.unload("aoz", libpath), silent=TRUE)
  if (class(confirm) != "try-error")
    cat("aoz package detached\n")
  else
    cat(confirm,"\n")
}

