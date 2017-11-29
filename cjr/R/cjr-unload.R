# This function would be called when namespace is unloaded (if it were not in the search path
# because it had been loaded via loadNamespace("cjr")
.onUnload <- function(libpath) {

  if (is.loaded("gconf", PACKAGE = "cjr")) {
  
    confirm <- try(library.dynam.unload("cjr", libpath), silent = TRUE)
    
    if (class(confirm) != "try-error")
      cat("cjr shared objects are unloaded\n")
    else
      cat(confirm,"\n")
  }
}

