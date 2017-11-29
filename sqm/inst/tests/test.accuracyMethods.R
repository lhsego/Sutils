test.accuracyMethods = function(x=NULL){
  
  # This function cannot be equal to John's function since the functions that are being
  # tested have different names. They do the same thing, however. 
  
  
  ref <- c("test.accuracy", "test.precision", "test.recall", "test.sensitivity", "test.TPR", "test.specificity",
                   "test.TNR", "test.FPR", "test.FNR", "test.Fscore")
  
  if (is.null(x)){
  methods <- ref;
  } else {
    methods <- x %in% ref;
    if (!all(methods)) 
      warning("'", paste(x[!methods], collapse = "', "), 
              "' is/are not valid.\n", "See test.accuracyMethods() for a list of valid methods")
  }
  return(methods)
}

exec.test.accuracyMethods <- function() {

	if (length(setdiff(accuracyMethods(),gsub("test.","",test.accuracyMethods())))==0) {
	  return(NULL)
	} else {
	  warning("The method(s) given below does/do not have an associated test method:")
	  warning(print(setdiff(accuracyMethods(),gsub("test.","",test.accuracyMethods()))))
	  stop("There is not an associated test.[name of method] for at least one of the methods in accuracyMethods().")
	}


}