##' Add a single variable to a dataframe that uniquely identifies the signature system
##'
##' Add a single variable to a dataframe that uniquely identifies the signature system
##' using one or more other variables
##'
##' @export
##'
##' @param df The input dataframe to which the identifying variable will be added
##'
##' @param groupVars A character vector containing one (or more) column names in \code{df}
##' that will be used to construct the identifying variable
##'
##' @param sigIDname A character string giving the name of the single variable that will
##' identify the signatuer systems uniquely.
##'
##' @param separator A character that will be used to separate the \code{groupVars} when
##' they are pasted together to form \code{sigIDname}.  If \code{NULL},
##' \code{createSignatureID} will attempt to identify a separator character that does not
##' exist in \code{groupVars}.  If one can be found, then no separator will be used to
##' construct \code{sigIDname}.
##'
##' @return The dataframe, \code{df}, with a column indicating the signature systems
##' added to it. The mapping of \code{groupVars} to \code{sigIDname} is captured succinctly
##' as a dataframe in the attributes of the returned dataframe.  See example.
##'
##' @examples
##' # Create a dataframe with 3 grouping variables
##' x <- expand.grid(a = 1:3, b = letters[1:2], c = c(TRUE, FALSE))
##' x <- rbind(x, x)
##' x$b <- as.character(x$b)
##' x$z <- rnorm(NROW(x))
##'
##' # Create the signatureID
##' y <- createSignatureID(x, c("a","b","c"))
##' print(y)
##' attributes(y)$groupVarMapping

createSignatureID <- function(df, groupVars, sigIDname = "signatureID", separator = NULL) {

  # Verify the types of the inputs
  stopifnot(is.data.frame(df),
            is.character(groupVars),
            is.character(sigIDname),
            length(sigIDname) == 1,
            all(groupVars %in% colnames(df)))

  # If sigIDname exists in the dataframe, append '.original' to it
  if (sigIDname %in% colnames(df)) {
    colnames(df)[which(colnames(df) == sigIDname)] <- paste(sigIDname, "original",
                                                            sep = ".")
  }

  # Initialze gVars, where we'll hold the mapping of gVars to signatureID
  gVars <- NULL

  # If only one group variable is supplied
  if (length(groupVars) == 1) {
    
    if (sigIDname == groupVars) {
      df[,sigIDname] <- as.character(df[,paste(sigIDname, "original", sep = ".")])
      gVars <- df[, c(groupVars, paste(sigIDname, "original", sep = "."))]
    }
    
    else
      df[,sigIDname] <- as.character(df[,groupVars])
  }

  # If more than one group variables are supplied
  else {

    if (!is.null(separator))
      stopifnot(is.character(separator))

    # Identify the separator
    else {
  
      # Identify a unique separator for the variables
      idText <- paste(df[, groupVars], collapse = "")
    
      sepCandidates <- c(".", "_", "-", ":", "+", "*")
    
      # If all of these strings are present, no separator will be used
      separator <- ""

      # Attempt to assign a separator
      for (s in sepCandidates) {
        if (!grepl(s, idText, fixed = TRUE)) {
          separator <- s
          break
        }
      }
    }
  
    # Now create the id variable
    textToEval <- paste("with(df, paste(", paste(groupVars, collapse = ", "),
                        ", sep = separator))", sep = "")

    df[, sigIDname] <- eval(parse(text = textToEval))
  }
    
  # Let a mapping of the groupVars and the sigIDname tag along
  if (is.null(gVars))
    gVars <- df[, c(groupVars, sigIDname)]

  # Create the attribute mapping
  attributes(df)$groupVarMapping <- gVars[!duplicated(gVars),]

  # Return the dataframe
  return(df)

} # createSignatureID
