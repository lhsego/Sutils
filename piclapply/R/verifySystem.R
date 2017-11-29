# Verify the PIC system being used is one of the acceptable systems

verifySystem <- function(AcceptableSystemNames) {

  # Get the system being used
  systemName <- Sys.getenv("SYSTEM_NAME")

  # Prepare error message
  errMsg <- paste("You must be using the following system(s) to call this function: ",
                  paste(AcceptableSystemNames, collapse = ", "), sep = "")

  # Stop and print error message as requested if the system doesn't match one of
  # the recognized acceptable systems
  Smisc::stopifnotMsg(systemName %in% AcceptableSystemNames, errMsg, level = 2)

  # Invisibly return the system name
  invisible(systemName)

}

