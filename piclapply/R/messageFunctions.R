# Messaging functions to improve readability

Star <- function(...) {
  cat("************************************************************\n",
      "*****  ", paste(unlist(list(...)), collapse = ""), "\n",
      "************************************************************\n\n",
      sep = "")
}

star <- function(...) {
  cat("****  ", paste(unlist(list(...)), collapse = ""), "\n\n", sep = "")   
}

