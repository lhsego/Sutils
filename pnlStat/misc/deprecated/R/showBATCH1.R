showBATCH1 <- function() {

  cat("To create 'BATCH1' from 'BATCH', near the end of the 'BATCH' file, change\n\n",
      "out=${2-`basename ${1} .R`.Rout}\n\n",
      "to\n\n",
      "out=${2-`basename ${1} .R`_`date +%Y%m%d%H%M%S%N`.Rout}\n\n",
      "and save the file as 'BATCH1'.\n")

} # showBATCH1
