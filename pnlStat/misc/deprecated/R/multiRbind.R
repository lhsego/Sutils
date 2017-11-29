multiRbind <- function(dataDir, pattern = NULL, outFile = NULL, compareSource = NULL) {

  # Get all list of all .Rdata files in the directory
  files <- dir(dataDir, pattern = pattern, full.names = TRUE)

  # Select the .Rdata files pattern
  files <- files[grepl(".rdata", tolower(files))]

  # load all the objects into memory
  for (i in 1:length(files))
    assign(paste("d", i, sep = ""), load.object(files[i]))

  # rbind them all into 1
  rbind.text <- paste("rbind(", paste(paste("d", 1:length(files), sep = ""), collapse = ","), ")", sep = "")
  d.combined <- eval(parse(text = rbind.text))

  if (!is.null(compareSource)) {

    d.source <- dataIn(compareSource)

    # order the combined data frame by the rownames in compareSource if they match
    if (NROW(d.source) == NROW(d.combined)) {
      if (setequal(rownames(d.source), rownames(d.combined)))
        d.combined.reordered <- d.combined[rownames(d.source),]
    }

    d.equal <- dframeEquiv(d.source, d.combined.reordered[, colnames(d.source)])
    
    # If it is equivalent, then return the reordered data frame    
    if (d.equal$equiv) 
      d.combined <- d.combined.reordered
    
  }
  else
    d.equal <- NULL

  
  # Save the file
  if (!is.null(outFile)) {
    save(d.combined, file = outFile)
    cat("Combined data frames written to '", outFile, "'\n", sep = "")
  }
                           
  invisible(list(d = d.combined, compareSource = d.equal))

} # multiRbind
