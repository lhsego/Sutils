# Tells you the status of the memory usage
memStatus <- function(units=c("GB","MB","KB","bytes"), verbose=TRUE) {

  if (.Platform$OS.type != "windows")
    stop("memStatus() is only supported on Windows\n")

  units <- match.arg(units) 
  div <- switch(units, GB=1073741824, MB=1048576, KB=1024, bytes=1) / 1048576
      
  used <- memory.size()/div
  limit <- memory.limit()/div
  available <- limit - used
  used.pct <- 100*round(used/limit, 2)
  available.pct <- 100*round(available/limit, 2)

  if (verbose)
    cat("     Used = ", round(used, 2), " ", units, "  (", used.pct, "%)\n",
        "Available = ", round(available, 2), " ", units, "  (", available.pct, "%)\n",
        "    Limit = ", round(limit, 2), " ", units, "\n", sep="")

  invisible(list(used=used, available=available, limit=limit, units=units))
  
} # end memStatus()


# Sets the memory size 
setMem <- function(req.size, units=c("GB","MB","KB","bytes"), verbose=TRUE) {

  if (.Platform$OS.type != "windows")
    stop("setMem() is only supported on Windows\n")
  
  units <- match.arg(units)
  
  # Convert to MB if necessary
  if (units != "MB") {
    factor <- switch(units, GB=1073741824, MB=1048576, KB=1024, bytes=1)
    req.size <- req.size * factor / 1048576
  }

  # Record the memory prior to increasing
  before <- memStatus(units=units, verbose=FALSE)

  # Increase the memory
  memory.limit(size=req.size)

  # Record the results after increasing the memory
  after <- memStatus(units=units, verbose=FALSE)

  if (verbose)
    cat("Memory limit increased from", before$limit, before$units,
        "to", after$limit, after$units, "\n")

  # Return the results of the change if requested
  invisible(list(before=before, after=after))

} # end setMem  
