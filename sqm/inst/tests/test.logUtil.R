test.logUtil <- function(z,
                    shift = 0,
                    zrange = range(z),
                    urange = c(0, 1)) {
  
  # Check the ranges
  stopifnot(is.vector(z),
            is.numeric(z),
            is.numeric(shift),
            length(shift) == 1,
            is.numeric(zrange),
            is.numeric(urange),
            length(zrange) == 2,
            length(urange) == 2,
            zrange[1] < zrange[2],
            all(z > shift),
            all(zrange > shift))
  
  util <- min(urange)+(max(urange)-min(urange))*(log(z - shift)-log(min(z) - shift))/(log(max(z) - shift)-log(min(z) - shift));
  
  # Set the attributes
  attributes(util) <- c(attributes(util),
                        list(saUtilMethod = "logUtil",
                             parms = list(z = z, shift = shift, zrange = zrange, urange = urange)))
  # Set class
  class(util) <- c("saUtilCall", class(util))
  
  # Restore names
  if (!is.null(nz <- names(z)))
    names(util) <- nz
  
  # Return the object
  return(util)
  
} # logUtil

check.logUtil <- function(){
  
  suppressPackageStartupMessages(require(SQM))
  
  if(!identical(test.logScore(esd),logScore(esd)))
    stop("The output of test.logScore and logScore is not exactly equal for the given input")
  else
    return(NULL)
}

check.logUtil()