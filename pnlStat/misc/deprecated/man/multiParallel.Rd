\name{multiParallel}
\Rdversion{1.1}
\alias{multiParallel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function that can be called (in parallel) many times to process subsets of a data frame
}
\description{
An extension of \code{\link{dapply}} meant to run in large scale
environments, where this function is launched repeatedly (potentially
hundreds of times) by a scheduler (Unix/Linux only).
}
\usage{
multiParallel(source.df.file, index.file, out.dir, process.function, rows.to.process = 100, max.hours = 48, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source.df.file}{
Character string giving the \code{.Rdata} file which contains the data
frame to be processed
}
  \item{index.file}{
Character string giving the \code{.Rdata} file which contains a single
vector of rownames of of the data frame in \code{source.df.file}.  These
rownames are assumed to be a sequence of integers (as characters) from 1
to the number of rows in the source data frame.
}
  \item{out.dir}{
Character string indicating the output directory where results will be stored.
}
  \item{process.function}{
Function that will be used to process the data frame in
\code{source.df.file}.  This function should have a single argument, the
data frame, and return a single object, the modified data frame. 
}
  \item{rows.to.process}{
The number of rows to process at a time.  When these rows are completed,
another set of rows are selected from the source data frame for
processing. 
}
  \item{max.hours}{
The maximum number of hours for the program to run.
}
  \item{verbose}{
\code{=TRUE} prints information on the status of the processing
}
}
\details{
\code{multiParallel} is supported on the Unix/Linux platform only.
 
\code{multiParallel} processes the source data frame
\code{rows.to.process} at a time, continuing to process new batches of
rows until all have been completed. Multiple instances of
\code{multiParallel} use the \code{index.file} to keep track of which
rows have been selected for processing by other processes and which have
not.  Uses \code{\link{lockFile}} and \code{\link{unlockFile}} to
prevent simultaneous read/writes to \code{index.file}.

}
\value{
Nothing is returned.  Processed subsets of the source data frame are
saved to as \code{.Rdata} files to \code{out.dir}.  These files can be
easily reassembled into a single data frame that resembles the source
data frame by using \code{\link{multiRbind}}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Landon Sego
}
\note{

\code{multiParallel} is best used with \code{R CMD BATCH1}, where \code{BATCH1} is the same as
\code{BATCH}, except for a simple modification that is explained by running
running \code{showBATCH1()}  (I included it in it's own function because
it R didn't like compiling the .Rd because it contains special characters).

This change causes R to append a unique timestamp identifier to the
\code{.Rout} file, making it possible to distinguish between the
resulting log files of multiple calls to \code{multiParallel}.  Note
that \code{BATCH} resides in the installation of R in the \code{bin}
folder.

You may also need to change the permissions of \code{BATCH1} as follows:

\code{chmod a+x BATCH1}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dapply}}, \code{\link{lapply.parallel}}, \code{\link{multiRbind}}
}
\examples{

### Note this example was meant to run on Unix/Linux only

# Create a directory in which to work and decend into that directory
system("mkdir multiParallel.Example")
setwd("multiParallel.Example")

# Make a quick data frame and an index set, save them
d <- data.frame(a=rnorm(10^3), b=rnorm(10^3))
d.index <- row.names(d)

save(d, file="dSource.Rdata")
save(d.index, file="dSource.index.Rdata")

# Create an output directory
system("mkdir tmpOut")

# Define a function to process the data frame
processF <- function(dframe) {

  dframe$c <- NA

  for (i in 1:NROW(dframe))
    dframe[i,"c"] <- dframe[i,"a"] / dframe[i, "b"]

  return(dframe)

} # processF

# Save this function to a .Rdata file
save(processF, file="processF.Rdata")

# Now write a file that we can call in batch mode
cat("library(pnlStat)\n",
    "load('processF.Rdata')\n",
    "multiParallel('dSource.Rdata', 'dSource.index.Rdata', 'tmpOut', processF)\n",
    sep="", file = "sf.R")

# Now launch 6 parallel processes, routing the nohup.out to /dev/null
system("nohup R CMD BATCH1 --vanilla sf.R > /dev/null 2>&1 &")
system("nohup R CMD BATCH1 --vanilla sf.R > /dev/null 2>&1 &")
system("nohup R CMD BATCH1 --vanilla sf.R > /dev/null 2>&1 &")
system("nohup R CMD BATCH1 --vanilla sf.R > /dev/null 2>&1 &")
system("nohup R CMD BATCH1 --vanilla sf.R > /dev/null 2>&1 &")
system("nohup R CMD BATCH1 --vanilla sf.R > /dev/null 2>&1 &")


# When they are finished, use the following code to reassemble the results
# to.compare <- d
# to.compare$c <- to.compare$a / to.compare$b
# bound <- multiRbind("tmpOut", compareSource=to.compare)

# Clean up files and objects from the example
# setwd("../")
# system("rm -r multiParallel.Example")
# rm(d, d.index, processF, to.compare, bound)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
