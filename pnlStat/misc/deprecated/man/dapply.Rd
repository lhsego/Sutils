\name{dapply}
\alias{dapply}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Applies a function to a dataframe by parsing it and launching
  separate parallel jobs}
\description{
Applies a function to a dataframe by parsing the dataframe into roughly
equal sizes and then applying the function to each subset of the
dataframe, launched as separate, parallel, batch instances of R.
}
\usage{
dapply(dframe.infile, dframe.outfile, fun.file,
       fun.name, njobs = 7, max.hours = 24, check.interval.sec = 120,
       random.seed = NULL, clean.up = TRUE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dframe.infile}{String indicating the .Rdata file containing the
    input dataframe to be processed}
  \item{dframe.outfile}{String indicating the .Rdata file that will contain
    the output dataframe}
  \item{fun.file}{String indicating the file which contains the function
    that will be applied to the dataframe subsets}
  \item{fun.name}{String indicating the name of the function in
    \code{fun.file} that will be applied to each dataframe subset}
  \item{njobs}{The number of jobs (subsets), which should probably not exceed
  the number of cores on the machine}
  \item{max.hours}{The maximum number of hours the program will wait in
    order to concatenate the results together into a single dataframe}
  \item{check.interval.sec}{The number of seconds to wait before
    checking to see whether all \code{njobs} have completed}
  \item{random.seed}{An integer setting the random seed, which will
    result in a random assignment of the rows in the input data frame to
    the various jobs.
    If \code{NULL}, then the rows of the input data frame are subdivided
    sequentially among the jobs.}
  \item{clean.up}{\code{=TRUE} will delete temporary files}
  \item{verbose}{\code{=TRUE} prints messages which show the progress of
  the jobs.}
}
\details{
It applies a function to a dataframe by parsing the dataframe into roughly
equal sizes and then applying the function to each subset of the
dataframe.  A separate batch instance of R is launched for each subset,
thus utlizing another core of the machine.
After the jobs complete, the \code{njobs} subsets are reassembled into a
dataframe which should have the same size as the input dataframe.

\code{fun.name} should have a single argument, a dataframe--and it
should return a single object:  the modified dataframe.  Hence
\code{fun.name} modifies the dataframe in some way, typically by
calculating new quantities for each row of the dataframe and then
including the results into new columns of the output dataframe.  
\code{fun.name} should be coded so that if it were naively applied to
the entire data frame, it would produce the intended result
(albeit slowly on a single process).

Setting \code{random.seed} to an integer can be useful if the rows to
be processed in the data frame are likely to take different amounts of
time to complete.  Hence, randomizing the rows among the various jobs
increases the likelihood of having the jobs complete at roughly the same time.
}
\value{
  Writes the completed dataframe to the file designated by \code{dframe.outfile}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{plapply}}, \code{\link{multiParallel}}}
\examples{
## Simple example.  For larger jobs, you would want to launch dapply()
## in a script via batch mode.

d.in <- data.frame(x=1:200, y=abs(rnorm(200)))
save(d.in, file="d.in.test.Rdata")

# Create a file which contains the function that operates on each row of
# the large data frame.
# Note how dframe.f() takes a single argument, the dataframe, and it returns the
# modified dataframe.

cat("dframe.f <- function(X) {\n",
    "  X$z <- NA\n",
    "  for (i in 1:NROW(X))\n",
    "    X[i, 'z'] <- X[i, 'x'] + X[i, 'y']\n",
    "  return(X)\n",
    "}\n",
    sep="",
    file = "tmp.fun.R")

dapply("d.in.test.Rdata", "d.out.test.Rdata", "tmp.fun.R", "dframe.f",
       njobs=4, check.interval.sec=2, random.seed=381)

# Read in the results
d.res <- load.object("d.out.test.Rdata")

# Now try just running the dframe.f function not in parallel
source("tmp.fun.R")
d.res.1 <- dframe.f(d.in)

# Now compare the two:
dframeEquiv(d.res, d.res.1)


# After looking at the results, this will delete all the files
# created in this example:
# unlink(c("d.in.test.Rdata", "tmp.fun.R", "d.out.test.Rdata", "tmp.fun.Rout"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
