% Generated by roxygen2 (4.0.2): do not edit by hand
\name{find.viable.lambda}
\alias{find.viable.lambda}
\title{Calculate the viable value of lambda that ensures a non-increasing
confidence function}
\usage{
find.viable.lambda(target.lambda, conf, n1, n2, r, beta, maxN,
  decimal.precision = 5, verbose = FALSE, show.plot = FALSE)
}
\arguments{
\item{target.lambda}{The starting value of \code{lambda}}

\item{conf}{The desired confidence level.}

\item{n1}{Number of judgmental samples}

\item{n2}{Number of random samples}

\item{r}{Ratio of prior expecation between judgmental and random samples}

\item{beta}{Second shape parameter of the beta prior of the judgmental
samples}

\item{maxN}{A large integer indicating the transition from finite population
to infinite population formulas.  This argument is now deprecated (not used)
but I've included it for backwards compatability with earlier versions.}

\item{decimal.precision}{The precision of the solution.}

\item{verbose}{Prints details showing the search This argument is now
deprecated (not used) but I've included it for backwards compatability with
earlier versions.}

\item{show.plot}{\code{=TRUE} makes a plot which illustrates the
identification of the viable percentage clean value.  This argument is now
deprecated (not used) but I've included it for backwards compatability with
earlier versions.}
}
\value{
The value of \code{lambda} which produces a decreasing confidence function
(wrt \code{N}).

If the value of \code{target.lambda} already produces a decreasing
confidence function, then that value is simply returned.  Otherwise, it
calculates the smallest value of \code{lambda} that is viable, according to
the theorem in the CJR report.
}
\description{
Calculate the viable value of \code{lambda} that ensure a non-increasing
confidence function.
}
\examples{
find.viable.lambda(0.99, 0.98, 10, 0, 3, 150, 10^6, verbose=TRUE, show.plot=TRUE)
}
\author{
Landon Sego
}
\references{
PNNL-19315
}
\seealso{
\code{\link{is.viable.lambda}}, \code{\link{n2.vs.viable.lambda}}
}
\keyword{misc}

