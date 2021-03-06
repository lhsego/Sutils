% Generated by roxygen2 (4.0.2): do not edit by hand
\name{calcConf}
\alias{calcConf}
\title{Calculates the Bayesian posterior probability of the number of contaminated
cells being less than a threshold}
\usage{
calcConf(n2, maxN, ..., arg.check = TRUE, discrete = FALSE)
}
\arguments{
\item{n2}{The number of random samples}

\item{maxN}{A large integer indicating the transition from finite population
to infinite population formulas.}

\item{arg.check}{\code{=TRUE} checks the arguments supplied in \code{...}
using \code{\link{cjrParms}}.  I would not recommend setting this to
\code{FALSE}.}

\item{discrete}{\code{=TRUE} uses \code{\link{calcConf.discrete}} to
calculate the confidence using the Binomial cdf (rather than the continuous
approximation of the Bionomial cdf)}

\item{\dots}{Arguments to \code{\link{cjrParms}}}
}
\value{
The value of the Bayesian confidence
}
\description{
Using the posterior predictive distribution, calculates the probability that
the number of contaminated cells is less than a determined threshold, given
that none of the judgmental nor random samples detected the presence of
contamination.
}
\details{
This serves as a wrapper for \code{\link{calcConf.raw}}.
}
\examples{
calcConf(239, 10^6, n1=10, N=25000, pct.clean=0.99, r=2, prior.prob=0.05)
calcConf(239, 10^6, n1=10, N=25000, pct.clean=0.99, r=2, beta=19)
}
\author{
Landon Sego
}
\seealso{
\code{\link{calcConf.raw}}, \code{\link{calcConf.discrete}}
}
\keyword{misc}

