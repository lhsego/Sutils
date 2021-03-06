% Generated by roxygen2 (4.0.2): do not edit by hand
\name{find.n2.viable}
\alias{find.n2.viable}
\title{Finds the required random sample size, ensuring a viable pct.clean is used}
\usage{
find.n2.viable(conf, ..., maxN = 10^6, decimal.precision = 5,
  round.up = FALSE, discrete = FALSE)
}
\arguments{
\item{conf}{A number in (0,1) giving the desired Bayesian confidence}

\item{maxN}{A large integer indicating the transition from finite population
to infinite population formulas.}

\item{decimal.precision}{The level of precision for the viable lambda,
passed to \code{\link{find.viable.lambda}}}

\item{round.up}{\code{=TRUE} rounds the random sample size up to the nearest
integer.}

\item{discrete}{\code{=TRUE} uses \code{\link{calcConf.discrete}} to
calculate the confidence when determining the sample size.}

\item{\dots}{Arguments to \code{\link{cjrParms}} (except do not pass in a
value for \code{arg.check})}
}
\value{
A list with the following components: \item{n2}{The required random
sample size} \item{conf.achieved}{The acheived level of confidence}
\item{prior.pct.clean}{The expected value of the percentage clean prior to
collecting data, which depends the choices for \code{N}, \code{n1},
\code{prior.prob} (or \code{beta}), and \code{r}.}
\item{requested.pct.clean}{The pct.clean that was requested in the call to
\code{find.n2.viable}} \item{use.pct.clean}{The pct.clean that was actually
used to calculate the required random sample size.  This will be greater
than or equal to \code{requested.pct.clean}.}
}
\description{
Finds the required random sample size and automatically ensures a viable
pct.clean is used.
}
\details{
Calls \code{\link{find.n2}}, and if the requested \code{pct.clean} is
viable, then it returns the results.  If it isn't viable,
\code{\link{find.n2}} is called a second time using the recommended
\code{pct.clean} that was identified by the first call.
}
\examples{
find.n2.viable(0.95, N=10000, n1=20, r=2, prior.prob=0.01, pct.clean=0.98)

find.n2.viable(0.97, n1=13, r=1.5, pct.clean=0.99, beta=35, N=2500, round.up=TRUE)
}
\author{
Landon Sego
}
\seealso{
\code{\link{find.n2}}
}
\keyword{misc}

