\name{gammaCusumSim}
\alias{gammaCusumSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulates the average run length of a (Gamma) CUSUM}
\description{
  Simulates the average run length (using variance reduction techniques) 
  of a CUSUM using the
  in-control parameters of the Gamma distribution, or, using the
  logspline fit of the distribution from any data set.
}
\usage{
gammaCusumSim(fit, k, h, reps = 10^4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{Either the object returned by \code{logspline} (the
    non-parametric density fit), or a list
    with names 'shape' and 'scale' that specifies the Gamma distribution
    that will be simulated}
  \item{k}{The reference value of the CUSUM}
  \item{h}{The control limit of the CUSUM}
  \item{reps}{The (minimum) number of Monte-Carlo replications}
}
\details{
  When \code{fit} is a 'logspline' object, more than the requested
  number of Monte-Carlo replications will be returned. (This is a
  byproduct of attempting to improve the efficiency of the simulation).

  In addition, if \code{fit} is a 'logspline' object, a slightly biased,
  lower variance estimator of the ARL is calculated in addition to the
  regular ARL estimated (the sample average of the simulated run lengths). 

}
\value{
  A list with the following components. 
  \item{arl}{The traditional estimate of the average run length}
  \item{arl.rv}{The reduced variance estimate of the ARL}
  \item{se.arl}{The standard error of the traditional ARL estimate}
  \item{se.arl.rv}{The standard error of the reduced variance estimate of
    the ARL}
  \item{n}{The number of Cusum runs that were
    simulated (Monte-Carlo replicates)}
}
\references{
Jun, Chi-Hyuck and Choi, Moon Soo. (1993) Simulating the Average Run
Length for Cusum Schemes Using Variance Reduction
Techniques. _Communications in Statistics B:  Simulation and
Computation_.  22(3): 877-887.
}
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

 # Simulating the gamma distribution
 gammaCusumSim(list(shape=2, scale=15), 36, 196, reps=5000)

 # Simulating the distribution fit to 'historical' (or other) data
 X <- rgamma(500, shape=2, scale=15)
 fit0 <- logspline(X, lbound=0)
 gammaCusumSim(fit0, 36, 196, reps=5000)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
