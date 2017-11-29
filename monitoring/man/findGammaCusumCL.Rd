\name{findGammaCusumCL}
\alias{findGammaCusumCL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find the control limit of the Gamma Cusum}
\description{
  Find the control limit that results in the smallest in-control ARL
  that is at least as large as the target in-control ARL.  It can find
  control limits for Gamma CUSUMS, as well as any CUSUM whose in-control
  distribution can be estimated by 'logspline()'.
}
\usage{
findGammaCusumCL(target, fit, k, upper.lim = 100, lower.lim = 2,
                 firstPartition = 100, secondPartition = 500,
                 check = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target}{The target in-control ARL}
  \item{fit}{Either the object returned by \code{logspline} (the
    non-parametric density fit), or a list
    with names 'shape' and 'scale' that specifies the in-control
    Gamma distribution
    that will be used to calculate the transition probabilities}
  \item{k}{The reference value}
  \item{upper.lim}{The target upper bound for the control limit}
  \item{lower.lim}{The target lower bound for the control limit}
  \item{firstPartition}{One less than the number of Markov chain states
    used to calculate the ARL during the first pass.}
  \item{secondPartition}{One less than the number of Markov chain states
    used to calculate the ARL during the second pass.}
  \item{check}{\code{=TRUE} checks the in-control ARL of the solution of \code{h}
    using simulation}
  \item{verbose}{\code{=TRUE} print information as the function searches
  for the control limit.}
}
\details{
  A coarse first pass is made with less precision in hopes of identifying a
  fairly narrow interval in which the true control limit exists.  The
  second pass works within the identified interval to locate the control
  limit with higher precision.  If \code{lower.lim} or \code{upper.lim}
  are too high or too low, respectively, then they are extended and the
  coarse first search is repeated.
}
\value{
  A list with the following components:
  \item{h}{Identified control limit }
  \item{arl.0.a}{The achieved in-control ARL that corresponds to the
    identified value of \code{h}}
  \item{arl.0.a.sim}{If \code{check=TRUE}, the object returned by
    \code{\link{gammaCusumSim}}.  Otherwise, \code{NULL}.}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{gammaCusumARL}}, \code{\link{gammaCusumSim}}}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
