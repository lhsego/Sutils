\name{gammaCusumARL}
\alias{gammaCusumARL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates the initial state ARL for a (Gamma) CUSUM using a
  Markov chain}
\description{
  Calculates the initial state ARL for a Gamma CUSUM designed to detect
  a shift in the scale, or for any continuous CUSUM where the distribution
  of the data can be estimated using logspline().
}
\usage{
gammaCusumARL(fit, k, h, numPartitions = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{Either the object returned by \code{logspline} (the
    non-parametric density fit), or a list
    with names 'shape' and 'scale' that specifies the Gamma distribution
    that will be used to calculate the transition probabilities}
  \item{h}{The control limit}
  \item{k}{The reference value}
  \item{numPartitions}{One less than the number of states in the Markov Chain}

}
\details{
  Assumes the CUSUM is of the form C[i] = max(0, C[i-1] + X[i] - k) that
  signals when C[i] > h.
}
\value{
  The intial state ARL
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{gammaCusumSim}},\code{\link{nonparTransMatrix}}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
