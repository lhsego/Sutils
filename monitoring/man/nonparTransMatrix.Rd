\name{nonparTransMatrix}
\alias{nonparTransMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate the probability transistion matrix using a
  nonparametric estimate of the density}
\description{
  Calculate the probability transition matrix for a continuous random
  variable using a logspline estimate of the density.
}
\usage{
nonparTransMatrix(fit, h, k, numPartitions = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{Object of class 'logspline', returned by 'logspline()'}
  \item{h}{The control limit}
  \item{k}{The reference value}
  \item{numPartitions}{One less than the number of states in the Markov Chain}
}
\details{
  Could be _easily_ adapted for any CUSUM that follows the basic form:
  C[i] = max(0, C[i-1] + X[i] - k).  This function is called by \code{\link{gammaCusumARL}}.
}
\value{
  The transition probability matrix (contains the transient states only)
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{gammaCusumARL}}, ~~~ }
\examples{

  x <- rgamma(500, shape=1.7, scale=8)
  nonparTransMatrix(logspline(x), 9, 7, numPartitions=10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
