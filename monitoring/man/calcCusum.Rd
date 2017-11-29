\name{calcCusum}
\alias{calcCusum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates a sequence of CUSUM statistics}
\description{
Calculates a sequence of one-sided upper CUSUM statistics given the reference value and
the control limit.
}
\usage{
calcCusum(X, k, h, initial = 0, type = "upper", reset = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{The raw data}
  \item{k}{The reference value}
  \item{h}{The upper control limit}
  \item{initial}{Starting value for the CUSUM}
  \item{type}{Only \code{type="upper"} is currently supported.
    \code{type="lower" or "both"} may be supported in the future.}
  \item{reset}{\code{=TRUE} resets the CUSUM to zero after
    a signal}
}
\details{
CUSUM is assumed to be of the form: C[i] = max(0, C[i-1] + X[i] - k),
where the signal occurs when C[i] > h.  Note that \code{X} can be the CUSUM scores
(given by the log-likelihood ratio) with \code{k=0}.
}
\value{
  A list of class "cusum" with the following components
  \item{data}{The values that were suplied to the function as \code{X}}
  \item{h}{The control limit}
  \item{cusum}{The CUSUM statistics, C[i]}
  \item{stagger}{The lag of the CUSUM statistics, C[i-1], that can be
    used to calculate the total hazard.}
  \item{resetCounter}{An vector of integers corresponding to \code{cusum}
  that indicates when the the CUSUM resets}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
  calcCusum(rnorm(50), 0.2, 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
