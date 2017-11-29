\name{calcIEWMA}
\alias{calcIEWMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the IEWMA chart.
}
\description{
Calculate the one-sided IEWMA chart statistics which can then be plotted and
compared against a threshold.
}
\usage{
calcIEWMA(Y, mu0, lambda, h, reset = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The data, in chronological order.  If this vector has names, they are
returned in the IEWMA chart statistic.
}
  \item{mu0}{
The historical, in-control mean (or target value, i.e. it could be a median).
}
  \item{lambda}{
The weight of the exponential weighting function.  The larger the value
of \code{lambda},
the more the chart behaves like a Shewhart chart, i.e., it will be more
sensitive to large shifts.  The smaller the value of \code{lambda}, the
more sensitive the chart will be to detecting smaller shifts.
Should be in (0, 1].
}
  \item{h}{
The control limit (alarm threshold).
}
  \item{reset}{
\code{=TRUE} resets the IEWMA chart statistic to \code{mu0} after a
signal.  If \code{FALSE}, the chart continues on as if no signal had occured.
}
}
\details{
  The IEWMA statistic, X, is given by
  \eqn{
    X[i] = (1-lambda) * X[i-1] + lambda * max(Y[i], mu0)
  }
  where X[0] is set to \code{mu0}.
}
\value{
A list with the following components
\item{iewma}{The vector of IEWMA chart statistics}
\item{mu0}{The value of \code{mu0}}
\item{h}{The value of the control limit}
}
\references{
Shu L, Jiang W, Wu S. 2007.  "A One-Sided EWMA Control Chart for
Monitoring Process Means."  Communications in Statistics--Simulation and
Computation.  36:901-920.
}
\author{
Landon Sego
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ShewIewmaSim}}
}
\examples{

calcIEWMA(rnorm(50), 0, 0.1, 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
