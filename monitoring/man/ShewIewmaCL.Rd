\name{ShewIewmaCL}
\alias{ShewIewmaCL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation of ARLs for a matrix of IEWMA and Shewhart and
control limits
}
\description{
Non-parametric bootstrap simulation of ARLs for an upper triangular matrix of IEWMA and Shewhart and
control limits
}
\usage{
ShewIewmaCL(Y, mu0.hat, lambda,
            h.e.seq = seq(round(mu0.hat, 1) + 0.1, 25, by = 1),
            h.s.seq = seq(round(mu0.hat, 1) + 0.1, 25, by = 1),
            nrep = 10^5, ncores = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The in-control data vector, in chronological order.  Ideally, the length of this vector should be, say, 5x
longer than the resulting ARL.
}
  \item{mu0.hat}{
The target value (mu0) for the IEWMA.  Typically the mean or median of the
in-control data (from which outliers have been removed).
}
\item{lambda}{
The weight of the exponential weighting function.  The larger the value
of \code{lambda},
the more the chart behaves like a Shewhart chart, i.e., it will be more
sensitive to large shifts.  The smaller the value of \code{lambda}, the
more sensitive the chart will be to detecting smaller shifts.
Should be in (0, 1].
}
  \item{h.e.seq}{
   A sequence of control limits for the IEWMA chart
}
  \item{h.s.seq}{
   A sequence of control limits for the Shewhart chart
}
  \item{nrep}{
The number of Monte-Carlo replicates used to simulate the ARL
}
\item{ncores}{
  The number of cores used to conduct the simulation
}
}
\details{
  The ARL is simulated using \code{\link{ShewIewmaSim}}
  for all combinations of the \code{h.e.seq} and
\code{h.s.seq} sequences, subject to the constraint that \code{h.e <
  h.s}
}
\value{
  A data frame with the following components:
  \item{h.e}{The control limit of the IEWMA chart}
  \item{h.s}{The control limit of the Shewhart chart}
  \item{nrep}{The number of Monte-Carlo reps used to simulate the ARL}
  \item{arl}{The estimate of the Average Run Length}
  \item{se.arl}{The standard error of the ARL estimate}
  \item{pct.ewma}{The percentage of the Monte-Carlo reps where the EWMA
    chart signaled (as opposed to the Shewhart chart signalling)}
  \item{time.sec}{The duration of the simulation time for that case, in seconds}
}
%\references{
%% ~put references to the literature/web site here ~
%}
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
y <- rexp(10^4)
ShewIewmaCL(y, 0.5, 0.1,
            h.e.seq = seq(1, 2, by=0.25),
            h.s.seq = 3:7,
            ncores = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
