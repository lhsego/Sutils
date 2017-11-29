\name{ShewIewmaSim}
\alias{ShewIewmaSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Esimate the Average Run Length of the combined Shewhart and IEWMA charts
}
\description{
Esimate the Average Run Length of the combined Shewhart and IEWMA charts
via non-parametric bootstrap simulation.  
}
\usage{
ShewIewmaSim(Y, mu0, lambda, h.e, h.s = NULL, nrep = 10^5, ncores = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The in-control data vector, in chronological order.  Ideally, the length of this vector should be, say, 5x
longer than the resulting ARL.
}
  \item{mu0}{
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
  \item{h.e}{
The control limit for the IEWMA chart.
}
  \item{h.s}{
The control limit for the Shewhart chart.  Typically, we'd expect that
\code{h.s} > \code{h.e}.  If \code{h.s = NULL}, the Shewhart control
limit is not applied, and thus only the IEWMA chart is run.
}
  \item{nrep}{
The number of Monte-Carlo replicates used to simulate the ARL.
}
  \item{ncores}{
The number of cores used to conduct the simulation. One core seems to be
the fastest for most situations.
}
  \item{\dots}{
Additional arguments to \code{\link{plapply}}.
}
}
\details{
The combined IEWMA chart (Shu et.al 2007) and Shewhart chart are
considered to raise a signal if the IEWMA chart statistic crosses
\code{h.e} or the data point \code{Y} crosses \code{h.s}.
  
The bootstrap simulation is accomplished by randomly selecting an index
in the data vector, \code{Y}, and then starting the control chart(s) at
that index.  The charts are run until a signal, and the resulting run
length is recorded.  This process is repeated \code{nrep} times.  If the
chart reaches the end of the data vector \code{Y} without signalling,
then the chart continues on at the first index of the data vector.  For
this reason, it's advantageous that the length of \code{Y} be
considerably longer than the resulting ARL, otherwise, the simulation
may be 'wrapping' through the data vector too many times.

This bootstrapping approach bears some to block resampling and the
stationary boostrap especially since the distribution of the ARL is
geometric. See Chernik (1999) for more details.
}
\value{
A list with the following components
\item{nrep}{The number of Monte-Carlo replicates}
\item{arl}{The estimated Average Run Length}
\item{se.arl}{The standard error of the ARL estimate}
\item{pct.ewma}{The percentage of runs where the IEWMA chart (as opposed
to the Shewhart chart) was responsible for the signal.  This may be
useful in tuning the two control limits}
\item{time.sec}{The elapsed time for the ARL calculation, in seconds}

}
\references{
Shu L, Jiang W, Wu S. 2007.  "A One-Sided EWMA Control Chart for
Monitoring Process Means."  Communications in Statistics--Simulation and
Computation.  36:901-920.

Chernik, M.R. 1999. Bootstrap Methods:  A Practitioner's Guide. Wiley & Sons.  pp 95-96.
}
\author{
Landon Sego
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{calcIEWMA}}, \code{\link{ShewIewmaCL}}
}
\examples{
ShewIewmaSim(rexp(10000), 0.5, 0.1, 1.5, h.s=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
