\name{designGammaCusum}
\alias{designGammaCusum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Design a Gamma CUSUM chart}
\description{
Using historical data, design a gamma based CUSUM chart for
detecting increases in the scale parameter
}
\usage{
designGammaCusum(X, beta.shift, arl.0,
                 filter.data = TRUE, nonpar.fit = FALSE,
                 cheb.level.1 = 0.01, cheb.level.2 = 0.01,
                 unimodal = TRUE, exp.cheby = FALSE,
                 ic.parms = list(alpha = NULL, beta = NULL),
                 verbose = FALSE, title = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{vector of historical data points}
  \item{beta.shift}{a positive number indicating the magnitude in the
    shift of the scale parameter, beta. Process shifts from beta_0 to
    beta_1 = beta.shift * beta_0.}
  \item{arl.0}{the target, in-control ARL that will be used to set the
    control limit. If a vector is supplied, a control limit will be
    calculated for each element of 'arl.0'}
  \item{filter.data}{\code{=TRUE} uses the 2-tiered Chebyshev approach
    to remove out-of-control points from the historical data set.  If
    \code{filter.data=FALSE}, then \code{cheb.level.1}, \code{cheb.level.2},
    \code{unimodal}, and \code{exp.cheby} are not used.}
  \item{nonpar.fit}{\code{=TRUE} uses the logspline fit of the
    historical data to calculate the in-control ARL (when searching for the
    control limit).  Otherwise, the gamma distribution fit is used.}
  \item{cheb.level.1}{A numeric value between 0 and 1/6 indicating the
    probability to be used to identify the first cut of the upper-tailed
    2-tiered Chebyshev limit}
  \item{cheb.level.2}{a numeric value between 0 and 1/6 indicating the
    probability to be used to identify the second cut of the
    upper-tailed 2-tiered Chebyshev limit}
  \item{unimodal}{\code{=TRUE} assumes the historical data are unimodal
    and uses the unimodal Chebyshev limit}
  \item{exp.cheby}{\code{=TRUE} means that the Chebyshev limit
    will be calculated on the exponentiated data and then the
    logarithm of that limit will be reported}
  \item{ic.parms}{list of in-control estimate of the alpha (scale) and
    beta (shape) parameters for historical data.  If this is left null
    (default), the parameters are estimated from the historical data.}
  \item{verbose}{\code{=TRUE} lists datapoints that were considered
    out-of-control in the historical data set and produces a histogram
    and a Gamma qqplot that describe the fit of the gamma distribution
    to the historical data}
  \item{title}{A character string for the title of the plots (used only
    if \code{verbose=TRUE})}
  \item{\dots}{Additional arguments to \code{\link{findGammaCusumCL}}}
}
\details{
  The historical data are filtered using the 2-tiered Chebyshev outlier
  detection algorithm, to avoid including out-of-control data points
  when designing the chart. Maximum-likelihood is used to estimate the
  shape and scale parameters of the gamma distribution.  If
  \code{nonpar.fit=TRUE}, the gamma distribution fit is still used to
  determine the reference value, k, since the logspline densities give
  rise to very unruly log-likelihood functions.
}
\value{
  A list with the following components
  \item{h}{The control limit(s) that achieve(s) the target
    in-control ARL(s)}
  \item{k}{The reference value of the CUSUM}
  \item{alpha.0}{The in-control estimate of the gamma shape parameter}
  \item{beta.0}{The in-control estimate of the gamma scale parameter}
  \item{arl.0.a}{The achieved in-control ARL(s)}
  \item{fit}{The logspline fit if \code{nonpar.fit=TRUE} or a list
    containing the in-control estimates of the Gamma parameters
    otherwise.  This contains the distributional information used to
    compute the ARL.}

}
\references{
Hawkins & Olwell,(1998) _Cumulative Sum Charts and Charting for Quality
Improvement_, Springer-Verlag New York, Inc. pp. 91-93, 142-143.
}
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{findGammaCusumCL}} and \code{cheb.2step.od} (\code{pnlStat} library)}
\examples{

  X <- rgamma(500, shape=2, scale=7)
  design <- designGammaCusum(X, 1.5, arl.0=c(500,1000))
  print(design)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
