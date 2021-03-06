\name{betaGof}
\alias{betaGof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chi-sqared goodness of fit (GOF) test for the Beta distribution.
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function performs a Chi-squared GOF test for fitting distributions
from the Beta family. 
  
}
\usage{
betaGof(X, theta, nbins = NULL, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
    %%     ~~Describe \code{X} here~~
    The data hypothosized to be generated by a Beta distribution.
}
  \item{theta}{
    %%     ~~Describe \code{theta} here~~
    A 2 by 1 vector of Beta parameters.
}
  \item{nbins}{
    %%     ~~Describe \code{nbins} here~~
    The number of bins used in the computation of the Chi-squared test
    statistic. If NULL the function computes nbins automatically.
}
  \item{alpha}{
    %%     ~~Describe \code{alpha} here~~
    The significance level required to declare the alternative.
}
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
   The test is constructed by dividing the support for the Beta
   distribution into bins and computed a Chi-squared statistic by the
   following formula:

   \deqn{\chi^{2^*}} = \Sigma_{i = 1}^{nbins}\frac{(O_i -
       E_i)^2}{E_i}}{%
     chi.star = sum_{i=1}^{nbins} (Obs_i - Exp_i)^2/Exp_i
   }
   where \eqn{O_i} are the observed bin counts and \eqn{E_i} are the
   expected bin counts.
  
   Equal probability bins are constructed such that the expected counts
   in the bins are at least 5. Twenty bins are used at maximum. The function
   produces an error if the number of data is such that nbins cannot be
   constructed to ensure at least 1 degree of freedom so that the
   expected frequencies in each bin is at least 5.

   The fitted Beta is deemed reasonable if the p-value > alpha. The
   p-value is reported for your interpretation.   
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{p.value}{
 The p-value of the test.
}
\item{conlusion}
{
 Character string providing the conclusion of the test indicating
 whether the fitted Beta distribution is reasonable.
}
\item{chi.star}{
 The value of the Chi-squared test statistic.
}
\item{nbins}{
 The number of bins used in the calculation of the Chi-squared test
 statistic. Degrees of freedom for this test is Df = nbins - 3.
}
\references{
  %% ~put references to the literature/web site here ~
  Sheskin, David J. Handbook of Parametric and Nonparametric Statistical
  Procedures - Fourth Edition. Chapman and Hall Publishers 2007. Boca Raton,
  FL. 
}
\author{
  %%  ~~who you are~~
  Stephen J. Walsh - Pacific Northwest National Laboratory. October 12 2010. 
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{
 This function comes with no guarantees (expressed or implied) - viewer
 discretion is advised.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- rbeta(62,5,.2)
betaGof(X = x,theta = c(5,.2))

y <- rbeta(77,1,10)
betaGof(X = y,theta = c(5,.2))



##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(X,
		    theta,
		    nbins = NULL,
	            alpha = 0.05)
{
 #Author: Stephen J. Walsh
 #Date: 10/08/2010

 #H0: Data come from fitted beta distribution.
 # large p.value provides no evidence against this hypothesis.

  X <- X[!is.na(X)]
  N <- length(X)
 #num params
  k <- length(theta)

 #get bins of equal probability: don't run test with fewer 3 or fewer bins -- need at least 1 DF
  if(is.null(nbins))
  {
    nbins <- floor(min(N/5,20))
  }
   
  if(nbins < 4) stop("Not enough data to perform the test!!") 

  eq.prob <- 1/nbins
  eq.prob.seq <- seq(0,1,by = eq.prob)
  bins <- qbeta(eq.prob.seq,shape1 = theta[1],shape2 = theta[2])
 
 #get obs.freqs
 
 obs.f <- NULL
 for(i in 1:nbins)
 {
  if(i == 1)
  {
    obs.f.i <- sum(X >= bins[i] & X <= bins[i+1]) 
  }else{
   obs.f.i <- sum(X >bins[i] & X <= bins[i+1])
  }  
  obs.f <- c(obs.f,obs.f.i)
 }

 #compute expected number in bins under estimated model
  exp.f <- round(N*eq.prob)
 #chisq test stat
  df <- nbins - k - 1
  chi.t <- sum(((obs.f - exp.f)^2)/exp.f)
  p.value <- 1 - pchisq(chi.t,df)
  if(p.value > alpha)
  {
   conclusion <- "Fitted Beta is Reasonable"
  }else{
   conclusion <- "Fitted Beta is Not Reasonable"
  }
  list(p.value = p.value,
       conclusion = conclusion,
       chi.star = chi.t,
       nbins = nbins)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Beta Distribution }
\keyword{ ~Goodness of Fit }% __ONLY ONE__ keyword per line
