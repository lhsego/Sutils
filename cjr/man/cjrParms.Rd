% Generated by roxygen2 (4.0.2): do not edit by hand
\name{cjrParms}
\alias{cjrParms}
\title{Check and insert CJR parameters into parent functions}
\usage{
cjrParms(n1 = NA, N = NA, pct.clean = NA, thresh = NA,
  prior.prob = NA, beta = NA, r = NA, fnr = 0, arg.check = TRUE)
}
\arguments{
\item{n1}{The number of judgment samples}

\item{N}{The total number of cells in the decision area}

\item{pct.clean}{Proportion of the decision area that needs to be clean. Can be specified
instead of \code{thresh}.}

\item{thresh}{A non-negative integer indicating the number of possible
unacceptable grid cells that will be tolerated.  Can be specified intead of \code{pct.clean}.}

\item{prior.prob}{The expected prior probability that a single judgment
sample location will contain detectable contamination}

\item{beta}{The second shape parameter of the prior distribution (which is a
Beta distribution).  Can be specified instead of \code{prior.prob}.  See
Details below.}

\item{r}{A high risk judgment sample location is expected to be \code{r}
times more likely to be contaminated than a low risk random sample location}

\item{fnr}{Numerical value in [0, 1) indicating whether the false negative rate for each sample.}

\item{arg.check}{\code{=TRUE} verifies that the choice for each parameter
(argument) is valid. \code{=FALSE} skips the verification. If the parameters have already been checked
and \code{cjrParms} is called multiple times (e.g. if a confidence function that uses \code{cjrParms} is
called multiple times in a sample size function), then it would make sense to set \code{arg.check = FALSE}
for subsequent calls to \code{cjrParms}.}
}
\value{
Assigns these objects into the environment of the function that
called \code{cjrParms} (the parent frame):
\describe{
\item{n1}{The number of judgment samples}
\item{N}{The total number of cells in the decision area}
\item{pct.clean}{Proportion of the decision area that needs to be clean}
\item{thresh}{The number of unacceptable grid cells that will be tolerated}
\item{prior.prob}{The prior probability that a single judgment sample location will contain
detectable contamination}
\item{beta}{The second shape parameter of the prior (which is a Beta distribution)}
\item{r}{A high risk judgment sample location is \code{r}
times more likely to be contaminated than a low risk random sample location}
\item{alpha}{The first shape parameter of the prior beta distribution, always set to 1}
\item{fnr}{The false negative rate}
}
}
\description{
Validates the correct user input of the CJR parameters and then assigns the
parameter objects into the environment (or scope) of the parent functions
which calculate Bayesian confidence, determine number of random samples,
etc.
}
\details{
\code{cjrParms} has little use in being called directly.  It is called by
other functions in the \code{cjr} package which pass arguments into
\code{cjrParms} using the \code{...} argument.

Either \code{prior.prob} or \code{beta} must be specified.  If both are
specified, \code{prior.prob} is ignored and the value of \code{beta} is
used.

Either \code{pct.clean} or \code{thresh} must be specified.  If both are
specified, \code{pct.clean} is ignored and the value of \code{thresh} is
used.

The parameterization of the Beta prior distribution is as follows, where
\code{alpha} \eqn{= a} and \code{beta} \eqn{= b}.  The density is given by
\deqn{f(x)=\frac{\Gamma(a+b)}{\Gamma(a)\Gamma(b)}{x}^{a} {(1-x)}^{b}%
}{Gamma(a+b)/(Gamma(a)Gamma(b))x^(a-1)(1-x)^(b-1)}\deqn{
}{Gamma(a+b)/(Gamma(a)Gamma(b))x^(a-1)(1-x)^(b-1)} for \eqn{a > 0}, \eqn{b >
0} and \eqn{0 \le x \le 1}{0 <= x <= 1} where the boundary values at
\eqn{x=0} or \eqn{x=1} are defined as by continuity (as limits).

For all CJR models, \code{alpha = 1}.
}
\examples{
# Checking the parameters and making the parameters available as objects inside a function:
fun1 <- function(...) {

  # Check the cjr parameters and assign them to the environment of fun1
  cjrParms(...)

  # Print a few of the objects that now reside in this environment
  pvar(n1, N, r, pct.clean, thresh, prior.prob, beta, fnr)

 } # fun1

fun1(n1 = 10, N=1000, pct.clean=0.99, prior.prob = 0.25, r=3)

# See what happens when not all the arguments are provided correctly:
try(fun1(n1 = 10, N = 1000))
try(fun1(n1 = 10001, N = 1000, pct.clean = 0.99, prior.prob = 1.25, r = -1, fnr = -0.3))
}
\author{
Landon Sego
}
\keyword{misc}

