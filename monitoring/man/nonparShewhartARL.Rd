\name{nonparShewhartARL}
\alias{nonparShewhartARL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nonparametrically estimates the Average Run Length of a one-sided upper Shewhart Chart
}
\description{
Nonparametrically estimates the Average Run Length of a one-sided upper  Shewhart Chart
}
\usage{
nonparShewhartARL(Y, h, method = c("upper", "lower", "both")) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The data vector
}
  \item{h}{
  The upper or lower control limit (or both, represented as a 2-vector),
  depending on the method. 
    
}
\item{method}{Specifies whether the chart is one-sided upper, one-sided
  lower, or two sided ('both').  Currently, 'upper' is the default and
  the only method that is supported.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The estimate of the ARL
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
nonparShewhartARL(rexp(500), 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
