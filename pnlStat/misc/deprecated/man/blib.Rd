\name{blib}
\alias{blib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Builds a package within R on martingale.pnl.gov
}
\description{
Builds a package within R by 1) detaching the package if it's already
loaded, building the package using
'martingale:/usr/local/scripts/blibcp', and then finally loads the
package.
}
\usage{
blib(lib)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lib}{
Name of the package or a character string indicating the name of the
package.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%}
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
# Doesn't actually work for pnlStat, but will for others
# blib(pnlStat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
