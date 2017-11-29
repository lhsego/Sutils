\name{multiRbind}
\Rdversion{1.1}
\alias{multiRbind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Row bind multiple data frames together that exist as separate .Rdata files
}
\description{
Row bind multiple data frames together that exist as separate .Rdata
files in a single directory and compare the result to a source data
frame if desired.  
}
\usage{
multiRbind(dataDir, pattern=NULL, outFile = NULL, compareSource = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataDir}{
    Character string indicating the directory where the \code{.Rdata}
    files reside.
      
  }
  \item{pattern}{
Character string that will select specific \code{.Rdata} files in the
directory for row binding. This is passed to the \code{pattern} argument in \code{\link{dir}}.
If \code{NULL}, all the \code{.Rdata} files are selected.
    }
  \item{outFile}{
    Character string indicating the filename to which the combined data
    frames will be saved.  If \code{NULL}, no file is written.
}
  \item{compareSource}{
Data frame that will be compared using \code{\link{dframeEquiv}} against the common columns of the
rbinded data frame. If \code{NULL}, no comparison is made.
\code{compareSource} can be in any format that is acceptable to \code{\link{dataIn}}.
}
}
\details{
\code{multiRbind} presumes that each file in \code{dataDir} contains a
single data frame, that all these data frames have the same name and
number of columns (i.e. ready to \code{\link{rbind}} together).

The \code{compareSource} option is valuable for verifying that the
results from \code{\link{multiParallel}} match their source data frame
as expected.  When a source data frame is provided (via \code{compareSource}, and if
the combined data are equivalent to the the source data, then the
combined data frame is ordered (by the rownames) so that is matches the
source data.
}
\value{
If \code{outFile} is provided, the combined data frame is saved.  Also a
list with the following components is returned invisibly:
\item{d}{The row binded data frame}
\item{compareSource}{The output list of \code{\link{dframeEquiv}} if the
\code{compareSource} argument in not \code{NULL}}
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
\code{\link{rbind}}, \code{\link{smartRbindMat}}, \code{\link{multiParallel}}
}
\examples{
# Refer to the example in \code{\link{multiParallel}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
