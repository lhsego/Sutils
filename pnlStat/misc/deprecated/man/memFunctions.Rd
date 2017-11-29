\name{Memory Functions}
\alias{setMem}
\alias{memStatus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Report the memory usage and increase the memory usage for Windows
systems}
\description{Functions that can set the memory limit and
  report the memory usage, the amount of free
  memory that is available for use, and the current memory limit.
}
\usage{
memStatus(units = c("GB", "MB", "KB", "bytes"), verbose = TRUE)
setMem(req.size, units = c("GB", "MB", "KB", "bytes"), verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{req.size}{The new requested memory limit}
  \item{units}{A character string long enough to uniquely identify one
               of  "GB","MB", "KB", or "bytes", defaults to "GB"}
  \item{verbose}{\code{=TRUE} prints information regarding the memory status}
}
\details{\code{memStatus} reports the memory status.
  \code{setMem} requests a new memory limit.
}
\value{
  \code{memStatus} and \code{setMem} invisibly return lists with the
  following components
  \item{memStatus list:}{}
  \item{used}{Amount of memory currently in use}
  \item{available}{Amount of free memory available for use}
  \item{limit}{The memory limit}
  \item{units}{Memory units}
  \item{setMem list:}{}
  \item{before}{List from \code{memStatus} indicating the memory status
    prior to increasing the memory}
  \item{after}{List from \code{memStatus} indicating the memory status
    after increasing the memory}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# See the memory status
memStatus()
memStatus(units="MB")
v1 <- memStatus(units="K", verbose=FALSE)
v1

# Increase the memory limit by 10 KB
setMem(v1$limit+10, units="KB")

# Increase the memory limit by 0.1 GB
v2 <- memStatus(verbose=FALSE)$limit
v3 <- setMem(v2 + 0.1, verbose=FALSE)
v3
}
\keyword{misc}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
