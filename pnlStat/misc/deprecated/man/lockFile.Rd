\name{lockFile}
\Rdversion{1.1}
\alias{lockFile}
\alias{unlockFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lock and unlock files for reading and writing
}
\description{
Lock and unlock files for reading and writing in the Unix environment.
Especially  useful when multiple processes are trying to read and/or
write to/from a single file.
}
\usage{
lockFile(file, maxWait = 5, maxAttempts = 10^5, startindDelay = TRUE, verbose = FALSE)
unlockFile(lockDir, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Character string giving the filename to be locked (leading path optional)
}
\item{lockDir}{
Character string of class \code{fileLock} (the value returned by
\code{lockFile}) indicating the directory that will be removed and
thereby unlock the file
  }
\item{maxWait}{
The maximum number of seconds to wait before attempting to lock the file again
}
  \item{maxAttempts}{
The maximum number of attempts that will be made to lock the file
}
\item{startingDelay}{
\code{=TRUE} causes \code{lockFile} to wait (for a random amount of time
up to 10 seconds) before attempting to lock the file, thereby staggering
the locking of many processes that may be trying to access the file at
the same time.
  }
\item{verbose}{
\code{=TRUE} prints details
}
}
\details{
\code{lockFile} and \code{unlockFile} are only supported on the Unix platform.
  
\code{lockFile} "locks" a file by creating a unique, empty directory based
on the filename.  Other processes that are trying to access the file
(via \code{lockFile}) wait until the directory is removed by \code{unlockFile}.

\code{unlockFile} removes the directory, thereby making it possible for
other processes to lock the file.

In order to stagger the lock requestes from multiple processes, \code{lockFile} waits a random amount of time
(between 0 and \code{maxWait} seconds) after each locking attempt.
}
\value{
  \code{lockFile} returns a character string (of class \code{fileLock})
  indicating the name of the locking directory for the file.  It returns
  \code{NULL} if it timed out waiting to try to lock the file.
  
  \code{unlockFile} invisibly returns \code{TRUE} if unlocking was
  succsesful, \code{FALSE} otherwise.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

# Create a temporary, empty file
tmp.file <- timeStamp("tmp", "txt")
cat(file=tmp.file)

# Lock it
a <- lockFile(tmp.file, maxWait=2, verbose=TRUE)

# See the file and the directory
dir(pattern=tmp.file)

# Pretending this is a competing process, attempt to lock it again:
b <- lockFile(tmp.file, maxWait=2, maxAttempts=7, verbose=TRUE)

# Now unlock it
unlockFile(a, verbose=TRUE)

# See the file and the directory
dir(pattern=tmp.file)

# Remove the temporary file
unlink(tmp.file)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
