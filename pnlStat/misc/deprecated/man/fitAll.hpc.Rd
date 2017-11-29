\name{fitAll.hpc}
\alias{fitAll.hpc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit all subsets of a multiple linear regression model using HPC
}
\description{
Fit all subsets of a multiple linear regression model using parallelized
high performance computing on the PNNL institutional computing cluster,
olympus.pnl.gov.  The job is parsed into smaller pieces and instantiated
on multiple cores using a SLURM batch job.
}
\usage{
fitAll.hpc(data, resp, predVars, account,
           predList = NULL,
           outFile = NULL,
           numNodes = 10,
           time.limit.mins = 30,
           sort.results.by = "+ sigma + BIC",
           include.logical.pred = FALSE,
           tarball.file = NULL,
           mpi.compiler = NULL,
           email.notification = NULL,
           remove.working.dir = TRUE,
           verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{data}{The data to which the linear models will be fit.
    \code{data} may be the data frame object itself or the filename (as
    a text string) of an .Rdata or
.csv file that contains the data (\code{data} is passed to \code{\link{dataIn}}).
}
  \item{resp}{
 a character string indicating the column name of \code{data} which contains
the response
}
 \item{predVars}{
    a character vector giving the column names of
    \code{data} that will be used as predictors
}

\item{account}{
A character string indicating the PIC account (e.g. "CSI", "SDI",
"USERS") that will charged for the computing time.
}

\item{predList}{A list of all subsets of predictor combinations.
    Specifically, the object returned by \code{\link{comboList}} or the
    .Rdata file containing this object (if it was produced by a previous
    call to \code{\link{comboList}}).  If \code{predList = NULL},
    then \code{\link{comboList}} is called within \code{\link{fitAll.hpc}}
    to produce the list. If multiple calls to \code{\link{fitAll.hpc}} will be made using the same
    number of predictors, creating the combination list with \code{\link{comboList}} initially and
    passing it via this argument is more efficient than using \code{NULL}.
}

\item{outFile}{
A character string indicating the .Rdata filename where the output data frame of model fits
will be saved.  If \code{NULL}, no file is saved.
}
\item{numNodes}{
Integer indicating the number of nodes requested for parallel processing.
Each node on olympus has 32 cores.  So, for example, requesting 3 nodes is equivalent
to requesting 96 parallel jobs.
}

\item{time.limit.mins}{
Integer indicating the time limit (in minutes) of the SLURM batch
job. If the SLURM job exceeds this limit it will be canceled.  However,
jobs with shorter limits are likely to be scheduled for launch sooner.
}

\item{sort.results.by}{
Text string which controls the sorting of the output data
set. This string is the right hand side of the 'formula' in
\code{\link{sort.data.frame}}. The string should only include one or more of the following
variable names:  \code{BIC}, \code{adj.r.squared}, and/or
\code{r.squared}.  Variables names in the string should be preceded by a
'+' or '-' to indicate ascending or descending sorting, respectively.}

\item{include.logical.pred}{\code{= TRUE} adds logical indicator
    columns of the predictors in the output data frame.  These indicator
    columns provide an alternative way to represent the fitted models.
}
  \item{tarball.file}{
A text string indicating the file (ending in .tar.gz) where all the files used to
create and support the SLURM job will be stored.  If \code{NULL}, a file of the form
'fitAll_output_####.tar.gz' will be stored in the working directory.  If
\code{tarball.file = "none"}, no tarball will be created.
}
  \item{remove.working.dir}{
\code{= TRUE} requests the working directory of the SLURM job files 
be deleted on successful completion.  If the SLURM job fails, this directory
will not be removed.
}
  \item{mpi.compiler}{
Text string indicating the full-path filename of the mpi compiler.  If
\code{mpi.compiler = NULL}, the mpi compiler will be automatically assigned.
%if the host is olympus.local or nwiceb.pnl.gov. 
}
  \item{email.notification}{
Text string containing an email address to which an email will be sent upon
completion of the SLURM job.  If \code{NULL}, no email will be sent.
}
  \item{verbose}{
\code{= TRUE} prints details (and timing) of the job 
}
}
%\details{
%}
\value{
Invisibly returns a data frame with the columns listed below.  If a
value for \code{outFile} is provided, this data frame is saved to \code{outFile}. 
\item{model}{Text giving the formula of the model that was fit}
\item{Columns for predictor indicators}{If \code{include.logical.pred =
    TRUE}, a logical column for each predictor is included,
  indicating whether that predictor is included in the model}
\item{sigma}{The square root of the MSE of the corresponding model}
\item{RMSE}{The square root of mean of the squared residuals of the corresponding model}
\item{adj.r.squared}{The adjusted r-squared value of the corresponding model}
\item{r.squared}{The r-squared value of the corresponding model}
\item{BIC}{The BIC of the corresponding model}
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
  \code{\link{fitAll}}
}
\examples{
# Simple example of a data set with 500 observations and 5 predictors
n <- 500
df <- data.frame(y = rnorm(n, mean = 17, sd = 3))
for (i in 1:10) 
  df[, letters[i]] <- rnorm(n, mean = i*5 - 7, sd = i*0.1 + 2)
head(df)

# Fit all the combinations
results <- fitAll.hpc(df, "y", letters[1:10], "USERS", numNodes = 2, time.limit.mins = 1, tarball.file = "none")
head(results)
tail(results)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
