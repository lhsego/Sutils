\name{fitAll}
\alias{fitAll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit all subsets of a multiple linear regression model in parallel
}
\description{
Fit all subsets of a multiple linear regression model in parallel. This
is meant to be run on a single machine with multiple cores (and not in an
HPC environment).
}
\usage{
fitAll(data, resp, predVars,
       predList = NULL,
       outFile = NULL,
       numjobs = 5,
       sort.results.by = "+ sigma + BIC",
       include.logical.pred = FALSE,
       check.num.models = FALSE,
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
  \item{predList}{A list of all subsets of predictor combinations.
    Specifically, the object returned by \code{\link{comboList}} or the
    .Rdata file containing this object (if it was produced by a previous
    call to \code{\link{comboList}}).  If \code{predList = NULL},
    then \code{\link{comboList}} is called within \code{\link{fitAll}}
    to produce the list. If multiple calls to \code{\link{fitAll}} will be made using the same
    number of predictors, creating the combination list with \code{\link{comboList}} initially and
    passing it via this argument is more efficient than using \code{NULL}.
}
  \item{outFile}{
A character string indicating the .Rdata filename where the output data frame of model fits
will be saved.  If \code{NULL}, no file is saved.
}
\item{numjobs}{
Integer indicating the number of jobs for parallel processing.
}
  \item{sort.results.by}{
Text string which controls the sorting of the output data
set. This string is the right hand side of the 'formula' in
\code{\link{sort.data.frame}}. The string should only include one or more of the following
variable names:  \code{BIC}, \code{adj.r.squared}, and/or
\code{r.squared}.  Variables names in the string should be preceded by a
'+' or '-' to indicate ascending or descending sorting, respectively.
}
\item{include.logical.pred}{\code{= TRUE} adds logical indicator
    columns of the predictors in the output data frame.  These indicator
    columns provide an alternative way to represent the fitted models.
}
  \item{check.num.models}{
\code{= TRUE} displays the number of model subsets that would be fit and
then returns \code{NULL}.  It's a simple way to assess how large the job
will be before running it.
}
  \item{verbose}{
\code{= TRUE} prints details (and timing) of the job 
}
}
%\details{
%}
\value{
If \code{check.num.models = TRUE}, returns \code{NULL} with a message
indicating the number of models that would be fit, given the number of
predictors.

Otherwise, \code{fitAll} invisibly returns a data frame with the columns listed below.  If a
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
  \code{\link{fitAll.hpc}}
}
\examples{
# Simple example of a data set with 500 observations and 5 predictors
n <- 500
df <- data.frame(y = rnorm(n, mean = 17, sd = 3))
for (i in 1:10) 
  df[, letters[i]] <- rnorm(n, mean = i*5 - 7, sd = i*0.1 + 2)
head(df)

# Fit all the combinations
results <- fitAll(df, "y", colnames(df)[-1])
head(results)
tail(results)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
