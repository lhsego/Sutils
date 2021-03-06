% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcScore.R
\name{calcScore}
\alias{calcScore}
\title{Calculates scores for a given matrix of predicted class
probabilities, along with the corresponding truth classes.}
\usage{
calcScore(signatures, methods = scoreMethods(), ...)
}
\arguments{
\item{signatures}{data.frame containing the true classes and class prediction
probabilities for each signature. See details.}

\item{methods}{vector of accuracy methods that will be used. For a list of
available accuracy methods, see \code{\link{scoreMethods}}.}

\item{...}{Additional arguments to methods}
}
\value{
A data.frame containing scores for each signature
}
\description{
For a dataframe of signatures, this function computes an assortment of 
scoring rule metrics for each signature. A data frame is returned containing 
the scoring metrics for each signature.
}
\details{
The data.frame \code{signatures} has the following columns: signatureID, 
truthClass, and multiple columns beginning with 'predictionClass' followed by
the class name. Each signature (denoted by 'signatureID') consists of many
observations, each of which has a true class membership (denoted by
'truthClass') and a corresponding probability value for membership in each 
class (denoted by 'predictionClass'+className). The rows of \code{signatures} 
correspond to each observation for which a classification is made.

The available scoring methods can be viewed with
\code{\link{scoreMethods}}.
}
\examples{
# Load data
data(exampleScoreData)

# Now calculate the scores on the whole data set
calcScore(exampleScoreData)
}
\author{
Amanda White, Landon Sego
}
\seealso{
\code{\link{logScore}}, \code{\link{brierScore}}
}

