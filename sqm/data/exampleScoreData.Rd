\name{exampleScoreData}
\alias{exampleScoreData}
\docType{data}
\title{
Example data for calculating proper scoring rules
}
\description{
Example data format for calculating proper scoring rules  
}
\usage{data(exampleScoreData)}
\format{
  A data frame with 500 observations on the following 12 variables.
  \describe{
    \item{\code{signatureID}}{a factor with levels \code{uniform}
      \code{worst} \code{perfect} \code{realistic1} \code{realistic2}}
    \item{\code{truthClass}}{a numeric vector}
    \item{\code{predictionClass1}}{a numeric vector}
    \item{\code{predictionClass2}}{a numeric vector}
    \item{\code{predictionClass3}}{a numeric vector}
    \item{\code{predictionClass4}}{a numeric vector}
    \item{\code{predictionClass5}}{a numeric vector}
    \item{\code{predictionClass6}}{a numeric vector}
    \item{\code{predictionClass7}}{a numeric vector}
    \item{\code{predictionClass8}}{a numeric vector}
    \item{\code{predictionClass9}}{a numeric vector}
    \item{\code{predictionClass10}}{a numeric vector}
  }
}
\details{
This data frame contains 5 example signatures to illustrate how
\code{\link{calcScore}} calcaultes scores for a signature that produces probability
estimates of 10 classes for each observational unit.  The \code{uniform}
signature consists of equal probability assignments (1/10) for each
class.  The \code{worst} signature captures the situation where
proability of 1 is assigned to the incorrect class, and all other
classes are 0.  \code{perfect} is when probability 1 is assigned to the
correct class, and \code{realistic1} and \code{realistic2} are simulated
data that illustrate how a "real" signatures might behave.
}
\source{
Source code that created these data can be found in the \code{data}
folder of the SQM package source.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(exampleScore)
}
\keyword{datasets}
