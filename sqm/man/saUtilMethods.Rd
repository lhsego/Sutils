% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saUtilMethods.R
\name{saUtilMethods}
\alias{saUtilMethods}
\title{Lists the available single attribute utility methods}
\usage{
saUtilMethods(method = NULL)
}
\arguments{
\item{method}{a character string for single attribute utility  method. If no value is
specified, then all available score methods are returned.}
}
\value{
By default, a vector with all available single attribute utility methods.
}
\description{
This function enumerates the single attribute utility function methods
}
\details{
By default, this function returns a vector with all of the available single attribute
utility methods that are used within the SQM package. This function also acts as a
lookup to determine if a particular score method is available. If 'method'
is specified, then a lookup is performed, and either TRUE or FALSE is returned
to indicate the presence or absence of the given 'method, respectively.
}

