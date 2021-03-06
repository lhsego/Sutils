% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fidelityMethods.R
\name{fidelityMethods}
\alias{fidelityMethods}
\title{Lists the SQM classification fidelity methods.}
\usage{
fidelityMethods(method = NULL)
}
\arguments{
\item{method}{a character string for a fidelity method. If no value is
specified, then all available fidelity methods are returned.}
}
\value{
By default, a vector with all available fidelityMethods. Otherwise,
returns a logical value indicating if the specified 'method' is available in
the SQM package.
}
\description{
This function enumerates the fidelity methods available in the SQM package.
}
\details{
By default, this function returns a vector with all of the available fidelity
methods that are used within the SQM package. This function also acts as a
lookup to determine if a particular fidelity method is available. If 'method'
is specified, then a lookup is performed, and either TRUE or FALSE is returned
to indicate the presence or absence of the given 'method, respectively.
}

