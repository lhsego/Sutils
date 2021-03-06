% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maUtil.R
\name{maUtil}
\alias{maUtil}
\alias{plot.maUtilFun}
\alias{print.maUtilCall}
\alias{print.maUtilFun}
\title{Generates or evaluates a multiattribute utility function}
\usage{
maUtil(maUtilMethod = "addMultUtil", Z = NULL, ...)

\method{print}{maUtilCall}(x, ...)

\method{print}{maUtilFun}(x, ...)

\method{plot}{maUtilFun}(x, ...)
}
\arguments{
\item{maUtilMethod}{A single character string (or unique abbreviation) indicating the multiattribute
utility function method. Acceptable methods are given by \code{\link{maUtilMethods}}.}

\item{Z}{A dataframe or matrix containing the values of the attributes, where the attributes
are on the columns and the observations are on the rows.}

\item{x}{An object returned by \code{maUtilMethod}, which will be of class \code{maUtilCall} or \code{maUtilFun}}

\item{dots}{Optional named arguments to \code{\link{maUtilMethods}} functions.}

\item{dots}{Additional arguments to \code{\link{print.default}} or \code{\link{plot.default}}}
}
\value{
\item{1}{If \code{Z = NULL}, the multiattribute utility function with fixed parameters is returned
and can be called later. This function has a single argument, \code{Z}, that matrix or dataframe
containing the measured
values of the attributes of interest. The returned object is of class \code{maUtilFun}.}
\item{2}{If a matrix or dataframe is provided for the argument \code{Z},
the multiattribute utility method is evaluated using \code{Z} for the given parameters \code{...} and the
calculated utility values are returned. The returned object is of class \code{maUtilCall}.}
}
\description{
Generate or evaluates a multiattribute utility function
}
\details{
Method functions must be constructed to operate on \code{Z}. Note that
}
\examples{
# Generate single attribute utility functions
sa.funs <- list(a1 = saUtil(theta = 2, zrange = c(-2, 4), urange = c(1, 0)),
                a2 = saUtil(zrange = c(0.4, 0.9), urange = c(1,0), theta = -1.7))

# The weights
alphas <- c(a2 = 0.2, a1 = 0.8)

# Cook up some data
someData <- data.frame(a1 = runif(10, -2, 4),
                       a2 = rpois(10, 3), row.names = letters[1:10])

# Calculate the utility
u <- maUtil(alpha = alphas, saUtilFun = sa.funs, Z = someData)
print(u)

# Create a function for calculating the utility
uf <- maUtil(alpha = alphas, saUtilFun = sa.funs)
uf(someData)

# Plot the single attribute utility functions side by side:  plot the uf object
par(mfrow = c(1, 2), pty = "s")
plot(uf)

# Notice how we can input graphing parameters to control the look of the chart
# The length of each graphic parameter should either be 1 (which will be applied to
# each plot), or equal to the number of attributes.
plot(uf, ylab = c(expression(u[1]), expression(u[2])), main = c("Attribute 1", "Attribute 2"), lwd = 2,
     col = "Blue", las = 1)

# Two ways to calculte the special case with only a single attribute utility function
s1 <- maUtil(alpha = c(a1 = 1), saUtilFun = list(a1 = sa.funs[[1]]), Z = someData)
s2 <- maUtil(alpha = c(a1 = 1, a2 = 0), saUtilFun = sa.funs, Z = someData)
max(abs(s1 - s2))
}
\author{
Landon Sego
}

