% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_n.R
\name{get_n}
\alias{get_n}
\title{Solve for the strata loading parameter}
\usage{
get_n(rho, N, alpha, xi = NULL)
}
\arguments{
\item{rho}{a vector of relative weights of finding an unacceptable item relative to
stratum 1. Note that rho[1] = 1 always.}

\item{N}{the vector of total grid cells for each stratum}

\item{alpha}{a numeric vector of arbitrary length with elements in c(0,1). This parameter
is used to obtain a locally optimal allocation according to the following function:
alpha*(N/sum(N)) + (1 - alpha)*rho.
Hence, an alpha close to zero favors allocating samples by risk (rho) and
an alpha close to one favors allocating samples by size of stratum.}

\item{xi}{the strata loading parameter. If specified, \code{get_n} will return
\code{pmin(xi * alpha*(N/sum(N)) + (1 - alpha)*rho, N)}. Otherwise, \code{get_n}
solves for the maximum possible loading parameter. Default = NULL.}
}
\description{
Solve for the strata loading parameter
}

