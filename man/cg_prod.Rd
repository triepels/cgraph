% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/array.R
\name{cg_prod}
\alias{cg_prod}
\title{Product of Vector Elements}
\usage{
cg_prod(x, name = NULL)
}
\arguments{
\item{x}{either a cg_node object or a numerical vector or array.}

\item{name}{character scalar, name of the operation (optional).}
}
\value{
cg_operator object.
}
\description{
Calculate \code{prod(x)}.
}
\note{
In contrast to the base \link[base:prod]{prod} function, this function only accepts a single argument.

Function \link[base:prod]{prod} is called without changing the default value of argument \code{na.rm}.
}
\seealso{
\link[base:prod]{prod}
}
\author{
Ron Triepels
}
