% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/array.R
\name{cg_array}
\alias{cg_array}
\title{Multidimensional Arrays}
\usage{
cg_array(data = NA, dim = length(data), dimnames = NULL, name = NULL)
}
\arguments{
\item{data}{either a cg_node object or a numerical vector.}

\item{dim}{either a cg_node object or a numerical vector.}

\item{dimnames}{either a cg_node object or a character vector.}

\item{name}{character scalar, name of the operation (optional).}
}
\value{
cg_operator object.
}
\description{
Create a multidimensional array.
}
\note{
This is a non-differentiable operator.
}
\seealso{
\link[base:array]{array}
}
\author{
Ron Triepels
}
