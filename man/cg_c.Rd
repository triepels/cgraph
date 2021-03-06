% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{cg_c}
\alias{cg_c}
\title{Combine Values into a Vector or List}
\usage{
cg_c(..., name = NULL)
}
\arguments{
\item{...}{either cg_node objects or R objects.}

\item{name}{character scalar, name of the operation (optional).}
}
\value{
cg_operator object.
}
\description{
Calculate \code{c(...)}.
}
\note{
This is a non-differentiable operator.
}
\seealso{
\link[base:c]{c}
}
\author{
Ron Triepels
}
