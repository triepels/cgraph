% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimizer.R
\name{cg_optim_adam}
\alias{cg_optim_adam}
\title{Adaptive Moment Estimation (ADAM)}
\usage{
cg_optim_adam(parms = list(), eta = 0.001, betas = c(0.9, 0.999), eps = 1e-08)
}
\arguments{
\item{parms}{list of cg_node objects, the nodes to be optimzed.}

\item{eta}{numeric scalar, learning rate.}

\item{betas}{numeric vector of length two, first and second moment rates.}

\item{eps}{numeric scalar, small term to improve numerical stability (optional).}
}
\value{
cg_optim object.
}
\description{
Initialize an ADAM optimizer.
}
\author{
Ron Triepels
}
