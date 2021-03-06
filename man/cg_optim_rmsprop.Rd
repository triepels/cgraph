% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimizer.R
\name{cg_optim_rmsprop}
\alias{cg_optim_rmsprop}
\title{Root Mean Square Propagation (RMSprop)}
\usage{
cg_optim_rmsprop(parms = list(), eta = 0.01, gamma = 0.9, eps = 1e-08)
}
\arguments{
\item{parms}{list of cg_node objects, the nodes to be optimzed.}

\item{eta}{numeric scalar, learning rate.}

\item{gamma}{numeric scalar, momentum rate.}

\item{eps}{numeric scalar, small term to improve numerical stability (optional).}
}
\value{
cg_optim object.
}
\description{
Initialize a RMSprop optimizer.
}
\author{
Ron Triepels
}
