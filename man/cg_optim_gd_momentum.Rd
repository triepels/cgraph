% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimizer.R
\name{cg_optim_gd_momentum}
\alias{cg_optim_gd_momentum}
\title{Gradient Descent with Momentum}
\usage{
cg_optim_gd_momentum(parms = list(), eta = 0.05, gamma = 0.9)
}
\arguments{
\item{parms}{list of cg_node objects, the nodes to be optimzed.}

\item{eta}{numeric scalar, learning rate.}

\item{gamma}{numeric scalar, momentum rate.}
}
\value{
cg_optim object.
}
\description{
Initialize a gradient descent optimizer with momentum.
}
\author{
Ron Triepels
}
