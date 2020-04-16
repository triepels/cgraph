# Copyright 2020 Ron Triepels
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Gradient Descent
#'
#' Initialize a gradient descent optimizer.
#'
#' @param parms list of cg_node objects, the nodes to be optimzed.
#' @param eta numeric scalar, learning rate.
#'
#' @return cg_optim object.
#'
#' @author Ron Triepels
#' @export
cg_optim_gd <- function(parms = list(), eta = 0.05)
{
  .Call("cg_optim_gd", parms, eta, PACKAGE = "cgraph")
}

#' Gradient Descent with Momentum
#'
#' Initialize a gradient descent optimizer with momentum.
#'
#' @param parms list of cg_node objects, the nodes to be optimzed.
#' @param eta numeric scalar, learning rate.
#' @param gamma numeric scalar, momentum rate.
#'
#' @return cg_optim object.
#'
#' @author Ron Triepels
#' @export
cg_optim_gd_momentum <- function(parms = list(), eta = 0.05, gamma = 0.9)
{
  .Call("cg_optim_gd_momentum", parms, eta, gamma, PACKAGE = "cgraph")
}

#' Adaptive Gradient (AdaGrad)
#'
#' Initialize an AdaGrad optimizer.
#'
#' @param parms list of cg_node objects, the nodes to be optimzed.
#' @param eta numeric scalar, learning rate.
#' @param eps numeric scalar, small term to improve numerical stability (optional).
#'
#' @return cg_optim object.
#'
#' @author Ron Triepels
#' @export
cg_optim_adagrad <- function(parms = list(), eta = 1e-2, eps = 1e-8)
{
  .Call("cg_optim_adagrad", parms, eta, eps, PACKAGE = "cgraph")
}

#' Root Mean Square Propagation (RMSprop)
#'
#' Initialize a RMSprop optimizer.
#'
#' @param parms list of cg_node objects, the nodes to be optimzed.
#' @param eta numeric scalar, learning rate.
#' @param gamma numeric scalar, momentum rate.
#' @param eps numeric scalar, small term to improve numerical stability (optional).
#'
#' @return cg_optim object.
#'
#' @author Ron Triepels
#' @export
cg_optim_rmsprop <- function(parms = list(), eta = 0.01, gamma = 0.9, eps = 1e-8)
{
  .Call("cg_optim_rmsprop", parms, eta, gamma, eps, PACKAGE = "cgraph")
}

#' Adaptive Moment Estimation (ADAM)
#'
#' Initialize an ADAM optimizer.
#'
#' @param parms list of cg_node objects, the nodes to be optimzed.
#' @param eta numeric scalar, learning rate.
#' @param betas numeric vector of length two, first and second moment rates.
#' @param eps numeric scalar, small term to improve numerical stability (optional).
#'
#' @return cg_optim object.
#'
#' @author Ron Triepels
#' @export
cg_optim_adam <- function(parms = list(), eta = 1e-3, betas = c(0.9, 0.999), eps = 1e-8)
{
  .Call("cg_optim_adam", parms, eta, betas, eps, PACKAGE = "cgraph")
}

#' Optimization Step
#'
#' Perform a single optimization step.
#'
#' @param optim cg_optim object, the optimizer to be used for the optimization.
#'
#' @return cg_optim object.
#'
#' @author Ron Triepels
#' @export
cg_optim_step <- function(optim)
{
  invisible(.Call("cg_optim_step", optim, PACKAGE = "cgraph"))
}

#' @author Ron Triepels
#' @export
print.cg_optim <- function(x, ...)
{
  invisible(.Call("cg_optim_print", x, PACKAGE = "cgraph"))
}
