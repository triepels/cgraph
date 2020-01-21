# Copyright 2019 Ron Triepels
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

dots <- function(env = parent.frame())
{
  .Call("dots", env, PACKAGE = "cgraph")
}

arg_list <- function(...)
{
  .External("arg_list", ..., PACKAGE = "cgraph")
}

#' Block Summation
#'
#' Divide a vector or array in consecutive fixed-sized blocks and sum the elements at each position in these blocks.
#'
#' @param x, numeric vector or array, the object that is summed.
#' @param block_size, numeric scalar, the size of each block. Defaults to 1.
#'
#' @note If \code{x} is an array and \code{block_size} is equal to the size of \code{x}'s first dimension, then \link[cgraph]{bsum} behaves as \link[base:colSums]{rowSums}.
#'
#' @return numeric vector. Each 1th element of the vector is the sum of each 1th element of the blocks, the 2nd element of the vector is the sum of each 2nd element of the blocks, and so on.
#'
#' @author Ron Triepels
#' @keywords internal
bsum <- function(x, block_size = 1)
{
  .Call("bsum", x, block_size, PACKAGE = "cgraph")
}

#' Approximate Gradient
#'
#' Differentiate a target node with respect to a given node by numerical differentiation.
#'
#' @param graph cg_graph object, graph that is differentiated.
#' @param target cg_node object, node in the graph that is differentiated.
#' @param node cg_node object, node with respect to which the target node is differentiated.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#' @param eps numeric scalar, step size. Defaults to 1e-4.
#'
#' @note All nodes required to compute the target node must have a value or their value must be able to be computed at run-time. Only those nodes needed to compute the target node (including the target itself) are evaluated.
#'
#' The graph is differentiation by the symmetric difference quotient. This method can only be used to differentiate scalars. In case the target node evaluates to a vector or an array, argument \code{index} can be used to specify which element of the vector or array is differentiated. The derivative has the same shape as the value of node supplied to argument \code{node}.
#'
#' Numerical differentiation is subject to estimation error and can be very slow. Therefore, this function should only be used for testing purposes.
#'
#' @return the derivative of the node supplied to argument \code{node} with respect to the node supplied to argument \code{target}.
#'
#' @author Ron Triepels
#' @keywords internal
approx_gradient <- function(graph, target, node, index = 1, eps = 1e-4)
{
  .Call("approx_gradient", graph, target, node, index, eps, PACKAGE = "cgraph")
}
