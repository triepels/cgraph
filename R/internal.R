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

#' Approximate Gradients
#'
#' Differentiate a graph with respect to a given target node by numerical differentiation.
#'
#' @param graph cg_graph object, graph that is differentiated.
#' @param target cg_node object, node in the graph that is differentiated.
#' @param values named list or environment, values that are subsituted for the input nodes in the graph.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#' @param eps numeric scalar, step size. Defaults to 1e-4.
#'
#' @note All nodes required to compute the target node must have a value, or their value must be able to be computed at run-time. The values of the nodes can be obtained by first evaluating function \link[cgraph]{cg_graph_run}. The values obtained by this function for the nodes can then be supplied to argument \code{values}.
#'
#' The graph is differentiation by the symmetric difference quotient. This method can only be used to differentiate scalars. In case the value of the target node is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is differentiated. The gradients have the same shape as the values of the nodes.
#'
#' Numerical differentiation is subject to estimation error and can be very slow. Therefore, this function should only be used for testing purposes.
#'
#' @return environment, the gradients of all ancestors of the node (including the target node itself) with respect to the target node.
#'
#' @author Ron Triepels
#' @keywords internal
approx_gradients <- function(graph, target, values = new.env(), index = 1, eps = 1e-4)
{
  if(is.list(values))
  {
    values <- list2env(values)
  }

  .Call("approx_gradients", graph, target, values, new.env(), index, eps, PACKAGE = "cgraph")
}
