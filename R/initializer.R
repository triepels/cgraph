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

#' Add Parameter
#'
#' Add a parameter to the active graph whose value is initialized by zeros.
#'
#' @param ... numerical scalars, the dimensions of the parameter.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note The dimensions provided to \code{...} must be non-negative whole numbers.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add a parameter to the graph
#' a <- cg_init_zeros(2, 3, name = "a")
#'
#' @author Ron Triepels
#' @export
cg_init_zeros <- function(..., name = NULL)
{
  .Call("cg_init_zeros", c(...), name, PACKAGE = "cgraph")
}

#' Add Parameter
#'
#' Add a parameter to the active graph whose value is initialized by ones.
#'
#' @param ... numerical scalars, the dimensions of the parameter.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note The dimensions provided to \code{...} must be non-negative whole numbers.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add a parameter to the graph
#' a <- cg_init_ones(2, 3, name = "a")
#'
#' @author Ron Triepels
#' @export
cg_init_ones <- function(..., name = NULL)
{
  .Call("cg_init_ones", c(...), name, PACKAGE = "cgraph")
}

#' Add Parameter
#'
#' Add a parameter to the active graph whose value is initialized by a uniform distribution.
#'
#' @param ... numerical scalars, the dimensions of the parameter.
#' @param min numerical scalar, the lower bound of the uniform distribution (optional).
#' @param max numerical scalar, the upper bound of the uniform distribution (optional).
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note The dimensions provided to \code{...} must be non-negative whole numbers.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add a parameter to the graph
#' a <- cg_init_uniform(2, 3, min = -1, max = 1, name = "a")
#'
#' @author Ron Triepels
#' @export
cg_init_uniform <- function(..., min = 0, max = 1, name = NULL)
{
  .Call("cg_init_uniform", c(...), min, max, name, PACKAGE = "cgraph")
}

#' Add Parameter
#'
#' Add a parameter to the active graph whose value is initialized by a Gaussian distribution.
#'
#' @param ... numerical scalars, the dimensions of the parameter.
#' @param mean numerical scalar, the mean of the Gaussian distribution (optional).
#' @param sd numerical scalar, the standard deviation of the Gaussian distribution (optional).
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note The dimensions provided to \code{...} must be non-negative whole numbers.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add a parameter to the graph
#' a <- cg_init_gaussian(2, 3, mean = 0, sd = 1, name = "a")
#'
#' @author Ron Triepels
#' @export
cg_init_gaussian <- function(..., mean = 0, sd = 1, name = NULL)
{
  .Call("cg_init_gaussian", c(...), mean, sd, name, PACKAGE = "cgraph")
}

#' Add Parameter
#'
#' Add a parameter to the active graph whose value is initialized by Xavier initialization using a uniform distribution.
#'
#' @param ... numerical scalars, the dimensions of the parameter.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note The dimensions provided to \code{...} must be non-negative whole numbers.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add a parameter to the graph
#' a <- cg_init_xavier_uniform(2, 3, name = "a")
#'
#' @author Ron Triepels
#' @export
cg_init_xavier_uniform <- function(..., name = NULL)
{
  .Call("cg_init_xavier_uniform", c(...), name, PACKAGE = "cgraph")
}

#' Add Parameter
#'
#' Add a parameter to the active graph whose value is initialized by Xavier initialization using a Gaussian distribution.
#'
#' @param ... numerical scalars, the dimensions of the parameter.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note The dimensions provided to \code{...} must be non-negative whole numbers.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add a parameter to the graph
#' a <- cg_init_xavier_gaussian(2, 3, name = "a")
#'
#' @author Ron Triepels
#' @export
cg_init_xavier_gaussian <- function(..., name = NULL)
{
  .Call("cg_init_xavier_gaussian", c(...), name, PACKAGE = "cgraph")
}
