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

#' @author Ron Triepels
#' @export
cg_init_zeros <- function(..., name = NULL)
{
  .Call("cg_init_zeros", c(...), name, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_init_ones <- function(..., name = NULL)
{
  .Call("cg_init_ones", c(...), name, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_init_uniform <- function(..., min = 0, max = 1, name = NULL)
{
  .Call("cg_init_uniform", c(...), min, max, name, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_init_gaussian <- function(..., mean = 0, sd = 1, name = NULL)
{
  .Call("cg_init_gaussian", c(...), mean, sd, name, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_init_xavier_uniform <- function(..., name = NULL)
{
  .Call("cg_init_xavier_uniform", c(...), name, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_init_xavier_gaussian <- function(..., name = NULL)
{
  .Call("cg_init_xavier_gaussian", c(...), name, PACKAGE = "cgraph")
}
