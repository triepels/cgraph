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

# Function definition
delayedAssign("subset1", cg_function(
  def = base::`[`,
  grads = list(
    function(x, ..., drop = TRUE, value, grad)
    {
      if(!is.numeric(x))
      {
        stop(sprintf("unable to differentiate object of type '%s'", typeof(x)))
      }

      if(is.array(x))
      {
        out <- array(0, dim(x))
        out[...] <- grad
        out
      }
      else
      {
        out <- rep(0, length(x))
        out[...] <- grad
        out
      }
    }
  )
))

#' @export
`[.cg_node` <- function(x, ...)
{
  cg_operator(subset1, c(x, dots()))
}

# Function definition
delayedAssign("subset2", cg_function(
  def = base::`[[`,
  grads = list(
    function(x, ..., exact = TRUE, value, grad)
    {
      if(!is.numeric(x))
      {
        stop(sprintf("unable to differentiate object of type '%s'", typeof(x)))
      }

      if(is.array(x))
      {
        out <- array(0, dim(x))
        out[[...]] <- grad
        out
      }
      else
      {
        out <- rep(0, length(x))
        out[[...]] <- grad
        out
      }
    }
  )
))

#' @export
`[[.cg_node` <- function(x, ...)
{
  cg_operator(subset2, c(x, dots()))
}
