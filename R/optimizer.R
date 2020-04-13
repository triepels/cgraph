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
cg_opt_gd <- function(parms = list(), eta = 0.05)
{
  .Call("cg_opt_gd", parms, eta, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_opt_gd_momentum <- function(parms = list(), eta = 0.05, gamma = 0.9)
{
  .Call("cg_opt_gd_momentum", parms, eta, gamma, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_opt_adagrad <- function(parms = list(), eta = 1e-2, eps = 1e-8)
{
  .Call("cg_opt_adagrad", parms, eta, eps, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_opt_rmsprop <- function(parms = list(), eta = 1e-3, gamma = 0.9, eps = 1e-8)
{
  .Call("cg_opt_rmsprop", parms, eta, gamma, eps, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_opt_adam <- function(parms = list(), eta = 1e-3, betas = c(0.9, 0.999), eps = 1e-8)
{
  .Call("cg_opt_adam", parms, eta, betas, eps, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_opt_step <- function(opt)
{
  invisible(.Call("cg_opt_step", opt, PACKAGE = "cgraph"))
}

#' @author Ron Triepels
#' @export
print.cg_opt <- function(x, ...)
{
  cat("<cg_opt>")
}
