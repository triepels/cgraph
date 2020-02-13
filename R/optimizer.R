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
cg_gd <- function(parms, lr = 0.05)
{
  .Call("cg_gd", parms, lr, PACKAGE = "cgraph")
}

#' @author Ron Triepels
#' @export
cg_optimizer_step <- function(optimizer)
{
  invisible(.Call("cg_optimizer_step", optimizer, PACKAGE = "cgraph"))
}

#' @author Ron Triepels
#' @export
print.cg_optimizer <- function(x, ...)
{
  cat("<cg_optimizer>")
}
