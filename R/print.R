# Copyright 2018 Ron Triepels
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

print.cgraph <- function(x, ...)
{
  cat(sprintf("<cgraph: %s>\n", address(x)))
}

print.cg.node = function(x, ..., autorun = getOption("cg.autorun"))
{
  value <- NULL

  if(is.null(autorun))
  {
    autorun = TRUE
  }

  if(!is.null(session$graph) & autorun)
  {
    tryCatch(
    {
      values <- run(x)

      parent.env(values) <- session$graph$values

      value <- get(x, envir = values)
    },
    error = function(e)
    {
      e$call <- NULL

      warning(e)
    })

    if(!is.null(value))
    {
      print(value); cat("\n")
    }

    cat(sprintf("<cg.node: \"%s\"> @ %s\n", x, address(session$graph)))
  }
  else
  {
    cat(sprintf("<cg.node: \"%s\">\n", x))
  }
}
