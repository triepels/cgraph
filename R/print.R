# Copyright (C) 2018 Ron Triepels
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

print.cgraph <- function(x, ...)
{
  cat(sprintf("<cgraph: %s>\n", address(x)))
}

print.cg.node = function(x, ..., autorun = getOption("cg.autorun"))
{
  val <- NULL

  if(is.null(autorun))
  {
    autorun = TRUE
  }

  if(autorun)
  {
    tryCatch(
    {
      val <- get(x, envir = run(x))
    },
    error = function(e)
    {
      e$call <- NULL

      warning(e)
    })
  }

  if(is.null(session$graph) | !autorun)
  {
    cat(sprintf("<cg.node: %s>\n", x))
  }
  else
  {
    if(!is.null(val))
    {
      print(val); cat("\n")
    }

    cat(sprintf("<cg.node: %s> @ %s\n", x, address(session$graph)))
  }
}
