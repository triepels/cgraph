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

#' Matrix Multiplication
#'
#' Calculate \code{x \%*\% y}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.matmul <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x %*% y),
    grads = list(
      x = quote(tcrossprod(grad, y)),
      y = quote(crossprod(x, grad))
    ),
    binding = list(x = x, y = y)
  )
}

#' Matrix Crossproduct
#'
#' Calculate \code{crossprod(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{crossprod} function, this function requires both \code{x} and \code{y} to be supplied.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.crossprod <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(crossprod(x, y)),
    grads = list(
      x = quote(y %*% grad),
      y = quote(x %*% grad)
    ),
    binding = list(x = x, y = y)
  )
}

#' Transpose Matrix Crossproduct
#'
#' Calculate \code{tcrossprod(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{tcrossprod} function, this function requires both \code{x} and \code{y} to be supplied.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.tcrossprod <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(tcrossprod(x, y)),
    grads = list(
      x = quote(grad %*% y),
      y = quote(grad %*% x)
    ),
    binding = list(x = x, y = y)
  )
}

#' Linear Transformation
#'
#' Calculate \code{x \%*\% y + as.numeric(z)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param z cg.node, placeholder for a numeric vector.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is equivalent to \code{cg.matmul(x, y) + as.numeric(z)}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.linear <- function(x, y, z, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x %*% y + c(z)),
    grads = list(
      x = quote(tcrossprod(grad, y)),
      y = quote(crossprod(x, grad)),
      z = quote(cg.linear.grad.z(z, grad))
    ),
    binding = list(x = x, y = y, z = z)
  )
}

#' Linear Transformation Gradient
#'
#' Calculate the gradient of \code{x \%*\% y + as.numeric(z)} with respect to \code{z}.
#'
#' @param z numeric vector or array, value of \code{z}.
#' @param grad numeric vector or array, gradient of \code{z}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.linear.grad.z <- function(z, grad)
{
  if(is.array(z))
  {
    array(rowSums(grad), dim(z))
  }
  else
  {
    bsum(grad, length(z))
  }
}

#' Sum of Vector Elements
#'
#' Calculate \code{sum(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{sum} function, this function only accepts a single variable.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sum <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(sum(x)),
    grads = list(x = quote(cg.sum.grad(x, grad))),
    binding = list(x = x)
  )
}

#' Sum of Vector Elements Gradient
#'
#' Calculate the gradient of \code{sum(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.sum.grad <- function(x, grad)
{
  if(is.array(x))
  {
    array(grad, dim(x))
  }
  else
  {
    rep(grad, length(x))
  }
}

#' Product of Vector Elements
#'
#' Calculate \code{prod(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{prod} function, this function only accepts a single variable.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.prod <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(prod(x)),
    grads = list(x = quote(cg.prod.grad(x, y, grad))),
    binding = list(x = x, y = name)
  )
}

#' Product of Vector Elements Gradient
#'
#' Calculate the gradient of \code{prod(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{prod(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.prod.grad <- function(x, y, grad)
{
  grad * y / x
}

#' Row Sums
#'
#' Calculate \code{rowSums(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.rowSums <- function(x, name = cgraph::name())
{
 cgraph::opr(name = name,
   call = quote(rowSums(x)),
   grads = list(x = quote(cg.rowSums.grad(x, grad))),
   binding = list(x = x)
 )
}

#' Row Sums Gradient
#'
#' Calculate the gradient of \code{rowSums(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.rowSums.grad <- function(x, grad)
{
  array(grad, dim(x))
}

#' Column Sums
#'
#' Calculate \code{colSums(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.colSums <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(colSums(x)),
    grads = list(x = quote(cg.colSums.grad(x, grad))),
    binding = list(x = x)
  )
}

#' Column Sums Gradient
#'
#' Calculate the gradient of \code{colSums(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.colSums.grad <- function(x, grad)
{
  aperm(array(grad, rev(dim(x))))
}

#' Arithmetic Mean
#'
#' Calculate \code{sum(x) / length(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note For computational efficiency, this function does not use the base \code{mean} function.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.mean <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(sum(x) / length(x)),
    grads = list(x = quote(cg.mean.grad(x, grad))),
    binding = list(x = x)
  )
}

#' Arithmetic Mean Gradient
#'
#' Calculate the gradient of \code{sum(x) / length(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.mean.grad <- function(x, grad)
{
  if(is.array(x))
  {
    1 / length(x) * array(grad, dim(x))
  }
  else
  {
    1 / length(x) * rep(grad, length(x))
  }
}

#' Row Means
#'
#' Calculate \code{rowMeans(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.rowMeans <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(rowMeans(x)),
    grads = list(x = quote(cg.rowMeans.grad(x, grad))),
    binding = list(x = x)
  )
}

#' Row Means Gradient
#'
#' Calculate the gradient of \code{rowMeans(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.rowMeans.grad <- function(x, grad)
{
  1 / prod(dim(x)[-1]) * array(grad, dim(x))
}

#' Column Means
#'
#' Calculate \code{colMeans(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.colMeans <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(colMeans(x)),
    grads = list(x = quote(cg.colMeans.grad(x, grad))),
    binding = list(x = x)
  )
}

#' Column Means Gradient
#'
#' Calculate the gradient of \code{colMeans(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.colMeans.grad <- function(x, grad)
{
  1 / dim(x)[1] * aperm(array(grad, rev(dim(x))))
}

#' Maxima
#'
#' Calculate \code{max(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{max} function, this function only accepts a single variable.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.max <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(max(x)),
    grads = list(x = quote(cg.max.grad(x, y, grad))),
    binding = list(x = x, y = name)
  )
}

#' Maxima Gradient
#'
#' Calculate the gradient of \code{max(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{max(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.max.grad <- function(x, y, grad)
{
  c(grad) * (x == c(y))
}

#' Minima
#'
#' Calculate \code{min(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{min} function, this function only accepts a single variable.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.min <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(min(x)),
    grads = list(x = quote(cg.min.grad(x, y, grad))),
    binding = list(x = x, y = name)
  )
}

#' Minima Gradient
#'
#' Calculate the gradient of \code{min(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{min(y)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.min.grad <- function(x, y, grad)
{
  c(grad) * (x == c(y))
}

#' Parallel Maxima
#'
#' Calculate \code{pmax(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{pmax} function, this function only accepts two variables.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pmax <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(pmax(x, y)),
    grads = list(
      x = quote(cg.pmax.grad.x(x, y, grad)),
      y = quote(cg.pmax.grad.y(x, y, grad))
    ),
    binding = list(x = x, y = y)
  )
}

#' Parallel Maxima Gradient
#'
#' Calculate the gradient of \code{pmax(x, y)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.pmax.grad.x <- function(x, y, grad)
{
  if(is.array(x))
  {
    grad * (x >= y)
  }
  else
  {
    bsum(grad * (x >= y), length(x))
  }
}

#' Parallel Maxima Gradient
#'
#' Calculate the gradient of \code{pmax(x, y)} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.pmax.grad.y <- function(x, y, grad)
{
  if(is.array(y))
  {
    grad * (x < y)
  }
  else
  {
    bsum(grad * (x < y), length(y))
  }
}

#' Parallel Minima
#'
#' Calculate \code{pmin(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{pmin} function, this function only accepts two variables.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pmin <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(pmin(x, y)),
    grads = list(
      x = quote(cg.pmin.grad.x(x, y, grad)),
      y = quote(cg.pmin.grad.y(x, y, grad))
    ),
    binding = list(x = x, y = y)
  )
}

#' Parallel Minima Gradient
#'
#' Calculate the gradient of \code{pmin(x, y)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.pmin.grad.x <- function(x, y, grad)
{
  if(is.array(x))
  {
    grad * (x <= y)
  }
  else
  {
    bsum(grad * (x <= y), length(x))
  }
}

#' Parallel Minima Gradient
#'
#' Calculate the gradient of \code{pmin(x, y)} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.pmin.grad.y <- function(x, y, grad)
{
  if(is.array(y))
  {
    grad * (x > y)
  }
  else
  {
    bsum(grad * (x > y), length(y))
  }
}
