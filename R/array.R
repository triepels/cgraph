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
cg.matmul <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`%*%`),
    grads = list(
      quote(matmul.grad.x),
      quote(matmul.grad.y)
    ),
    args = list(x, y)
  )
}

matmul.grad.x <- function(x, y, val, grad)
{
  tcrossprod(grad, y)
}

matmul.grad.y <- function(x, y, val, grad)
{
  crossprod(x, grad)
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
cg.crossprod <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(crossprod),
    grads = list(
      quote(crossprod.grad.x),
      quote(crossprod.grad.y)
    ),
    args = list(x, y)
  )
}

crossprod.grad.x <- function(x, y, val, grad)
{
  y %*% grad
}

crossprod.grad.y <- function(x, y, val, grad)
{
  x %*% grad
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
cg.tcrossprod <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(tcrossprod),
    grads = list(
      quote(tcrossprod.grad.x),
      quote(tcrossprod.grad.y)
    ),
    args = list(x, y)
  )
}

tcrossprod.grad.x <- function(x, y, val, grad)
{
  grad %*% y
}

tcrossprod.grad.y <- function(x, y, val, grad)
{
  grad %*% x
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
cg.linear <- function(x, y, z, name)
{
  cgraph::opr(name = name,
    call = quote(linear),
    grads = list(
      quote(linear.grad.x),
      quote(linear.grad.y),
      quote(linear.grad.z)
    ),
    args = list(x, y, z)
  )
}

linear <- function(x, y, z)
{
  x %*% y + c(z)
}

linear.grad.x  <- function(x, y, z, val, grad)
{
  tcrossprod(grad, y)
}

linear.grad.y <- function(x, y, z, val, grad)
{
  crossprod(x, grad)
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

linear.grad.z <- function(x, y, z, val, grad)
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
cg.sum <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(sum),
    grads = list(
      quote(sum.grad)
    ),
    args = list(x)
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
sum.grad <- function(x, val, grad)
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
cg.prod <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(prod),
    grads = list(
      quote(prod.grad)
    ),
    args = list(x)
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
prod.grad <- function(x, val, grad)
{
  grad * val / x
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
cg.rowSums <- function(x, name)
{
 cgraph::opr(name = name,
   call = quote(rowSums),
   grads = list(
     quote(rowSums.grad)
   ),
   args = list(x)
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
rowSums.grad <- function(x, val, grad)
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
cg.colSums <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(colSums),
    grads = list(
      quote(colSums.grad)
    ),
    args = list(x)
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
colSums.grad <- function(x, val, grad)
{
  aperm(array(grad, rev(dim(x))))
}



# To do:
# Update documentation of cg.mean



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
cg.mean <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(mean),
    grads = list(
      quote(mean.grad)
    ),
    args = list(x)
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
mean.grad <- function(x, val, grad)
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
cg.rowMeans <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(rowMeans),
    grads = list(
      quote(rowMeans.grad)
    ),
    args = list(x)
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
rowMeans.grad <- function(x, val, grad)
{
  1 / prod(dim(x)[-1L]) * array(grad, dim(x))
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
cg.colMeans <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(colMeans),
    grads = list(
      quote(colMeans.grad)
    ),
    args = list(x)
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
colMeans.grad <- function(x, val, grad)
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
cg.max <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(max),
    grads = list(
      quote(max.grad)
    ),
    args = list(x)
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
max.grad <- function(x, val, grad)
{
  c(grad) * (x == c(val))
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
cg.min <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(min),
    grads = list(
      quote(min.grad)
    ),
    args = list(x)
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
cg.min.grad <- function(x, val, grad)
{
  c(grad) * (x == c(val))
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
cg.pmax <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(pmax),
    grads = list(
      quote(pmax.grad.x),
      quote(pmax.grad.y)
    ),
    args = list(x, y)
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
pmax.grad.x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad * (x >= c(y))
  }
  else
  {
    bsum(grad * (x >= c(y)), length(x))
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
pmax.grad.y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    grad * (x < c(y))
  }
  else
  {
    bsum(grad * (x < c(y)), length(y))
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
cg.pmin <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(pmin),
    grads = list(
      quote(pmin.grad.x),
      quote(pmin.grad.y)
    ),
    args = list(x, y)
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
pmin.grad.x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad * (x <= c(y))
  }
  else
  {
    bsum(grad * (x <= c(y)), length(x))
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
pmin.grad.y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    grad * (x > c(y))
  }
  else
  {
    bsum(grad * (x > c(y)), length(y))
  }
}
