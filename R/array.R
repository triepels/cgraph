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
#' @seealso \link[base:matmult]{matmult}
#'
#' @author Ron Triepels
#' @export
cg_matmul <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`%*%`),
    grads = list(
      quote(matmul_grad_x),
      quote(matmul_grad_y)
    ),
    args = list(x, y)
  )
}

#' Matrix Multiplication Gradient
#'
#' Calculate the gradient of \code{x \%*\% y} with respect to \code{x}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{x \%*\% y}.
#' @param grad numeric matrix, gradient of \code{x}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
matmul_grad_x <- function(x, y, val, grad)
{
  tcrossprod(grad, y)
}

#' Matrix Multiplication Gradient
#'
#' Calculate the gradient of \code{x \%*\% y} with respect to \code{y}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{x \%*\% y}.
#' @param grad numeric matrix, gradient of \code{y}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
matmul_grad_y <- function(x, y, val, grad)
{
  crossprod(x, grad)
}

#' Matrix Crossproduct
#'
#' Calculate \code{crossprod(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix (optional).
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:crossprod]{crossprod}
#'
#' @author Ron Triepels
#' @export
cg_crossprod <- function(x, y = NULL, name = NULL)
{
  if(is.null(y))
  {
    args <- list(x, x)
  }
  else
  {
    args <- list(x, y)
  }

  cgraph::opr(name = name,
    call = quote(crossprod),
    grads = list(
      quote(crossprod_grad_x),
      quote(crossprod_grad_y)
    ),
    args = args
  )
}

#' Matrix Crossproduct Gradient
#'
#' Calculate the gradient of \code{crossprod(x, y)} with respect to \code{x}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{crossprod(x, y)}.
#' @param grad numeric matrix, gradient of \code{x}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
crossprod_grad_x <- function(x, y, val, grad)
{
  y %*% grad
}

#' Matrix Crossproduct Gradient
#'
#' Calculate the gradient of \code{crossprod(x, y)} with respect to \code{y}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{crossprod(x, y)}.
#' @param grad numeric matrix, gradient of \code{y}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
crossprod_grad_y <- function(x, y, val, grad)
{
  x %*% grad
}

#' Transpose Matrix Crossproduct
#'
#' Calculate \code{tcrossprod(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix (optional).
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:crossprod]{tcrossprod}
#'
#' @author Ron Triepels
#' @export
cg_tcrossprod <- function(x, y = NULL, name = NULL)
{
  if(missing(y))
  {
    args <- list(x, x)
  }
  else
  {
    args <- list(x, y)
  }

  cgraph::opr(name = name,
    call = quote(tcrossprod),
    grads = list(
      quote(tcrossprod_grad_x),
      quote(tcrossprod_grad_y)
    ),
    args = args
  )
}

#' Transpose Matrix Crossproduct Gradient
#'
#' Calculate the gradient of \code{tcrossprod(x, y)} with respect to \code{x}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{tcrossprod(x, y)}.
#' @param grad numeric matrix, gradient of \code{x}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
tcrossprod_grad_x <- function(x, y, val, grad)
{
  grad %*% y
}

#' Transpose Matrix Crossproduct Gradient
#'
#' Calculate the gradient of \code{tcrossprod(x, y)} with respect to \code{y}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{tcrossprod(x, y)}.
#' @param grad numeric matrix, gradient of \code{y}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
tcrossprod_grad_y <- function(x, y, val, grad)
{
  grad %*% x
}

#' Linear Transformation
#'
#' Calculate \code{linear(x, y, z)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param z cg.node, placeholder for a numeric vector.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @note This function is equivalent to \code{cg.matmul(x, y) + cg.as.numeric(z)}.
#'
#' @seealso \link[cgraph:linear]{linear}
#'
#' @author Ron Triepels
#' @export
cg_linear <- function(x, y, z, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(linear),
    grads = list(
      quote(linear_grad_x),
      quote(linear_grad_y),
      quote(linear_grad_z)
    ),
    args = list(x, y, z)
  )
}


#' Linear Transformation
#'
#' Calculate the linear transformation \code{x \%*\% y + c(z)}.
#'
#' @param x numeric matrix.
#' @param y numeric matrix.
#' @param z numeric vector.
#'
#' @return numeric matrix, result of the transformation.
#'
#' @author Ron Triepels
#' @export
linear <- function(x, y, z)
{
  x %*% y + c(z)
}

#' Linear Transformation Gradient
#'
#' Calculate the gradient of \code{linear(x, y, z)} with respect to \code{x}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{linear(x, y, z)}.
#' @param grad numeric matrix, gradient of \code{x}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
linear_grad_x  <- function(x, y, z, val, grad)
{
  tcrossprod(grad, y)
}

#' Linear Transformation Gradient
#'
#' Calculate the gradient of \code{linear(x, y, z)} with respect to \code{y}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{linear(x, y, z)}.
#' @param grad numeric matrix, gradient of \code{y}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
linear_grad_y <- function(x, y, z, val, grad)
{
  crossprod(x, grad)
}

#' Linear Transformation Gradient
#'
#' Calculate the gradient of \code{linear(x, y, z)} with respect to \code{z}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{linear(x, y, z)}.
#' @param grad numeric matrix, gradient of \code{z}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
linear_grad_z <- function(x, y, z, val, grad)
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
#' @note In contrast to the base \link[base:sum]{sum} function, this function only accepts a single vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @note Function \link[base:sum]{sum} is called without setting argument \code{na.rm}.
#'
#' @seealso \link[base:sum]{sum}
#'
#' @author Ron Triepels
#' @export
cg_sum <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sum),
    grads = list(
      quote(sum_grad)
    ),
    args = list(x)
  )
}

#' Sum of Vector Elements Gradient
#'
#' Calculate the gradient of \code{sum(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{sum(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
sum_grad <- function(x, val, grad)
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
#' @note In contrast to the base \link[base:prod]{prod} function, this function only accepts a single vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @note Function \link[base:prod]{prod} is called without setting argument \code{na.rm}.
#'
#' @seealso \link[base:prod]{prod}
#'
#' @author Ron Triepels
#' @export
cg_prod <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(prod),
    grads = list(
      quote(prod_grad)
    ),
    args = list(x)
  )
}

#' Product of Vector Elements Gradient
#'
#' Calculate the gradient of \code{prod(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{prod(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
prod_grad <- function(x, val, grad)
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
#' @note Function \link[base:colSums]{rowSums} is called without setting argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{rowSums}
#'
#' @author Ron Triepels
#' @export
cg_rowSums <- function(x, name = NULL)
{
 cgraph::opr(name = name,
   call = quote(rowSums),
   grads = list(
     quote(rowSums_grad)
   ),
   args = list(x)
 )
}

#' Row Sums Gradient
#'
#' Calculate the gradient of \code{rowSums(x)} with respect to \code{x}.
#'
#' @param x numeric array, value of \code{x}.
#' @param val numeric array, value of \code{rowSums(x)}.
#' @param grad numeric array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
rowSums_grad <- function(x, val, grad)
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
#' @note Function \link[base:colSums]{colSums} is called without setting argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{colSums}
#'
#' @author Ron Triepels
cg_colSums <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(colSums),
    grads = list(
      quote(colSums_grad)
    ),
    args = list(x)
  )
}

#' Column Sums Gradient
#'
#' Calculate the gradient of \code{colSums(x)} with respect to \code{x}.
#'
#' @param x numeric array, value of \code{x}.
#' @param val numeric array, value of \code{colSums(x)}.
#' @param grad numeric array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
colSums_grad <- function(x, val, grad)
{
  aperm(array(grad, rev(dim(x))))
}

#' Arithmetic Mean
#'
#' Calculate \code{mean(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @note Function \link[base:mean]{mean} is called without setting argument \code{trim} and \code{na.rm}.
#'
#' @seealso \link[base:mean]{mean}
#'
#' @author Ron Triepels
#' @export
cg_mean <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(mean),
    grads = list(
      quote(mean_grad)
    ),
    args = list(x)
  )
}

#' Arithmetic Mean Gradient
#'
#' Calculate the gradient of \code{mean(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{mean(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
mean_grad <- function(x, val, grad)
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
#' @note Function \link[base:colSums]{rowMeans} is called without setting argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{rowMeans}
#'
#' @author Ron Triepels
#' @export
cg_rowMeans <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(rowMeans),
    grads = list(
      quote(rowMeans_grad)
    ),
    args = list(x)
  )
}

#' Row Means Gradient
#'
#' Calculate the gradient of \code{rowMeans(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{rowMeans(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
rowMeans_grad <- function(x, val, grad)
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
#' @note Function \link[base:colSums]{colMeans} is called without setting argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{colMeans}
#'
#' @author Ron Triepels
#' @export
cg_colMeans <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(colMeans),
    grads = list(
      quote(colMeans_grad)
    ),
    args = list(x)
  )
}

#' Column Means Gradient
#'
#' Calculate the gradient of \code{colMeans(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{colMeans(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
colMeans_grad <- function(x, val, grad)
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
#' @return cg.node, node of the operation.
#'
#' @note Function \link[base:max]{max} is called without setting argument \code{na.rm}.
#'
#' @seealso \link[base:max]{max}
#'
#' @author Ron Triepels
#' @export
cg_max <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(max),
    grads = list(
      quote(max_grad)
    ),
    args = list(x)
  )
}

#' Maxima Gradient
#'
#' Calculate the gradient of \code{max(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{max(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
max_grad <- function(x, val, grad)
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
#' @note Function \link[base:min]{min} is called without setting argument \code{na.rm}.
#'
#' @seealso \link[base:min]{min}
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
#' @export
cg_min <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(min),
    grads = list(
      quote(min_grad)
    ),
    args = list(x)
  )
}

#' Minima Gradient
#'
#' Calculate the gradient of \code{min(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{min(y)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
min_grad <- function(x, val, grad)
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
#' @note Function \link[base:pmax]{pmax} is called without setting argument \code{na.rm}.
#'
#' @seealso \link[base:pmax]{pmax}
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
#' @export
cg_pmax <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(pmax),
    grads = list(
      quote(pmax_grad_x),
      quote(pmax_grad_y)
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
#' @param val numeric vector or array, value of \code{pmax(x, y)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
pmax_grad_x <- function(x, y, val, grad)
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
#' @param val numeric vector or array, value of \code{pmax(x, y)}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
pmax_grad_y <- function(x, y, val, grad)
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
#' @note Function \link[base:pmin]{pmin} is called without setting argument \code{na.rm}.
#'
#' @seealso \link[base:pmin]{pmin}
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
#' @export
cg_pmin <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(pmin),
    grads = list(
      quote(pmin_grad_x),
      quote(pmin_grad_y)
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
#' @param val numeric vector or array, value of \code{pmin(x, y)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
pmin_grad_x <- function(x, y, val, grad)
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
#' @param val numeric vector or array, value of \code{pmin(x, y)}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
pmin_grad_y <- function(x, y, val, grad)
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
