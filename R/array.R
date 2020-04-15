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

#' Matrices
#'
#' Create a matrix.
#'
#' @param data either a cg_node object or a numerical vector.
#' @param nrow either a cg_node object or a numerical scalar.
#' @param ncol either a cg_node object or a numerical scalar.
#' @param byrow either a cg_node object or a logical scalar.
#' @param dimnames either a cg_node object or a character vector.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable. Any attempt to differentiate this operator will result in an error.
#'
#' @seealso \link[base:matrix]{matrix}
#'
#' @author Ron Triepels
#' @export
cg_matrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL, name = NULL)
{
  cg_operator(cg_fun_matrix, list(data, nrow, ncol, byrow, dimnames), name)
}

# Function definition
delayedAssign("cg_fun_matrix", cg_function(def = base::matrix))

#' Multidimensional Arrays
#'
#' Create a multidimensional array.
#'
#' @param data either a cg_node object or a numerical vector.
#' @param dim either a cg_node object or a numerical vector.
#' @param dimnames either a cg_node object or a character vector.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable. Any attempt to differentiate this operator will result in an error.
#'
#' @seealso \link[base:array]{array}
#'
#' @author Ron Triepels
#' @export
cg_array <- function(data = NA, dim = length(data), dimnames = NULL, name = NULL)
{
  cg_operator(cg_fun_array, list(data, dim, dimnames), name)
}

# Function definition
delayedAssign("cg_fun_array", cg_function(def = base::array))

#' Dimensions of an Array
#'
#' Calculate \code{dim(x)}.
#'
#' @param x either a cg_node object or a numerical array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable. Any attempt to differentiate this operator will result in an error.
#'
#' @seealso \link[base:dim]{dim}
#'
#' @author Ron Triepels
#' @export
cg_dim <- function(x, name = NULL)
{
  cg_operator(cg_fun_dim, list(x), name)
}

# Function definition
delayedAssign("cg_fun_dim", cg_function(def = base::dim))

#' Number of Rows of an Array
#'
#' Calculate \code{nrow(x)}.
#'
#' @param x either a cg_node object or a numerical array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable. Any attempt to differentiate this operator will result in an error.
#'
#' @seealso \link[base:nrow]{nrow}
#'
#' @author Ron Triepels
#' @export
cg_nrow <- function(x, name = NULL)
{
  cg_operator(cg_fun_nrow, list(x), name)
}

# Function definition
delayedAssign("cg_fun_nrow", cg_function(def = base::nrow))

#' Number of Columns of an Array
#'
#' Calculate \code{ncol(x)}.
#'
#' @param x either a cg_node object or a numerical array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable. Any attempt to differentiate this operator will result in an error.
#'
#' @seealso \link[base:nrow]{ncol}
#'
#' @author Ron Triepels
#' @export
cg_ncol <- function(x, name = NULL)
{
  cg_operator(cg_fun_ncol, list(x), name)
}

# Function definition
delayedAssign("cg_fun_ncol", cg_function(def = base::ncol))

#' Matrix Multiplication
#'
#' Calculate \code{x \%*\% y}.
#'
#' @param x either a cg_node object or a numerical matrix.
#' @param y either a cg_node object or a numerical matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:matmult]{matmult}
#'
#' @author Ron Triepels
#' @export
cg_matmul <- function(x, y, name = NULL)
{
  cg_operator(cg_fun_matmul, list(x, y), name)
}

# Function definition
delayedAssign("cg_fun_matmul", cg_function(
  def = base::`%*%`,
  grads = list(
    function(x, y, value, grad)
    {
      tcrossprod(grad, y)
    },
    function(x, y, value, grad)
    {
      crossprod(x, grad)
    }
  )
))

#' Matrix Crossproduct
#'
#' Calculate \code{crossprod(x, y)}.
#'
#' @param x either a cg_node object or a numerical matrix.
#' @param y either a cg_node object or a numerical matrix (optional).
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:crossprod]{crossprod}
#'
#' @author Ron Triepels
#' @export
cg_crossprod <- function(x, y = x, name = NULL)
{
  cg_operator(cg_fun_crossprod, list(x, y), name)
}

# Function definition
delayedAssign("cg_fun_crossprod", cg_function(
  def = base::crossprod,
  grads = list(
    function(x, y, value, grad)
    {
      y %*% grad
    },
    function(x, y, value, grad)
    {
      x %*% grad
    }
  )
))

#' Transpose Matrix Crossproduct
#'
#' Calculate \code{tcrossprod(x, y)}.
#'
#' @param x either a cg_node object or a numerical matrix.
#' @param y either a cg_node object or a numerical matrix (optional).
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:crossprod]{tcrossprod}
#'
#' @author Ron Triepels
#' @export
cg_tcrossprod <- function(x, y = x, name = NULL)
{
  cg_operator(cg_fun_tcrossprod, list(x, y), name)
}

# Function definition
delayedAssign("cg_fun_tcrossprod", cg_function(
  def = base::tcrossprod,
  grads = list(
    function(x, y, value, grad)
    {
      grad %*% y
    },
    function(x, y, value, grad)
    {
      grad %*% x
    }
  )
))

#' Linear Transformation
#'
#' Calculate \code{x \%*\% y + c(z)}.
#'
#' @param x either a cg_node object or a numerical matrix.
#' @param y either a cg_node object or a numerical matrix.
#' @param z either a cg_node object or a numerical vector.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This function is deprecated and will be removed in the next major release. Please use function \link[cgraph:cg_linear1]{cg_linear1} instead.
#'
#' @author Ron Triepels
#' @export
cg_linear <- function(x, y, z, name = NULL)
{
  .Deprecated("cg_linear1")
  cg_operator(cg_fun_linear1, list(x, y, z), name)
}

#' Linear Transformation
#'
#' Calculate \code{x1 \%*\% y1 + c(z)}.
#'
#' @param x1 either a cg_node object or a numerical matrix.
#' @param y1 either a cg_node object or a numerical matrix.
#' @param z either a cg_node object or a numerical vector (optional).
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This function is equivalent to \code{cg_matmul(x1, y1) + cg_as_numeric(z)} but shorter and more efficient.
#'
#' @author Ron Triepels
#' @export
cg_linear1 <- function(x1, y1, z = NULL, name = NULL)
{
  if(is.null(z))
  {
    cg_operator(cg_fun_linear1, list(x1, y1), name)
  }
  else
  {
    cg_operator(cg_fun_linear1, list(x1, y1, z), name)
  }
}

# Function definition
delayedAssign("cg_fun_linear1", cg_function(
  def = function(x1, y1, z = NULL)
  {
    if(is.null(z))
    {
      x1 %*% y1
    }
    else
    {
      x1 %*% y1 + c(z)
    }
  },
  grads = list(
    function(x1, y1, z, value, grad)
    {
      tcrossprod(grad, y1)
    },
    function(x1, y1, z, value, grad)
    {
      crossprod(x1, grad)
    },
    function(x1, y1, z, value, grad)
    {
      grad <- bsum(grad, length(z))
      dim(grad) <- dim(z)
      grad
    }
  )
))

#' Linear Transformation
#'
#' Calculate \code{x1 \%*\% y1 + x2 \%*\% y2 + c(z)}.
#'
#' @param x1 either a cg_node object or a numerical matrix.
#' @param y1 either a cg_node object or a numerical matrix.
#' @param x2 either a cg_node object or a numerical matrix.
#' @param y2 either a cg_node object or a numerical matrix.
#' @param z either a cg_node object or a numerical vector (optional).
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This function is equivalent to \code{cg_matmul(x1, y1) + cg_matmul(x2, y2) + cg_as_numeric(z)} but shorter and more efficient.
#'
#' @author Ron Triepels
#' @export
cg_linear2 <- function(x1, y1, x2, y2, z = NULL, name = NULL)
{
  if(is.null(z))
  {
    cg_operator(cg_fun_linear2, list(x1, y1, x2, y2), name)
  }
  else
  {
    cg_operator(cg_fun_linear2, list(x1, y1, x2, y2, z), name)
  }
}

# Function definition
delayedAssign("cg_fun_linear2", cg_function(
  def = function(x1, y1, x2, y2, z = NULL)
  {
    if(is.null(z))
    {
      x1 %*% y1 + x2 %*% y2
    }
    else
    {
      x1 %*% y1 + x2 %*% y2 + c(z)
    }
  },
  grads = list(
    function(x1, y1, x2, y2, z, value, grad)
    {
      tcrossprod(grad, y1)
    },
    function(x1, y1, x2, y2, z, value, grad)
    {
      crossprod(x1, grad)
    },
    function(x1, y1, x2, y2, z, value, grad)
    {
      tcrossprod(grad, y2)
    },
    function(x1, y1, x2, y2, z, value, grad)
    {
      crossprod(x2, grad)
    },
    function(x1, y1, x2, y2, z, value, grad)
    {
      grad <- bsum(grad, length(z))
      dim(grad) <- dim(z)
      grad
    }
  )
))

#' Sum of Vector Elements
#'
#' Calculate \code{sum(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \link[base:sum]{sum} function, this function only accepts a single argument.
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:sum]{sum} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:sum]{sum}
#'
#' @author Ron Triepels
#' @export
cg_sum <- function(x, name = NULL)
{
  cg_operator(cg_fun_sum, list(x), name)
}

# Function definition
delayedAssign("cg_fun_sum", cg_function(
  def = base::sum,
  grads = list(
    function(x, value, grad)
    {
      grad <- rep(grad, length(x))
      dim(grad) <- dim(x)
      grad
    }
  )
))

#' Product of Vector Elements
#'
#' Calculate \code{prod(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \link[base:prod]{prod} function, this function only accepts a single argument.
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:prod]{prod} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:prod]{prod}
#'
#' @author Ron Triepels
#' @export
cg_prod <- function(x, name = NULL)
{
  cg_operator(cg_fun_prod, list(x), name)
}

# Function definition
delayedAssign("cg_fun_prod", cg_function(
  def = base::prod,
  grads = list(
    function(x, value, grad)
    {
      grad * value / x
    }
  )
))

#' Row Sums
#'
#' Calculate \code{rowSums(x)}.
#'
#' @param x either a cg_node object or a numerical matrix or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:colSums]{rowSums} is called without changing the default value of argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{rowSums}
#'
#' @author Ron Triepels
#' @export
cg_rowsums <- function(x, name = NULL)
{
  cg_operator(cg_fun_rowsums, list(x), name)
}

# Function definition
delayedAssign("cg_fun_rowsums", cg_function(
  def = base::rowSums,
  grads = list(
    function(x, value, grad)
    {
      grad <- rep(grad, nrow(x))
      dim(grad) <- dim(x)
      grad
    }
  )
))

#' Column Sums
#'
#' Calculate \code{colSums(x)}.
#'
#' @param x either a cg_node object or a numerical matrix or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:colSums]{colSums} is called without changing the default value of argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{colSums}
#'
#' @author Ron Triepels
#' @export
cg_colsums <- function(x, name = NULL)
{
  cg_operator(cg_fun_colsums, list(x), name)
}

# Function definition
delayedAssign("cg_fun_colsums", cg_function(
  def = base::colSums,
  grads = list(
    function(x, value, grad)
    {
      grad <- rep(grad, nrow(x))
      dim(grad) <- rev(dim(x))
      aperm.default(grad)
    }
  )
))

#' Arithmetic Mean
#'
#' Calculate \code{mean(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:mean]{mean} is called without changing the default value of argument \code{trim} and \code{na.rm}.
#'
#' @seealso \link[base:mean]{mean}
#'
#' @author Ron Triepels
#' @export
cg_mean <- function(x, name = NULL)
{
  cg_operator(cg_fun_mean, list(x), name)
}

# Function definition
delayedAssign("cg_fun_mean", cg_function(
  def = base::mean.default,
  grads = list(
    function(x, value, grad)
    {
      grad <- rep(grad / length(x), length(x))
      dim(grad) <- dim(x)
      grad
    }
  )
))

#' Row Means
#'
#' Calculate \code{rowMeans(x)}.
#'
#' @param x either a cg_node object or a numerical matrix or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:colSums]{rowMeans} is called without changing the default value of argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{rowMeans}
#'
#' @author Ron Triepels
#' @export
cg_rowmeans <- function(x, name = NULL)
{
  cg_operator(cg_fun_rowmeans, list(x), name)
}

# Function definition
delayedAssign("cg_fun_rowmeans", cg_function(
  def = base::rowMeans,
  grads = list(
    function(x, value, grad)
    {
      grad <- rep(grad / prod(dim(x)[-1L]), nrow(x))
      dim(grad) <- dim(x)
      grad
    }
  )
))

#' Column Means
#'
#' Calculate \code{colMeans(x)}.
#'
#' @param x either a cg_node object or a numerical matrix or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:colSums]{colMeans} is called without changing the default value of argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{colMeans}
#'
#' @author Ron Triepels
#' @export
cg_colmeans <- function(x, name = NULL)
{
  cg_operator(cg_fun_colmeans, list(x), name)
}

# Function definition
delayedAssign("cg_fun_colmeans", cg_function(
  def = base::colMeans,
  grads = list(
    function(x, value, grad)
    {
      grad <- rep(grad / nrow(x), nrow(x))
      dim(grad) <- rev(dim(x))
      aperm.default(grad)
    }
  )
))

#' Maxima
#'
#' Calculate \code{max(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:Extremes]{max} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:Extremes]{max}
#'
#' @author Ron Triepels
#' @export
cg_max <- function(x, name = NULL)
{
  cg_operator(cg_fun_max, list(x), name)
}

# Function definition
delayedAssign("cg_fun_max", cg_function(
  def = base::max,
  grads = list(
    function(x, value, grad)
    {
      c(grad) * (x == c(value))
    }
  )
))

#' Minima
#'
#' Calculate \code{min(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:Extremes]{min} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:Extremes]{min}
#'
#' @author Ron Triepels
#' @export
cg_min <- function(x, name = NULL)
{
  cg_operator(cg_fun_min, list(x), name)
}

# Function definition
delayedAssign("cg_fun_min", cg_function(
  def = base::min,
  grads = list(
    function(x, value, grad)
    {
      c(grad) * (x == c(value))
    }
  )
))

#' Parallel Maxima
#'
#' Calculate \code{pmax(x, y)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param y either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:Extremes]{pmax} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:Extremes]{pmax}
#'
#' @author Ron Triepels
#' @export
cg_pmax <- function(x, y, name = NULL)
{
  cg_operator(cg_fun_pmax, list(x, y), name)
}

# Function definition
delayedAssign("cg_fun_pmax", cg_function(
  def = base::pmax,
  grads = list(
    function(x, y, value, grad)
    {
      if(is.array(x))
      {
        grad * (x >= c(y))
      }
      else
      {
        bsum(grad * (x >= c(y)), length(x))
      }
    },
    function(x, y, value, grad)
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
  )
))

#' Parallel Minima
#'
#' Calculate \code{pmin(x, y)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param y either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note Function \link[base:Extremes]{pmin} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:Extremes]{pmin}
#'
#' @author Ron Triepels
#' @export
cg_pmin <- function(x, y, name = NULL)
{
  cg_operator(cg_fun_pmin, list(x, y), name)
}

# Function definition
delayedAssign("cg_fun_pmin", cg_function(
  def = base::pmin,
  grads = list(
    function(x, y, value, grad)
    {
      if(is.array(x))
      {
        grad * (x <= c(y))
      }
      else
      {
        bsum(grad * (x <= c(y)), length(x))
      }
    },
    function(x, y, value, grad)
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
  )
))

#' Matrix Transpose
#'
#' Calculate \code{t(x)}.
#'
#' @param x either a cg_node object or a numerical matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:t]{t}
#'
#' @author Ron Triepels
#' @export
cg_t <- function(x, name = NULL)
{
  cg_operator(cg_fun_t, list(x), name)
}

# Function definition
delayedAssign("cg_fun_t", cg_function(
  def = base::t.default,
  grads = list(
    function(x, value, grad)
    {
      t.default(grad)
    }
  )
))
