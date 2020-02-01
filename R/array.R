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

#' Dimensions of an Object
#'
#' Calculate \code{dim(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @note This operator is not differentiable. Any attempt to differentiate this operator will result in an error.
#'
#' @seealso \link[base]{dim}
#'
#' @author Ron Triepels
#' @export
cg_dim <- function(x, name = NULL)
{
  cg_operator(dim, list(x), name)
}

# Function definition
delayedAssign("dim", cg_function(def = base::dim))

#' Matrix Multiplication
#'
#' Calculate \code{x \%*\% y}.
#'
#' @param x either a cg_node object or a numerical matrix.
#' @param y either a cg_node object or a numerical matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:matmult]{matmult}
#'
#' @author Ron Triepels
#' @export
cg_matmul <- function(x, y, name = NULL)
{
  cg_operator(matmul, list(x, y), name)
}

# Function definition
delayedAssign("matmul", cg_function(
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
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:crossprod]{crossprod}
#'
#' @author Ron Triepels
#' @export
cg_crossprod <- function(x, y = x, name = NULL)
{
  cg_operator(crossprod, list(x, y), name)
}

# Function definition
delayedAssign("crossprod", cg_function(
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
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:crossprod]{tcrossprod}
#'
#' @author Ron Triepels
#' @export
cg_tcrossprod <- function(x, y = x, name = NULL)
{
  cg_operator(tcrossprod, list(x, y), name)
}

# Function definition
delayedAssign("tcrossprod", cg_function(
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
#' @return cg_operator object, node of the operation.
#'
#' @note This function is equivalent to \code{cg_matmul(x, y) + cg_as_numeric(z)}.
#'
#' @author Ron Triepels
#' @export
cg_linear <- function(x, y, z, name = NULL)
{
  cg_operator(linear, list(x, y, z), name)
}

# Function definition
delayedAssign("linear", cg_function(
  def = function(x, y, z)
  {
    x %*% y + c(z)
  },
  grads = list(
    function(x, y, z, value, grad)
    {
      tcrossprod(grad, y)
    },
    function(x, y, z, value, grad)
    {
      crossprod(x, grad)
    },
    function(x, y, z, value, grad)
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
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:sum]{sum} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:sum]{sum}
#'
#' @author Ron Triepels
#' @export
cg_sum <- function(x, name = NULL)
{
  cg_operator(sum, list(x), name)
}

# Function definition
delayedAssign("sum", cg_function(
  def = base::sum,
  grads = list(
    function(x, value, grad)
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
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:prod]{prod} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:prod]{prod}
#'
#' @author Ron Triepels
#' @export
cg_prod <- function(x, name = NULL)
{
  cg_operator(prod, list(x), name)
}

# Function definition
delayedAssign("prod", cg_function(
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
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:colSums]{rowSums} is called without changing the default value of argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{rowSums}
#'
#' @author Ron Triepels
#' @export
cg_rowsums <- function(x, name = NULL)
{
  cg_operator(rowsums, list(x), name)
}

# Function definition
delayedAssign("rowsums", cg_function(
  def = base::rowSums,
  grads = list(
    function(x, value, grad)
    {
      array(grad, dim(x))
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
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:colSums]{colSums} is called without changing the default value of argument \code{na.rm} and \code{dims}.
#'
#' @seealso \link[base:colSums]{colSums}
#'
#' @author Ron Triepels
#' @export
cg_colsums <- function(x, name = NULL)
{
  cg_operator(colsums, list(x), name)
}

# Function definition
delayedAssign("colsums", cg_function(
  def = base::colSums,
  grads = list(
    function(x, value, grad)
    {
      aperm(array(grad, rev(dim(x))))
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
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:mean]{mean} is called without changing the default value of argument \code{trim} and \code{na.rm}.
#'
#' @seealso \link[base:mean]{mean}
#'
#' @author Ron Triepels
#' @export
cg_mean <- function(x, name = NULL)
{
  cg_operator(mean, list(x), name)
}

# Function definition
delayedAssign("mean", cg_function(
  def = base::mean.default,
  grads = list(
    function(x, value, grad)
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
  )
))

#' Maxima
#'
#' Calculate \code{max(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:Extremes]{max} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:Extremes]{max}
#'
#' @author Ron Triepels
#' @export
cg_max <- function(x, name = NULL)
{
  cg_operator(max, list(x), name)
}

# Function definition
delayedAssign("max", cg_function(
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
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:Extremes]{min} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:Extremes]{min}
#'
#' @author Ron Triepels
#' @export
cg_min <- function(x, name = NULL)
{
  cg_operator(min, list(x), name)
}

# Function definition
delayedAssign("min", cg_function(
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
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:Extremes]{pmax} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:Extremes]{pmax}
#'
#' @author Ron Triepels
#' @export
cg_pmax <- function(x, y, name = NULL)
{
  cg_operator(pmax, list(x, y), name)
}

# Function definition
delayedAssign("pmax", cg_function(
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
#' @return cg_operator object, node of the operation.
#'
#' @note Function \link[base:Extremes]{pmin} is called without changing the default value of argument \code{na.rm}.
#'
#' @seealso \link[base:Extremes]{pmin}
#'
#' @author Ron Triepels
#' @export
cg_pmin <- function(x, y, name = NULL)
{
  cg_operator(pmin, list(x, y), name)
}

# Function definition
delayedAssign("pmin", cg_function(
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

#' Coerce to a Numerical Vector
#'
#' Coerce \code{x} to a one-dimensional numerical vector.
#'
#' @param x either a cg_node object or a numerical matrix or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @note This function is identical to \code{cg_as_numeric}.
#'
#' @seealso \link[base:double]{as.double}
#'
#' @author Ron Triepels
#' @export
cg_as_double <- function(x, name = NULL)
{
  cg_operator(as_double, list(x), name)
}

# Function definition
delayedAssign("as_double", cg_function(
  def = base::as.double,
  grads = list(
    function(x, value, grad)
    {
      if(is.array(x))
      {
        array(grad, dim(x))
      }
      else
      {
        grad
      }
    }
  )
))

#' Coerce to a Numerical Vector
#'
#' Coerce \code{x} to a one-dimensional numerical vector.
#'
#' @param x either a cg_node object or a numerical matrix or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @note This function is identical to \code{cg_as_double}.
#'
#' @seealso \link[base:double]{as.numeric}
#'
#' @author Ron Triepels
#' @export
cg_as_numeric <- function(x, name = NULL)
{
  cg_operator(as_double, list(x), name)
}

#' Matrix Transpose
#'
#' Perform \code{t(x)}.
#'
#' @param x either a cg_node object or a numerical matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:t]{t}
#'
#' @author Ron Triepels
#' @export
cg_t <- function(x, name = NULL)
{
  cg_operator(t, list(x), name)
}

# Function definition
delayedAssign("t", cg_function(
  def = base::t.default,
  grads = list(
    function(x, value, grad)
    {
      t.default(grad)
    }
  )
))
