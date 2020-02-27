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

#' Length of an Object
#'
#' Calculate \code{length(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable. Any attempt to differentiate this operator will result in an error.
#'
#' @seealso \link[base]{length}
#'
#' @author Ron Triepels
#' @export
cg_length <- function(x, name = NULL)
{
  cg_operator(length, list(x), name)
}

# Function definition
delayedAssign("length", cg_function(def = base::length))

#' Coerce to a Numerical Vector
#'
#' Coerce \code{x} to a one-dimensional numerical vector.
#'
#' @param x either a cg_node object or a numerical matrix or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
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
#' @return cg_operator object.
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

#' Positive
#'
#' Calculate \code{x}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Arithmetic]{positive}
#'
#' @author Ron Triepels
#' @export
cg_pos <- function(x, name = NULL)
{
  cg_operator(pos, list(x), name)
}

# Function definition
delayedAssign("pos", cg_function(
  def = function(x, out)
  {
    .Call("cg_pos_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_pos_grad", x, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Negative
#'
#' Calculate \code{-x}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Arithmetic]{negative}
#'
#' @author Ron Triepels
#' @export
cg_neg <- function(x, name = NULL)
{
  cg_operator(neg, list(x), name)
}

# Function definition
delayedAssign("neg", cg_function(
  def = function(x, out)
  {
    .Call("cg_neg_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_neg_grad", x, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Add
#'
#' Calculate \code{x + y}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param y either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Arithmetic]{add}
#'
#' @author Ron Triepels
#' @export
cg_add <- function(x, y, name = NULL)
{
  cg_operator(add, list(x, y), name)
}

# Function definition
delayedAssign("add", cg_function(
  def = function(x, y, out)
  {
    .Call("cg_add_def", x, y, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, y, value, grad, out)
    {
      .Call("cg_pos_grad", x, grad, out, PACKAGE = "cgraph")
    },
    function(x, y, value, grad, out)
    {
      .Call("cg_pos_grad", y, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' @export
#' @author Ron Triepels
`+.cg_node` <- function(x, y)
{
  if(missing(y))
  {
    cg_pos(x)
  }
  else
  {
    cg_add(x, y)
  }
}

#' Subtract
#'
#' Calculate \code{x - y}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param y either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Arithmetic]{subtract}
#'
#' @author Ron Triepels
#' @export
cg_sub <- function(x, y, name = NULL)
{
  cg_operator(sub, list(x, y), name)
}

# Function definition
delayedAssign("sub", cg_function(
  def = function(x, y, out)
  {
    .Call("cg_sub_def", x, y, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, y, value, grad, out)
    {
      .Call("cg_pos_grad", x, grad, out, PACKAGE = "cgraph")
    },
    function(x, y, value, grad, out)
    {
      .Call("cg_neg_grad", y, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' @export
#' @author Ron Triepels
`-.cg_node` <- function(x, y)
{
  if(missing(y))
  {
    cg_neg(x)
  }
  else
  {
    cg_sub(x, y)
  }
}

#' Multiply
#'
#' Calculate \code{x * y}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param y either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Arithmetic]{multiply}
#'
#' @author Ron Triepels
#' @export
cg_mul <- function(x, y, name = NULL)
{
  cg_operator(mul, list(x, y), name)
}

# Function definition
delayedAssign("mul", cg_function(
  def = function(x, y, out)
  {
    .Call("cg_mul_def", x, y, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, y, value, grad, out)
    {
      .Call("cg_mul_grad_x", x, y, grad, out, PACKAGE = "cgraph")
    },
    function(x, y, value, grad, out)
    {
      .Call("cg_mul_grad_y", x, y, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' @export
#' @author Ron Triepels
`*.cg_node` <- function(x, y)
{
  cg_mul(x, y)
}

#' Divide
#'
#' Calculate \code{x / y}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param y either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Arithmetic]{divide}
#'
#' @author Ron Triepels
#' @export
cg_div <- function(x, y, name = NULL)
{
  cg_operator(div, list(x, y), name)
}

# Function definition
delayedAssign("div", cg_function(
  def = function(x, y, out)
  {
    .Call("cg_div_def", x, y, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, y, value, grad, out)
    {
      .Call("cg_div_grad_x", x, y, grad, out, PACKAGE = "cgraph")
    },
    function(x, y, value, grad, out)
    {
      .Call("cg_div_grad_y", x, y, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' @export
#' @author Ron Triepels
`/.cg_node` <- function(x, y)
{
  cg_div(x, y)
}

#' Power
#'
#' Calculate \code{x ^ y}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param y either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Arithmetic]{power}
#'
#' @author Ron Triepels
#' @export
cg_pow <- function(x, y, name = NULL)
{
  cg_operator(pow, list(x, y), name)
}

# Function definition
delayedAssign("pow", cg_function(
  def = function(x, y, out)
  {
    .Call("cg_pow_def", x, y, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, y, value, grad, out)
    {
      .Call("cg_pow_grad_x", x, y, grad, out, PACKAGE = "cgraph")
    },
    function(x, y, value, grad, out)
    {
      .Call("cg_pow_grad_y", x, y, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' @export
#' @author Ron Triepels
`^.cg_node` <- function(x, y)
{
  cg_pow(x, y)
}

#' Square
#'
#' Calculate \code{x ^ 2}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This function is equivalent to \code{cg_pow(x, 2)} but more efficient.
#'
#' @seealso \link[base:Arithmetic]{square}
#'
#' @author Ron Triepels
#' @export
cg_square <- function(x, name = NULL)
{
  cg_operator(square, list(x), name)
}

# Function definition
delayedAssign("square", cg_function(
  def = function(x, out)
  {
    .Call("cg_square_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_square_grad", x, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:MathFun]{sqrt}
#'
#' @author Ron Triepels
#' @export
cg_sqrt <- function(x, name = NULL)
{
  cg_operator(sqrt, list(x), name)
}

# Function definition
delayedAssign("sqrt", cg_function(
  def = function(x, out)
  {
    .Call("cg_sqrt_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_sqrt_grad", x, value, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Cubic Root
#'
#' Calculate \code{x^(1/3)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @author Ron Triepels
#' @export
cg_cbrt <- function(x, name = NULL)
{
  cg_operator(cbrt, list(x), name)
}

# Function definition
delayedAssign("cbrt", cg_function(
  def = function(x, out)
  {
    .Call("cg_cbrt_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_cbrt_grad", x, value, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Exponential Function
#'
#' Calculate \code{exp(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:log]{exp}
#'
#' @author Ron Triepels
#' @export
cg_exp <- function(x, name = NULL)
{
  cg_operator(exp, list(x), name)
}

# Function definition
delayedAssign("exp", cg_function(
  def = function(x, out)
  {
    .Call("cg_exp_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_exp_grad", x, value, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Base-2 Exponential Function
#'
#' Calculate \code{2^x}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @author Ron Triepels
#' @export
cg_exp2 <- function(x, name = NULL)
{
  cg_operator(exp2, list(x), name)
}

# Function definition
delayedAssign("exp2", cg_function(
  def = function(x, out)
  {
    .Call("cg_exp2_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_exp2_grad", x, value, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Natural Logarithm
#'
#' Calculate \code{log(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:log]{log}
#'
#' @author Ron Triepels
#' @export
cg_ln <- function(x, name = NULL)
{
  cg_operator(ln, list(x), name)
}

# Function definition
delayedAssign("ln", cg_function(
  def = function(x, out)
  {
    .Call("cg_ln_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_ln_grad", x, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Logarithm Base 2
#'
#' Calculate \code{log2(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:log]{log2}
#'
#' @author Ron Triepels
#' @export
cg_log2 <- function(x, name = NULL)
{
  cg_operator(log2, list(x), name)
}

# Function definition
delayedAssign("log2", cg_function(
  def = function(x, out)
  {
    .Call("cg_log2_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_log2_grad", x, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Logarithm Base 10
#'
#' Calculate \code{log10(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:log]{log10}
#'
#' @author Ron Triepels
#' @export
cg_log10 <- function(x, name = NULL)
{
  cg_operator(log10, list(x), name)
}

# Function definition
delayedAssign("log10", cg_function(
  def = function(x, out)
  {
    .Call("cg_log10_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_log10_grad", x, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:MathFun]{abs}
#'
#' @author Ron Triepels
#' @export
cg_abs <- function(x, name = NULL)
{
  cg_operator(abs, list(x), name)
}

# Function definition
delayedAssign("abs", cg_function(
  def = function(x, out)
  {
    .Call("cg_abs_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_abs_grad", x, value, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Sine
#'
#' Calculate \code{sin(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Trig]{sin}
#'
#' @author Ron Triepels
#' @export
cg_sin <- function(x, name = NULL)
{
  cg_operator(sin, list(x), name)
}

# Function definition
delayedAssign("sin", cg_function(
  def = function(x, out)
  {
    .Call("cg_sin_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_sin_grad", x, grad, out, PACKAGE = "cgraph")
    }
  )
))

#' Cosine
#'
#' Calculate \code{cos(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Trig]{cos}
#'
#' @author Ron Triepels
#' @export
cg_cos <- function(x, name = NULL)
{
  cg_operator(cos, list(x), name)
}

# Function definition
delayedAssign("cos", cg_function(
  def = base::cos,
  grads = list(
    function(x, value, grad)
    {
      -grad * sin(x)
    }
  )
))

#' Tangent
#'
#' Calculate \code{tan(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Trig]{tan}
#'
#' @author Ron Triepels
#' @export
cg_tan <- function(x, name = NULL)
{
  cg_operator(tan, list(x), name)
}

# Function definition
delayedAssign("tan", cg_function(
  def = base::tan,
  grads = list(
    function(x, value, grad)
    {
      grad / cos(x) ^ 2
    }
  )
))

#' Hyperbolic Sine
#'
#' Calculate \code{sinh(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Hyperbolic]{sinh}
#'
#' @author Ron Triepels
#' @export
cg_sinh <- function(x, name = NULL)
{
  cg_operator(sinh, list(x), name)
}

# Function definition
delayedAssign("sinh", cg_function(
  def = base::sinh,
  grads = list(
    function(x, value, grad)
    {
      grad * cosh(x)
    }
  )
))

#' Hyperbolic Cosine
#'
#' Calculate \code{cosh(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Hyperbolic]{cosh}
#'
#' @author Ron Triepels
#' @export
cg_cosh <- function(x, name = NULL)
{
  cg_operator(cosh, list(x), name)
}

# Function definition
delayedAssign("cosh", cg_function(
  def = base::cosh,
  grads = list(
    function(x, value, grad)
    {
      grad * sinh(x)
    }
  )
))

#' Hyperbolic Tangent
#'
#' Calculate \code{tanh(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Hyperbolic]{tanh}
#'
#' @author Ron Triepels
#' @export
cg_tanh <- function(x, name = NULL)
{
  cg_operator(tanh, list(x), name)
}

# Function definition
delayedAssign("tanh", cg_function(
  def = base::tanh,
  grads = list(
    function(x, value, grad)
    {
      grad * (1 - value ^ 2)
    }
  )
))

#' Inverse Sine
#'
#' Calculate \code{asin(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Trig]{asin}
#'
#' @author Ron Triepels
#' @export
cg_asin <- function(x, name = NULL)
{
  cg_operator(asin, list(x), name)
}

# Function definition
delayedAssign("asin", cg_function(
  def = base::asin,
  grads = list(
    function(x, value, grad)
    {
      grad / sqrt(1 - x ^ 2)
    }
  )
))

#' Inverse Cosine
#'
#' Calculate \code{acos(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Trig]{acos}
#'
#' @author Ron Triepels
#' @export
cg_acos <- function(x, name = NULL)
{
  cg_operator(acos, list(x), name)
}

# Function definition
delayedAssign("acos", cg_function(
  def = base::acos,
  grads = list(
    function(x, value, grad)
    {
      -grad / sqrt(1 - x ^ 2)
    }
  )
))

#' Inverse Tangent
#'
#' Calculate \code{atan(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Trig]{atan}
#'
#' @author Ron Triepels
#' @export
cg_atan <- function(x, name = NULL)
{
  cg_operator(atan, list(x), name)
}

# Function definition
delayedAssign("atan", cg_function(
  def = base::atan,
  grads = list(
    function(x, value, grad)
    {
      grad / (x ^ 2 + 1)
    }
  )
))

#' Inverse Hyperbolic Sine
#'
#' Calculate \code{asinh(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Hyperbolic]{asinh}
#'
#' @author Ron Triepels
#' @export
cg_asinh <- function(x, name = NULL)
{
  cg_operator(asinh, list(x), name)
}

# Function definition
delayedAssign("asinh", cg_function(
  def = base::asinh,
  grads = list(
    function(x, value, grad)
    {
      grad / sqrt(x ^ 2 + 1)
    }
  )
))

#' Inverse Hyperbolic Cosine
#'
#' Calculate \code{acosh(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Hyperbolic]{acosh}
#'
#' @author Ron Triepels
#' @export
cg_acosh <- function(x, name = NULL)
{
  cg_operator(acosh, list(x), name)
}

# Function definition
delayedAssign("acosh", cg_function(
  def = base::acosh,
  grads = list(
    function(x, value, grad)
    {
      grad / sqrt(x ^ 2 - 1)
    }
  )
))

#' Inverse Hyperbolic Tangent
#'
#' Calculate \code{atanh(x)}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @seealso \link[base:Hyperbolic]{atanh}
#'
#' @author Ron Triepels
#' @export
cg_atanh <- function(x, name = NULL)
{
  cg_operator(atanh, list(x), name)
}

# Function definition
delayedAssign("atanh", cg_function(
  def = base::atanh,
  grads = list(
    function(x, value, grad)
    {
      grad / (1 - x ^ 2)
    }
  )
))

#' Sigmoid
#'
#' Calculate \code{1 / (1 + exp(-x))}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @author Ron Triepels
#' @export
cg_sigmoid <- function(x, name = NULL)
{
  cg_operator(sigmoid, list(x), name)
}

# Function definition
delayedAssign("sigmoid", cg_function(
  def = function(x, out)
  {
    .Call("cg_sigmoid_def", x, out, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, value, grad, out)
    {
      .Call("cg_sigmoid_grad", value, grad, out, PACKAGE = "cgraph")
    }
  )
))
