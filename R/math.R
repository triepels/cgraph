# Copyright 2019 Ron Triepels
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

#' Positive
#'
#' Calculate \code{x}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{positive}
#'
#' @author Ron Triepels
#' @export
cg_pos <- function(n1, name = NULL)
{
  cg_operator(pos, list(n1), name)
}

# Function definition
delayedAssign("pos", cg_function(
  def = function(a1)
  {
    .Call("cg_math_pos", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_pos_grad", a1, grad, PACKAGE = "cgraph")
    }
  )
))

#' Negative
#'
#' Calculate \code{-x}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{negative}
#'
#' @author Ron Triepels
#' @export
cg_neg <- function(n1, name = NULL)
{
  cg_operator(neg, list(n1), name)
}

# Function definition
delayedAssign("neg", cg_function(
  def = function(a1)
  {
    .Call("cg_math_neg", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_neg_grad", a1, grad, PACKAGE = "cgraph")
    }
  )
))

#' Add
#'
#' Calculate \code{x + y}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param y either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{add}
#'
#' @author Ron Triepels
#' @export
cg_add <- function(n1, n2, name = NULL)
{
  cg_operator(add, list(n1, n2), name)
}

# Function definition
delayedAssign("add", cg_function(
  def = function(a1, a2)
  {
    .Call("cg_math_add", a1, a2, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, a2, val, grad)
    {
      .Call("cg_math_add_grad", a1, grad, PACKAGE = "cgraph")
    },
    function(a1, a2, val, grad)
    {
      .Call("cg_math_add_grad", a2, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
`+.cg_node` <- function(n1, n2)
{
  if(missing(n2))
  {
    cg_pos(n1)
  }
  else
  {
    cg_add(n1, n2)
  }
}

#' Subtract
#'
#' Calculate \code{x - y}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param y either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{subtract}
#'
#' @author Ron Triepels
#' @export
cg_sub <- function(n1, n2, name = NULL)
{
  cg_operator(sub, list(n1, n2), name)
}

# Function definition
delayedAssign("sub", cg_function(
  def = function(a1, a2)
  {
    .Call("cg_math_sub", a1, a2, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, a2, val, grad)
    {
      .Call("cg_math_add_grad", a1, grad, PACKAGE = "cgraph")
    },
    function(a1, a2, val, grad)
    {
      .Call("cg_math_sub_grad", a2, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
#' @author Ron Triepels
`-.cg_node` <- function(n1, n2)
{
  if(missing(n2))
  {
    cg_neg(n1)
  }
  else
  {
    cg_sub(n1, n2)
  }
}

#' Multiply
#'
#' Calculate \code{x * y}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param y either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{multiply}
#'
#' @author Ron Triepels
#' @export
cg_mul <- function(n1, n2, name = NULL)
{
  cg_operator(mul, list(n1, n2), name)
}

# Function definition
delayedAssign("mul", cg_function(
  def = function(a1, a2)
  {
    .Call("cg_math_mul", a1, a2, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, a2, val, grad)
    {
      .Call("cg_math_mul_grad1", a1, a2, grad, PACKAGE = "cgraph")
    },
    function(a1, a2, val, grad)
    {
      .Call("cg_math_mul_grad2", a1, a2, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
`*.cg_node` <- function(n1, n2)
{
  cg_mul(n1, n2)
}

#' Divide
#'
#' Calculate \code{x / y}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param y either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{divide}
#'
#' @author Ron Triepels
#' @export
cg_div <- function(n1, n2, name = NULL)
{
  cg_operator(div, list(n1, n2), name)
}

# Function definition
delayedAssign("div", cg_function(
  def = function(a1, a2)
  {
    .Call("cg_math_div", a1, a2, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, a2, val, grad)
    {
      .Call("cg_math_div_grad1", a1, a2, grad, PACKAGE = "cgraph")
    },
    function(a1, a2, val, grad)
    {
      .Call("cg_math_div_grad2", a1, a2, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
`/.cg_node` <- function(n1, n2)
{
  cg_div(n1, n2)
}

#' Power
#'
#' Calculate \code{x ^ y}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param y either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{power}
#'
#' @author Ron Triepels
#' @export
cg_pow <- function(n1, n2, name = NULL)
{
  cg_operator(pow, list(n1, n2), name)
}

# Function definition
delayedAssign("pow", cg_function(
  def = function(a1, a2)
  {
    .Call("cg_math_pow", a1, a2, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, a2, val, grad)
    {
      .Call("cg_math_pow_grad1", a1, a2, grad, PACKAGE = "cgraph")
    },
    function(a1, a2, val, grad)
    {
      .Call("cg_math_pow_grad2", a1, a2, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
`^.cg_node` <- function(n1, n2)
{
  cg_pow(n1, n2)
}

#' Square
#'
#' Calculate \code{x ^ 2}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @note This function is equivalent to \code{cg_pow(x, 2)}.
#'
#' @seealso \link[base:Arithmetic]{square}
#'
#' @author Ron Triepels
#' @export
cg_square <- function(n1, name = NULL)
{
  cg_operator(square, list(n1), name)
}

# Function definition
delayedAssign("square", cg_function(
  def = function(a1)
  {
    .Call("cg_math_square", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_square_grad", a1, grad, PACKAGE = "cgraph")
    }
  )
))

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:MathFun]{sqrt}
#'
#' @author Ron Triepels
#' @export
cg_sqrt <- function(n1, name = NULL)
{
  cg_operator(sqrt, list(n1), name)
}

# Function definition
delayedAssign("sqrt", cg_function(
  def = function(a1)
  {
    .Call("cg_math_sqrt", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_sqrt_grad", a1, val, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
cg_cbrt <- function(n1, name = NULL)
{
  cg_operator(cbrt, list(n1), name)
}

# Function definition
delayedAssign("cbrt", cg_function(
  def = function(a1)
  {
    .Call("cg_math_cbrt", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_cbrt_grad", a1, val, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
cg_hypot <- function(n1, n2, name = NULL)
{
  cg_operator(hypot, list(n1, n2), name)
}

# Function definition
delayedAssign("hypot", cg_function(
  def = function(a1, a2)
  {
    .Call("cg_math_hypot", a1, a2, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, a2, val, grad)
    {
      .Call("cg_math_hypot_grad", a1, val, grad, PACKAGE = "cgraph")
    },
    function(a1, a2, val, grad)
    {
      .Call("cg_math_hypot_grad", a2, val, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
cg_fma <- function(n1, n2, n3, name = NULL)
{
  cg_operator(fma, list(n1, n2, n3), name)
}

# Function definition
delayedAssign("fma", cg_function(
  def = function(a1, a2, a3)
  {
    .Call("cg_math_fma", a1, a2, a3, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, a2, a3, val, grad)
    {
      .Call("cg_math_mul_grad1", a1, a2, grad, PACKAGE = "cgraph")
    },
    function(a1, a2, a3, val, grad)
    {
      .Call("cg_math_mul_grad2", a1, a2, grad, PACKAGE = "cgraph")
    },
    function(a1, a2, a3, val, grad)
    {
      .Call("cg_math_add_grad", a3, grad, PACKAGE = "cgraph")
    }
  )
))

#' Exponential Function
#'
#' Calculate \code{exp(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:log]{exp}
#'
#' @author Ron Triepels
#' @export
cg_exp <- function(n1, name = NULL)
{
  cg_operator(exp, list(n1), name)
}

# Function definition
delayedAssign("exp", cg_function(
  def = function(a1)
  {
    .Call("cg_math_exp", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_exp_grad", a1, val, grad, PACKAGE = "cgraph")
    }
  )
))

#' @export
cg_exp2 <- function(n1, name = NULL)
{
  cg_operator(exp2, list(n1), name)
}

# Function definition
delayedAssign("exp2", cg_function(
  def = function(a1)
  {
    .Call("cg_math_exp2", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_exp2_grad", a1, val, grad, PACKAGE = "cgraph")
    }
  )
))

#' Natural Logarithm
#'
#' Calculate \code{log(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:log]{log}
#'
#' @author Ron Triepels
#' @export
cg_ln <- function(n1, name = NULL)
{
  cg_operator(ln, list(n1), name)
}

# Function definition
delayedAssign("ln", cg_function(
  def = function(a1)
  {
    .Call("cg_math_ln", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_ln_grad", a1, grad, PACKAGE = "cgraph")
    }
  )
))

#' Logarithm Base 2
#'
#' Calculate \code{log2(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:log]{log2}
#'
#' @author Ron Triepels
#' @export
cg_log2 <- function(n1, name = NULL)
{
  cg_operator(log2, list(n1), name)
}

# Function definition
delayedAssign("log2", cg_function(
  def = function(a1)
  {
    .Call("cg_math_log2", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_log2_grad", a1, grad, PACKAGE = "cgraph")
    }
  )
))

#' Logarithm Base 10
#'
#' Calculate \code{log10(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
  def = base::log10,
  grads = list(
    function(x, val, grad)
    {
      grad / (x * log(10))
    }
  )
))

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @seealso \link[base:MathFun]{abs}
#'
#' @author Ron Triepels
#' @export
cg_abs <- function(n1, name = NULL)
{
  cg_operator(abs, list(n1), name)
}

# Function definition
delayedAssign("abs", cg_function(
  def = function(a1)
  {
    .Call("cg_math_abs", a1, PACKAGE = "cgraph")
  },
  grads = list(
    function(a1, val, grad)
    {
      .Call("cg_math_abs_grad", a1, val, grad, PACKAGE = "cgraph")
    }
  )
))

#' Sine
#'
#' Calculate \code{sin(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
  def = base::sin,
  grads = list(
    function(x, val, grad)
    {
      grad * cos(x)
    }
  )
))

#' Cosine
#'
#' Calculate \code{cos(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      -grad * sin(x)
    }
  )
))

#' Tangent
#'
#' Calculate \code{tan(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad / cos(x) ^ 2
    }
  )
))

#' Hyperbolic Sine
#'
#' Calculate \code{sinh(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad * cosh(x)
    }
  )
))

#' Hyperbolic Cosine
#'
#' Calculate \code{cosh(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad * sinh(x)
    }
  )
))

#' Hyperbolic Tangent
#'
#' Calculate \code{tanh(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad * (1 - val ^ 2)
    }
  )
))

#' Inverse Sine
#'
#' Calculate \code{asin(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad / sqrt(1 - x ^ 2)
    }
  )
))

#' Inverse Cosine
#'
#' Calculate \code{acos(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      -grad / sqrt(1 - x ^ 2)
    }
  )
))

#' Inverse Tangent
#'
#' Calculate \code{atan(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad / (x ^ 2 + 1)
    }
  )
))

#' Inverse Hyperbolic Sine
#'
#' Calculate \code{asinh(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad / sqrt(x ^ 2 + 1)
    }
  )
))

#' Inverse Hyperbolic Cosine
#'
#' Calculate \code{acosh(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad / sqrt(x ^ 2 - 1)
    }
  )
))

#' Inverse Hyperbolic Tangent
#'
#' Calculate \code{atanh(x)}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
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
    function(x, val, grad)
    {
      grad / (1 - x ^ 2)
    }
  )
))

#' Sigmoid
#'
#' Calculate \code{1 / (1 + exp(-x))}.
#'
#' @param x either a cg_node object or a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object, node of the operation.
#'
#' @author Ron Triepels
#' @export
cg_sigmoid <- function(x, name = NULL)
{
  cg_operator(sigmoid, list(x), name)
}

# Function definition
delayedAssign("sigmoid", cg_function(
  def = function(x)
  {
    .Call("sigmoid", x, PACKAGE = "cgraph")
  },
  grads = list(
    function(x, val, grad)
    {
      .Call("sigmoid_grad", x, val, grad, PACKAGE = "cgraph")
    }
  )
))
