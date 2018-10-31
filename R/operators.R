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

#
#
# Math Operators
#
#

#' Positive
#'
#' Calculate \code{x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{positive}
#'
#' @author Ron Triepels
#' @export
cg_pos <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`+`),
    grads = list(
      quote(pos_grad)
    ),
    args = list(x)
  )
}

#' Negative
#'
#' Calculate \code{-x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{negative}
#'
#' @author Ron Triepels
#' @export
cg_neg <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`-`),
    grads = list(
      quote(neg_grad)
    ),
    args = list(x)
  )
}

#' Add
#'
#' Calculate \code{x + y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{add}
#'
#' @author Ron Triepels
#' @export
cg_add <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = as.name("+"),
    grads = list(
      as.name("add_grad_x"),
      as.name("add_grad_y")
    ),
    args = list(x, y)
  )
}

#' @export
`+.cg_node` <- function(x, y)
{
  if(missing(y))
  {
    cgraph::cg_pos(x)
  }
  else
  {
    cgraph::cg_add(x, y)
  }
}

#' Subtract
#'
#' Calculate \code{x - y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{subtract}
#'
#' @author Ron Triepels
#' @export
cg_sub <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`-`),
    grads = list(
      quote(sub_grad_x),
      quote(sub_grad_y)
    ),
    args = list(x, y)
  )
}

#' @export
`-.cg_node` <- function(x, y)
{
  if(missing(y))
  {
    cgraph::cg_neg(x)
  }
  else
  {
    cgraph::cg_sub(x, y)
  }
}

#' Multiply
#'
#' Calculate \code{x * y} element-wise.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{multiply}
#'
#' @author Ron Triepels
#' @export
cg_mul <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`*`),
    grads = list(
      x = quote(mul_grad_x),
      y = quote(mul_grad_y)
    ),
    args = list(x, y)
  )
}

#' @export
`*.cg_node` <- function(x, y)
{
  cgraph::cg_mul(x, y)
}

#' Divide
#'
#' Calculate \code{x / y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{divide}
#'
#' @author Ron Triepels
#' @export
cg_div <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`/`),
    grads = list(
      quote(div_grad_x),
      quote(div_grad_y)
    ),
    args = list(x, y)
  )
}

#' @export
`/.cg_node` <- function(x, y)
{
  cgraph::cg_div(x, y)
}

#' Power
#'
#' Calculate \code{x ^ y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{power}
#'
#' @author Ron Triepels
#' @export
cg_pow <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`^`),
    grads = list(
      quote(pow_grad_x),
      quote(pow_grad_y)
    ),
    args = list(x, y)
  )
}

#' @export
`^.cg_node` <- function(x, y)
{
  cgraph::cg_pow(x, y)
}

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:MathFun]{sqrt}
#'
#' @author Ron Triepels
#' @export
cg_sqrt <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sqrt),
    grads = list(
      quote(sqrt_grad)
    ),
    args = list(x)
  )
}

#' Exponential Function
#'
#' Calculate \code{exp(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:log]{exp}
#'
#' @author Ron Triepels
#' @export
cg_exp <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(exp),
    grads = list(
      quote(exp_grad)
    ),
    args = list(x)
  )
}

#' Natural Logarithm
#'
#' Calculate \code{log(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:log]{log}
#'
#' @author Ron Triepels
#' @export
cg_ln <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(log),
    grads = list(
      quote(ln_grad)
    ),
    args = list(x)
  )
}

#' Logarithm Base 2
#'
#' Calculate \code{log2(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:log]{log}
#'
#' @author Ron Triepels
#' @export
cg_log2 <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(log2),
    grads = list(
      quote(log2_grad)
    ),
    args = list(x)
  )
}

#' Logarithmic Base 10
#'
#' Calculate \code{log10(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:log]{log10}
#'
#' @author Ron Triepels
#' @export
cg_log10 <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(log10),
    grads = list(
      quote(log10_grad)
    ),
    args = list(x)
  )
}

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:MathFun]{abs}
#'
#' @author Ron Triepels
#' @export
cg_abs <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(abs),
    grads = list(
      quote(abs_grad)
    ),
    args = list(x)
  )
}

#
#
# Logical Operators
#
#

#' Not
#'
#' Calculate \code{!x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Logic]{not}
#'
#' @author Ron Triepels
#' @export
cg_not <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`!`),
    grads = list(
      quote(not_grad)
    ),
    args = list(x)
  )
}

#' @export
`!.cg_node` <- function(x)
{
  cgraph::cg_not(x)
}

#' Equal
#'
#' Calculate \code{x == y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{equal}
#'
#' @author Ron Triepels
#' @export
cg_equal <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`==`),
    grads = list(
      quote(equal_grad_x),
      quote(equal_grad_y)
    ),
    args = list(x, y)
  )
}

#' @export
`==.cg_node` <- function(x, y)
{
  cgraph::cg_equal(x, y)
}

#' Not equal
#'
#' Calculate \code{x != y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{not equal}
#'
#' @author Ron Triepels
#' @export
cg_not_equal <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`!=`),
    grads = list(
      quote(not_equal_grad_x),
      quote(not_equal_grad_y)
    ),
    args = list(x, y)
  )
}

#' @export
`!=.cg_node` <- function(x, y)
{
  cgraph::cg_not_equal(x, y)
}

#' Less
#'
#' Calculate \code{x < y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{less}
#'
#' @author Ron Triepels
#' @export
cg_less <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`<`),
    grads = list(
      quote(less_grad_x),
      quote(less_grad_y)
    ),
    args = list(x, y)
  )
}

#' @export
`<.cg_node` <- function(x, y)
{
  cgraph::cg_less(x, y)
}

#' Greater
#'
#' Calculate \code{x > y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{greater}
#'
#' @author Ron Triepels
#' @export
cg_greater <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`>`),
    grads = list(
      quote(greater_grad_x),
      quote(greater_grad_y)
    ),
    args = list(x, y)
  )
}

#' @export
`>.cg_node` <- function(x, y)
{
  cgraph::cg_greater(x, y)
}

#' Less or Equal
#'
#' Calculate \code{x <= y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{less or equal}
#'
#' @author Ron Triepels
#' @export
cg_less_equal <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`<=`),
    grads = list(
      quote(less.equal.grad.x),
      quote(less.equal.grad.y)
    ),
    args = list(x, y)
  )
}

#' @export
`<=.cg_node` <- function(x, y)
{
  cgraph::cg_less_equal(x, y)
}

#' Greater or Equal
#'
#' Calculate \code{x >= y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{greater or equal}
#'
#' @author Ron Triepels
#' @export
cg_greater_equal <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`>=`),
    grads = list(
      quote(greater.equal.grad.x),
      quote(greater.equal.grad.y)
    ),
    args = list(x, y)
  )
}

#' @export
`>=.cg_node` <- function(x, y)
{
  cgraph::cg_greater_equal(x, y)
}

#
#
# Trigonometric Operators
#
#

#' Sine
#'
#' Calculate \code{sin(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Trig]{sin}
#'
#' @author Ron Triepels
#' @export
cg_sin <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sin),
    grads = list(
      x = quote(sin_grad)
    ),
    args = list(x)
  )
}

#' Cosine
#'
#' Calculate \code{cos(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Trig]{cos}
#'
#' @author Ron Triepels
#' @export
cg_cos <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(cos),
    grads = list(
      quote(cos_grad)
    ),
    args = list(x)
  )
}

#' Tangent
#'
#' Calculate \code{tan(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Trig]{tan}
#'
#' @author Ron Triepels
#' @export
cg_tan <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(tan),
    grads = list(
      quote(tan_grad)
    ),
    args = list(x)
  )
}

#' Hyperbolic Sine
#'
#' Calculate \code{sinh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Hyperbolic]{sinh}
#'
#' @author Ron Triepels
#' @export
cg_sinh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sinh),
    grads = list(
      quote(sinh_grad)
    ),
    args = list(x)
  )
}

#' Hyperbolic Cosine
#'
#' Calculate \code{cosh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Hyperbolic]{cosh}
#'
#' @author Ron Triepels
#' @export
cg_cosh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(cosh),
    grads = list(
      quote(cosh_grad)
    ),
    args = list(x)
  )
}

#' Hyperbolic Tangent
#'
#' Calculate \code{tanh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Hyperbolic]{tanh}
#'
#' @author Ron Triepels
#' @export
cg_tanh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(tanh),
    grads = list(
      quote(tanh_grad)
    ),
    args = list(x)
  )
}

#' Inverse Sine
#'
#' Calculate \code{asin(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Trig]{asin}
#'
#' @author Ron Triepels
#' @export
cg_asin <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(asin),
    grads = list(
      quote(asin_grad)
    ),
    args = list(x)
  )
}

#' Inverse Cosine
#'
#' Calculate \code{acos(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Trig]{acos}
#'
#' @author Ron Triepels
#' @export
cg_acos <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(acos),
    grads = list(
      quote(acos_grad)
    ),
    args = list(x)
  )
}

#' Inverse Tangent
#'
#' Calculate \code{atan(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Trig]{atan}
#'
#' @author Ron Triepels
#' @export
cg_atan <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(atan),
    grads = list(
      quote(atan_grad)
    ),
    args = list(x)
  )
}

#' Inverse Hyperbolic Sine
#'
#' Calculate \code{asinh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Hyperbolic]{asinh}
#'
#' @author Ron Triepels
#' @export
cg_asinh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(asinh),
    grads = list(
      quote(asinh_grad)
    ),
    args = list(x)
  )
}

#' Inverse Hyperbolic Cosine
#'
#' Calculate \code{acosh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Hyperbolic]{acosh}
#'
#' @author Ron Triepels
#' @export
cg_acosh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(acosh),
    grads = list(
      quote(acosh_grad)
    ),
    args = list(x)
  )
}

#' Inverse Hyperbolic Tangent
#'
#' Calculate \code{atanh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Hyperbolic]{atanh}
#'
#' @author Ron Triepels
#' @export
cg_atanh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(atanh),
    grads = list(
      quote(atanh_grad)
    ),
    args = list(x)
  )
}

#
#
# Array Operators
#
#

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
#' @export
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

#
#
# Transformation Operators
#
#

#' Coerce to a Numeric Vector
#'
#' Coerce \code{x} to a one-dimensional numeric vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is identical to \code{cg.as.numeric}.
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:double]{as.double}
#'
#' @author Ron Triepels
#' @export
cg_as_double <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(as.double),
    grads = list(
      quote(as_double_grad)
    ),
    args = list(x)
  )
}

#' Coerce to a Numeric Vector
#'
#' Coerce \code{x} to a one-dimensional numeric vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is identical to \code{cg.as.double}.
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:double]{as.numeric}
#'
#' @author Ron Triepels
#' @export
cg_as_numeric <- function(x, name = NULL)
{
  cgraph::cg_as_double(x, name)
}

#' Reshape Array Dimensions
#'
#' Change the dimensions of array \code{x} to \code{dim}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param dim cg.node, placeholder for a numeric scalar or vector, the dimensions of the new array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note The elements of \code{x} are re-arranged column-wise by base function \link[base:array]{array}.
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:array]{array}
#'
#' @author Ron Triepels
#' @export
cg_reshape <- function(x, dim, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(array),
    grads = list(
      quote(reshape_grad_x),
      quote(reshape_grad_dim)
    ),
    args = list(x, dim)
  )
}

#' Matrix Transpose
#'
#' Perform \code{t(x)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:t]{t}
#'
#' @author Ron Triepels
#' @export
cg_t <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(t),
    grads = list(
      x = quote(t_grad)
    ),
    args = list(x)
  )
}

#
#
# Miscellaneous Operators
#
#

#' Sigmoid
#'
#' Calculate \code{sigmoid(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
#' @export
cg_sigmoid <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sigmoid),
    grads = list(
      quote(sigmoid_grad)
    ),
    args = list(x)
  )
}
