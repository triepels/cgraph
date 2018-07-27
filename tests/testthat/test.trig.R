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

context("Trigonometric Operations")

test_that("Scalar [+, -, sin, cos, tan]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")

  # Create test expression
  c <- sin(a) + cos(a) - tan(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
})

test_that("Scalar [+, -, sinh, cosh, tanh]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")

  # Create test expression
  c <- sinh(a) + cosh(a) - tanh(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
})

test_that("Scalar [+, -, asin, acos, atan]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")

  # Create test expression
  c <- asin(a) + acos(a) - atan(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
})

test_that("Scalar [+, -, asinh, acosh, atanh]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")

  # Create test expression
  c <- asinh(a) + acosh(a) - atanh(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
})

