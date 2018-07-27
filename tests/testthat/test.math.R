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

context("Math Operations")

test_that("Scalar [+, -, /, *]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- (a + b) * (a - b) * (a / b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, pow, sqrt]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- a^b + cg.sqrt(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, ln, log2, log10, exp]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- cg.ln(a) + cg.log2(b) + log10(a) + cg.exp(b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [-, /, abs]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- abs(-a / b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})
