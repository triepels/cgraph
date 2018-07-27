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

context("Graph Methods")

test_that("Duplicate Nodes",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameter
  a <- parm(name = "a")

  # Create duplicate parameter
  expect_error(parm(name = "a"))
})

test_that("Invalid Node Scope",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameter
  a <- parm(name = "sum")

  # Evaluate graph
  expect_error(run(a))
})

test_that("Expression with Equivalent Inputs",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")

  # Create test expression
  b <- (a + a) + (a - a) + (a * a) + (a / a)

  # Calculate gradients
  grads <- gradients(b, run(b))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(b, a), tolerance = 1e-4)
})


test_that("Network with Multiple Outputs",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- const(4, name = "b")

  # Create test expression
  c <- a * b
  d <- a / b

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)

  # Calculate gradients
  grads <- gradients(d, run(d))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(d, a), tolerance = 1e-4)
})

test_that("Large Network",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameter
  a <- parm(2, name = "a")

  # Generate expressions
  for(i in 1:10000)
  {
    a <- cg.abs(a)
  }

  # Calculate gradients
  grads <- gradients(a, run(a))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(a, "a", eps = 1e-6), tolerance = 1e-4)
})
