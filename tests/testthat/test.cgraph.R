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
