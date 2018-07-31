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

context("Trigonometric Operations")

test_that("Scalar [+, -, sin, cos, tan]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")
  b <- parm(1.2, name = "b")

  # Create test expression
  c <- sin(a) + cos(b) - tan(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, -, sinh, cosh, tanh]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")
  b <- parm(1.2, name = "b")

  # Create test expression
  c <- sinh(a) + cosh(b) - tanh(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, -, asin, acos, atan]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")
  b <- parm(0.8, name = "b")

  # Create test expression
  c <- asin(a) + acos(b) - atan(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, -, asinh, acosh, atanh]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")
  b <- parm(1.2, name = "b")

  # Create test expression
  c <- asinh(a) + acosh(b) - atanh(a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

