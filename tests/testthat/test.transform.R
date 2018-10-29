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

context("Transform Operations")

test_that("Array [+, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg_sum(a + cg_as_numeric(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Array [-, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg_sum(a - cg_as_numeric(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Array [*, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg_sum(a * cg_as_numeric(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Array [/, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg_sum(cg_as_numeric(b) / a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Array [^, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg_sum(a ^ cg_as_numeric(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Array [ln, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg_sum(a * cg_ln(cg_as_numeric(b)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Array [pmax, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg_sum(cg_pmax(a, cg_as_numeric(b)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Array [pmin, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg_sum(cg_pmin(a, cg_as_numeric(b)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [%*%, reshape, t]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(1:4, name = "a")
  b <- parm(2:5, name = "b")

  # Create constants
  dim <- const(c(1, 4))

  # Create test expression
  c <- cg_matmul(cg_reshape(a, dim), cg_t(cg_reshape(b, dim)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Mixed [sum, reshape]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:10, c(5,2)), name = "a")
  b <- parm(1:3, name = "b")

  # Create constants
  dim <- const(c(4, 2))

  # Create test expression
  c <- cg_sum(cg_reshape(a, dim) * cg_reshape(b, dim))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})
