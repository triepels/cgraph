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

context("Array Operations")

test_that("Matrix [-, %*%, sum, prod]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_prod(cg_matmul(a, b)) * cg_sum(cg_matmul(a, b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Matrix [+, mean, crossprod, tcrossprod]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_mean(cg_crossprod(a, b) + cg_tcrossprod(a, b) + cg_crossprod(a) + cg_tcrossprod(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Matrix [linear, rowSums, colSums]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_rowSums(cg_linear(a, b, cg_colSums(b)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Array [*, rowMeans, colMeans]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:25, 2:4), name = "b")

  # Create test expression
  c <- cg_rowMeans(a) * cg_colMeans(b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Matrix [*, max, min]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_max(a) * cg_min(b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})

test_that("Matrix [*, pmax, pmin]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_pmax(a, b) * cg_pmin(a, b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx_grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx_grad(c, b), tolerance = 1e-4)
})
