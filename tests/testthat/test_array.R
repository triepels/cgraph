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

context("Array")

test_that("Array 1",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_prod(cg_matmul(a, b)) * cg_sum(cg_matmul(a, b))

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 2",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_mean(cg_crossprod(a, b) + cg_tcrossprod(a, b) + cg_crossprod(a) + cg_tcrossprod(b))

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 3",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_rowsums(cg_linear(a, b, cg_colsums(b)))

  # Perform backward pass
  cg_graph_backward(graph, c, index = 1)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 4",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_rowmeans(cg_linear(a, b, cg_colmeans(b)))

  # Perform backward pass
  cg_graph_backward(graph, c, index = 1)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 5",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_max(a) * cg_min(b)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 6",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_pmax(a, b) * cg_pmin(a, b)

  # Perform backward pass
  cg_graph_backward(graph, c, index = 1)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 7",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_sum(a + cg_as_numeric(cg_t(b)))

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})
