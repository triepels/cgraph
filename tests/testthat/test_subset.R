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

context("Subset")

test_that("Subset 1",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(1:24, name = "a")
  b <- cg_parameter(1:24, name = "b")

  # Create test expression
  c <- a[2] + b[4]

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Subset 2",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(array(1:24, c(2,3,4)), name = "a")
  b <- cg_parameter(array(1:24, c(4,3,2)), name = "b")

  # Create test expression
  c <- cg_sum(cg_matmul(a[1,,], b[,,1]))

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Subset 3",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(array(1:24, c(2,3,4)), name = "a")
  b <- cg_parameter(array(1:24, c(4,3,2)), name = "b")

  # Create test expression
  c <- a[2,1,3, drop = TRUE] + b[3,1,1, drop = TRUE]

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Subset 4",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_constant(matrix(c(1,3,4,9), 2, 2), name = "a")
  b <- cg_parameter(c(4,5), name = "b")

  # Create test expressions
  a[2,] <- b^2
  c <- cg_sum(a)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Subset 5",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_constant(matrix(c(1,3,4,9), 2, 2), name = "a")
  b <- cg_parameter(c(4,5), name = "b")

  # Create test expressions
  a[[2:3]] <- b^2
  c <- cg_sum(a)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Subset 6",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(1:24, name = "a")
  b <- cg_parameter(1:24, name = "b")

  # Create test expression
  c <- a[[2]] + b[[4]]

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Subset 7",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(array(1:24, c(2,3,4)), name = "a")
  b <- cg_parameter(array(1:24, c(4,3,2)), name = "b")

  # Create test expression
  c <- a[[2,1,3]] + b[[3,1,1]]

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})
