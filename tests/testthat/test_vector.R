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

context("Vector")

test_that("Vector 1",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- (a + b) * (a - b) * (a / b)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Vector 2",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- a^b + cg_square(a - b) + cg_sqrt(a)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Vector 3",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_exp(a) + cg_ln(b) + cg_log2(a) + cg_log10(b)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Vector 4",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_abs(-a / b)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Vector 5",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sin(a) + cg_cos(b) - cg_tan(a)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Vector 6",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sinh(a) + cg_cosh(b) - cg_tanh(a)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Vector 7",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_asin(a) + cg_acos(b) - cg_atan(a)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Vector 8",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(1.4, name = "b")

  # Create test expression
  c <- cg_asinh(a) + cg_acosh(b) - cg_atanh(a)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Vector 9",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sigmoid(a) * cg_sigmoid(b)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})
