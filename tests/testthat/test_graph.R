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

context("Graph")

test_that("Graph 1",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(1, name = "a")

  # Create duplicate parameter
  expect_error(cg_parameter(1, name = "a"))
})

test_that("Graph 2",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(2, name = "a")

  # Create test expression
  b <- (a + a) + (a - a) + (a * a) + (a / a)

  # Perform forward pass
  cg_graph_forward(graph, b)

  # Perform backward pass
  cg_graph_backward(graph, b)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, b, a), tolerance = 1e-4)
})

test_that("Graph 3",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(2, name = "a")
  b <- cg_parameter(4, name = "b")

  # Create test expressions
  c <- a * b
  d <- a / b

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)

  # Perform forward pass
  cg_graph_forward(graph, d)

  # Perform backward pass
  cg_graph_backward(graph, d)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, d, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, d, b), tolerance = 1e-4)
})

test_that("Graph 4",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(2, "a")
  b <- cg_parameter(2, "b")

  # Create test expressions
  c <- cg_sin(b)
  d <- cg_add(cg_sin(a) + c, cg_sin(c))

  # Perform forward pass
  cg_graph_forward(graph, d)

  # Perform backward pass
  cg_graph_backward(graph, d)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, d, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, d, b), tolerance = 1e-4)
})

test_that("Graph 5",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameters
  a <- cg_parameter(2, name = "a")

  # Generate test expression
  b <- cg_abs(a)

  # Generate some more test expressions
  for(i in 1:9999)
  {
    b <- cg_abs(b)
  }

  # Perform forward pass
  cg_graph_forward(graph, b)

  # Perform backward pass
  cg_graph_backward(graph, b)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, b, a), tolerance = 1e-4)
})
