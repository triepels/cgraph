context("Math")

test_that("Math 1",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- (a + b) * (a - b) * (a / b)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Math 2",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- a^b + cg_square(a - b) + cg_sqrt(a)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Math 3",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_exp(a) + cg_ln(b) + cg_log2(a) + cg_log10(b)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Math 4",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_abs(-a / b)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Math 5",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sin(a) + cg_cos(b) - cg_tan(a)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Math 6",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sinh(a) + cg_cosh(b) - cg_tanh(a)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Math 7",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_asin(a) + cg_acos(b) - cg_atan(a)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Math 8",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(1.4, name = "b")

  # Create test expression
  c <- cg_asinh(a) + cg_acosh(b) - cg_atanh(a)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Math 9",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sigmoid(a) * cg_sigmoid(b)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})
