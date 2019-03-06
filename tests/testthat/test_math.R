context("Math")

test_that("Math 1",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- (a + b) * (a - b) * (a / b)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Math 2",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- a^b + cg_sqrt(a)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Math 3",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_exp(a) + cg_ln(b) + cg_log2(a) + cg_log10(b)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Math 4",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_abs(-a / b)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Math 5",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sin(a) + cg_cos(b) - cg_tan(a)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Math 6",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sinh(a) + cg_cosh(b) - cg_tanh(a)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Math 7",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_asin(a) + cg_acos(b) - cg_atan(a)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Math 8",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(1.4, name = "b")

  # Create test expression
  c <- cg_asinh(a) + cg_acosh(b) - cg_atanh(a)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Math 9",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(0.2, name = "a")
  b <- cg_parameter(0.4, name = "b")

  # Create test expression
  c <- cg_sigmoid(a) * cg_sigmoid(b)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})
