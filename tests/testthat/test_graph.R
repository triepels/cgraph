context("Graph")

test_that("Duplicate nodes",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(1, name = "a")

  # Create duplicate parameter
  expect_error(cg_parameter(1, name = "a"))
})

test_that("Operators with equivalent inputs",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(2, name = "a")

  # Create test expression
  b <- (a + a) + (a - a) + (a * a) + (a / a)

  # Evaluate graph
  values <- cg_run(x, b)

  # Calculate gradients
  grads <- cg_gradients(x, b, values)

  # Approximate gradients
  approx <- approx_gradients(x, b, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
})

test_that("Graph with multiple outputs",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameters
  a <- cg_parameter(2, name = "a")
  b <- cg_parameter(4, name = "b")

  # Create test expressions
  c <- a * b
  d <- a / b

  # Evaluate node c in the graph
  values <- cg_run(x, c)

  # Calculate gradients
  grads <- cg_gradients(x, c, values)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)

  # Evaluate node d in the graph
  values <- cg_run(x, d)

  # Calculate gradients
  grads <- cg_gradients(x, d, values)

  # Approximate gradients
  approx <- approx_gradients(x, d, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Large graph (10000 operators)",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameters
  a <- cg_parameter(2, name = "a")

  # Generate test expressions
  for(i in 1:10000)
  {
    a <- cg_abs(a)
  }

  # Evaluate the graph
  values <- cg_run(x, a)

  # Calculate gradients
  grads <- cg_gradients(x, a, values)

  # Approximate gradients
  approx <- approx_gradients(x, a, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
})
