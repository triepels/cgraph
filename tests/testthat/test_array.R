context("Array")

test_that("Array 1",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_prod(cg_matmul(a, b)) * cg_sum(cg_matmul(a, b))

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

test_that("Array 2",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_mean(cg_crossprod(a, b) + cg_tcrossprod(a, b) + cg_crossprod(a) + cg_tcrossprod(b))

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

test_that("Array 3",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_rowsums(cg_linear(a, b, cg_colsums(b)))

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values, index = 1)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Array 4",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_max(a) * cg_min(b)

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

test_that("Array 5",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_pmax(a, b) * cg_pmin(a, b)

  # Evaluate graph
  values <- cg_graph_run(x, c)

  # Calculate gradients
  grads <- cg_graph_gradients(x, c, values, index = 1)

  # Approximate gradients
  approx <- approx_gradients(x, c, values)

  # Check gradients
  expect_equivalent(grads$a, approx$a, tolerance = 1e-4)
  expect_equivalent(grads$b, approx$b, tolerance = 1e-4)
})

test_that("Array 6",
{
  # Initialize graph
  x <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_sum(a + cg_as_numeric(cg_t(b)))

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
