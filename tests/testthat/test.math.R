context("Math Expressions")

test_that("Scalar [+, -, /, *]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2)
  b <- parm(4)

  # Create test expression
  c <- (a + b) * (a - b) * (a / b)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$prm1, -1)
  expect_equivalent(grads$prm2, -2.5)
})

test_that("Scalar [+, pow, sqrt]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2)
  b <- parm(4)

  # Create test expression
  c <- a^b + cg.sqrt(a)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$prm1, 32 + 1 / (2 * sqrt(2)))
  expect_equivalent(grads$prm2, 16 * log(2))
})

test_that("Scalar [+, exp, log]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2)
  b <- parm(4)

  # Create test expression
  c <- exp(a) + log(a, base = b)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$prm1, exp(1)^2 + 1 / log(16))
  expect_equivalent(grads$prm2, -log(2) / (4 * log(4)^2))
})

test_that("Scalar [+, log2, log10, ln]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2)
  b <- parm(4)

  # Create test expression
  c <- log2(a) + log10(b) + cg.ln(a)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$prm1, (1 + log(2)) / (2 * log(2)))
  expect_equivalent(grads$prm2, 1 / (4 * log(10)))
})

test_that("Scalar [-, /, abs]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2)
  b <- parm(4)

  # Create test expression
  c <- abs(-a / b)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$prm1, 1 / 4)
  expect_equivalent(grads$prm2, -1 / 8)
})
