context("Math Expressions")

test_that("Scalar [+, -, /, *]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- (a + b) * (a - b) * (a / b)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, pow, sqrt]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- a^b + cg.sqrt(a)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, exp, log]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- exp(a) + log(a, base = b)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, log2, log10, ln]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- log2(a) + log10(b) + cg.ln(a)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})

test_that("Scalar [-, /, abs]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- abs(-a / b)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})
