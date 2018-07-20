context("Trig Expressions")

test_that("Scalar [+, -, sin, cos, tan, tanh]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")
  b <- parm(4, name = "b")

  # Create test expression
  c <- sin(a) + cos(b) - tan(a) + tanh(b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [+, -, asin, acos, atan, atanh]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(0.2, name = "a")
  b <- parm(0.4, name = "b")

  # Create test expression
  c <- asin(a) + acos(b) - atan(a) + atanh(b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})
