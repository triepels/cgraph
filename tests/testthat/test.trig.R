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
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})
