context("Transform Expressions")

test_that("Array [*, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg.sum(as.numeric(a) * as.numeric(b))

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})

test_that("Array [*, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(1:6, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(a * as.numeric(b))

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})


test_that("Scalar [%*%, reshape, t]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(1:4, name = "a")
  b <- parm(2:5, name = "b")

  # Create test expression
  c <- cg.reshape(a, c(1,4)) %mul% cg.t(cg.reshape(b, c(1,4)))

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$a, x$approx(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, x$approx(c, b), tolerance = 1e-4)
})
