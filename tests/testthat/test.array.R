context("Array Expressions")

test_that("Array [%*%, sum]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2))
  b <- parm(matrix(2:5, 2, 2))

  # Create test expression
  c <- cg.sum(a %mul% b)

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$prm1, matrix(c(6,6,8,8), 2, 2))
  expect_equivalent(grads$prm2, matrix(c(3,7,3,7), 2, 2))
})

test_that("Array [+, mean, crossprod, tcrossprod]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2))
  b <- parm(matrix(2:5, 2, 2))

  # Create test expression
  c <- mean(cg.crossprod(a, b) + cg.tcrossprod(a, b))

  # Calculate gradients
  grads <- x$gradients(c, x$run(c))

  # Check gradients
  expect_equivalent(grads$prm1, matrix(c(6,6,8,8), 2, 2))
  expect_equivalent(grads$prm2, matrix(c(3,7,3,7), 2, 2))
})
