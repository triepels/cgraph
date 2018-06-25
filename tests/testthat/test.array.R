context("Array Expressions")

test_that("Matrix [-, %*%, sum, prod]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg.prod(cg.matmul(a, b)) - cg.sum(cg.matmul(a, b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Matrix [+, mean, crossprod, tcrossprod]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg.mean(cg.crossprod(a, b) + cg.tcrossprod(a, b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Matrix [linear, rowSums, colSums]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg.rowSums(cg.linear(a, b, cg.colSums(b)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Array [*, rowMeans, colMeans]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:25, 2:4), name = "b")

  # Create test expression
  c <- cg.rowMeans(a) * cg.colMeans(b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Matrix [*, max, min]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg.max(a) * cg.min(b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Matrix [*, pmax, pmin]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(matrix(1:4, 2, 2), name = "a")
  b <- parm(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg.pmax(a, b) * cg.pmin(a, b)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})
