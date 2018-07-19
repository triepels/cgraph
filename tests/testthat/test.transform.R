context("Transform Expressions")

test_that("Array [+, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(a + as.numeric(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Array [-, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(a - as.numeric(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Array [*, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(a * as.numeric(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Array [/, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(as.numeric(b) / a)

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Array [^, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(a ^ as.numeric(b))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Array [ln, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(a * cg.ln(as.numeric(b)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Array [pmax, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(cg.pmax(a, as.numeric(b)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Array [pmin, as.numeric]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:24, 2:4), name = "a")
  b <- parm(array(2:7, 2:3), name = "b")

  # Create test expression
  c <- cg.sum(cg.pmin(a, as.numeric(b)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Scalar [%*%, reshape, t]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(1:4, name = "a")
  b <- parm(2:5, name = "b")

  # Create test expression
  c <- cg.matmul(cg.reshape(a, c(1,4)), cg.t(cg.reshape(b, c(1,4))))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})

test_that("Mixed [sum, reshape]",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(array(1:10, c(5,2)), name = "a")
  b <- parm(1:3, name = "b")

  # Create test expression
  c <- cg.sum(cg.reshape(a, c(4, 2)) * cg.reshape(b, c(4, 2)))

  # Calculate gradients
  grads <- gradients(c, run(c))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(c, a), tolerance = 1e-4)
  expect_equivalent(grads$b, approx.grad(c, b), tolerance = 1e-4)
})
