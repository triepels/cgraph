context("Graph Methods")

test_that("Duplicate Nodes",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameter
  a <- parm(name = "a")

  # Create duplicate parameter
  expect_error(parm(name = "a"))
})

test_that("Invalid Node Scope",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameter
  a <- parm(name = "sum")

  # Evaluate graph
  expect_error(run(a))
})

test_that("Large Network",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameters
  a <- parm(2, name = "a")

  # Generate expressions
  for(i in 1:10000)
  {
    a <- cg.abs(a)
  }

  # Calculate gradients
  grads <- gradients(a, run(a))

  # Check gradients
  expect_equivalent(grads$a, approx.grad(a, "a", eps = 1e-6), tolerance = 1e-4)
})
