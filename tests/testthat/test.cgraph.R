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

test_that("Generate Existing Name",
{
  # Initialize graph
  x <- cgraph$new()

  # Create parameter
  a <- parm(name = "prm2")

  # Create duplicate parameter
  expect_error(parm(), NA)
})
