context("Array")

test_that("Array 1",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_prod(cg_matmul(a, b)) * cg_sum(cg_matmul(a, b))

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 2",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_mean(cg_crossprod(a, b) + cg_tcrossprod(a, b) + cg_crossprod(a) + cg_tcrossprod(b))

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 3",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_rowsums(cg_linear(a, b, cg_colsums(b)))

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c, index = 1)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 4",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_max(a) * cg_min(b)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 5",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_pmax(a, b) * cg_pmin(a, b)

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c, index = 1)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})

test_that("Array 6",
{
  # Initialize graph
  graph <- cg_graph()

  # Create parameter
  a <- cg_parameter(matrix(1:4, 2, 2), name = "a")
  b <- cg_parameter(matrix(2:5, 2, 2), name = "b")

  # Create test expression
  c <- cg_sum(a + cg_as_numeric(cg_t(b)))

  # Perform forward pass
  cg_graph_forward(graph, c)

  # Perform backward pass
  cg_graph_backward(graph, c)

  # Check gradients
  expect_equivalent(a$grad, approx_gradient(graph, c, a), tolerance = 1e-4)
  expect_equivalent(b$grad, approx_gradient(graph, c, b), tolerance = 1e-4)
})
