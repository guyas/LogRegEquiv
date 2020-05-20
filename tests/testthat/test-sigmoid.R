test_that("sigmoid function works", {
  theta <- rnorm(100, 0, 10)
  sig <- 1 / (1 + exp(-theta))
  expect_equal(sigmoid(theta), sig)
})

test_that("sigmoid rejects mismatching", {
  expect_error(sigmoid("aaa"), "non-numeric input")
})
