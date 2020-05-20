test_that("brier score function works", {
  pi <- runif(100)
  y <- rbinom(100, 1, pi)
  bs <- mean((y - pi)^2)
  expect_equal(brier_score(y, pi), bs)
})

test_that("brier score rejects mismatching", {
  expect_error(brier_score(1:10, 1:9), "mismatching vectors")
})

test_that("brier score rejects illegal y", {
  expect_error(brier_score(1:10, 1:10), "illegal y vector")
})

test_that("brier score rejects illegal pi", {
  expect_error(brier_score(rep(1, 10), 1:10), "illegal pi vector")
})
