context("Test for model parameterisation")

test_that("rmse returns correct value", {

  set.seed <- 1234
  obs <- rnorm(1000, 8.3, .9)
  pred <- rnorm(1000, 8.5, .5)

  val <- sqrt(sum((obs - pred) ^ 2))

  expect_equal(rmse(obs, pred), val)

})
