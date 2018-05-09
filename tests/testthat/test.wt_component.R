context("Test for wt_component")

test_that("function returns correct weight", {

  param_vec <- c(10, .3, 600, 200)
  lower <- 120
  upper <- 700

  val <- integrate(fs_function(x, 10, .3, 600, 200), lower, upper)

  expect_equal(wt_component(1, param_vec, lower, upper), val)

})
