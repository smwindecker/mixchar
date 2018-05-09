context("Test for Fraser-Suzuki mixture function")

test_that("function returns correct value", {

  fxn_value <- fs_mixture(500, .1, .3, 200, 100, .2, .25, 400, 60, .15, .26, 600, 200)
  expect_equal(fxn_value, 0.06263259)

  fxn_four <- fs_mixture(500, .1, .3, 200, 100, .2, .25, 400, 60, .15, .26, 600, 200, .08, .22, 180, 150)
  expect_equal(fxn_four, fxn_value + 0.0001504429)

  expect_error(fs_mixture(500, .1, .3, 200, 100, .2, .25, 400, 60, .15, .26, 600, 200, .08, .22, 180),
               'Specify correct number of parameters parameters')

})
