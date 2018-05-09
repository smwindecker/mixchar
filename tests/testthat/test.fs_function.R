context("Test for basic Fraser-Suzuki function")

test_that("function returns correct y value", {

  fxn_value <- fs_function(500, 10, .3, 600, 200)

  expect_equal(fxn_value, 3.75394)

})
