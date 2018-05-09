context("Test for Bounds accessor function")

test_that("function returns bounds", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
  output <- deconvolve(tmp, lower_temp = 130, upper_temp = 660)
  expect_equal(Bounds(output), output$bounds)

})
