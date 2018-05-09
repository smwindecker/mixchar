context("Test for Weights accessor function")

test_that("function returns bounds", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
  output <- deconvolve(tmp)

  expect_identical(Weights(output), output$weights)

})
