context("Test for accessor functions")

test_that("accessor functions return correct values", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
  output <- deconvolve(tmp, lower_temp = 130, upper_temp = 660)

  expect_equal(temp_bounds(output), output$bounds)
  expect_identical(rate_data(tmp), tmp$data)
  expect_identical(model_fit(tmp), tmp$minpack.lm)
  expect_identical(component_weights(output), output$weights)


  params <- as.data.frame(summary(output$model_fit)$coefficients[,1])
  expect_identical(model_parameters(output), params)

})
