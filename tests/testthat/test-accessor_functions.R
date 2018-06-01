context("Test for accessor functions")

test_that("Bounds returns bounds", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
  output <- deconvolve(tmp, lower_temp = 130, upper_temp = 660)
  expect_equal(Bounds(output), output$bounds)

})

test_that("ModData returns data", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
  expect_identical(ModData(tmp), tmp$data)

})

test_that("Model returns model output", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
  expect_identical(Model(tmp), tmp$minpack.lm)

})

test_that("Weights returns weight estimates", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
  output <- deconvolve(tmp)

  expect_identical(Weights(output), output$weights)

})
