Context("Test print and plot functions")

test_that("print.process produces correct output", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 16.85)

  output <- "Derivative thermogravimetry data (DTG) calculated for 768 datapoints from 31.51 to 798.52 degrees C."

  expect_identical(tmp, output)

})
