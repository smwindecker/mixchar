Context("Test print and plot functions")

test_that("print functions produce correct output", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 16.85)
  output <- deconvolve(tmp)

  pro_output <- "Derivative thermogravimetry data (DTG) calculated for 768 datapoints from 31.51 to 798.52 degrees C."
  out_output <- "Deconvolution by 3-part Fraser-Suzuki mixture model fitted to 580 datapoints from 120 to 700 degrees C."

  expect_identical(tmp, pro_output)
  expect_identical(output, out_output)

})


