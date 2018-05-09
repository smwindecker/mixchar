context("Test for ModData accessor function")

test_that("function returns data", {

  data(juncus)
  tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
  expect_identical(ModData(tmp), tmp$data)

})
