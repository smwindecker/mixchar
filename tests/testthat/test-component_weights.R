context("Test for extracting weights of components")

test_that("correct weights returned", {

  params3 <- c(height_1 = .1, skew_1 = .3, position_1 = 200, width_1 = 100,
               height_2 = .2, skew_2 = .25, position_2 = 400, width_2 = 60,
               height_3 = .15, skew_3 = .26, position_3 = 600, width_3 = 200)

  param_vec <- c(params3[[1]], params3[[2]], params3[[3]], params3[[4]])
  lower <- 120
  upper <- 700

  f_j <- function (x) {
    fs_function(x, param_vec[1], param_vec[2], param_vec[3], param_vec[4])
  }

  # weight percent of each component, where the integral is the fraction
  # of initial mass of that compoenent.
  val <- integrate(Vectorize(f_j), lower = lower,
                  upper = upper)$value * 100

  expect_equal(wt_component(1, params3, lower, upper), val, tolerance = 1e-3)

  params4 <- c(height_1 = .1, skew_1 = .3, position_1 = 200, width_1 = 100,
               height_2 = .2, skew_2 = .25, position_2 = 400, width_2 = 60,
               height_3 = .15, skew_3 = .26, position_3 = 600, width_3 = 200,
               height_0 = .08, skew_0 = .22, position_0 = 180, width_0 = 150)

  output3 <- list(n_peaks = 3, temp_bounds = c(120, 700))
  output4 <- list(n_peaks = 4, temp_bounds = c(120, 700))

  expect_equal(get_weights(params3, output3), c(HC = 1096.957, CL = 1306.482, LG = 2603.153), tolerance = 1e-3)
  expect_equal(get_weights(params4, output4), c(HC_1 = 1155.978, HC_2 = 1096.957, CL = 1306.482, LG = 2603.153), tolerance = 1e-3)

})
