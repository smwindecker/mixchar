context("Test for extracting weights of components")

test_that("wt_component returns correct weight", {

  params3 <- c(h1 = .1, s1 = .3, p1 = 200, w1 = 100,
               h2 = .2, s2 = .25, p2 = 400, w2 = 60,
               h3 = .15, s3 = .26, p3 = 600, w3 = 200)

  param_vec <- c(.1, .3, 200, 100)
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

})

test_that("get_weights returns correct weight values for each component", {

  params3 <- c(h1 = .1, s1 = .3, p1 = 200, w1 = 100,
               h2 = .2, s2 = .25, p2 = 400, w2 = 60,
               h3 = .15, s3 = .26, p3 = 600, w3 = 200)
  params4 <- c(h1 = .1, s1 = .3, p1 = 200, w1 = 100,
               h2 = .2, s2 = .25, p2 = 400, w2 = 60,
               h3 = .15, s3 = .26, p3 = 600, w3 = 200,
               h0 = .08, s0 = .22, p0 = 180, w0 = 150)

  output3 <- list(n_peaks = 3, bounds = c(120, 700))
  output4 <- list(n_peaks = 4, bounds = c(120, 700))

  expect_equal(get_weights(params3, output3), c(HC = 1096.957, CL = 1306.482, LG = 2603.153), tolerance = 0.0001)
  expect_equal(get_weights(params4, output4), c(HC_1 = 1155.978, HC_2 = 1096.957, CL = 1306.482, LG = 2603.153), tolerance = 0.0001)

})
