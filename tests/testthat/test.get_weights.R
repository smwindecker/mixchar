context("Test for get_weights")

test_that("function returns correct weight values", {

  params3 <- c(h1 = .1, s1 = .3, p1 = 200, w1 = 100,
               h2 = .2, s2 = .25, p2 = 400, w2 = 60,
               h3 = .15, s3 = .26, p3 = 600, w3 = 200)
  params4 <- c(h1 = .1, s1 = .3, p1 = 200, w1 = 100,
               h2 = .2, s2 = .25, p2 = 400, w2 = 60,
               h3 = .15, s3 = .26, p3 = 600, w3 = 200,
               h0 = .08, s0 = .22, p0 = 180, w0 = 150)

  output3 <- list(n_peaks = 3, bounds = c(120, 700))
  output4 <- list(n_peaks = 4, bounds = c(120, 700))

  expect_identical(get_weights(params3, output3), c(HC = 1096.957, CL = 1306.482, LG = 2603.153))
  expect_identical(get_weights(params4, output4), c(HC_1 = 1155.978, HC_2 = 1096.957, CL = 1306.482, LG = 2603.153))

})
