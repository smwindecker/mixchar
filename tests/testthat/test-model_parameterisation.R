context("Test for model parameterisation")

test_that("rmse returns correct value", {

  set.seed <- 1234
  obs <- rnorm(1000, 8.3, .9)
  pred <- rnorm(1000, 8.5, .5)

  val <- sqrt(sum((obs - pred) ^ 2))

  expect_equal(rmse(obs, pred), val)

})

test_that("model output correct", {

  tmp <- process(juncus, init_mass = 18.96,
                 temp = 'temp_C', mass_loss = 'mass_loss')

  output <- deconvolve(tmp)
  params <- model_parameters(output)
  start_params <- c(height_1 = params[1,2], skew_1 = params[2,2],
                    position_1 = params[3,2], width_1 = params[4,2],
                    height_2 = params[5,2], skew_2 = params[6,2],
                    position_2 = params[7,2], width_2 = params[8,2],
                    height_3 = params[9,2], skew_3 = params[10,2],
                    position_3 = params[11,2], width_3 = params[12,2])

  lb <- c(0, -0.33, 0, 50,
          0, -0.33, 290, 0,
          0, -0.29, 330, 160)
  ub <- c(2, 0.25, 280, 100,
          2, 0.25, 380, 50,
          2, 0.25, 430, 250)

  fit <- fs_model(rate_data(tmp), start_params, lb, ub)

  expect_equal(fs_model(rate_data(tmp), start_params, lb, ub), fit)

})
