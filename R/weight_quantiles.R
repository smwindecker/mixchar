# Monte Carlo

weight_quantiles <- function (output) {

  # least squares estimate:
  est <- coef(output$minpack.lm)

  # varaince-covariance matrix, assuming multivariate normal distribution over LS surface
  sry <- summary(output$minpack.lm)
  residvar <- deviance(output$minpack.lm) / sry$df[2]
  vcov <- sry$cov.unscaled * residvar

  # random draws of parameters, proportional to likelihood
  draws <- MASS::mvrnorm(1000, est, vcov)

  # equivalent random draws of weight estimates
  weights_draws <- t(apply(draws, 1, get_weights, output))

  # means and uncertainty intervals of weight estimates calculated from MC
  # simulation
  CIs <- apply(weights_draws, 2, quantile, c(0.025, 0.975))

  # weights calculated from the maximum likelihood estimates
  means <- get_weights(est, output)

  list(CI_weights = CIs, mean_weights = means)

}


