#' Calculate weight quantiles
#'
#' @param output dataframe
#' @param seed seed
#' @importFrom stats coef deviance quantile
#' @importFrom tmvtnorm rtmvnorm
#' @return list of means and confidence intervals of weight estimates
#'

weight_quantiles <- function (output, seed) {

  # least squares estimate:
  est <- stats::coef(output$minpack.lm)

  # variance-covariance matrix, assuming truncated multivariate normal distribution over LS surface
  sry <- summary(output$minpack.lm)
  residvar <- stats::deviance(output$minpack.lm) / sry$df[2]
  vcov <- sry$cov.unscaled * residvar

  # random draws of parameters, proportional to likelihood
  if (length(est) == 12) {
    lower <- c(0, -Inf, 0, 0, 0, -Inf, 0, 0, 0, -Inf, 0, 0)
    upper <- c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf)
  }
  if (length(est) == 16) {
    lower <- c(0, -Inf, 0, 0, 0, -Inf, 0, 0, 0, -Inf, 0, 0, 0, -Inf, 0, 0)
    upper <- c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf)
  }

  set.seed(seed)
  draws <- tmvtnorm::rtmvnorm(1000, mean = est, sigma = vcov, lower = lower, upper = upper)
  colnames(draws) <- names(est)
  # draws_mvnorm <- MASS::mvnorm(1000, est, vcov)

  # equivalent random draws of weight estimates
  weights_draws <- t(apply(draws, 1, get_weights, output))

  # means and uncertainty intervals of weight estimates calculated from MC
  # simulation
  CI <- apply(weights_draws, 2, quantile, c(0.025, 0.975))
  CI_df <- as.data.frame(CI)

  # weights calculated from the maximum likelihood estimates
  means <- get_weights(est, output)

  all_weights <- rbind(CI_df, means)
  all_weights$value_type <- row.names(all_weights)
  all_weights$value_type[all_weights$value_type == 3] <- 'mean'

  rownames(all_weights) <- c()

  list(weights = all_weights)
}


