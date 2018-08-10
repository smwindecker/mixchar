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
  est <- stats::coef(output$model_fit)

  # variance-covariance matrix, assuming truncated
  # multivariate normal distribution over LS surface
  sry <- summary(output$model_fit)
  residvar <- stats::deviance(output$model_fit) / sry$df[2]
  vcov <- sry$cov.unscaled * residvar

  # random draws of parameters, proportional to likelihood
  if (length(est) == 12) {
    lower <- c(0, -Inf, 0, 0,
               0, -Inf, 0, 0,
               0, -Inf, 0, 0)
    upper <- c(Inf, Inf, Inf, Inf,
               Inf, Inf, Inf, Inf,
               Inf, Inf, Inf, Inf)
  }
  if (length(est) == 16) {
    lower <- c(0, -Inf, 0, 0,
               0, -Inf, 0, 0,
               0, -Inf, 0, 0,
               0, -Inf, 0, 0)
    upper <- c(Inf, Inf, Inf, Inf,
               Inf, Inf, Inf, Inf,
               Inf, Inf, Inf, Inf,
               Inf, Inf, Inf, Inf)
  }

  set.seed(seed)
  draws <- tmvtnorm::rtmvnorm(1000,
                              mean = est,
                              sigma = vcov,
                              lower = lower,
                              upper = upper)
  colnames(draws) <- names(est)
  # draws_mvnorm <- MASS::mvnorm(1000, est, vcov)

  # equivalent random draws of weight estimates
  weights_draws <- t(apply(draws, 1, get_weights, output))

  # uncertainty intervals and median of weight estimates
  CI <- apply(weights_draws, 2, quantile, c(0.025, 0.5, 0.975))
  CI_df <- as.data.frame(CI)

  # standard deviation
  sd <- apply(weights_draws, 2, sd)
  sd_df <- as.data.frame(sd)

  # weights calculated from the maximum likelihood estimates
  means <- get_weights(est, output)

  all_weights <- rbind(means, CI_df, sd)
  all_weights$value_type <- row.names(all_weights)
  all_weights$value_type[all_weights$value_type == 1] <- 'mean'

  rownames(all_weights) <- c()

  list(weights = all_weights)
}

#' Calculate weight quantiles
#'
#' @param param_vec parameter estimates from minpack model
#' @param output deconvolve output of model
#' @importFrom stats coef deviance
#' @return weights for each component
#'

get_weights <- function (param_vec, output) {

  n_peaks <- output$n_peaks
  lower_temp <- output$temp_bounds[1]
  upper_temp <- output$temp_bounds[2]

  if (n_peaks == 3) {
    curve_vec <- 1:3
  }

  if (n_peaks == 4) {
    curve_vec <- 0:3
  }

  wt_percent <- vapply(curve_vec, wt_component,
                       FUN.VALUE = numeric(1),
                       param_vec = param_vec,
                       lower_temp = lower_temp,
                       upper_temp = upper_temp)

  if (length(wt_percent) == 3) names(wt_percent) <- c('HC', 'CL', 'LG')
  if (length(wt_percent) == 4) names(wt_percent) <- c('HC_1', 'HC_2', 'CL', 'LG')

  wt_percent
}

#' Calculate weight single component
#'
#' @param j component
#' @param param_vec vector of parameters
#' @param lower_temp lower temperature bound
#' @param upper_temp upper temperature bound
#' @importFrom stats integrate
#' @return weight of component
#'

wt_component <- function (j,
                          param_vec,
                          lower_temp,
                          upper_temp) {

  # extract relevant parameter vector
  names <- paste0(c("height_", "skew_",
                    "position_", "width_"), j)

  idx <- match(names, names(param_vec))
  param_sub <- param_vec[idx]

  f_j <- function (x) {
    fs_function(x,
                param_sub[1],
                param_sub[2],
                param_sub[3],
                param_sub[4])
  }

  # weight percent of each component,
  # where the integral is the fraction
  # of initial mass of that component.
  wt <- stats::integrate(Vectorize(f_j),
                         lower = lower_temp,
                         upper = upper_temp)$value * 100

  wt
}

