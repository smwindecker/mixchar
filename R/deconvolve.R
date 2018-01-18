#' Deconvolves Thermogravimetric Data
#'
#' This function deconvolves thermogravimetric data using a three-part
#' Fraser-Suzuki mixture model
#'
#' @param process_object process object obtained from process function
#' @param n_curves number of curves
#' @param lower_temp lower temperature bound to crop dataset
#' @param upper_temp upper temperature bound to crop dataset
#' @param start_vec vector of starting values for nls function. Only specify this vector if
#' you have selected the number of curves in the n_curves parameter.
#' @param lower_vec vector of lower bound values for nls. Only specify this vector if
#' you have selected the number of curves in the n_curves parameter.
#' @param upper_vec vector of upper bound values for nls. Only specify this vector if
#' you have selected the number of curves in the n_curves parameter.
#' @return decon list containing amended dataframe, bounds, model output, mass fractions
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @import minpack.lm nloptr plyr zoo
#' @importFrom stats integrate setNames loess
#' @examples
#' data(juncus)
#' munge <- process(juncus, 'temp_C', 'mass_loss', 18.96)
#' output <- deconvolve(munge)
#'
#' data(marsilea)
#' munge <- process(marsilea, 'temp_C', 'mass_loss', 10.92)
#' output <- deconvolve(munge)
#'
#' @export

deconvolve <- function (process_object, n_curves = NULL, lower_temp = 120, upper_temp = 700,
                                start_vec = NULL, lower_vec = NULL, upper_vec = NULL) {

  # identify dataframe
  mod_df <- ModData(process_object)

  # crop dataset at bounds
  mod_df <- mod_df[!(mod_df$temp_C < lower_temp | mod_df$temp_C > upper_temp),]

  # figure out peaks
  x <- mod_df$temp_C[mod_df$temp_C < 220]
  y <- mod_df$deriv[mod_df$temp_C < 220]

  fourth_peak <- !three_peaks(inflection(x, y, w = 15, span = 0.1)$x)

  # name variables
  temp <- mod_df$temp_C
  obs <- mod_df$deriv

  # init mass
  mass_init <- process_object$mass_init
  W <- mod_df$mass_T
  n <- length(W)

  if (is.null(n_curves) & fourth_peak == FALSE) {
    n_peaks <- 3
  } else if (is.null(n_curves) & fourth_peak == TRUE) {
    n_peaks <- 4
  } else if (n_curves == 3) {
    n_peaks <- 3
  } else if (n_curves == 4) {
    n_peaks <- 4
  } else {
    stop('Manually select peak')
  }

  if (is.null(start_vec) & n_peaks == 3) {
    theta <- c(0.003, 0.006, 0.001, -0.15, -0.15, -0.15, 250, 320, 390, 50, 30, 200)
  } else if (is.null(start_vec) & n_peaks == 4) {
    theta <- c(0.002, 0.003, 0.006, 0.001, -0.15, -0.15, -0.15, -0.15,
               210, 270, 310, 410, 50, 50, 30, 200)
  } else if (!is.null(start_vec)) {
    theta <- start_vec
  }

  if (is.null(lower_vec) & n_peaks == 3) {
    lb <- c(0, 0, 0, -0.33, -0.33, -0.29, 0, 290, 330, 50, 0, 160)
  } else if (is.null(lower_vec) & n_peaks == 4) {
    lb <- c(0, 0, 0, 0, -0.33, -0.33, -0.33, -0.29, 0, 0, 290, 330, 0, 0, 0, 160)
  } else if (!is.null(lower_vec)) {
    lb <- lower_vec
  }

  if (is.null(upper_vec) & n_peaks == 3) {
    ub <- c(2, 2, 2, 0.25, 0.25, 0.25, 280, 380, 430, 100, 50, 250)
  } else if (is.null(upper_vec) & n_peaks == 4) {
    ub <- c(2, 2, 2, 2, 0.2, 0.2, 0.2, 0.2, 210, 280, 380, 430, 80, 90, 50, 250)
  } else if (!is.null(upper_vec)) {
    ub <- upper_vec
  }

  if (n_peaks == 3) {

    # parameter optimisation
    params_opt <- param_select(theta, lb, ub, fs_mixture, temp, obs, restarts = 300)

    # model fit
    fit <- fs_model(mod_df, params_opt, lb, ub)

    wt_percent <- list('HC' = NA, 'CL' = NA, 'LG' = NA)

  } else if (n_peaks == 4) {

    # parameter optimisation
    params_opt <- param_select(theta, lb, ub, fs_mixture_4, temp, obs, restarts = 300)

    # model fit
    fit <- fs_model_4(mod_df, params_opt, lb, ub)

    wt_percent <- list('HC_1' = NA, 'HC_2' = NA, 'CL' = NA, 'LG' = NA)

  } else {
    stop('Specify either three or four curves')
  }

  # get the proportions of the pseudo-components
  for (j in 1:n_peaks) {

    f_j <- function (x) {

      h <- single_param(fit, 'h', j)
      s <- single_param(fit, 's', j)
      p <- single_param(fit, 'p', j)
      w <- single_param(fit, 'w', j)

      fs_function(x, h, s, p, w)

    }

    # weight percent of each component, where the integral is the fraction
    # of initial mass of that compoenent.
    wt_percent[j] <- integrate(Vectorize(f_j), lower = lower_temp,
                              upper = upper_temp)$value * 100

  }

  # output
  output <- list(data = mod_df, bounds = c(lower_temp, upper_temp),
                 minpack.lm = fit, wt_percents = wt_percent, n_peaks = n_peaks)

  class(output) <- 'decon'
  output

}
