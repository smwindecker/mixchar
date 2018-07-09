#' Deconvolves Thermogravimetric Data
#'
#' This function deconvolves thermogravimetric data using a three-part
#' Fraser-Suzuki mixture model
#'
#' @param process_object process object obtained from process function
#' @param n_curves number of curves optional specification
#' @param lower_temp lower temperature bound to crop dataset, default to 120
#' @param upper_temp upper temperature bound to crop dataset, default to 700
#' @param ranseed random seed for nloptr optimiser
#' @param start_vec vector of starting values for nls function. Only specify this vector if
#' you have selected the number of curves in the n_curves parameter.
#' @param lower_vec vector of lower bound values for nls. Only specify this vector if
#' you have selected the number of curves in the n_curves parameter.
#' @param upper_vec vector of upper bound values for nls. Only specify this vector if
#' you have selected the number of curves in the n_curves parameter.
#' @return decon list containing amended dataframe, bounds, model output, mass fractions
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @import plyr
#' @importFrom stats integrate setNames loess
#' @examples
#' data(juncus)
#' tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96)
#' output <- deconvolve(tmp)
#'
#' @export

deconvolve <- function (process_object, lower_temp = 120, upper_temp = 700, ranseed = 1,
                        n_curves = NULL, start_vec = NULL, lower_vec = NULL, upper_vec = NULL) {

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

  if (is.null(n_curves) & fourth_peak == FALSE) {
    n_peaks <- 3
  } else if (is.null(n_curves) & fourth_peak == TRUE) {
    n_peaks <- 4
  } else if (n_curves == 3) {
    n_peaks <- 3
  } else if (n_curves == 4) {
    n_peaks <- 4
  } else {
    stop('Manually select peaks')
  }

  if (is.null(start_vec) & n_peaks == 3) {
    theta <- c(0.003, -0.15, 250, 50,
               0.006, -0.15, 320, 30,
               0.001, -0.15, 390, 200)
  } else if (is.null(start_vec) & n_peaks == 4) {
    theta <- c(0.002, -0.15, 210, 50,
               0.003, -0.15, 270, 50,
               0.006, -0.15, 310, 30,
               0.001, -0.15, 410, 200)
  } else if (!is.null(start_vec)) {
    theta <- start_vec
  }

  if (is.null(lower_vec) & n_peaks == 3) {
    lb <- c(0, -0.33, 0, 50,
            0, -0.33, 290, 0,
            0, -0.29, 330, 160)
  } else if (is.null(lower_vec) & n_peaks == 4) {
    lb <- c(0, -0.33, 0, 0,
            0, -0.33, 0, 0,
            0, -0.33, 290, 0,
            0, -0.29, 330, 160)
  } else if (!is.null(lower_vec)) {
    lb <- lower_vec
  }

  if (is.null(upper_vec) & n_peaks == 3) {
    ub <- c(2, 0.25, 280, 100,
            2, 0.25, 380, 50,
            2, 0.25, 430, 250)
  } else if (is.null(upper_vec) & n_peaks == 4) {
    ub <- c(2, 0.2, 210, 80,
            2, 0.2, 280, 90,
            2, 0.2, 380, 50,
            2, 0.2, 430, 250)
  } else if (!is.null(upper_vec)) {
    ub <- upper_vec
  }

  # parameter optimisation
  params_opt <- param_select(theta, lb, ub, fs_mixture, temp, obs, ranseed = ranseed, restarts = 300)

  # model fit
  fit <- fs_model(mod_df, params_opt, lb, ub)

  # output
  output <- list(data = mod_df, bounds = c(lower_temp, upper_temp),
                 minpack.lm = fit, n_peaks = n_peaks)

  weights <- weight_quantiles(output)

  decon_output <- c(output, weights = weights)

  class(decon_output) <- 'decon'
  decon_output

}
