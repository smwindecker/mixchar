#' Deconvolves Thermogravimetric Data
#'
#' This function deconvolves thermogravimetric data using a Fraser-Suzuki mixture model
#'
#' @param process_object process object obtained from process function
#' @param lower_temp lower temperature bound to crop dataset, default to 120
#' @param upper_temp upper temperature bound to crop dataset, default to 700
#' @param seed random seed for nloptr optimiser
#' @param n_peaks number of curves optional specification
#' @param start_vec vector of starting values for nls function. Only specify this vector if
#' you have selected the number of curves in the n_peaks parameter.
#' @param lower_vec vector of lower bound values for nls. Only specify this vector if
#' you have selected the number of curves in the n_peaks parameter.
#' @param upper_vec vector of upper bound values for nls. Only specify this vector if
#' you have selected the number of curves in the n_peaks parameter.
#' @return decon list containing amended dataframe, temperature bounds,
#' minpack.lm model fit, the number of curves fit, and estimated component weights
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' \donttest{
#' data(juncus)
#' tmp <- process(juncus, init_mass = 18.96,
#'                temp = 'temp_C', mass_loss = 'mass_loss')
#' output <- deconvolve(tmp)
#' my_starting_vec <- c(height_1 = 0.003, skew_1 = -0.15, position_1 = 250, width_1 = 50,
#'                      height_2 = 0.006, skew_2 = -0.15, position_2 = 320, width_2 = 30,
#'                      height_3 = 0.001, skew_3 = -0.15, position_3 = 390, width_3 = 200)
#' output <- deconvolve(tmp, n_peaks = 3, start_vec = my_starting_vec)
#' }
#' @export

deconvolve <- function (process_object,
                        lower_temp = 120,
                        upper_temp = 700,
                        seed = 1,
                        n_peaks = NULL,
                        start_vec = NULL,
                        lower_vec = NULL,
                        upper_vec = NULL) {

  set.seed(seed)

  # identify dataframe
  mod_df <- process_object$data

  # crop dataset at bounds
  mod_df <- mod_df[!(mod_df$temp_C < lower_temp |
                       mod_df$temp_C > upper_temp),]

  # figure out peaks
  x <- mod_df$temp_C[mod_df$temp_C < 220]
  y <- mod_df$deriv[mod_df$temp_C < 220]

  fourth_peak <- !three_peaks(inflection(x, y, w = 15, span = 0.1)$x)

  # name variables
  temp <- mod_df$temp_C
  obs <- mod_df$deriv

  if (is.null(n_peaks) & fourth_peak == FALSE) {
    n_peaks <- 3
  }
  if (is.null(n_peaks) & fourth_peak == TRUE) {
    n_peaks <- 4
  }
  if (isTRUE(n_peaks == 3)) {
    n_peaks <- 3
  }
  if (isTRUE(n_peaks == 4)) {
    n_peaks <- 4
  }

  # assign starting values for parameters
  if (is.null(start_vec) & n_peaks == 3) {
    theta <- c(0.003, -0.15, 250, 50,
               0.006, -0.15, 320, 30,
               0.001, -0.15, 390, 200)
  }
  if (is.null(start_vec) & n_peaks == 4) {
    theta <- c(0.002, -0.15, 210, 50,
               0.003, -0.15, 270, 50,
               0.006, -0.15, 310, 30,
               0.001, -0.15, 410, 200)
  }
  if (!is.null(start_vec)) {
    theta <- start_vec
  }

  # assign lower bound for parameter estimates
  if (is.null(lower_vec) & n_peaks == 3) {
    lb <- c(0, -0.33, 0, 50,
            0, -0.33, 290, 0,
            0, -0.29, 330, 160)
  }
  if (is.null(lower_vec) & n_peaks == 4) {
    lb <- c(0, -0.33, 0, 0,
            0, -0.33, 0, 0,
            0, -0.33, 290, 0,
            0, -0.29, 330, 160)
  }
  if (!is.null(lower_vec)) {
    lb <- lower_vec
  }

  # assign upper bound for parameter estimates
  if (is.null(upper_vec) & n_peaks == 3) {
    ub <- c(2, 0.25, 280, 100,
            2, 0.25, 380, 50,
            2, 0.25, 430, 250)
  }
  if (is.null(upper_vec) & n_peaks == 4) {
    ub <- c(2, 0.2, 210, 80,
            2, 0.2, 280, 90,
            2, 0.2, 380, 50,
            2, 0.2, 430, 250)
  }
  if (!is.null(upper_vec)) {
    ub <- upper_vec
  }

  # parameter optimisation
  params_opt <- param_select(theta = theta,
                             lb = lb,
                             ub = ub,
                             model = fs_mixture,
                             temp = temp,
                             obs = obs,
                             seed = seed,
                             restarts = 300)

  # model fit
  fit <- fs_model(mod_df, params_opt, lb, ub)

  # output
  output <- list(data = mod_df,
                 temp_bounds = c(lower_temp, upper_temp),
                 model_fit = fit,
                 n_peaks = n_peaks)

  weights <- weight_quantiles(output, seed)

  decon_output <- c(output, weights)

  class(decon_output) <- 'decon'
  decon_output

}

