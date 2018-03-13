#' Deconvolves Thermogravimetric Data
#'
#' This function deconvolves thermogravimetric data using a three-part
#' Fraser-Suzuki mixture model
#'
#' @param process_object process object obtained from process function
#' @param n_curves number of curves
#' @param lower lower temperature bound to crop dataset
#' @param upper upper temperature bound to crop dataset
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


deconvolve_old <- function (process_object, lower = 120, upper = 700, n_curves = NULL) {

  # identify dataframe
  mod_df <- ModData(process_object)

  # crop dataset at bounds
  mod_df <- mod_df[!(mod_df$temp_C < lower | mod_df$temp_C > upper),]

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

  if (n_peaks == 3) {

    theta <- c(0.003, 0.006, 0.001, -0.15, -0.15, -0.15, 270, 330, 410, 50, 30, 200)

    lb <- c(0, 0, 0, -0.33, -0.33, -0.29, 0, 0, 330, 0, 0, 160)
    ub <- c(2, 2, 2, 0.25, 0.25, 0.25, 280, 380, 430, 100, 80, 250)

    # parameter optimisation
    params_opt <- param_select(theta, lb, ub, fs_mixture, temp, obs, restarts = 300)

    # model fit
    fit <- fs_model(mod_df, params_opt, lb, ub)

    mass_frac <- list('HC' = NA, 'CL' = NA, 'LG' = NA)

  } else if (n_peaks == 4) {

    theta <- c(0.002, 0.003, 0.006, 0.001, -0.15, -0.15, -0.15, -0.15,
               210, 270, 310, 410, 50, 50, 30, 200)

    lb <- c(0, 0, 0, 0, -0.33, -0.33, -0.33, -0.29, 0, 0, 0, 330, 0, 0, 0, 160)
    ub <- c(2, 2, 2, 2, 0.2, 0.2, 0.2, 0.2, 210, 280, 380, 430, 80, 100, 80, 250)

    # parameter optimisation
    params_opt <- param_select(theta, lb, ub, fs_mixture_4, temp, obs, restarts = 300)

    # model fit
    fit <- fs_model_4(mod_df, params_opt, lb, ub)

    mass_frac <- list('HC_1' = NA, 'HC_2' = NA, 'CL' = NA, 'LG' = NA)

  } else {
    stop('specify either three or four curves')
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

    # area under the curves
    mass_frac[j] <- (integrate(Vectorize(f_j), lower = lower,
                               upper = upper)$value) * 100

  }

  # output
  output <- list(data = mod_df, bounds = c(lower, upper),
                 minpack.lm = fit, mass_fractions = mass_frac, n_peaks = n_peaks)

  class(output) <- 'decon'
  output

}
