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
#' @import minpack.lm nloptr plyr
#' @importFrom stats integrate setNames
#' @examples
#' data(juncus)
#' munge <- process(juncus, 'temp_C', 'mass_loss', 16.85, 'C')
#' output <- deconvolve(munge, n_curves = 3, lower = 420, upper = 860)
#'
#' data(cyperus)
#' munge <- process(cyperus, 'temp_C', 'mass_loss', 10.92, 'C')
#' output <- deconvolve(munge, n_curves = 4, lower == 400, upper = 900)
#'
#' @export

deconvolve <- function (process_object, n_curves = 3, lower = 400, upper = 900) {

  # identify dataframe
  mod_df <- ModData(process_object)

  # crop dataset at bounds
  mod_df <- mod_df[!(mod_df$temp_K < lower | mod_df$temp_K > upper),]

  # name variables
  temp <- mod_df$temp_K
  obs <- mod_df$deriv

  # init mass
  mass_init <- process_object$mass_init
  W <- mod_df$mass_T
  n <- length(W)

  if (n_curves == 4 || process_object$fourth_peak == TRUE) {

    theta <- c(0.02, 0.03, 0.07, 0.01, -0.15, -0.15, -0.15, -0.15,
               480, 540, 580, 680, 50, 50, 30, 200)
    lb <- c(0, 0, 0, 0, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0)

    # parameter optimisation
    params_opt <- param_select(theta, lb, fs_mixture_4, temp, obs, restarts = 300)

    # model fit
    fit <- fs_model_4(mod_df, params_opt)

    n_curves <- 4
    mass_frac <- list('P-SC' = NA, 'P-HC' = NA, 'P-CL' = NA, 'P-LG' = NA)

  } else { ## with three curves
    theta <- c(0.015, 0.013, 0.01, -0.15, -0.15, -0.15, 540, 600, 700, 50, 30, 200)
    lb <- c(0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0)

    # parameter optimisation
    params_opt <- param_select(theta, lb, fs_mixture, temp, obs, restarts = 300)

    # model fit
    fit <- fs_model(mod_df, params_opt)

    n_curves <- 3
    mass_frac <- list('P-HC' = NA, 'P-CL' = NA, 'P-LG' = NA)

  }

  # get the proportions of the pseudo-components
  a_j <- vector(length = n_curves)

  for (j in 1:n_curves) {

    f_j <- function (x) {

      h <- single_param(fit, 'h', j)
      s <- single_param(fit, 's', j)
      p <- single_param(fit, 'p', j)
      w <- single_param(fit, 'w', j)

      fs_function(x, h, s, p, w)

    }

    # area under the curves
    a_j[j] <- integrate(Vectorize(f_j), lower = lower,
                        upper = upper)$value / (W[1] - W[n])

    # calculate mass fraction using proportion of total mass loss and
    # mass of remaining char
    mass_frac[j] <- a_j[j] * (W[1] - W[n]) / mass_init

  }

  # output
  output <- list(data = mod_df, bounds = c(lower, upper),
                 minpack.lm = fit, mass_fractions = mass_frac, n_curves = n_curves)

  class(output) <- 'decon'
  output

}

