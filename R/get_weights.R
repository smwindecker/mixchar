#' Calculate weight quantiles
#'
#' @param param_vec parameter estimates from minpack model
#' @param output deconvolve output of model
#' @importFrom stats coef deviance
#' @return weights for each component
#'

get_weights <- function (param_vec, output) {

  n_peaks <- output$n_peaks
  lower_temp <- output$bounds[1]
  upper_temp <- output$bounds[2]

  if (n_peaks == 3) {
    wt_percent <- c('HC' = NA, 'CL' = NA, 'LG' = NA)
    curve_vec <- 1:3
  }

  if (n_peaks == 4) {
    wt_percent <- c('HC_1' = NA, 'HC_2' = NA, 'CL' = NA, 'LG' = NA)
    curve_vec <- 0:3
  }

  wt_percent <- sapply(curve_vec, wt_component,
                       param_vec = param_vec,
                       lower_temp = lower_temp,
                       upper_temp = upper_temp)

  if (length(wt_percent) == 3) names(wt_percent) <- c('HC', 'CL', 'LG')
  if (length(wt_percent) == 4) names(wt_percent) <- c('HC_1', 'HC_2', 'CL', 'LG')

  wt_percent
}


