#' Calculate weight single component
#'
#' @param j component
#' @param param_vec vector of parameters
#' @param lower_temp lower temperature bound
#' @param upper_temp upper temperature bound
#' @return weight of component
#'

wt_component <- function (j, param_vec, lower_temp, upper_temp) {

  # extract relevant parameter vector
  names <- paste0(c("h", "s", "p", "w"), j)
  idx <- match(names, names(param_vec))
  param_sub <- param_vec[idx]

  f_j <- function (x) {
    fs_function(x, param_sub[1], param_sub[2], param_sub[3], param_sub[4])
  }

  # weight percent of each component, where the integral is the fraction
  # of initial mass of that compoenent.
  wt <- integrate(Vectorize(f_j), lower = lower_temp,
                               upper = upper_temp)$value * 100

  wt
}
