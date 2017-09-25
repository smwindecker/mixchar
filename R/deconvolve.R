#' Deconvolves Thermogravimetric Data
#'
#' This function deconvolves thermogravimetric data using a three-part
#' Fraser-Suzuki mixture model
#'
#' @param process_object process object obtained from process function
#' @param start_height vector of height starting values for parameter optimisation in order of curve
#' @param start_skew vector of skew starting values for parameter optimisation in order of curve
#' @param start_position vector of position starting values for parameter optimisation in order of curve
#' @param start_width vector of width starting values for parameter optimisation in order of curve
#' @return decon list containing amended dataframe, bounds, model output, mass fractions
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @import minpack.lm nloptr plyr
#' @importFrom stats integrate setNames
#' @examples
#' data(juncus)
#' output <- deconvolve(munge, c(0.015, 0.013, 0.01), c(-0.15, -0.15, -0.15),
#' c(540, 600, 700), c(50, 30, 200))
#'
#' @export

deconvolve <- function (process_object, start_height = 'height', start_skew = 'skew',
                        start_position = 'position', start_width = 'width') {

  # identify dataframe
  munge <- process_object$data

  # name variables
  temp <- munge$temp_K
  obs <- munge$deriv

  # starting values for height
  if (start_height == 'height') {
    height <- c(.015, .013, .01)
  }
  else {
    height <- start_height
  }

  # starting values for skew
  if (start_skew == 'skew') {
    skew <- c(-.15, -.15, -.15)
  }
  else {
    skew <- start_skew
  }

  # starting values for position
  if (start_position == 'position') {
    position <- c(540, 600, 700)
  }
  else {
    position <- start_position
  }

  # starting values for width
  if (start_width == 'width') {
    width <- c(50, 30, 200)
  }
  else {
    width <- start_width
  }

  # vector of starting values
  theta <- c(height, skew, position, width)

  # lower bounds for parameters
  lb <- c(0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0)

  # parameter optimisation
  params_opt <- param_select(theta, lb, fs_mixture, temp, obs, restarts = 300)

  # model fit
  fit <- fs_model(data_sub, params_opt)

  W <- munge$mass_T
  n <- length(W)

  # get the proportions of the three pseudo-components
  a_j <- vector(length = 3)

  for (j in 1:3) {

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

  }

  # calculate mass fraction using proportion of total mass loss and
  # mass of remaining char
  mg_p1 <- a_j[1] * (W[1] - W[n]) / massinit_value
  mg_p2 <- a_j[2] * (W[1] - W[n]) / massinit_value
  mg_p3 <- a_j[3] * (W[1] - W[n]) / massinit_value

  mass_frac <- setNames(as.list(c(mg_p1, mg_p2, mg_p3)), c('P-HC', 'P-CL', 'P-LG'))

  # output
  output <- list(data = munge, bounds = c(lower, upper), start_vec = theta,
                 minpack.lm_object = fit, mass_fractions = mass_frac)

  class(output) <- 'decon'
  output

}

