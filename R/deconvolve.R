#' Deconvolves Thermogravimetric Data
#'
#' This function deconvolves thermogravimetric data using a three-part
#' Fraser-Suzuki mixture model
#'
#' @param data dataframe
#' @param temp_col column name containing temperature values
#' @param massloss_col column name containing mass loss values in grams
#' @param massinit_value numeric value of initial sample mass in grams
#' @param temp_type specify units of temperature, default = Kelvin. specify 'C' if in Celsius
#' @param start_vec vector of starting values for parameter optimisation in following order:
#' h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3.
#' @param lower lower bound to crop temperature data in Kelvin. Defaults to 400.
#' @param upper upper bound to crop temperature data in Kelvin. Defaults to 900.
#' @return decon list containing amended dataframe, bounds, model output, mass fractions
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @import minpack.lm nloptr plyr
#' @importFrom stats integrate setNames
#' @examples
#' data(juncus)
#' output <- deconvolve(juncus, 'temp_C', 'mass_loss', 16.85, 'C')
#'
#' @export

deconvolve <- function (data, temp_col, massloss_col, massinit_value,
                        temp_type = NULL, start_vec = 'theta', lower = 400, upper = 900) {

  if (temp_type == 'C') {
    data$temp_K <- data[, temp_col] + 273
  } else {
    data$temp_K <- data[, temp_col]
  }

  if (data[1, 'temp_K']%%1!=0) {
    data$roundK <- round_any(data$temp_K, 1)
    data_1 <- data[!duplicated(data$roundK),]
  } else {
    data_1 <- data[!duplicated(data$temp_K),]
  }

  d <- -as.data.frame(diff(data_1[, massloss_col])/diff(data_1$temp_K))
  x <- rep(NA, ncol(d))
  deriv <- rbind(x, d)
  colnames(deriv) <- 'deriv'
  data_2 <- cbind(data_1, deriv)
  data_sub <- data_2[!(data_2$temp_K < lower | data_2$temp_K > upper),]
  data_sub$mass_T <- data_sub[, massloss_col] + massinit_value

  temp <- data_sub$temp_K
  obs <- data_sub$deriv

  if (start_vec == 'theta') {
    theta <- c(.015, .013, .01, -.15, -.15, -.15, 540, 600, 700, 50, 30, 200)
  }
  else {
    theta <- start_vec
  }

  lb <- c(0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0)
  params_opt <- param_select(theta, lb, fs_mixture, temp, obs, restarts = 300)
  fit <- fs_model(data_sub, params_opt)

  ######
  W <- data_sub$mass_T
  n <- length(W)

  # get the proportions
  a_j <- vector(length = 3)

  for (j in 1:3) {

    f_j <- function (x) {

      h <- single_param(fit, 'h', j)
      s <- single_param(fit, 's', j)
      p <- single_param(fit, 'p', j)
      w <- single_param(fit, 'w', j)

      fs_function(x, h, s, p, w)

    }

    a_j[j] <- integrate(Vectorize(f_j), lower = lower,
                        upper = upper)$value / (W[1] - W[n])

  }

  mg_p1 <- a_j[1] * (W[1] - W[n]) / massinit_value
  mg_p2 <- a_j[2] * (W[1] - W[n]) / massinit_value
  mg_p3 <- a_j[3] * (W[1] - W[n]) / massinit_value

  mass_frac <- setNames(as.list(c(mg_p1, mg_p2, mg_p3)), c('P-HC', 'P-CL', 'P-LG'))

  #####
  output <- list(data = data_sub, bounds = c(lower, upper), minpack.lm_object = fit, mass_fractions = mass_frac)

  class(output) <- 'decon'
  output

}

