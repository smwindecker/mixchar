#' Processes Thermogravimetric Data
#'
#' This function processes thermogravimetric data by calculating
#' the derivative of mass loss
#'
#' @param data dataframe
#' @param temp_col column name containing temperature values
#' @param massloss_col column name containing mass loss values in grams
#' @param massinit_value numeric value of initial sample mass in grams
#' @param temp_type specify units of temperature, default = Kelvin. specify 'C' if in Celsius
#' @return decon list containing amended dataframe, bounds, model output, mass fractions
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @import plyr zoo
#' @importFrom stats integrate setNames loess
#' @examples
#' data(juncus)
#' munge <- process(juncus, 'temp_C', 'mass_loss', 16.85, 'C')
#'
#' @export

process <- function (data, temp_col, massloss_col, massinit_value,
                        temp_type = NULL) {

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
  data_2$mass_T <- data_2[, massloss_col] + massinit_value

  lower <- min(data_2$temp_K)
  upper <- max(data_2$temp_K)

  data_3 <- data_2[-1, ]
  x <- data_3$temp_K[data_3$temp_K < 500]
  y <- data_3$deriv[data_3$temp_K < 500]

  peak <- !is.na(argmax(x, y, w = 10, span = 0.1)$x)

  #####
  output <- list(data = data_2, mass_init = massinit_value, bounds = c(lower, upper), fourth_peak = peak)

  class(output) <- 'process'
  output

}

