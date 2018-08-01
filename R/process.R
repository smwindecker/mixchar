#' Calculates the derivative rate of mass loss of thermogravimetric data
#'
#' This function processes thermogravimetric data by calculating
#' the derivative of mass loss
#'
#' @param data dataframe
#' @param temp column name containing temperature values
#' @param mass column name containing mass loss values in grams
#' @param init_mass numeric value of initial sample mass in grams
#' @param temp_type specify units of temperature, default = Celsius. Specify 'K' if in Kelvin
#' @return decon list containing amended dataframe, bounds, model output, mass fractions
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @importFrom stats integrate setNames
#' @examples
#' data(juncus)
#' tmp <- process(juncus, 'temp_C', 'mass_loss', 16.85)
#'
#' @export

process <- function (data, temp, mass, init_mass,
                     temp_type = 'C') {

  temp_measures <- c('C', 'Celsius', 'K', 'Kelvin')

  if (!isTRUE(is.element(temp_type, temp_measures))) {
    stop('Specify temperature either in Celsius or Kelvin')
  }

  if (temp_type == 'K' | temp_type == 'Kelvin') {
    data$temp_C <- data[, temp] - 273
  }

  if (temp_type == 'C' | temp_type == 'Celsius') {
    data$temp_C <- data[, temp]
  }

  if (data[1, 'temp_C']%%1!=0) {
    data$roundC <- round(data$temp_C, 0)
    data_1 <- data[!duplicated(data$roundC),]
  } else {
    data_1 <- data[!duplicated(data$temp_C),]
  }

  # adjust mass loss given initial mass, use this for derivative
  data_1$adj_massloss <- data_1[, mass] / init_mass

  d <- -as.data.frame(diff(data_1$adj_massloss)/diff(data_1$temp_C))
  x <- rep(NA, ncol(d))
  deriv <- rbind(x, d)
  colnames(deriv) <- 'deriv'
  data_2 <- cbind(data_1, deriv)
  data_2 <- data_2[-1,]
  data_2$mass_T <- data_2[, mass] + init_mass

  mod_data <- data_2[,c('temp_C', 'deriv', 'mass_T')]

  lower <- min(mod_data$temp_C)
  upper <- max(mod_data$temp_C)

  output <- list(data = mod_data, mass_init = init_mass, bounds = c(lower, upper))

  class(output) <- 'process'
  output

}

