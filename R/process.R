#' Calculates the derivative rate of mass loss of thermogravimetric data
#'
#' This function processes thermogravimetric data by calculating
#' the derivative of mass loss
#'
#' @param data dataframe
#' @param init_mass numeric value of initial sample mass in mg
#' @param temp column name containing temperature values
#' @param mass_loss column name containing mass loss values in mg
#' @param mass column name containing mass values in mg
#' @param temp_units specify units of temperature, default = Celsius.
#' Can specify 'K' or 'Kelvin' if in Kelvin
#' @return process list containing modified dataframe, initial mass
#' of sample, and maximum and minimum temperature values
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' data(juncus)
#' tmp <- process(juncus, init_mass = 18.96,
#'                temp = 'temp_C', mass_loss = 'mass_loss')
#'
#' @export

process <- function (data, init_mass, temp,
                     mass_loss = NULL,
                     mass = NULL,
                     temp_units = 'C') {

  # subset provided data to avoid presence of
  # other columns with conflicting names
  subset <- data[, c(temp, mass_loss, mass)]

  # check that mass data is provided
  if (is.null(mass_loss) & is.null(mass)) {
    stop('Specify either mass or mass loss
         data column name')
  }

  # check temperature inputs
  temp_measures <- c('C', 'Celsius', 'K', 'Kelvin')

  if (!isTRUE(is.element(temp_units, temp_measures))) {
    stop('Specify temperature either in Celsius or Kelvin')
  }

  if (temp_units == 'K' | temp_units == 'Kelvin') {
    subset$temp_C <- subset[, temp] - 273
  }

  if (temp_units == 'C' | temp_units == 'Celsius') {
    subset$temp_C <- subset[, temp]
  }

  if (subset[1, 'temp_C']%%1 != 0) {
    subset$roundC <- round(subset$temp_C, 0)
    subset_1 <- subset[!duplicated(subset$roundC),]
  }
  if (subset[1, 'temp_C']%%1 == 0) {
    subset_1 <- subset[!duplicated(subset$temp_C),]
  }

  # calculate mass_T
  if (!is.null(mass)) {
    subset_1$mass_T <- subset_1[, mass]
  }

  if (is.null(mass)) {
    subset_1$mass_T <- subset_1[, mass_loss] + init_mass
  }

  # calculate adjusted mass loss given initial mass
  if (!is.null(mass_loss)) {
    subset_1$adj_massloss <- subset_1[, mass_loss] / init_mass
  }

  if (is.null(mass_loss)) {
    subset_1$mass_loss <- subset_1[, mass] - init_mass
    subset_1$adj_massloss <- subset_1$mass_loss / init_mass
  }

  # calculate the derivative
  d <- -as.data.frame(diff(subset_1$adj_massloss)/diff(subset_1$temp_C))
  x <- rep(NA, ncol(d))
  deriv <- rbind(x, d)
  colnames(deriv) <- 'deriv'
  subset_2 <- cbind(subset_1, deriv)
  subset_2 <- subset_2[-1,]

  mod_data <- subset_2[,c('temp_C', 'deriv', 'mass_T')]

  lower <- min(mod_data$temp_C)
  upper <- max(mod_data$temp_C)

  output <- list(data = mod_data,
                 mass_init = init_mass,
                 temp_range = c(lower, upper))

  class(output) <- 'process'
  output

}
