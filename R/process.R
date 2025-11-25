#' Calculates the derivative rate of mass loss of thermogravimetric data
#'
#' This function processes thermogravimetric data by calculating
#' the derivative of mass loss
#'
#' @param data dataframe with thermogravimetric analysis output
#' @param init_mass numeric value of initial sample mass in mg
#' @param temp column name containing temperature values
#' @param mass_loss column name containing mass loss values in mg
#' @param time index column of time
#' @param pyrolysis_start_time value from `time` column that indicates when
#'   the pyrolysis period begins
#' @param pyrolysis_end_time value from `time` column that indicates when
#'   the pyrolysis period ends
#' @param temp_units specify units of temperature, default = Celsius.
#' Can specify 'K' or 'Kelvin' if in Kelvin
#' @return process list containing modified dataframe, initial mass
#' of sample, and maximum and minimum temperature values
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' data(juncus)
#' tmp <- process(juncus, init_mass = 18.96,
#'                temp = 'temp_C', mass_loss = 'mass_loss'
#'                time,
#'                )
#'
#' @export
process <- function (data, init_mass, temp,
                     mass_loss, time,
                     pyrolysis_start_time, pyrolysis_end_time,
                     temp_units = c('C', 'K')) {

  # subset provided data to avoid presence of
  # other columns with conflicting names
  subset <- data[, c(temp, mass_loss, time)]

  # check that initial mass is positive
  if (init_mass < 0) {
    stop('Initial mass must be positive')
  }

  # check temperature inputs
  valid_temps <- c('C', 'K')
  temp_units <- rlang::arg_match(
    arg = temp_units,
    values = valid_temps
  )

  if (temp_units == 'K') {
    subset$temp_C <- subset[, temp] - 273
  }

  if (temp_units == 'C') {
    subset$temp_C <- subset[, temp]
  }

  subset$mass_T <- subset[, mass_loss] + init_mass

  m_pyrolysis_start <- subset$mass_T[subset$time == pyrolysis_start_time]
  moisture_loss_stage <- subset$time < pyrolysis_start_time
  pyrolysis_stage <- subset$time >= pyrolysis_start_time &
    subset$time <= pyrolysis_end_time
  combustion_stage <- subset$time > pyrolysis_end_time

  subset$stage[moisture_loss_stage] <- 'moisture_content'
  subset$stage[pyrolysis_stage] <- 'volatile_matter'
  subset$stage[combustion_stage] <- 'fixed_carbon'

  pyrolysis <- subset[subset$stage == 'volatile_matter', ]
  pyrolysis$adj_massloss <- (pyrolysis$mass_T - m_pyrolysis_start) /
    m_pyrolysis_start

  if (pyrolysis[1, 'temp_C']%%1 != 0) {
    pyrolysis$roundC <- round(pyrolysis$temp_C, 0)
    pyrolysis_1 <- pyrolysis[!duplicated(pyrolysis$roundC),]
  }
  if (pyrolysis[1, 'temp_C']%%1 == 0) {
    pyrolysis_1 <- pyrolysis[!duplicated(pyrolysis$temp_C),]
  }

  # calculate the derivative
  d <- -as.data.frame(diff(pyrolysis_1$adj_massloss)/diff(pyrolysis_1$temp_C))
  x <- rep(NA, ncol(d))
  deriv <- rbind(x, d)
  colnames(deriv) <- 'deriv'
  pyrolysis_2 <- cbind(pyrolysis_1, deriv)
  pyrolysis_2 <- pyrolysis_2[-1,]

  mod_data <- pyrolysis_2[, c('temp_C', 'time', 'deriv', 'mass_T')]
  all_data <- subset[, c('temp_C', 'time', 'mass_T', 'stage')]

  lower <- min(mod_data$temp_C)
  upper <- max(mod_data$temp_C)

  output <- list(pyrolysis_data = mod_data,
                 all_data = all_data,
                 mass_init = init_mass,
                 pyrolysis_temp_range = c(lower, upper))

  class(output) <- 'process'
  output

}
