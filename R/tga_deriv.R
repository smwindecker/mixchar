#' Prepare raw thermogram data
#'
#' This function prepares raw thermogravimetry data for deconvolution
#' by rounding temperature values, taking the derivative, and
#' clipping at upper and lower bounds.
#'
#' @param data dataframe
#' @param temp dataframe column name containing temperature in Kelvin
#' @param mass_loss dataframe column name containing mass loss data
#' @param lower lower bound for temperature for cropping dataset
#' @param upper upper bound for temperature for cropping dataset
#' @return new dataframe
#' @keywords derivative
#' @import plyr

#' @export

tga_deriv <- function (data, temp, mass_loss, lower, upper) {

  require(plyr)

  if (data[1, temp]%%1!=0) {
    data$roundK <- round_any(data[,temp], 1)
    data_1 <- data[!duplicated(data[,'roundK']),]
  } else {
    data_1 <- data[!duplicated(data[, temp]),]
  }

  d <- -as.data.frame(diff(data_1[, mass])/diff(data_1$roundK))
  x <- rep(NA, ncol(d))
  deriv <- rbind(x, d)
  colnames(deriv) <- 'deriv'
  data_2 <- cbind(data_1, deriv)
  data_trunc <- data_2[!(data_2$roundK < lower | data_2$roundK > upper),]

  return(data_trunc)

}
