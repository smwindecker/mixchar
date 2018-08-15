#' Fraser-Suzuki function for a single curve
#'
#' This function calculates the Fraser-Suzuki function.
#'
#' @param temp temperature values
#' @param height height value
#' @param skew shape value
#' @param position position value
#' @param width width value
#' @return Fraser-Suzuki function
#' @examples
#' temp <- 150:600
#' fs_output <- fs_function(temp, height = 0.004, skew = -.15,
#' position = 250, width = 50)
#' @export

fs_function <- function (temp, height, skew, position, width) {

  interior <- 2 * skew * ((temp - position) / width)
  exterior <- -log(2) / skew^2

  answer <- rep(0, length(interior))
  valid <- interior >= -1
  answer[valid] <- height * exp(exterior * (log(1 + interior[valid])^2))
  answer

}
