# Fraser-Suzuki function
#
# This function calculates the Fraser-Suzuki function.
#
# @param x temperature values
# @param h height value
# @param s shape value
# @param p position value
# @param w width value
# @return Fraser-Suzuki model output
# @keywords internal

# Frazer-Suzuki function
fs_function <- function (x, h, s, p, w) {

  interior <- 2 * s * ((x - p) / w)
  exterior <- -log(2) / s^2

  answer <- rep(0, length(interior))
  valid <- interior >= -1
  answer[valid] <- h * exp(exterior * (log(1 + interior[valid])^2))
  answer

}
