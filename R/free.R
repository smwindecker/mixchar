# Make position values 'free' - full step
#
# This function revises position value.
#
# @param values vector of starting values
# @return 'free' starting values
# @keywords internal

free <- function (values) {

  if (length(values) == 12) {
    c(values[1:6],
      free_peaks(values[7:9]),
      values[10:12])
  } else {
    c(values[1:8],
      free_peaks(values[9:12]),
      values[13:16])
  }

}
