# Make position values 'free'
#
# This function revises position value.
#
# @param pos_values position values
# @return 'free' position values
# @keywords internal

free_peaks <- function (pos_values) {
  pos_diff <- c(pos_values[1], diff(pos_values))
  log(pos_diff)
}
