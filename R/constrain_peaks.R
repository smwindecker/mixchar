# Make position values 'constrained'
#
# This function constrains free position values.
#
# @param free_pos_values vector of unconstrained position values
# @return 'constrained' position values - make them monotone increasing
# @keywords internal

constrain_peaks <- function (free_pos_values) {
  pos_diff <- exp(free_pos_values)
  cumsum(pos_diff)
}
