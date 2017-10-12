# Make position values 'constrained' - full function
#
# This function constrains free position values.
#
# @param free_values vector of unconstrained starting values
# @return 'constrained' starting values - make them monotone increasing
# @keywords internal

constrain <- function (values) {

  if (length(values) == 12) {
    c(values[1:6],
      constrain_peaks(values[7:9]),
      values[10:12])
  } else {
    c(values[1:8],
      constrain_peaks(values[9:12]),
      values[13:16])
  }

}
