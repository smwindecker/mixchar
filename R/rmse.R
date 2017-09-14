# Calculate room mean squared error
#
# This function calculated root mean squarer error of prediction.
#
# @param pred predicted parameter value
# @param obs DTG mass values
# @return root mean squared error
# @keywords internal

rmse <- function (pred, obs) {
  sqrt(sum((obs - pred) ^ 2))
}
