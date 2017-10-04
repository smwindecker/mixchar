# Objective function
#
# This function evaluates a model using rmse.
#
# @param params values for each of 12 parameters
# @param model mathematical function to be used to model data
# @param x temperature values
# @param obs DTG mass values
# @return root mean square error of model output
# @keywords internal

objective <- function (theta, model, x, obs) {

  pred <- model(x, theta)

  # see how good it is
  target <- rmse(pred, obs)

  target

}

