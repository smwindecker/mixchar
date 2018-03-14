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

  if (length(theta) == 12) {
    pred <- model(x,
                  theta[1], theta[2], theta[3], theta[4],
                  theta[5], theta[6], theta[7], theta[8],
                  theta[9], theta[10], theta[11], theta[12])
  }

  if (length(theta) == 16) {
    pred <- model(x,
                  theta[1], theta[2], theta[3], theta[4],
                  theta[5], theta[6], theta[7], theta[8],
                  theta[9], theta[10], theta[11], theta[12],
                  theta[13], theta[14], theta[15], theta[16])
  }

  # see how good it is
  target <- rmse(pred, obs)

  target

}

