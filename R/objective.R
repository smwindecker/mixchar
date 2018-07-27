# Objective function
#
#' This function evaluates root mean squared error of a model
#'
#' @param theta values for each of the parameters
#' @param model mathematical function to be used to model data
#' @param temp temperature values
#' @param obs DTG mass values
#' @return root mean square error of model output
#' @keywords internal

objective <- function (theta, model, temp, obs) {

  if (length(theta) == 12) {
    pred <- model(temp,
                  theta[1], theta[2], theta[3], theta[4],
                  theta[5], theta[6], theta[7], theta[8],
                  theta[9], theta[10], theta[11], theta[12])
  }

  if (length(theta) == 16) {
    pred <- model(temp,
                  theta[5], theta[6], theta[7], theta[8],
                  theta[9], theta[10], theta[11], theta[12],
                  theta[13], theta[14], theta[15], theta[16],
                  theta[1], theta[2], theta[3], theta[4])
  }

  # see how good it is
  target <- rmse(pred, obs)

  target

}

