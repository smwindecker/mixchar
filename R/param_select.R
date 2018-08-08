#' Optimise parameters
#'
#' This function otimises parameter selection so that the model will ultimately converge.
#'
#' @param theta vector of starting values for each parameter
#' @param lb vector of lower bounds on each parameter
#' @param ub vector of upper bounds on each parameter
#' @param model mathematical function to be applied
#' @param temp temperature values
#' @param obs DTG mass values
#' @param seed random seed for optimiser
#' @param restarts number of times for optimiser to restart
#' @return optimised starting parameter values
#' @keywords internal
#' @importFrom nloptr nloptr

# create params_opt with this function
param_select <- function (theta, lb, ub, model, temp,
                          obs, seed, restarts = 300) {

  opts <- list("algorithm" = "NLOPT_LN_BOBYQA",
               "xtol_rel" = 1.0e-12,
               "ranseed" = seed)

  # fit the model `restarts` times with different starting locations
  o_list <- replicate(restarts,
                      nloptr::nloptr(x0 = theta,
                                     eval_f = objective,
                                     lb = lb,
                                     ub = ub,
                                     model = model,
                                     temp = temp,
                                     obs = obs,
                                     opts = opts),
                      simplify = FALSE)

  # find the best one
  fits <- vapply(o_list,
                 function (x) x$objective,
                 FUN.VALUE = 0)
  best <- which.min(fits)
  o <- o_list[[best]]

  o$solution

}

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

#' Calculate room mean squared error
#'
#' This function calculated root mean squarer error of prediction.
#'
#' @param pred predicted parameter value
#' @param obs DTG mass values
#' @return root mean squared error
#' @keywords internal

rmse <- function (pred, obs) {
  sqrt(sum((obs - pred) ^ 2))
}
