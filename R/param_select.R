#' Optimise parameters
#'
#' This function otimises parameter selection so that the model will ultimately converge.
#'
#' @param theta vector of starting values for each of 12 parameters
#' @param lb vector of lower bounds on each of 12 parameters
#' @param model mathematical function to be applied
#' @param x temperature values
#' @param obs DTG mass values
#' @param restarts number of times for optimiser to restart
#' @return optimised starting parameter values
#' @keywords internal
#' @import nloptr

# create params_opt with this function
param_select <- function (theta, lb, ub, model, x, obs, restarts = 300) {

  opts <- list("algorithm"="NLOPT_LN_BOBYQA",
               "xtol_rel"=1.0e-12)

  # fit the model `restarts` times with different starting locations
  o_list <- replicate(restarts,
                      nloptr(x0 = theta,
                             eval_f = objective,
                             lb = lb,
                             ub = ub,
                             model = model,
                             x = x,
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
