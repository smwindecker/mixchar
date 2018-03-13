#' Non-linear model using Fraser-Suzuki mixture model
#'
#' Non-linear model output using optimised parameter values
#' with a three-part mixture model using Fraser-Suzuki equation
#'
#' @param dataframe dataframe
#' @param params starting parameter values
#' @param lb lower bounds for model
#' @param ub upper bounds for model
#' @importFrom minpack.lm nlsLM
#' @return model output
#'
#' @export

fs_model <- function (dataframe, params, lb, ub) {

  if (length(params) == 12) {
    pred <- fs_mixture(temp_C, h1, s1, p1, w1, h2, s2, p2, w2, h3, s3, p3, w3)
    start_list <- list(h1 = params[1], s1 = params[2], p1 = params[3], w1 = params[4],
                       h2 = params[5], s2 = params[6], p2 = params[7], w2 = params[8],
                       h3 = params[9], s3 = params[10], p3 = params[11], w3 = params[12])
  }

  if (length(params) == 16) {
    pred <- fs_mixture(temp_C, h1, s1, p1, w1, h2, s2, p2, w2, h3, s3, p3, w3, h4, s4, p4, w4)
    start_list <- list(h1 = params[1], s1 = params[2], p1 = params[3], w1 = params[4],
                       h2 = params[5], s2 = params[6], p2 = params[7], w2 = params[8],
                       h3 = params[9], s3 = params[10], p3 = params[11], w3 = params[12],
                       h4 = params[13], s4 = params[14], p4 = params[15], w4 = params[16])
  }

  nlsLM(deriv ~ pred,
        start = start_list,
        data = dataframe,
        control = nls.lm.control(maxiter = 1024, maxfev = 1e6),
        lower = lb,
        upper = ub)

}
