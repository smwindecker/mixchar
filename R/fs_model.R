#' Non-linear model using Fraser-Suzuki mixture model
#'
#' Non-linear model output using optimised parameter values
#' with a three-part mixture model using Fraser-Suzuki equation
#'
#' @param dataframe dataframe
#' @param params starting parameter values
#' @return model output
#' @keywords internal
#' @import minpack.lm

# function to do the nls fit with the correct starting values
fs_model <- function (dataframe, params) {

  nlsLM(deriv ~ deconvolve:::fs_mixture_wrap(temp_K, h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3),
        start = list(h1 = params[1], h2 = params[2], h3 = params[3],
                     s1 = params[4], s2 = params[5], s3 = params[6],
                     p1 = params[7], p2 = params[8], p3 = params[9],
                     w1 = params[10], w2 = params[11], w3 = params[12]),
        data = dataframe,
        control = nls.lm.control(maxiter = 1024, maxfev = 1e6))

}
