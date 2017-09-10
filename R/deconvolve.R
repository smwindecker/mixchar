#' Optimise Parameters
#'
#' This function otimises parameter selection so that the model will ultimately converge.
#'
#' @param data vector of starting values for each of 12 parameters
#' @param temp temperature values
#' @param obs DTG mass values
#' @return model output
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @export deconvolve()

deconvolve <- function (data, temp, obs) {

  theta <- c(.015, .013, .01, -.15, -.15, -.15, 540, 600, 700, 50, 30, 200)
  lb <- c(0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0)
  params_opt <- .paramSelect(theta, lb, .fsTotal, temp, obs, restarts = 300)
  fs <- .fsModel(data, params_opt)

  return(fs)

}
