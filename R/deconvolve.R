#' Deconvolves Thermogravimetric Data
#'
#' This function deconvolves thermogravimetric data using a three-part
#' Fraser-Suzuki mixture model
#'
#' @param data vector of starting values for each of 12 parameters
#' @param temp temperature values
#' @param obs DTG mass values
#' @return model output
#' @keywords thermogravimetry fraser-suzuki deconvolution

#' @export

deconvolve <- function (data, temp, obs) {

  theta <- c(.015, .013, .01, -.15, -.15, -.15, 540, 600, 700, 50, 30, 200)
  lb <- c(0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0)
  params_opt <- .param_select(theta, lb, .fsTotal, temp, obs, restarts = 300)
  fs <- .fs_model(data, params_opt)

  return(fs)

}
