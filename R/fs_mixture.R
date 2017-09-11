#' Fraser-Suzuki three-part mixture
#'
#' This function combines three Fraser-Suzuki functions for the total mixture model.
#'
#' @param x temperature values
#' @param params parameter starting values
#' @return Fraser-Suzuki mixture model output
#' @keywords internal
#'
#' @export

# combine the FS function three times for the three pseudo-components
fs_mixture <- function (x, params) {

  fs_mixture_function <- deconvolve:::fs_function(x, params[1], params[4], params[7], params[10]) +
    deconvolve:::fs_function(x, params[2], params[5], params[8], params[11]) +
    deconvolve:::fs_function(x, params[3], params[6], params[9], params[12])

}
