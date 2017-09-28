#' Accessor function to extract model fit
#'
#' @param object a decon object
#' @return $minpack.lm of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' data(juncus)
#' munge <- process(juncus, 'temp_C', 'mass_loss', 18.96, temp_type = 'C')
#' output <- deconvolve(munge)
#' Model(output)
#'
#' @export

Model <- function (object) {
  object$minpack.lm
}
