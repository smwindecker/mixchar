#' Accessor function to extract model fit
#'
#' @param object a decon object
#' @return $minpack.lm of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' \donttest{
#' data(juncus)
#' tmp <- process(juncus, init_mass = 18.96,
#'                temp = 'temp_C', mass_loss = 'mass_loss')
#' output <- deconvolve(tmp)
#' model_fit(output)
#' }
#' @export

model_fit <- function (object) {
  object$model_fit
}
