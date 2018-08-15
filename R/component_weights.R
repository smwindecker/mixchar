#' Accessor function to extract mean weights
#'
#' @param object a decon object
#' @return Extract mean fractions of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' \donttest{
#' data(juncus)
#' tmp <- process(juncus, init_mass = 18.96,
#'                temp = 'temp_C', mass_loss = 'mass_loss')
#' output <- deconvolve(tmp)
#' component_weights(output)
#' }
#' @export

component_weights <- function (object) {
  object$weights
}
