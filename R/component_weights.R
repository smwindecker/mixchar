#' Accessor function to extract mean weights
#'
#' @param object a decon object
#' @return Extract mean fractions of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' \dontrun{
#' data(juncus)
#' tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96, temp_type = 'C')
#' output <- deconvolve(tmp)
#' component_weights(output)
#' }
#' @export

component_weights <- function (object) {
  object$weights
}
