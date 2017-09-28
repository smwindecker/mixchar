#' Accessor function to extract mass fractions
#'
#' @param object a decon object
#' @return $mass_fractions of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' data(juncus)
#' munge <- process(juncus, 'temp_C', 'mass_loss', 18.96, temp_type = 'C')
#' output <- deconvolve(munge)
#' MassFrac(output)
#'
#' @export

MassFrac <- function (object) {
  object$mass_fractions
}
