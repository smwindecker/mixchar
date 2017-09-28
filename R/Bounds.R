#' Accessor function to extract selected bounds
#'
#' @param object a process object
#' @return $bounds of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' data(juncus)
#' munge <- process(juncus, 'temp_C', 'mass_loss', 18.96, temp_type = 'C')
#' Bounds(munge)
#'
#' @export

Bounds <- function (object) {
  object$bounds
}
