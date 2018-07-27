#' Accessor function to extract selected temperature bounds
#'
#' @param object the output of either the process or deconvolve functions
#' @return Temperature bounds of the data in the object
#' @keywords thermogravimetry fraser-suzuki deconvolution temperature
#' @examples
#' data(juncus)
#' tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96, temp_type = 'C')
#' temp_bounds(tmp)
#'
#' @export

temp_bounds <- function (object) {

  object$bounds

}
