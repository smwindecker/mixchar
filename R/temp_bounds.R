#' Accessor function to extract selected temperature bounds
#'
#' @param object the output of either the process or deconvolve functions
#' @return Temperature bounds of the data in the object
#' @keywords thermogravimetry fraser-suzuki deconvolution temperature
#' @examples
#' data(juncus)
#' tmp <- process(juncus, init_mass = 18.96,
#'                temp = 'temp_C', mass_loss = 'mass_loss')
#' temp_bounds(tmp)
#'
#' @export

temp_bounds <- function (object) {

  if (class(object) == 'process') return(object$temp_range)
  if (class(object) == 'decon') return(object$temp_bounds)

}
