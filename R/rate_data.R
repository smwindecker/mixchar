#' Accessor function to extract processed dataframe
#'
#' @param object a process or deconvolve object
#' @return Dataframe of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' data(juncus)
#' tmp <- process(juncus, init_mass = 18.96,
#'                temp = 'temp_C', mass_loss = 'mass_loss')
#' rate_data(tmp)
#'
#' @export

rate_data <- function (object) {
  object$data
}
