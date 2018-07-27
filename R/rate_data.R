#' Accessor function to extract processed dataframe
#'
#' @param object a process or deconvolve object
#' @return Dataframe of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' data(juncus)
#' tmp <- process(juncus, 'temp_C', 'mass_loss', 18.96, temp_type = 'C')
#' rate_data(tmp)
#'
#' @export

rate_data <- function (object) {

  object$data

}
