#' Accessor function to extract processed dataframe
#'
#' @param object a process object
#' @return $data of the object
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' data(juncus)
#' munge <- process(juncus, 'temp_C', 'mass_loss', 18.96, temp_type = 'C')
#' ModData(munge)
#'
#' @export

ModData <- function (object) {
  object$data
}
