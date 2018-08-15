#' Accessor function to extract model parameters
#'
#' @param object a decon object
#' @return model parameters from minpack.lm::nlsLM fit
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @examples
#' \donttest{
#' data(juncus)
#' tmp <- process(juncus, init_mass = 18.96,
#'                temp = 'temp_C', mass_loss = 'mass_loss')
#' output <- deconvolve(tmp)
#' model_parameters(output)
#' }
#' @export

model_parameters <- function (object) {

  params <- as.data.frame(summary(object$model_fit)$coefficients[,1])
  colnames(params) <- 'parameter_value'
  params$parameter_name <- row.names(params)
  row.names(params) <- c()
  params <- params[,c(2,1)]

  params
}
