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
  fit <- object$model_fit
  as.data.frame(summary(fit)$coefficients[,1])
}
