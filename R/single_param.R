#' Calculate single parameter estimates
#'
#' This function gives single parameter estimates for given
#' parameter and pseudo-component.
#'
#' @param fit model fit output from deconvolve function
#' @param param parameter value of interest
#' @param component Number of pseudo-component of interest
#' @return parameter estimate value
#' @keywords thermogravimetry fraser-suzuki deconvolution

#' @export

single_param <- function (fit, param, component) {

  coef <- as.data.frame(summary(model)$coefficients[,1])
  colnames(coef) <- 'estimate'
  coef$param <- rownames(coef)
  coef$parameter <- substr(coef$param, 1, 1)
  coef$component <- substr(coef$param, 2, 2)

  value <- coef$estimate[coef$parameter == param & coef$component == component]
  print(value)

}
