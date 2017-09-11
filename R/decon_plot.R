#' Plot pseudo-components
#'
#' This function produces a plot of the overal DTG curve as well as the deconvolved pseudo-components.
#'
#' @param temp vector of temperature data in K
#' @param deriv vector of DTG data
#' @param fit model fit output from deconvolve function
#' @param lower lower bound for temperature for cropping dataset
#' @param upper upper bound for temperature for cropping dataset
#' @return plot of DTG against K, with curve for each pseudo-component included
#' @keywords thermogravimetry fraser-suzuki deconvolution

#' @export

decon_plot <- function (temp, deriv, fit, lower, upper) {

  plot(temp, deriv, xlab = 'Temperature (K)', ylab = 'DTG (mg/K)')
  curve(fs_mixture_wrap(x, single_param(fit, 'h', '1'), single_param(fit, 'h', '2'), single_param(fit, 'h', '3'),
                        single_param(fit, 's', '1'), single_param(fit, 's', '2'), single_param(fit, 's', '3'),
                        single_param(fit, 'p', '1'), single_param(fit, 'p', '2'), single_param(fit, 'p', '3'),
                        single_param(fit, 'w', '1'), single_param(fit, 'w', '2'), single_param(fit, 'w', '3')),
        from = lower, to = upper, add = TRUE, col = 'red')
  curve(fs_function(x, single_param(fit, 'h', '1'), single_param(fit, 's', '1'), single_param(fit, 'p', '1'),
                    single_param(fit, 'w', '1')), from = lower, to = upper, add = TRUE, col = 'blue')
  curve(fs_function(x, single_param(fit, 'h', '2'), single_param(fit, 's', '2'), single_param(fit, 'p', '2'),
                    single_param(fit, 'w', '2')), from = lower, to = upper, add = TRUE, col = 'green')
  curve(fs_function(x, single_param(fit, 'h', '3'), single_param(fit, 's', '3'), single_param(fit, 'p', '3'),
                    single_param(fit, 'w', '3')), from = lower, to = upper, add = TRUE, col = 'orange')
  legend(upper-150, max(deriv), legend=c('Total DTG', 'Pseudo-component 1', 'Pseudo-component 2', 'Pseudo-component 3'),
         col = c('red', 'blue', 'green', 'orange'), lty = 1, cex=0.8)

}

