library(deconvolve)

data(raw_data)
raw_data$temp_K <- raw_data$temp_C + 273
munge_data <- tga_process(raw_data, 'temp_K', 'mass_loss', 18.9615, 400, 900)
fit <- deconvolve(munge_data, munge_data$temp_K, munge_data$deriv)

png('vignettes/Figures/plot1.png',
  width     = 3.25,
  height    = 3.25,
  units     = "in",
  res       = 1200,
  pointsize = 4
)
plot(munge_data$temp_K, munge_data$mass_T, xlab = '', ylab = '', cex.axis = 2)
dev.off()
png('vignettes/Figures/plot2.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
)
plot(munge_data$temp_K, munge_data$deriv, xlab = '', ylab = '', cex.axis = 2)
dev.off()
png('vignettes/Figures/plot3.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
)
decon_plot <- function (temp, deriv, fit, lower, upper) {

  plot(temp, deriv, xlab = '', ylab = '', cex.axis = 2)
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
  legend(680, 0.13, legend=c('Total DTG', 'Pseudo-component 1', 'Pseudo-component 2', 'Pseudo-component 3'),
         col = c('red', 'blue', 'green', 'orange'), lty = 1, cex = 1.5)

}
decon_plot(munge_data$temp_K, munge_data$deriv, fit, 400, 900)
dev.off()

