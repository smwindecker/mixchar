library(deconvolve)

data(juncus)
output <- deconvolve(juncus, 'temp_C', 'mass_loss', 18.96, temp_type = 'C')
munge_data <- output$data

temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
munge_data <- output$data
fit <- output$minpack.lm_object

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
par(xpd = T, mar = par()$mar + c(0,0,0,5))
plot(output)
dev.off()
