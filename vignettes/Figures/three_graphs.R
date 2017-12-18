library(deconvolve)

data(juncus)
munge <- process(juncus, 'temp_C', 'mass_loss', 16.85, 'C')
output <- deconvolve(munge, lower = 420, upper = 860)
#
# munge_data <- output$data
#
# temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
# munge_data <- output$data
# fit <- output$minpack.lm_object

data <- munge$data[munge$data$temp_K >400 & munge$data$temp_K<900,]








png('vignettes/Figures/plot1.png', width = 560, height = 480, 'px', bg = 'transparent')
plot(data$temp_K, data$mass_T, yaxs = 'i', ylim = c(0, max(data$mass_T) + 0.06*max(data$mass_T)),
     ylab = '', xlab = '', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
plot(data$temp_K, data$mass_T, yaxs = 'i', ylim = c(0, max(data$mass_T) + 0.06*max(data$mass_T)),
     ylab = '', xlab = '', yaxt = 'n', pch = 20, cex = 0.6, cex.axis = 1.8)
increment <- max(data$mass_T)/2
axis(side = 2, at = c(0, increment, max(data$mass_T)), cex.axis = 1.8,
     labels = sprintf("%.2f", c(0, increment, max(data$mass_T))))
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
