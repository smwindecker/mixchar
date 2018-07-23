
tga_theory <- function (data) {

  # read raw TGA
  tmp <- deconvolve::process(data, 'temp_C', 'mass_loss', 18.96)

  # plot TG curve
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = TRUE), heights = c(0.8, 0.2))
  par(oma = c(5, 3, 0, 2), mar = c(3, 6, 3, 3))

  plot(tmp$data$temp_C, tmp$data$mass_T, yaxs = 'i', ylim = c(0, 22), xlim = c(0, 900),
       xaxs = 'i', ylab = 'Mass (mg)', xlab = '', xaxt = 'n', yaxt = 'n',
       pch = 20, cex = 0.3, cex.lab = 3)
  axis(side = 1, at = c(0, 200, 400, 600, 800), cex.axis = 2.8, labels = c(0, 200, 400, 600, 800))
  axis(side = 2, at = c(0, 10, 20), cex.axis = 2.8,
       labels = c(0, 10, 20))
  legend('topleft',
         legend = '(a)',
         bty = 'n',
         cex = 2.5)
  arrows(x0 = 266, y0 = 19, x1 = 266, y1 = 17, lwd = 2, length = 0.1)
  arrows(x0 = 317, y0 = 15, x1 = 317, y1 = 13, lwd = 2, length = 0.1)
  arrows(x0 = 340, y0 = 11.5, x1 = 340, y1 = 9.5, lwd = 2, length = 0.1)

  # plot DTG curve
  plot(tmp$data$temp_C, tmp$data$deriv, yaxs = 'i', ylim = c(0, 0.009),
       xaxs = 'i', ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), xlab = '', xaxt = 'n', yaxt = 'n',
       pch = 20, cex.lab = 3, cex = 0.9)
  axis(side = 1, at = c(0, 200, 400, 600, 800), cex.axis = 2.8, labels = c(0, 200, 400, 600, 800))
  axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 2.8,
       labels = c(0, 0.004, 0.008))
  legend('topleft',
         legend = '(b)',
         bty = 'n',
         cex = 2.5)

  # deconvolve data
  output <- deconvolve::deconvolve(tmp, upper_temp = 650, n_curves = NULL)
  temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
  fit <- output$minpack.lm
  params <- as.data.frame(summary(fit)$coefficients[,1])

  # plot mixture model outcome on DTG data
  plot(output$data$temp_C, output$data$deriv, yaxs = 'i', ylim = c(0, 0.009),
       ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), xlab = '',
       xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.9, cex.lab = 3)
  axis(side = 1, at = c(200, 400, 600, 800), cex.axis = 2.8, labels = c(200, 400, 600, 800))
  axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 2.8,
       labels = c(0, 0.004, 0.008))
  arrows(x0 = 266, y0 = 0.0062, x1 = 266, y1 = 0.0055, lwd = 2, length = 0.1)
  arrows(x0 = 317, y0 = 0.0087, x1 = 317, y1 = 0.008, lwd = 2, length = 0.1)
  arrows(x0 = 365, y0 = 0.0022, x1 = 365, y1 = 0.0015, lwd = 2, length = 0.1)

  y1 <- deconvolve::fs_mixture(x = temp,
                               h1 = params['h1',], s1 = params['s1',],
                               p1 = params['p1',], w1 = params['w1',],
                               h2 = params['h2',], s2 = params['s2',],
                               p2 = params['p2',], w2 = params['w2',],
                               h3 = params['h3',], s3 = params['s3',],
                               p3 = params['p3',], w3 = params['w3',])

  y2 <- deconvolve::fs_function(x = temp,
                                h = params['h1',], s = params['s1',],
                                p = params['p1',], w = params['w1',])

  y3 <- deconvolve::fs_function(x = temp,
                                h = params['h2',], s = params['s2',],
                                p = params['p2',], w = params['w2',])

  y4 <- deconvolve::fs_function(x = temp,
                                h = params['h3',], s = params['s3',],
                                p = params['p3',], w = params['w3',])

  lines(temp, y1, lty = 1, lwd = 2)
  lines(temp, y2, lty = 3, lwd = 3.5, col = '#440154FF')
  lines(temp, y3, lty = 4, lwd = 3.5, col = '#B8DE29FF')
  lines(temp, y4, lty = 5, lwd = 3.5, col = '#3CBB75FF')

  legend('topright',
         legend = c('DTG data', 'DTG modelled', 'Hemicelluloses', 'Cellulose', 'Lignin'),
         ncol = 1,
         cex = 2.2,
         bty = 'n',
         lty = c(NA, 1, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA),
         col = c('black', 'black', '#440154FF', '#B8DE29FF', '#3CBB75FF'),
         lwd = 2)
  legend('topleft',
         legend = '(c)',
         bty = 'n',
         cex = 2.5)

  mtext(text = 'Temperature (C)',
        side = 1,
        line = 2,
        outer = TRUE,
        cex = 2.5)

}
