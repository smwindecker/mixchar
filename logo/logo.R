png('hex_bg.png', width = 1500, height = 1500, bg = 'transparent')
hexd <- data.frame(x = 1 + c(rep(-sqrt(3)/2, 2), 0, rep(sqrt(3)/2, 2), 0),
                   y = 1 + c(0.5, -0.5, -1, -0.5, 0.5, 1))
hexd <- rbind(hexd, hexd[1, ])
x_lim <- range(hexd$x)
y_lim <- range(hexd$y)
plot.new()
plot.window(x_lim, y_lim)
polygon(hexd, col = "#ededed", border = NA)
segments(x0 = hexd$x[1] + .03,
         y0 = hexd$y[1] - .03,
         x1 = hexd$x[2] + .03,
         y1 = hexd$y[2] + .03, lwd = 16, col = '#000000')
segments(x0 = hexd$x[3],
         y0 = hexd$y[3] + .033,
         x1 = hexd$x[4] - .03,
         y1 = hexd$y[4] + .02, lwd = 13, col = '#000000')
segments(x0 = hexd$x[5] - .03,
         y0 = hexd$y[5] - .02,
         x1 = hexd$x[6],
         y1 = hexd$y[6] - .033, lwd = 13, col = '#000000')
img <- png::readPNG("mixchar_logo.png")
rasterImage(img, .4, .4, 1.6, 1.6)
dev.off()

hexSticker::sticker('hex_bg.png', s_x = .985, s_y = .99, s_width = 1.1,
        package = '', h_color = '#000000', h_fill = '#ededed',
        filename = "hexsticker.png")




