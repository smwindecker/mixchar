library(shiny)

shinyApp(

  ui <- fixedPage(theme = 'bootstrap.css',

                  fixedRow(class = "myRow1",
                           'Three-part Fraser-Suzuki mixture model for deconvolution of thermogravimetric decay curves',
                           column(10,
                                  plotOutput('fsplot')
                           ),
                           fixedRow(
                             column(10,
                                    tabsetPanel(

                                      tabPanel(title = 'Hemicellulose (HC)',

                                               tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge,
                                                               .js-irs-0 .irs-bar {background: #440154FF}")),
                                               tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge,
                                                               .js-irs-1 .irs-bar {background: #440154FF}")),
                                               tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge,
                                                               .js-irs-2 .irs-bar {background: #440154FF}")),
                                               tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge,
                                                               .js-irs-3 .irs-bar {background: #440154FF}")),

                                               sliderInput(inputId = 'h_HC', ticks = FALSE,
                                                           label = 'Height',
                                                           value = 0.003, step = 0.001, min = 0, max = 0.01),
                                               sliderInput(inputId = 's_HC', ticks = FALSE,
                                                           label = 'Skew',
                                                           value = -0.15, step = 0.01, min = -1, max = 1),
                                               sliderInput(inputId = 'p_HC', ticks = FALSE,
                                                           label = 'Position',
                                                           value = 250, step = 10, min = 120, max = 700),
                                               sliderInput(inputId = 'w_HC', ticks = FALSE,
                                                           label = 'Width',
                                                           value = 50, step = 10, min = 0, max = 250)
                                               ),

                                      tabPanel(title = 'Cellulose (CL)',

                                               tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge,
                                                               .js-irs-4 .irs-bar {background: #B8DE29FF}")),
                                               tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge,
                                                               .js-irs-5 .irs-bar {background: #B8DE29FF}")),
                                               tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge,
                                                               .js-irs-6 .irs-bar {background: #B8DE29FF}")),
                                               tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge,
                                                               .js-irs-7 .irs-bar {background: #B8DE29FF}")),

                                               sliderInput(inputId = 'h_CL', ticks = FALSE,
                                                           label = 'Height',
                                                           value = 0.005, step = 0.001, min = 0, max = 0.01),
                                               sliderInput(inputId = 's_CL', ticks = FALSE,
                                                           label = 'Skew',
                                                           value = -0.15, step = 0.01, min = -1, max = 1),
                                               sliderInput(inputId = 'p_CL', ticks = FALSE,
                                                           label = 'Position',
                                                           value = 300, step = 10, min = 120, max = 700),
                                               sliderInput(inputId = 'w_CL', ticks = FALSE,
                                                           label = 'Width',
                                                           value = 30, step = 10, min = 0, max = 250)
                                               ),

                                      tabPanel(title = 'Lignin (LG)',

                                               tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge,
                                                               .js-irs-8 .irs-bar {background: #3CBB75FF}")),
                                               tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge,
                                                               .js-irs-9 .irs-bar {background: #3CBB75FF}")),
                                               tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge,
                                                               .js-irs-10 .irs-bar {background: #3CBB75FF}")),
                                               tags$style(HTML(".js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge,
                                                               .js-irs-11 .irs-bar {background: #3CBB75FF}")),

                                               sliderInput(inputId = 'h_LG', ticks = FALSE,
                                                           label = 'Height',
                                                           value = 0.001, step = 0.001, min = 0, max = 0.01),
                                               sliderInput(inputId = 's_LG', ticks = FALSE,
                                                           label = 'Skew',
                                                           value = -0.15, step = 0.01, min = -1, max = 1),
                                               sliderInput(inputId = 'p_LG', ticks = FALSE,
                                                           label = 'Position',
                                                           value = 330, step = 10, min = 120, max = 700),
                                               sliderInput(inputId = 'w_LG', ticks = FALSE,
                                                           label = 'Width',
                                                           value = 250, step = 10, min = 0, max = 300)
                                               )
                                               )
                                               )
                                               )


                                               )
                           ),

  server <- function (input, output) {

    fs_function <- function (x, h, s, p, w) {

      interior <- 2 * s * ((x - p) / w)
      exterior <- -log(2) / s^2

      answer <- rep(0, length(interior))
      valid <- interior >= -1
      answer[valid] <- h * exp(exterior * (log(1 + interior[valid])^2))
      answer

    }

    fs_mixture <- function (x,
                            h1, s1, p1, w1,
                            h2, s2, p2, w2,
                            h3, s3, p3, w3,
                            h0 = NULL, s0 = NULL, p0 = NULL, w0 = NULL) {

      output <- fs_function(x, h1, s1, p1, w1) +
        fs_function(x, h2, s2, p2, w2) +
        fs_function(x, h3, s3, p3, w3)

      n_params <- length(c(h1, s1, p1, w1, h2, s2, p2, w2, h3, s3, p3, w3, h0, s0, p0, w0))

      if (n_params != 12 & n_params != 16) {
        stop('Specify correct number of parameters')
      }

      if (!is.null(h0) & !is.null(s0) & !is.null(p0) & !is.null(w0)) {
        output <- output + fs_function(x, h0, s0, p0, w0)
      }

      output
    }

    x <- seq(120, 700, length.out = 500)

    y_HC <- reactive({
      fs_function(x, input$h_HC, input$s_HC, input$p_HC, input$w_HC)
    })

    y_CL <- reactive({
      fs_function(x, input$h_CL, input$s_CL, input$p_CL, input$w_CL)
    })

    y_LG <- reactive({
      fs_function(x, input$h_LG, input$s_LG, input$p_LG, input$w_LG)
    })

    y_all <- reactive({
      fs_mixture(x, input$h_HC, input$s_HC, input$p_HC, input$w_HC,
                 input$h_CL, input$s_CL, input$p_CL, input$w_CL,
                 input$h_LG, input$s_LG, input$p_LG, input$w_LG)
    })

    output$fsplot <- renderPlot({
      x <- seq(120, 700, length.out = 500)

      par(xpd = TRUE, mar = c(5, 5, 3, 5))
      plot(x, y_all(), type = 'l', col = 'black',
           xlab = 'Temperature (C)', ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')),
           yaxs = 'i', ylim = c(0, 0.01),
           pch = 20, cex = 0.5, cex.axis = 1.4, cex.lab = 1.6, bty = 'L')

      lines(x, y_HC(), col = '#440154FF', lty = 3, lwd = 3)
      lines(x, y_CL(), col = '#B8DE29FF', lty = 4, lwd = 3)
      lines(x, y_LG(), col = '#3CBB75FF', lty = 5, lwd = 3)

      legend(600, 0.0088,
             yjust = 1,
             legend = c('Total', 'Hemicellulose', 'Cellulose', 'Lignin'),
             cex = 1.2,
             bty = 'n',
             col = c('black', '#440154FF', '#B8DE29FF', '#3CBB75FF'),
             lty = c(1, 3, 4, 5),
             lwd = c(2, 3, 3, 3))

    }, height = 400)

  },

  options = list(height = 500)

)
