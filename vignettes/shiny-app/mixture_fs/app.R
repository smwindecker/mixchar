
library(shiny)

ui <- fluidPage(theme = 'bootstrap.css',

  fluidRow(class = "myRow1",
    column(8,
           plotOutput('fsplot')
    ),

    column(4,
           tabsetPanel(

             tabPanel(title = 'P-HC',

                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge,
                                      .js-irs-0 .irs-bar {background: blue}")),
                      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge,
                                      .js-irs-1 .irs-bar {background: blue}")),
                      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge,
                                      .js-irs-2 .irs-bar {background: blue}")),
                      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge,
                                      .js-irs-3 .irs-bar {background: blue}")),

                      sliderInput(inputId = 'h_HC', ticks = FALSE,
                                  label = 'HEIGHT',
                                  value = 0.015, step = 0.001, min = 0, max = 0.02),
                      sliderInput(inputId = 's_HC', ticks = FALSE,
                                  label = 'SKEW',
                                  value = -0.15, step = 0.01, min = -1, max = 1),
                      sliderInput(inputId = 'p_HC', ticks = FALSE,
                                  label = 'POSITION',
                                  value = 540, step = 10, min = 400, max = 900),
                      sliderInput(inputId = 'w_HC', ticks = FALSE,
                                  label = 'WIDTH',
                                  value = 50, step = 10, min = 0, max = 250)
                      ),

              tabPanel(title = 'P-CL',

                      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge,
                             .js-irs-4 .irs-bar {background: green}")),
                      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge,
                                      .js-irs-5 .irs-bar {background: green}")),
                      tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge,
                                      .js-irs-6 .irs-bar {background: green}")),
                      tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge,
                                      .js-irs-7 .irs-bar {background: green}")),

                      sliderInput(inputId = 'h_CL', ticks = FALSE,
                                  label = 'HEIGHT',
                                  value = 0.013, step = 0.001, min = 0, max = 0.02),
                      sliderInput(inputId = 's_CL', ticks = FALSE,
                                  label = 'SKEW',
                                  value = -0.15, step = 0.01, min = -1, max = 1),
                      sliderInput(inputId = 'p_CL', ticks = FALSE,
                                  label = 'POSITION',
                                  value = 600, step = 10, min = 400, max = 900),
                      sliderInput(inputId = 'w_CL', ticks = FALSE,
                                  label = 'WIDTH',
                                  value = 30, step = 10, min = 0, max = 250)
                      ),

             tabPanel(title = 'P-LG',

                      tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge,
                             .js-irs-8 .irs-bar {background: orange}")),
                      tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge,
                                      .js-irs-9 .irs-bar {background: orange}")),
                      tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge,
                                      .js-irs-10 .irs-bar {background: orange}")),
                      tags$style(HTML(".js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge,
                                      .js-irs-11 .irs-bar {background: orange}")),

                      sliderInput(inputId = 'h_LG', ticks = FALSE,
                                  label = 'HEIGHT',
                                  value = 0.01, step = 0.001, min = 0, max = 0.02),
                      sliderInput(inputId = 's_LG', ticks = FALSE,
                                  label = 'SKEW',
                                  value = -0.15, step = 0.01, min = -1, max = 1),
                      sliderInput(inputId = 'p_LG', ticks = FALSE,
                                  label = 'POSITION',
                                  value = 700, step = 10, min = 400, max = 900),
                      sliderInput(inputId = 'w_LG', ticks = FALSE,
                                  label = 'WIDTH',
                                  value = 200, step = 10, min = 0, max = 250)
                      )
             )
           )
    )
)

server <- function(input, output) {

  fs_function <- function (x, h, s, p, w) {
    interior <- 2 * s * ((x - p) / w)
    exterior <- -log(2) / s^2
    answer <- rep(0, length(interior))
    valid <- interior >= -1
    answer[valid] <- h * exp(exterior * (log(1 + interior[valid])^2))
    answer
  }

  fs_mixture <- function (x, params) {
    fs_mixture_function <- fs_function(x, params[1], params[4], params[7], params[10]) +
      fs_function(x, params[2], params[5], params[8], params[11]) +
      fs_function(x, params[3], params[6], params[9], params[12])
  }

  fs_mixture_wrap <- function (x, h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3) {
    params <- c(h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3)
    fs_mixture(x, params)
  }

  x <- seq(400, 900, length.out = 500)

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
    fs_mixture_wrap(x, input$h_HC, input$h_CL, input$h_LG, input$s_HC, input$s_CL, input$s_LG,
                    input$p_HC, input$p_CL, input$p_LG, input$w_HC, input$w_CL, input$w_LG)
  })

  output$fsplot <- renderPlot({
    x <- seq(400, 900, length.out = 500)
    plot(x, y_all(), type = 'l', col = 'black', ylim = c(0, 0.025),
         ylab = 'dm/dT (K-1)', xlab = 'T (K)')
    lines(x, y_HC(), col = 'blue')
    lines(x, y_CL(), col = 'green')
    lines(x, y_LG(), col = 'orange')
    legend(800, 0.022, c('Mixture model', 'P-HC', 'P-CL', 'P-LG'),
           col = c('black', 'blue', 'green', 'orange'), pch = 16)
  }, height = 400)

}

shinyApp(ui = ui, server = server)
