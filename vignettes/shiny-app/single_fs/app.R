
library(shiny)

# add button to 'print parameters', which prints table of selected parameters.

ui <- fluidPage(theme = 'bootstrap.css',

  fluidRow(class = "myRow1",
    column(8,
           plotOutput('plot')
    ),

    column(4,
tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge,
                .js-irs-0 .irs-bar {background: red}")),
tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge,
                .js-irs-1 .irs-bar {background: red}")),
tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge,
                .js-irs-2 .irs-bar {background: red}")),
tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge,
                .js-irs-3 .irs-bar {background: red}")),

sliderInput(inputId = 'h', ticks = FALSE,
            label = 'HEIGHT',
            value = 0.015, step = 0.001, min = 0, max = 0.02),
sliderInput(inputId = 's', ticks = FALSE,
            label = 'SKEW',
            value = -0.15, step = 0.01, min = -1, max = 1),
sliderInput(inputId = 'p', ticks = FALSE,
            label = 'POSITION',
            value = 540, step = 10, min = 400, max = 900),
sliderInput(inputId = 'w', ticks = FALSE,
            label = 'WIDTH',
            value = 50, step = 10, min = 0, max = 250)
    )
))

server <- function(input, output) {

  fs_function <- function (x, h, s, p, w) {
    interior <- 2 * s * ((x - p) / w)
    exterior <- -log(2) / s^2
    answer <- rep(0, length(interior))
    valid <- interior >= -1
    answer[valid] <- h * exp(exterior * (log(1 + interior[valid])^2))
    answer
  }

  x <- seq(400, 900, length.out = 500)

  y <- reactive({
    fs_function(x, input$h, input$s, input$p, input$w)
  })

  output$plot <- renderPlot({
    plot(x, y(), col = 'red', ylim = c(0, 0.02), ylab = 'dm/dT (K-1)', xlab = 'T (K)',
         main = 'Single Fraser-Suzuki curve')
  }, height = 300, width = 300)

}

shinyApp(ui = ui, server = server)
