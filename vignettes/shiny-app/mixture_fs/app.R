shinyApp(

  ui <- fluidPage(theme = 'bootstrap.css',

                  fluidRow(class = "myRow1",
                           column(8,
                                  plotOutput('fsplot')
                           ),

                           column(4,
                                  tabsetPanel(

                                    tabPanel(title = 'HC',

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
                                                         value = 0.003, step = 0.001, min = 0, max = 0.01),
                                             sliderInput(inputId = 's_HC', ticks = FALSE,
                                                         label = 'SKEW',
                                                         value = -0.15, step = 0.01, min = -1, max = 1),
                                             sliderInput(inputId = 'p_HC', ticks = FALSE,
                                                         label = 'POSITION',
                                                         value = 250, step = 10, min = 120, max = 700),
                                             sliderInput(inputId = 'w_HC', ticks = FALSE,
                                                         label = 'WIDTH',
                                                         value = 50, step = 10, min = 0, max = 250)
                                             ),

                                    tabPanel(title = 'CL',

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
                                                         value = 0.005, step = 0.001, min = 0, max = 0.01),
                                             sliderInput(inputId = 's_CL', ticks = FALSE,
                                                         label = 'SKEW',
                                                         value = -0.15, step = 0.01, min = -1, max = 1),
                                             sliderInput(inputId = 'p_CL', ticks = FALSE,
                                                         label = 'POSITION',
                                                         value = 300, step = 10, min = 120, max = 700),
                                             sliderInput(inputId = 'w_CL', ticks = FALSE,
                                                         label = 'WIDTH',
                                                         value = 30, step = 10, min = 0, max = 250)
                                             ),

                                    tabPanel(title = 'LG',

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
                                                         value = 0.001, step = 0.001, min = 0, max = 0.01),
                                             sliderInput(inputId = 's_LG', ticks = FALSE,
                                                         label = 'SKEW',
                                                         value = -0.15, step = 0.01, min = -1, max = 1),
                                             sliderInput(inputId = 'p_LG', ticks = FALSE,
                                                         label = 'POSITION',
                                                         value = 330, step = 10, min = 120, max = 700),
                                             sliderInput(inputId = 'w_LG', ticks = FALSE,
                                                         label = 'WIDTH',
                                                         value = 250, step = 10, min = 0, max = 300)
                                             )
                                             )
                                             )
                                             )
                                             ),

  server <- function (input, output) {

    x <- seq(120, 700, length.out = 500)

    y_HC <- reactive({
      deconvolve::fs_function(x, input$h_HC, input$s_HC, input$p_HC, input$w_HC)
    })

    y_CL <- reactive({
      deconvolve::fs_function(x, input$h_CL, input$s_CL, input$p_CL, input$w_CL)
    })

    y_LG <- reactive({
      deconvolve::fs_function(x, input$h_LG, input$s_LG, input$p_LG, input$w_LG)
    })

    y_all <- reactive({
      deconvolve::fs_mixture(x, input$h_HC, input$s_HC, input$p_HC, input$w_HC,
                             input$h_CL, input$s_CL, input$p_CL, input$w_CL,
                             input$h_LG, input$s_LG, input$p_LG, input$w_LG)
    })

    output$fsplot <- renderPlot({
      x <- seq(120, 700, length.out = 500)

      plot(x, y_all(), type = 'l', col = 'black',
           xlab = 'Temperature (C)', ylab = 'DTG (dm/dT) (C-1)',
           yaxs = 'i', ylim = c(0, 0.01),
           pch = 20, cex = 0.3, cex.axis = 1.2)

      lines(x, y_HC(), col = 'blue')
      lines(x, y_CL(), col = 'green')
      lines(x, y_LG(), col = 'orange')

      legend(120, 0.0088,
             yjust = 0,
             legend = c('Total DTG', 'HC', 'CL', 'LG'),
             ncol = 4,
             cex = 1,
             bty = 'n',
             col = c('black', 'blue', 'green', 'orange'),
             lwd = 2)

    }, height = 400)

  },

  options = list(height = 500)

)
