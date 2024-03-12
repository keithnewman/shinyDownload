library(shiny)
library(shinyDownload)
library(ggplot2)

function(input, output) {

  createData <- reactive({
    x <- runif(input$numberOfPoints, -10, 50)
    y <- input$intercept + input$coefa * x + rnorm(input$numberOfPoints, 0, 4)
    return(data.frame(x = x, y = y))
  })

  output$scatterPlot <- renderPlot({
    g <- ggplot(createData(), aes(x = x, y = y)) + geom_point()
    if (input$regLine) {
      g <- g + geom_smooth(method = "lm", formula = "y ~ x")
    }

    # The download manager is packaged into a Shiny module called
    # "downloadGGPlotButton". You'll need to call this module every time
    # the target plot is updated, so the module will have the latest plot
    output$plotDownload <- callModule(
      module = downloadGGPlotButton,
      id = "plotDownload", # <= this should match the outputId name
      ggplotObject = g
    )

    return(g)
  })
}
