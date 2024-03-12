library(shiny)
library(shinyDownload)
library(ggplot2)

function(input, output) {

  createData <- reactive({
    x <- runif(input$numberOfPoints, -10, 50)
    y <- input$intercept + input$coefa * x + rnorm(input$numberOfPoints, 0, 4)
    return(data.frame(x = x, y = y))
  })

  # Return plot from a reactive expression.
  scatterPlot <- reactive({
    g <- ggplot(createData(), aes(x = x, y = y)) + geom_point()
    if (input$regLine) {
      g <- g + geom_smooth(method = "lm", formula = "y ~ x")
    }
    return(g)
  })

  # Display the reactive value of the scatter plot (optional)
  output$scatterPlot <- renderPlot({
    scatterPlot()
  })
  
  # Download module. Note the scatterPlot reactive expression is passed
  # directly into the module.
  downloadGGPlotButtonServer(id = "plotDownload", ggplotObject = scatterPlot)
}
