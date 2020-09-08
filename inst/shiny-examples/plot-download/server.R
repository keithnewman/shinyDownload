library(shiny)
library(shinyDownload)
library(ggplot2)
shinyServer(
  function(input, output) {

    createData <- reactive({
      x <- runif(input$numberOfPoints, -10, 50)
      y <- input$intercept + input$coefa * x + rnorm(input$numberOfPoints, 0, 4)
      return(data.frame(x = x, y = y))
    })

    # Creating the plot in its own reactive environment allows it to be used
    # for both output and to be passed to the plot download module.
    createPlot <- reactive({
      g <- ggplot(createData(), aes(x = x, y = y)) + geom_point()
      if (input$regLine) {
        g <- g + geom_smooth(method = "lm", formula = "y ~ x")
      }
      return(g)
    })

    output$scatterPlot <- renderPlot({createPlot()})

    # The download manager is packaged into a Shiny module called
    # "downloadGGPlotButton".
    output$plotDownload <- callModule(
      module = downloadGGPlotButton,
      id = "plotDownload", # <= this should match the outputId name
      ggplotObject = createPlot())

  }
)
