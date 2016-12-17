library(shiny)
library(ggplot2)
library(raptormodules)
shinyServer(
  function(input, output) {
    # All operations must happen inside this function

    createData <- reactive({
      x <- runif(input$numberOfPoints, -10, 50)
      y <- input$intercept + input$coefb * x + rnorm(input$numberOfPoints, 0, 4)
      return(data.frame(x = x, y = y))
    })

    createPlot <- reactive({
      g <- ggplot(createData(), aes(x = x, y = y)) + geom_point()
      if (input$regLine) {
        g <- g + geom_smooth(method = "lm", formula = "y ~ x")
      }
      return(g)
    })

    output$scatterPlot <- renderPlot({createPlot()})

    createReportData <- reactive({
      d <- list()
      for (i in 1:input$repeats) {
        d[[letters[i]]] <- list(data = createData() + rnorm(input$numberOfPoints * 2),
                       n = input$numberOfPoints,
                       a = input$intercept,
                       b = input$coefb)
      }
      return(d)
    })

    output$regressionReport <- callModule(downloadReportButton,
                                          "regressionReport",
                                          reportTemplateMaster = "report-head.Rmd",
                                          reportTemplateImport = "report-body.Rmd",
                                          reportData = createReportData())
  }
)
