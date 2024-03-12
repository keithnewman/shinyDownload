library(shiny)
library(ggplot2)
library(shinyDownload)
library(rmarkdown)

function(input, output) {

  allData <- reactive({
    d <- lapply(
      seq(input$numberOfModels),
      function(i) {
        x <- runif(input$numberOfPoints, -10, 50)
        y <- input$intercept + input$coefa * x +
          rnorm(input$numberOfPoints, 0, 4)
        return(data.frame(x = x, y = y))
      }
    )
    names(d) <- letters[seq(length.out = input$numberOfModels)]

    return(d)
  })

  allPlots <- reactive({
    p <- lapply(
      allData(),
      function(d) {
        g <- ggplot(d, aes(x = x, y = y)) + geom_point()
        if (input$regLine) {
          g <- g + geom_smooth(method = "lm", formula = "y ~ x")
        }
        return(g)
      }
    )
    return(p)
  })

  output$scatterPlot1 <- renderPlot(allPlots()[["a"]])
  output$scatterPlot2 <- renderPlot(allPlots()[["b"]])
  output$scatterPlot3 <- renderPlot(allPlots()[["c"]])
  output$scatterPlot4 <- renderPlot(allPlots()[["d"]])
  output$scatterPlot5 <- renderPlot(allPlots()[["e"]])
  
  # Download module. Note the allData reactive expression is passed
  # directly into the module, and not the value (as allData()).
  downloadReportButtonServer(
    id = "regressionReport",
    reportTemplateMaster = "report-head.Rmd",
    reportTemplateImport = "report-body.Rmd",
    params = list(n = input$numberOfPoints,
                  a = input$coefa,
                  intercept = input$intercept,
                  dataset = allData),
    toc = "float"
  )

}
