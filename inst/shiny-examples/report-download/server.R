library(shiny)
library(ggplot2)
library(shinyDownload)
library(rmarkdown)
shinyServer(
  function(input, output) {
    # All operations must happen inside this function

    createData <- reactive({
      x <- runif(input$numberOfPoints, -10, 50)
      y <- input$intercept + input$coefa * x + rnorm(input$numberOfPoints, 0, 4)
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

    # Package all the data you want to use in the report into a single list.
    # You can do this using a function with isolated content, or as a reactive.
    reportData <- function(dataset, replicates) {
      isolate({
        original <- list(original = list(data = dataset,
                                         n = input$numberOfPoints,
                                         a = input$coefa,
                                         b = input$intercept,
                                         plot = createPlot()))
        d <- lapply(
          X = seq(length.out = replicates),
          function(i, n, a, b, d) {
            return(list(
              data = d + rnorm(input$numberOfPoints * 2),
              n = n,
              a = a,
              b = b
            ))
          },
          n = input$numberOfPoints,
          a = input$coefa,
          b = input$intercept,
          d = dataset
        )
        names(d) <- letters[seq(length.out = replicates)]
      })
      cat(str(d))
      return(append(original, d))
    }

    # The download manager is packaged into a Shiny module called
    # "downloadReportButton".
    output$regressionReport <- callModule(
      module = downloadReportButton,
      id = "regressionReport", # <= This should match the outputId name
      reportTemplateMaster = "report-head.Rmd",
      reportTemplateImport = "report-body.Rmd",
      params = reportData(createData(), input$repeats),
      toc = "float"
    )
  }
)
