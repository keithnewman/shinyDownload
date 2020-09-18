library(shiny)
library(shinyDownload)
shinyUI(
  fluidPage(
    titlePanel("Report Download"),
    sidebarLayout(
      sidebarPanel(
        withMathJax(p("Pick how many data points to generate, and the parameters
                       for the underlying model, $$y = ax + b$$")),
        sliderInput("numberOfModels",
                    "Number of models to feature",
                    min = 1, max = 5, value = 3),
        numericInput("numberOfPoints",
                     "Number of data points:",
                     min = 10, value = 30
        ),
        sliderInput("coefa",
                    "a (the coefficient of x):",
                    min = -10, max = 10, value = 0, step = 0.1
        ),
        sliderInput("intercept",
                    "b (intercept):",
                    min = -100, max = 100, value = 0
        ),
        checkboxInput("regLine", "Include regression line", TRUE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Model 1",
            h2("Model 1 plot"),
            plotOutput("scatterPlot1")
          ),
          tabPanel(
            "Model 2",
            h2("Model 2 plot"),
            plotOutput("scatterPlot2")
          ),
          tabPanel(
            "Model 3",
            h2("Model 3 plot"),
            plotOutput("scatterPlot3")
          ),
          tabPanel(
            "Model 4",
            h2("Model 4 plot"),
            plotOutput("scatterPlot4")
          ),
          tabPanel(
            "Model 5",
            h2("Model 5 plot"),
            plotOutput("scatterPlot5")
          )
        ),
        wellPanel(
          h2("Plot"),
          downloadReportButtonUI("regressionReport", "this-regression-report")
        )
      )
    )
  )
)
