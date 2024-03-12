library(shiny)
library(shinyDownload)
fluidPage(
  titlePanel("Plot Download"),

  sidebarLayout(
    # sidebarPanel() is the (grey) panel on the left
    sidebarPanel(
      withMathJax(p("Pick how many data points to generate, and the
                    parameters for the underlying model, $$y = ax + b$$")),
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
      checkboxInput(inputId = "regLine",
                    label = "Include regression line",
                    value = TRUE)
    ),

    # mainPanel is the larger plain space on the right
    mainPanel(
      h2("Plot"),
      plotOutput("scatterPlot"),
      downloadGGPlotButtonUI("plotDownload", "this-plot")
    )
  )
)
