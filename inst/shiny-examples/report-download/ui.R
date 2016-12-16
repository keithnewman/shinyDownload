library(shiny)
library(raptormodules)
shinyUI(
  # fluidPage() means content will automatically get narrower
  # if the browser window gets narrower
  fluidPage(
    # Title to my page (also appears in title of browser window)
    titlePanel("Plot Download"),

    # Sidebar layout has a grey panel on the left and
    # a larger space on the right.
    sidebarLayout(
      # sidebarPanel() is the (grey) panel on the left
      sidebarPanel(
        withMathJax(p("Pick how many data points to generate, and the parameters for the underlying model, $$y = ax + b$$")),
        numericInput("numberOfPoints",
                     "How many data points:",
                     min = 10, value = 30
        ),
        sliderInput("intercept",
                    "Pick the intercept, a:",
                    min = -100, max = 100, value = 0
        ),
        sliderInput("coefb",
                    "Pick b, the coefficient of x:",
                    min = -10, max = 10, value = 0, step = 0.5
        ),
        checkboxInput("regLine", "Include regression line", TRUE)
      ),

      # mainPanel is the larger plain space on the right
      mainPanel(
        h2("Plot"),
        plotOutput("scatterPlot"),
        sliderInput("repeats",
                    "How many replications should appear in the report",
                    min = 1, max = 10, value = 1
        ),
        downloadReportButtonUI("regressionReport", "this-regression-report")#,
        #textOutput("downloadConfirmation")
      )
    )
  )
)
