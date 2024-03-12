library(shiny)
library(shinyDownload)

fluidPage(
  titlePanel("shinyDownload using a bootstrap panel"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(p("Pick how many data points to generate, and the parameters
                    for the underlying model, $$y = ax + b$$")),
      numericInput("numberOfPoints",
                   "Number of data points:",
                   min = 10, value = 30),
      sliderInput("coefa",
                  "a (the coefficient of x):",
                  min = -10, max = 10, value = 0, step = 0.1),
      sliderInput("intercept",
                  "b (intercept):",
                  min = -100, max = 100, value = 0),
      checkboxInput(inputId = "regLine",
                    label = "Include regression line",
                    value = TRUE)
    ),

    mainPanel(
      div(
        class = "panel panel-primary",
        div(
          class = "panel-heading",
          h2("Plot in a panel", class = "panel-title")
        ),
        div(
          class = "panel-body",
          plotOutput("scatterPlot")
        ),
        div(
          class = "panel-footer",
          downloadGGPlotButtonUI("plotDownload", "this-plot")
        )
      ), # End of first (Bootstrap) Panel

      div(
        class = "panel panel-info",
        div(
          class = "panel-heading",
          h2("About Bootstrap panels", class = "panel-title")
        ),
        div(
          class = "panel-body",
          p("Bootstrap panels are a component that lets you put content in a
            box. You can optionally add a heading and/or footer to the box,
            and provide the box with one of the Bootstrap context classes."),
          p("In cases where you have multiple plots or tables which you wish
            to offer shinyDownload widgets for, consider placing your
            content in the panel-body, and the download widget in the
            panel-footer. This groups related items together."),
          p("Visit the",
            tags$a(
              "Bootstrap 3.3 docs",
              href = "https://getbootstrap.com/docs/3.3/components/#panels",
              target = "_blank"
            ),
            "for more information on Bootstrap panels."),
          p("Be careful not to confuse Bootstrap Panels with panels in Shiny,
            which refers to which side content is to be displayed in the UI!")
        )
      ) # End of second (Bootstrap) Panel
    ) # End of (Shiny) mainPanel
  )
)
