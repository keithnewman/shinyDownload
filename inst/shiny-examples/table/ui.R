library(shiny)
library(shinyDownload)

fluidPage(
  titlePanel("Table Download"),

  sidebarLayout(
    # sidebarPanel() is the (grey) panel on the left
    sidebarPanel(
      selectInput(
        "dataChoice",
        "Select a dataset",
        c("anscombe", "CO2", "faithful", "mtcars", "Nile", "random")
      )
    ),

    # mainPanel is the larger plain space on the right
    mainPanel(
      h2("Table"),
      tableOutput("table"),
      downloadTableButtonUI("tableDownload", "this-table")
    )
  )
)
