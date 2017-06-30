library(shiny)
library(shinyDownload)
shinyUI(
  # fluidPage() means content will automatically get narrower
  # if the browser window gets narrower
  fluidPage(
    # Title to my page (also appears in title of browser window)
    titlePanel("Table Download"),

    # Sidebar layout has a grey panel on the left and
    # a larger space on the right.
    sidebarLayout(
      # sidebarPanel() is the (grey) panel on the left
      sidebarPanel(
        selectInput("dataChoice",
                    "Select a dataset",
                    c("anscombe", "CO2", "faithful", "mtcars", "Nile", "random")
        )
      ),

      # mainPanel is the larger plain space on the right
      mainPanel(
        h2("Table"),
        tableOutput("table"),
        downloadTableButtonUI("tableDownload", "this-table")#,
        #textOutput("downloadConfirmation")
      )
    )
  )
)
