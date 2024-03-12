library(shiny)
library(shinyDownload)

function(input, output) {

  createTable <- reactive({
    switch(input$dataChoice, "random" = rnorm(1000), get(input$dataChoice))
  })

  output$table <- renderTable(createTable())

  # Download module. Note the createTable reactive expression is passed
  # directly into the module, and not the value (as createTable()).
  downloadTableButtonServer(id = "tableDownload", dataFrameObject = createTable)

}
