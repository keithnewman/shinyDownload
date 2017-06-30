library(shiny)
library(shinyDownload)
shinyServer(
  function(input, output) {

    createTable <- reactive({
      switch(input$dataChoice, "random" = rnorm(1000), get(input$dataChoice))
    })

    output$table <- renderTable({createTable()})

    observeEvent(input$dataChoice, {
      output$tableDownload <- callModule(downloadTableButton,
                                         "tableDownload",
                                         dataFrameObject = createTable())
    })
  }
)
