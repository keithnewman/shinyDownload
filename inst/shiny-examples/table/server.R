library(shiny)
library(shinyDownload)
shinyServer(
  function(input, output) {

    createTable <- reactive({
      switch(input$dataChoice, "random" = rnorm(1000), get(input$dataChoice))
    })

    output$table <- renderTable(createTable())

    # The download manager is packaged into a Shiny module called
    # "downloadTableButton".
    observeEvent(input$dataChoice, {
      output$tableDownload <- callModule(
        module = downloadTableButton,
        id = "tableDownload", # <= this should match the outputId name
        dataFrameObject = createTable())
    })
  }
)
