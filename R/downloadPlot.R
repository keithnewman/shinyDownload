#' Creates the user interface for a download button to download a ggplot
#'
#' @param id A unique id name for this Shiny object
#' @param initialFileName The default name that will be used for the Filename
#'                        of the downloaded file.
#' @return A set of options for downloading the ggplot, including filename,
#'         file format and the all-important download button
#' @export
downloadGGPlotButtonUI <- function(id, initialFileName) {
  # create namespace using supplied id
  ns <- NS(id)

  return(
    tagList(
      fluidRow(
        column(4,
          textInput(ns("filename"), label = "Filename:", value = initialFileName)
        ),
        column(3,
          selectInput(ns("format"),
            label = "Select filetype",
            choices = list(
              `.pdf` = "pdf",
              `.ps` = "postscript",
              `.png` = "png",
              `.bmp` = "bmp",
              `.jpeg` = "jpeg"
            ),
            selected = ".pdf",
            selectize = FALSE,
            width = "100px"
          )
        ),
        column(3,
          # TODO: find neater way of filling this space. Don't want this text on
          # the next line but it's needed to drop the download button down a bit
          p("Click to download plot:"),
          downloadButton(ns("download"), "Save plot")
        )
      )
    )
  )
}

#' Processes and initiates the download of a ggplot object.
#'
#' @param input Needed for Shiny
#' @param output Needed for Shiny
#' @param session Needed for Shiny
#' @param ggplotObject The name of the ggplot object that is to be plotted
#' @param height Height of the plot to be outputted
#' @param width Width of the plot to be outputted
#' @return Downloads the ggplot with the inputted filename
#'
#' @export
downloadGGPlotButton <- function(input, output, session, ggplotObject,
                                 height = NULL, width = NULL) {
  # Determine what the file extension should be
  fileExtension <- reactive({
    return(switch(input$format,
      "postscript" = ".ps",
      paste0(".", input$format) # Default
    ))
  })

  # Determine the application mime type so file formats are recognised
  mimeType <- reactive({
    return(
      switch(input$format,
        pdf = "application/pdf",
        postscript = "application/ps",
        paste0("image/", input$format) # default for all other formats
      )
    )
  })

  output$download <- downloadHandler(
    filename = function() return(paste0(input$filename, fileExtension())),
    content = function(file) {
      # Compile a list of arguments to pass to do.call
      a <- list()
      a$`file` = file
      if (!is.null(height)) {
        a$height <- height
      }
      if (!is.null(width)) {
        a$width <- width
      }

      # Make the plot
      openDevices = Inf
      do.call(input$format, args = a)
      #pdf(file)
      print(ggplotObject)
      while (openDevices > 1) {
        openDevices = dev.off()
      }
    }#,
    #contentType = mimeType()
  )
}
