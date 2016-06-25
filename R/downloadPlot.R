#' Creates the user interface for a download button to download a ggplot
#'
#' @param id A unique id name for this Shiny object
#' @param initialFileName The default name that will be used for the Filename
#'                        of the downloaded file.
#' @return A set of options for downloading the ggplot, including filename,
#'         file format and the all-important download button
#' @export
downloadGGPlotButtonUI <- function(id, intialFileName) {
  # create namespace using supplied id
  ns <- NS(id)

  textInput(ns("filename"), label = "Filename:", value = initialFileName)
  selectInput(ns("format"),
    label = "Select filetype",
    choices = list(
      PDF = "pdf",
      PS = "postscript",
      PNG = "png",
      Bitmap = "bmp",
      JPEG = "jpeg"
    ),
    selected = ".pdf"
  )
  downloadButton(ns("download"), "Save picture")
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
                                 height, width) {
  # Determine what the file extension should be
  fileExtension <- reactive({
    return(switch(input$format,
      "postscript" = ".ps",
      paste0(".", input$format) # Default
    ))
  })

  # pre-construct the list of arguments to be passed to the plotting function
  arg <- reactive({
    a <- list(file = paste0(input$filename, fileExtension()))
    if (!empty(height)) {
      arg$height <- height
    }
    if (!empty(width)) {
      arg$width <- width
    }
    return(a)
  })

  return(
    renderText({
      # Open connection for plot to be created; make the plot; close the device
      do.call(input$format, args = arg())
      print(ggplotObject)
      dev.off()
      return("Plot saved!")
    })
  )
}
