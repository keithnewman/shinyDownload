#' @title Download Plot Button (UI)
#' @description Creates the user interface for a download button to download a
#'              ggplot
#' @name downloadGGPlotButtonUI
#' @param id A unique id name for this Shiny object
#' @param initialFileName The default name that will be used for the filename
#'                        of the downloaded file.
#' @param placeholder Placeholder text for the filename input. This adds the
#'                    placeholder attribute to the HTML input tag.
#' @param buttonLabel Text to appear on the download link button.
#' @return An inline form for downloading the ggplot, including filename,
#'         file format and the all-important download button.
#' @export
downloadGGPlotButtonUI <- function(id, initialFileName = "",
                                   placeholder = "Select filename...",
                                   buttonLabel = "Download plot") {
  # create namespace using supplied id
  ns <- shiny::NS(id)

  restoredValue <- shiny::restoreInput(id = ns("filename"), default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", ns("filename"), " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- jsonlite::toJSON(restoredValue, strict_atomic = FALSE)
  }

  shiny::div(class = "form-inline",
    shiny::div(
      class = "form-group",
      shiny::tags$label(class = "sr-only", `for` = ns("filename"), "Filename"),
      shiny::tags$input(type = "text",
                        id = ns("filename"),
                        name = ns("filename"),
                        class = "form-control",
                        placeholder = placeholder,
                        value = initialFileName,
                        `data-restore` = restoredValue,
                        `aria-label` = "Filename")
    ),
    shiny::div(
      class = "form-group",
      shiny::tags$label(class = "sr-only", `for` = ns("format"), "File format"),
      shiny::tags$select(
        id = ns("format"),
        name = ns("format"),
        class = "form-control",
        shiny::tags$option(".pdf", value = "pdf", selected = "selected"),
        shiny::tags$option(".ps", value = "postscript"),
        shiny::tags$option(".png", value = "png"),
        shiny::tags$option(".bmp", value = "bmp"),
        shiny::tags$option(".jpeg", value = "jpeg"),
        `aria-label` = "File format"
      )
    ),
    shiny::div(
      class = "form-group",
      shiny::tags$label(class = "sr-only",
                        `for` = ns("download"),
                        buttonLabel),
      shiny::tags$a(id = ns("download"),
                    class = paste("btn btn-default shiny-download-link"),
                    href = "",
                    target = "_blank",
                    download = NA,
                    `aria-label` = buttonLabel,
                    shiny::icon("download"), buttonLabel)
    )
  )
}

#' @title Download Plot Button (server)
#' @description Processes and initiates the download of a ggplot object.
#' @name downloadGGPlotButton
#' @param input Needed for Shiny
#' @param output Needed for Shiny
#' @param session Needed for Shiny
#' @param ggplotObject The ggplot object to be outputted for download.
#' @param height Height of the plot to be outputted
#' @param width Width of the plot to be outputted
#' @return Downloads the ggplot with the inputted filename
#'
#' @export
downloadGGPlotButton <- function(input, output, session, ggplotObject,
                                 height = NULL, width = NULL) {
  # Determine what the file extension should be
  fileExtension <- shiny::reactive({
    return(switch(input$format,
      "postscript" = ".ps",
      paste0(".", input$format) # Default
    ))
  })

  # Determine the application mime type so file formats are recognised
  mimeType <- shiny::reactive({
    return(
      switch(input$format,
        pdf = "application/pdf",
        postscript = "application/ps",
        paste0("image/", input$format) # default for all other formats
      )
    )
  })

  output$download <- shiny::downloadHandler(
    filename = function() return(paste0(input$filename, fileExtension())),
    content = function(file) {
      # Compile a list of arguments to pass to do.call
      a <- list()
      a$`file` <- file
      if (!is.null(height)) {
        a$height <- height
      }
      if (!is.null(width)) {
        a$width <- width
      }

      # Make the plot
      openDevices <- Inf
      do.call(input$format, args = a)
      print(ggplotObject)
      while (openDevices > 1) {
        openDevices <- grDevices::dev.off()
      }
    }
  )
}
