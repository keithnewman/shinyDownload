#' @title Download Plot Button (UI)
#' @description Creates the user interface for a download button to download a
#'   ggplot
#' @name downloadGGPlotButtonUI
#' @param id A unique id name for this Shiny object. Should match up with 1
#'   instance of [downloadGGPlotButtonServer()].
#' @param initialFileName The default name that will be used for the filename of
#'   the downloaded file.
#' @param placeholder Placeholder text for the filename input. This adds the
#'   placeholder attribute to the HTML input tag.
#' @param buttonLabel Text to appear on the download link button.
#' @return An inline form for downloading the ggplot, including filename, file
#'   format and the all-important download button.
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


#' @name downloadGGPlotButtonServer
#' @title Download Plot Button (server)
#' @description Processes and initiates the download of a ggplot object.
#' @param id Character value that uniquely identifies this instance of the
#'   module. Should match up with the `id` value for one
#'   [downloadGGPlotButtonUI()].
#' @param ggplotObject Reactive expression containing the ggplot object to be
#'   outputted for download.
#' @param height,width Height and width of the plot to be outputted
#' @return Downloads the ggplot with the inputted filename
#'
#' @export
downloadGGPlotButtonServer <- function(id,
                                       ggplotObject,
                                       height = NULL,
                                       width = NULL) {
  if (!shiny::is.reactive(ggplotObject)) {
    cli::cli_warn(
      message = c(
        "!" = "{.var ggplotObject} is not a reactive expression.
        The downloaded plot might not update after its initial creation.",
        "i" = "Have you provided the reactive value rather than the
        reactive expression ?"
      )
    )
  }
  
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # Determine what the file extension should be
      fileExtension <- shiny::reactive({
        return(
          switch(input$format,
                 "postscript" = ".ps",
                 paste0(".", input$format)) # Default
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
          print(ggplotObject())
          while (openDevices > 1) {
            openDevices <- grDevices::dev.off()
          }
        }
      )
    }
  )
}

#' @title Download Plot Button (server)
#' @description `r lifecycle::badge("deprecated")` Please use
#'   [downloadGGPlotButtonServer()] instead.
#'
#'   Processes and initiates the download of a ggplot object.
#' @name downloadGGPlotButton
#' @param input,output,session Needed for Shiny
#' @inheritParams downloadGGPlotButtonServer
#' @return Downloads the ggplot with the inputted filename
#'
#' @export
downloadGGPlotButton <- function(input, output, session, ggplotObject,
                                 height = NULL, width = NULL) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "shinyDownload::downloadGGPlotButton()",
    with = "shinyDownload::downloadGGPlotButtonServer()"
  )
  
  # Determine what the file extension should be
  fileExtension <- shiny::reactive({
    return(switch(input$format,
      "postscript" = ".ps",
      paste0(".", input$format) # Default
    ))
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
