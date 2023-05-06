#' @title Download Table Button (UI)
#' @description Creates the user interface for a download button to download a
#'              data.frame object
#' @name downloadTableButtonUI
#' @param id A unique id name for this Shiny object
#' @param initialFileName The default name that will be used for the Filename
#'                        of the downloaded file.
#' @param placeholder Placeholder text for the filename input. This adds the
#'                    placeholder attribute to the HTML input tag.
#' @param buttonLabel Text to appear on the download link button
#' @return A set of options for downloading the table, including filename,
#'         file format and the all-important download button
#' @export
downloadTableButtonUI <- function(id, initialFileName,
                                  placeholder = "Select filename...",
                                  buttonLabel = "Download table") {
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
        shiny:::selectOptions(list(
          `.csv` = "csv",
          `.csv (for excel)` = "excel_csv",
          `.txt (space-delimited)` = "delim",
          `.txt (tab-delimited)` = "tsv",
          `.rds (for R)` = "rds"
        ), "csv"),
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
#' @description Processes and initiates the download of a table, matrix or data
#'              frame.
#' @name downloadTableButton
#' @param input Needed for Shiny.
#' @param output Needed for Shiny.
#' @param session Needed for Shiny.
#' @param dataFrameObject The name of the table, matrix or data frame object
#'        that is to be exported.
#' @return Downloads the table with the inputted filename
#' @export
downloadTableButton <- function(input, output, session, dataFrameObject) {
  # Determine what the file extension should be
  fileExtension <- shiny::reactive({
    return(switch(input$format,
                  "delim" = , "tsv" = ".txt",
                  "excel_csv" = ".csv",
                  paste0(".", input$format)))
  })

  output$download <- shiny::downloadHandler(
    filename = function() return(paste0(input$filename, fileExtension())),
    content = function(file) {
      # Compile a list of arguments to pass to do.call
      a <- list()
      if (input$format == "rds") {
        a$`object` <- dataFrameObject
      } else {
        a$`x` <- dataFrameObject
      }
      a$`file` <- file

      # Decide how the table will be written
      func <- switch(input$format,
                     "csv" = "write.csv",
                     "rds" = "saveRDS",
                     "write.table")
      do.call(func, a)
    }
  )
}
