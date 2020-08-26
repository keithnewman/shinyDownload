#' @title Download Report Button (UI)
#' @description Creates the user interface for a download button to download a
#'              report
#' @name downloadReportButtonUI
#' @param id A unique id name for this Shiny object
#' @param initialFileName The default name that will be used for the Filename
#'                        of the downloaded file.
#' @param placeholder Placeholder text for the filename input
#' @param buttonLabel Text to appear on the download link button
#' @return A set of options for downloading the report, including filename,
#'         file format and the all-important download button
#' @export
downloadReportButtonUI <- function(id, initialFileName,
                                   placeholder = "Select filename...",
                                   buttonLabel = "Download plot") {
  # create namespace using supplied id
  ns <- NS(id)

  restoredValue <- restoreInput(id = ns("filename"), default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
      warning("Restored value for ", ns("filename"), " has incorrect format.")
      restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
      restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }

  div(class = "form-inline",
    div(
      class = "form-group",
      tags$label(class = "sr-only", `for` = ns("filename"), "Filename"),
      tags$input(type = "text",
                 id = ns("filename"),
                 name = ns("filename"),
                 class = "form-control",
                 placeholder = placeholder,
                 value = initialFileName,
                 `data-restore` = restoredValue,
                 `aria-label` = "Filename")
    ),
    div(
      class = "form-group",
      tags$label(class = "sr-only", `for` = ns("format"), "File format"),
      tags$select(
        id = ns("format"),
        name = ns("format"),
        class = "form-control",
        shiny:::selectOptions(list(
          `.pdf` = "pdf",
          `.html` = "HTML",
          `.docx` = "docx",
          `.tex` = "tex",
          `.rtf` = "rtf",
          `.odt` = "odt",
          `.md` = "md"
        ), "pdf"),
        `aria-label` = "File format"
      )
    ),
    div(
      class = "form-group",
      tags$label(class = "sr-only", `for` = ns("download"), "Download plot"),
      tags$a(id = ns("download"),
             class = paste("btn btn-default shiny-download-link"),
             href = "",
             target = "_blank",
             download = NA,
             `aria-label` = "Download plot",
             icon("download"), buttonLabel)
    )
  )
}

#' @title Download Report Button (server)
#' @description Processes and initiates the download of a report.
#' @name downloadReportButton
#' @param input Needed for Shiny
#' @param output Needed for Shiny
#' @param session Needed for Shiny
#' @param reportTemplateMaster The master rmarkdown template for the report.
#'        This template should not have a YAML header as this will be added
#'        by this report generation script.
#' @param reportTemplateImport A character vector of additional files that may
#'        be referred to by reportTemplateMaster. These will be copied into the
#'        same directory.
#' @param params A list of data/parameters that may be referred to by
#'        the report templates.
#' @param ... Arguments to pass to the yaml::as.yaml method, which may be
#'        necessary if params is a data frame and you want to alter the
#'        "column.major" option, overriding the "precision" of floating point
#'        values, and determining whether to allow escaped "unicode" characters.
#' @return Downloads the report with the inputted filename
#'
#' @export
downloadReportButton <- function(input, output, session,
                                 title = "Report",
                                 author = "shinyDownload",
                                 date = format(Sys.time(), "%d %B %Y"),
                                 reportTemplateMaster,
                                 reportTemplateImport = NULL,
                                 params = NULL,
                                 ...) {
  # Determine what the file extension should be
  fileExtension <- reactive({
    return(paste0(".", tolower(input$format)))
  })

  # Determine the application mime type so file formats are recognised
  # mimeType <- reactive({
  #   return(
  #     switch(
  #       input$format,
  #       docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  #       html = "text/html",
  #       tex = "application/x-latex",
  #       odt = "application/vnd.oasis.opendocument.text",
  #       paste0("application/", input$format) # default for all other formats
  #     )
  #   )
  # })

  formatName <- reactive ({
    return(switch(input$format,
                  docx = "word_document",
                  paste0(tolower(input$format), "_document")))
  })

  output$download <- downloadHandler(
    filename = function() return(paste0(input$filename, fileExtension())),
    content = function(file_) {

      # NOTE: Do not delete the tmpDir object when you are done! This belongs
      # to the entire R session, and deleting it causes other things to break!
      tmpDir <- tempdir()
      # Create a temporary file for all this to go in
      tmpReport <- tempfile("report", tmpDir, fileext = ".Rmd")
      file.create(tmpReport)

      # Create the YAML header
      yamlHead <- list(`title` = title,
                       `author` = author,
                       `date` = date)
      # if (!is.null(params)) {
      #   yamlHead <- append(yamlHead, `params` = params)
      # }

      # Write YAML to the start of the main document
      con <- file(tmpReport)
      writeLines(c("---", yaml::as.yaml(x = yamlHead, ...), "---"), con)
      close(con)

      # Append the Master template
      file.append(tmpReport, reportTemplateMaster)

      # Copy any dependancies to the temporary directory
      if (!is.null(reportTemplateImport)) {
        file.copy(reportTemplateImport,
                  file.path(tmpDir, reportTemplateImport),
                  overwrite = TRUE)
      }

      # Render the completed document
      rmarkdown::render(input = tmpReport,
                        output_format = formatName(),
                        output_file = file_,
                        params = reportData)
    }#,
    #contentType = mimeType()
  )
}
