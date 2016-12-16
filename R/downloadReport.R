#' @title Download Report Button (UI)
#' @description Creates the user interface for a download button to download a report
#' @name downloadReportButtonUI
#' @param id A unique id name for this Shiny object
#' @param initialFileName The default name that will be used for the Filename
#'                        of the downloaded file.
#' @return A set of options for downloading the report, including filename,
#'         file format and the all-important download button
#' @export
downloadReportButtonUI <- function(id, initialFileName) {
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
                             `.html` = "HTML",
                             `.docx` = "docx",
                             `.tex` = "tex",
                             `.rtf` = "rtf",
                             `.odt` = "odt",
                             `.md` = "md"
                           ),
                           selected = "pdf",
                           selectize = FALSE,
                           width = "100px"
               )
        ),
        column(3,
               # TODO: find neater way of filling this space. Don't want this text on
               # the next line but it's needed to drop the download button down a bit
               p("Click to download report:"),
               downloadButton(ns("download"), "Download Report")
        )
      )
    )
  )
}

#' @title Download Report Button (server)
#' @description Processes and initiates the download of a report.
#' @name downloadReportButton
#' @param input Needed for Shiny
#' @param output Needed for Shiny
#' @param session Needed for Shiny
#' @param report The rmarkdown code for the report
#' @return Downloads the report with the inputted filename
#'
#' @export
downloadReportButton <- function(input, output, session, title = "Report",
                                 author = "Raptor", date = format(Sys.time(), "%d %B %Y"),
                                 reportTemplateMaster, reportTemplateImport = NULL, reportData) {
  # Determine what the file extension should be
  fileExtension <- reactive({
    return(paste0(".", tolower(input$format)))
  })

  # Determine the application mime type so file formats are recognised
  mimeType <- reactive({
    return(
      switch(input$format,
             docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
             html = "text/html",
             tex = "application/x-latex",
             odt = "application/vnd.oasis.opendocument.text",
             paste0("application/", input$format) # default for all other formats
      )
    )
  })

  formatName <- reactive ({
    return(switch(input$format,
                  docx = "word_document",
                  paste0(tolower(input$format), "_document")))
  })

  output$download <- downloadHandler(
    filename = function() return(paste0(input$filename, fileExtension())),
    content = function(file) {

      # Create a temporary file for all this to go in
      tmpDir <- tempdir()
      tmpReport <- tempfile("report", tmpDir, fileext = ".Rmd")
      file.create(tmpReport)

      # Write YAML to the start of the main document
      con <- file(tmpReport)
      writeLines(c("---",
                   paste0("title: \"", title,"\""),
                   paste0("author: \"", author, "\""),
                   paste0("date: \"", date,"\""),
                   "params:",
                   paste(sapply(names(reportData), function(x) paste0("    ", x, ": 0")), collapse="\n"),
                   "---"),
                 con)
      close(con)

      # Append the Master template
      file.append(tmpReport, reportTemplateMaster)

      # Copy any dependancies to the temporary directory
      if (!is.null(reportTemplateImport)) {
        file.copy(reportTemplateImport, file.path(tmpDir, reportTemplateImport), overwrite = TRUE)
      }

      # Render the completed document
      render(input = tmpReport,
             output_format = formatName(),
             output_file = file,
             params = reportData)
    }#,
    #contentType = mimeType()
  )
}
