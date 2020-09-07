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
                                   buttonLabel = "Download report") {
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
          # `.tex` = "tex",
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
#' @param xelatex Specify that the xelatex compiler should be used for PDF
#'        documents instead of the default (pdflatex) compiler. xelatex will
#'        will be useful if you have unicode characters in your report.
#'        This is a logical value, evaluated according to shiny::isTruthy().
#'        This is ignored if the format from the UI selection is not ".pdf".
#' @param toc Logical to determine whether a table of contents is included at
#'        the start of the document. If the target file format is HTML, this
#'        value can also be written as "float" to get a floating table of
#'        contents which is always visible at the side of the screen or
#'        "collapse" for a floating toc showing expandable top-level (H2)
#'        headers.
#' @param ... Arguments to pass to the yaml::as.yaml method, which may be
#'        necessary if params is a data frame and you want to alter the
#'        "column.major" option, overriding the "precision" of floating point
#'        values, and determining whether to allow escaped "unicode" characters.
#' @return Downloads the report with the inputted filename. In the case of a
#'         .tex or .md output, the files will be provided as a .zip folder
#'         because these are intended to be compiled later by the user, who will
#'         need their plots which are attached as separate files.
#'         For all other formats, the downloaded file is in the intended format
#'         with all plots already embedded into the document.
#'         By default, HTML documents compile as a standalone document, meaning
#'         PNG figures are embedded into the standalone HTML file.
#'
#' @export
downloadReportButton <- function(input, output, session,
                                 title = "Report",
                                 author = "shinyDownload",
                                 date = format(Sys.time(), "%d %B %Y"),
                                 reportTemplateMaster,
                                 reportTemplateImport = NULL,
                                 params = NULL,
                                 xelatex = FALSE,
                                 toc = TRUE,
                                 ...) {
  # State if this will be a zip download
  willBeZip <- reactive({
    return(input$format %in% c("tex", "md"))
  })

  # Determine what the file extension should be
  fileExtension <- reactive({
    return(paste0(".", tolower(input$format)))
  })

  # If the file will be .tex or .md, the downloaded file will actually be a .zip
  downloadExtension <- reactive({
    if (willBeZip()) {
      return(".zip")
    } else {
      return(fileExtension())
    }
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
    filename = function() return(paste0(basename(input$filename),
                                        downloadExtension())),
    content = function(file_) {

      # NOTE: Do not delete the tmpDir object when you are done! This belongs
      # to the entire R session, and deleting it causes other things to break!
      tmpDir <- tempdir()
      # Create a temporary file for all this to go in
      tmpReport <- tempfile("report", tmpDir, fileext = ".Rmd")
      ok <- file.create(tmpReport)
      if (!ok)
        stop("Failed to create file ", tmpReport, "!")

      # Create the YAML header
      yamlHead <- list(`title` = title,
                       `author` = author,
                       `date` = date,
                       output = list())

      # Determine the type of table of contents to use
      if (!input$format %in% c("docx", "rtf", "odt")) {
        if (!isTruthy(toc)) {
          tocList <- list(toc = FALSE)
        } else {
          tocList <- list(toc = TRUE)
          if (input$format == "HTML") {
            if (toc == "float") {
              tocList$toc_float <- TRUE
            } else if (toc == "collapse") {
              tocList$toc_float <- list(collapsed = TRUE)
            }
          }
        }
        yamlHead$output[[formatName()]] = tocList
      }

      # Override default latex compiler is xelatex is requested.
      if (input$format == "pdf" && shiny::isTruthy(xelatex)) {
        yamlHead$output[[formatName()]][["latex_engine"]] = "xelatex"
      }

      # Write YAML to the start of the main document
      con <- file(tmpReport)
      writeLines(c("---", yaml::as.yaml(x = yamlHead, ...), "---"), con)
      close(con)

      # Append the Master template
      ok <- file.append(tmpReport, reportTemplateMaster)
      if (!ok)
        stop("Failed to append report template to file ", tmpReport, "!")

      # Copy any dependancies to the temporary directory
      if (!is.null(reportTemplateImport)) {
        # Make subdirectories if necessary
        importPath <- dirname(reportTemplateImport)
        fullImportPath <- file.path(tmpDir, importPath)
        if (importPath != "." && !dir.exists(fullImportPath)) {
          if (!dir.create(fullImportPath, recursive = TRUE)) {
            stop("Failed to create temporary directory at [",
                 fullImportPath,
                 "] to store report template imports!")
          }
        }
        ok <- file.copy(reportTemplateImport,
                        file.path(tmpDir, reportTemplateImport),
                        overwrite = TRUE)
        if (!ok)
          stop("Failed to copy file imports!")
      }

      if (willBeZip()) {
        # Create a temporary folder for this document
        importDir <- file.path(tmpDir, "report_TRENWKW3456nAJV")
        if (!dir.create(importDir)) {
          stop("Failed to create temporary directory at [",
               importDir,
               "] to store report imports!")
        }

        try({
          # NOTE: If we don't render in the current working directory, all
          # relative paths are converted into absolute paths, which we don't
          # want in the final document provided to the user. Therefore, we're
          # temporarily moving into the temp directory, even if it makes me
          # hate myself for resorting to this.
          wd <- setwd(tmpDir)
          fName <- paste0(basename(input$filename), fileExtension())
          out <- rmarkdown::render(input = tmpReport,
                            output_file = fName,
                            params = reportData)
          zip::zip(
            zipfile = file_,
            files = c(fName,
                      paste(basename(input$filename), "files", sep = "_"))
          )
        })
        # Move back to the original working directory and remove the temporary
        # files and directories that we made for this.
        setwd(wd)
        if (dir.exists(importDir)) {
          unlink(importDir, recursive = TRUE)
        }
      } else {
        # Render the completed document directly to the target file_ location.
        rmarkdown::render(input = tmpReport,
                          output_file = file_,
                          params = reportData)
      }
    }
  )
}
