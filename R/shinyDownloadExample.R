#' @title Run an example Shiny app
#' @description Demonstrate how the package methods work in an example Shiny app
#' @name shinyDownloadExample
#' @param example Character string containing the name of one of the example
#'        Shiny apps. You can choose from "ggplot", "report", "table" or
#'        "panel".
#'        The app will open in "showcase" mode, so the app's source code is also
#'        provided.
#' @details Thanks to Dean Attali for this method which was obtained from
#'          \url{https://deanattali.com/2015/04/21/r-package-shiny-app/}
#' @export
#' @examples
#' \donttest{
#' # For downloading ggplot objects
#' shinyDownloadExample("ggplot")
#'
#' # For downloading compiled rmarkdown documents
#' shinyDownloadExample("report")
#'
#' # For downloading data frame objects
#' shinyDownloadExample("table")
#'
#' # Inspiration for how shinyDownload can be combined with a Bootstrap panel
#' shinyDownloadExample("panel")
#' }
shinyDownloadExample <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples",
                                          package = "shinyDownload"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "shinyDownload")
  shiny::runApp(appDir, display.mode = "showcase")
}
