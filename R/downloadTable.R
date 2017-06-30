#' @title Download Table Button (UI)
#' @description Creates the user interface for a download button to download a ggplot
#' @name downloadTableButtonUI
#' @param id A unique id name for this Shiny object
#' @param initialFileName The default name that will be used for the Filename
#'                        of the downloaded file.
#' @return A set of options for downloading the table, including filename,
#'         file format and the all-important download button
#' @export
downloadTableButtonUI <- function(id, initialFileName) {
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
							 						label = "Select format",
							 						choices = list(
							 							`CSV` = "csv",
							 							`CSV for excel` = "excel_csv",
							 							`Text delim.` = "delim",
							 							`Tab delim.` = "tsv",
							 							`RDS (for R)` = "rds"
							 						),
							 						selected = "csv",
							 						selectize = FALSE,
							 						width = "100px"
							 )
				),
				column(3,
							 # TODO: find neater way of filling this space. Don't want this text on
							 # the next line but it's needed to drop the download button down a bit
							 p("Download table:"),
							 downloadButton(ns("download"), "Save Table")
				)
			)
		)
	)
}

#' @title Download Plot Button (server)
#' @description Processes and initiates the download of a table, matrix or data frame.
#' @name downloadTableButton
#' @param input Needed for Shiny
#' @param output Needed for Shiny
#' @param session Needed for Shiny
#' @param dataFrameObject The name of the table, matrix or data frame object that is to be exported
#' @param fileFormat File type format that dataFrameObject will be exported as.
#' A "csv" format uses comma (",") to separate values.
#' A "txt" format will use tab characters ("\\t") to separate the values
#' @return Downloads the table with the inputted filename
#'
#' @importFrom readr write_delim write_csv write_csv write_excel_csv write_tsv
#' @export
downloadTableButton <- function(input, output, session, dataFrameObject) {
	# Determine what the file extension should be
	fileExtension <- reactive({
		return(switch(input$format,
									"delim" = , "tsv" = ".txt",
									"excel_csv" = ".csv",
									paste0(".", input$format)))
	})

	# Determine the application mime type so file formats are recognised
	# mimeType <- reactive({
	# 	return(
	# 		switch(input$format,
	# 					 pdf = "application/pdf",
	# 					 postscript = "application/ps",
	# 					 paste0("image/", input$format) # default for all other formats
	# 		)
	# 	)
	# })

	output$download <- downloadHandler(
		filename = function() return(paste0(input$filename, fileExtension())),
		content = function(file) {
			# Compile a list of arguments to pass to do.call
			a <- list()
			if (input$format == "rds") {
				a$`object` = dataFrameObject
			} else {
				a$`x` = dataFrameObject
			}
			a$`file` = file

			# Decide how the table will be written
			func <- switch(input$format,
			               "csv" = "write.csv",
										 "rds" = "saveRDS",
										 "write.table")
			do.call(func, a)
		}#,
		#contentType = mimeType()
	)
}
