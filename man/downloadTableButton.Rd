% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadTable.R
\name{downloadTableButton}
\alias{downloadTableButton}
\title{Download Plot Button (server)}
\usage{
downloadTableButton(input, output, session, dataFrameObject)
}
\arguments{
\item{input}{Needed for Shiny.}

\item{output}{Needed for Shiny.}

\item{session}{Needed for Shiny.}

\item{dataFrameObject}{The name of the table, matrix or data frame object
that is to be exported.}
}
\value{
Downloads the table with the inputted filename
}
\description{
Processes and initiates the download of a table, matrix or data
             frame.
}
