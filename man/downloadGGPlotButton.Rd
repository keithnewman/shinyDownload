% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadGGPlot.R
\name{downloadGGPlotButton}
\alias{downloadGGPlotButton}
\title{Download Plot Button (server)}
\usage{
downloadGGPlotButton(
  input,
  output,
  session,
  ggplotObject,
  height = NULL,
  width = NULL
)
}
\arguments{
\item{input}{Needed for Shiny}

\item{output}{Needed for Shiny}

\item{session}{Needed for Shiny}

\item{ggplotObject}{The ggplot object to be outputted for download.}

\item{height}{Height of the plot to be outputted}

\item{width}{Width of the plot to be outputted}
}
\value{
Downloads the ggplot with the inputted filename
}
\description{
Processes and initiates the download of a ggplot object.
}
