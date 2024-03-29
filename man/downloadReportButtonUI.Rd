% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadReport.R
\name{downloadReportButtonUI}
\alias{downloadReportButtonUI}
\title{Download Report Button (UI)}
\usage{
downloadReportButtonUI(
  id,
  initialFileName,
  placeholder = "Select filename...",
  buttonLabel = "Download report"
)
}
\arguments{
\item{id}{A unique id name for this Shiny object. Should match up with the
\code{id} value for one \code{\link[=downloadReportButtonServer]{downloadReportButtonServer()}}.}

\item{initialFileName}{The default name that will be used for the Filename of
the downloaded file.}

\item{placeholder}{Placeholder text for the filename input. This adds the
placeholder attribute to the HTML input tag.}

\item{buttonLabel}{Text to appear on the download link button.}
}
\value{
A set of options for downloading the report, including filename, file
format and the all-important download button
}
\description{
Creates the user interface for a download button to download a
report
}
