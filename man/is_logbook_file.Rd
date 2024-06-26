% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_dltr.R
\name{is_logbook_file}
\alias{is_logbook_file}
\alias{is_trends_file}
\title{Test Logbook and Trend file format}
\usage{
is_logbook_file(file)

is_trends_file(file)
}
\arguments{
\item{file}{\code{character(1)}, file path to logbook or trend data.}
}
\value{
\code{logical(1)}, \code{TRUE} if the value is readable and has the correct
header (first row).
}
\description{
Test Logbook and Trend file format
}
\examples{
is_logbook_file(system.file("logbook.txt", package = "dltr"))
is_logbook_file(system.file("trends.txt", package = "dltr"))
}
