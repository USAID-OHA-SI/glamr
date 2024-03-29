% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{open_path}
\alias{open_path}
\title{Open directory explorer or files}
\usage{
open_path(path)
}
\arguments{
\item{path}{Full path of the file to be opened}
}
\description{
Open directory explorer or files
}
\note{
This assumes default applications are set for various file type
}
\examples{
\dontrun{
 dir_name <- "C:/Users/<username>/Downloads"
 open_path(dir_name)

 file_name <- "C:/Users/<username>/Downloads/test.csv"
 open_path(file_name)
}
}
