% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_drivefile.R
\name{gdrive_metadata}
\alias{gdrive_metadata}
\title{Unpack Google Drive Metadata}
\usage{
gdrive_metadata(df, show_details = FALSE)
}
\arguments{
\item{df}{Results from Google Drive drive_ls}

\item{show_details}{Show all metadata fields, default is FALSE}
}
\value{
adds extra meta data to data frame
}
\description{
Google API provides extra metadata stored as a list in the dribble
returned, eg modified time, permissions, owner, etc.
}
\examples{
\dontrun{
library(googledrive)
drive_auth()
fldr <- as_id("<google-folder-id>")
drive_ls(fldr) \%>\% gdrive_metadata()
}
}
