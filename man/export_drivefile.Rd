% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_drivefile.R
\name{export_drivefile}
\alias{export_drivefile}
\title{Export local files for googledrive folder}
\usage{
export_drivefile(
  filename,
  to_drive,
  to_folder = NULL,
  add_folder = TRUE,
  overwrite = TRUE,
  ...
)
}
\arguments{
\item{filename}{Character, Full name of the file to be uploaded}

\item{to_drive}{Character, Google drive id}

\item{to_folder}{Character, Google drive sub-folder}

\item{add_folder}{Logical. If TRUE, add sub-folders if they are not present}

\item{overwrite}{Logical. If yes, existing files will be overwritten}

\item{...}{Additional parameters to be passed to `googledrive::drive_upload()`}
}
\value{
Googledrive file(s) id(s)
}
\description{
`r lifecycle::badge("experimental")`

`export_drivefile() is designed to move files googledrive`
}
\examples{
\dontrun{
 library(glamr)

 list.files("./Graphics", "NIGERIA", full.names = TRUE) \%>\%
      export_drivefile(filename = .,
                       to_drive = "<path-id>",
                       to_folder = "FY99Q4/VL Suppression",
                       add_folder = TRUE)

}

}
