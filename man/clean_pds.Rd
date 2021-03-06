% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_datim.R
\name{clean_pds}
\alias{clean_pds}
\title{Title}
\usage{
clean_pds(df)
}
\arguments{
\item{df}{dataframe from \code{extract_datim()}}
}
\value{
Convert periods from long CY dates to PEPFAR standard FY
}
\description{
Title
}
\examples{
\dontrun{
pds <- c("Jan to Mar 2019", "Oct 2018 to Sep 2019")
clean_pds(pds) }
}
